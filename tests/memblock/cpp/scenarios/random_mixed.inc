// This file is included inside memblock_main.cpp's anonymous namespace.
// Keep scenario-local helpers here; shared infrastructure belongs in memblock_env.hpp.

int run_random_mixed(int argc, char **argv, const Options &options)
{
    const RandomConstraints constraints = resolve_random_constraints(options);
    // The constrained coverage prefix consumes most of a short run. Require
    // enough randomized windows afterwards to exercise producer overlap.
    constexpr unsigned minimum_actions = 3072;
    if (!options.allow_short_mixed && options.transactions < minimum_actions) {
        std::cerr << "MEMBLOCK_RANDOM_MIXED_FAIL seed=" << options.seed
                  << " transactions=0 cycle=0"
                  << " reason=random-mixed_requires_at_least_"
                  << minimum_actions << "_actions\n";
        return 1;
    }

    memblock::Environment environment(argc, argv);
    std::mt19937_64 random(options.seed ^ 0x510e527fade682d1ULL);
    MixedCoverage coverage;
    ConstraintCoverage constraint_coverage;
    std::string phase = "reset";
    unsigned actions = 0;
    std::uint64_t rob_offset = 0;
    std::uint64_t lq_offset = 0;
    std::uint64_t sq_offset = 0;
    std::uint64_t constrained_cold_line = 0;
    std::array<std::uint64_t, 3> cmo_locality_lines{};
    std::uint64_t cmo_overlap_line = 0;
    std::uint64_t cmo_auxiliary_probe_group = 0;
    std::uint64_t atomic_probe_target_line = 0;
    std::uint64_t atomic_auxiliary_probe_group = 0;
    std::uint64_t probe_overlap_line = 0;
    std::uint64_t dcache_error_line = 0;
    std::uint64_t atomic_error_line = 0;
    std::uint64_t ptw_error_line = 0;
    std::uint64_t load_merge_line = 0;
    std::array<std::uint64_t, 4> set_pressure_sequences{};
    std::uint64_t miss_burst_line = 0;
    std::uint64_t bank_conflict_line = 0;
    constexpr std::uint64_t bare_base = memblock::kDefaultMemoryBase + 0x100000;
    constexpr std::uint64_t cache0_base = memblock::kDefaultMemoryBase + 0x200000;
    constexpr std::uint64_t cache1_base =
        memblock::kDefaultMemoryBase + (std::uint64_t{1} << 30) + 0x200000;
    constexpr std::uint64_t nc_base =
        memblock::kDefaultMemoryBase + (std::uint64_t{2} << 30) + 0x200000;
    constexpr std::uint64_t mmio_virtual = 0x50038000ULL;
    constexpr std::uint64_t mmio_physical = 0x90038000ULL;
    constexpr std::uint64_t hypervisor_span = 0x80000;
    const auto hypervisor_virtual_base = [](unsigned spvp, unsigned pbmt,
                                            unsigned topology) {
        return 0x70000000ULL +
            (spvp * RandomConstraints::hypervisor_pbmt_pair_count + pbmt) *
                0x400000ULL +
            topology * 0x100000ULL;
    };
    const auto hypervisor_guest_base = [](unsigned spvp, unsigned pbmt,
                                          unsigned topology) {
        return 0x1000000000ULL +
            (spvp * RandomConstraints::hypervisor_pbmt_pair_count + pbmt) *
                0x400000ULL +
            topology * 0x100000ULL;
    };
    const auto hypervisor_physical_base = [](unsigned pbmt,
                                             unsigned topology) {
        return 0x200000000ULL + pbmt * 0x400000ULL +
            topology * 0x100000ULL;
    };
    const auto hypervisor_pma_device_virtual_base = [](unsigned spvp,
                                                       unsigned topology) {
        return 0x74000000ULL + spvp * 0x400000ULL + topology * 0x100000ULL;
    };
    const auto hypervisor_pma_device_guest_base = [](unsigned spvp,
                                                     unsigned topology) {
        return 0x1040000000ULL + spvp * 0x400000ULL + topology * 0x100000ULL;
    };
    const auto hypervisor_pma_device_physical_base = [](unsigned spvp,
                                                        unsigned topology) {
        return 0x35000000ULL + spvp * 0x400000ULL + topology * 0x100000ULL;
    };
    const auto hypervisor_pmp_virtual_base = [](unsigned spvp,
                                                unsigned topology) {
        return 0x75000000ULL + spvp * 0x400000ULL + topology * 0x100000ULL;
    };
    const auto hypervisor_pmp_guest_base = [](unsigned spvp,
                                              unsigned topology) {
        return 0x1050000000ULL + spvp * 0x400000ULL + topology * 0x100000ULL;
    };
    const auto hypervisor_pmp_physical_base = [](unsigned spvp,
                                                 unsigned topology) {
        return 0x240000000ULL + spvp * 0x400000ULL + topology * 0x100000ULL;
    };
    constexpr std::uint64_t hypervisor_pmp_region_offset = 0x1000;
    constexpr std::uint64_t hypervisor_pmp_region_size = 0x1000;
    struct HypervisorPbmtMapping {
        bool vs_nc;
        bool vs_io;
        bool g_nc;
        bool g_io;
    };
    constexpr std::array<
        HypervisorPbmtMapping, RandomConstraints::hypervisor_pbmt_pair_count>
        hypervisor_pbmt_mappings{{
            {false, false, false, false}, // PMA/PMA
            {false, false, true, false},  // PMA/NC
            {false, false, false, true},  // PMA/IO
            {true, false, false, true},   // NC/IO
            {false, true, true, false},   // IO/NC
        }};
    constexpr std::uint64_t sv39_root = 0x90000000ULL;
    constexpr std::uint64_t atomic_base = cache0_base + 0xa0000;
    constexpr std::uint64_t stage1_napot_base = cache0_base + 0xb0000;
    constexpr std::uint64_t nested_vs_napot_base = cache0_base + 0xc0000;
    constexpr std::uint64_t nested_g_napot_base = cache0_base + 0xd0000;
    constexpr std::uint64_t nested_both_napot_base = cache0_base + 0xe0000;
    constexpr std::uint64_t cmo_base = cache0_base + 0x200000;
    constexpr std::uint64_t cmo_span = 0x1000000;
    constexpr std::uint64_t probe_overlap_base = cmo_base + cmo_span;
    constexpr std::uint64_t probe_overlap_span = 0x4000000;
    constexpr std::uint64_t cmo_auxiliary_probe_base =
        probe_overlap_base + probe_overlap_span / 2;
    constexpr std::uint64_t cmo_auxiliary_probe_span =
        probe_overlap_span / 4;
    constexpr std::uint64_t atomic_auxiliary_probe_base =
        probe_overlap_base + 3 * probe_overlap_span / 4;
    constexpr std::uint64_t atomic_auxiliary_probe_span =
        probe_overlap_span / 4;
    constexpr std::uint64_t dcache_error_base =
        probe_overlap_base + probe_overlap_span;
    constexpr std::uint64_t dcache_error_span = 0x4000000;
    constexpr std::uint64_t atomic_error_base =
        dcache_error_base + dcache_error_span;
    constexpr std::uint64_t atomic_error_span = 0x4000000;
    constexpr std::uint64_t atomic_probe_target_base =
        atomic_error_base + atomic_error_span / 2;
    constexpr std::uint64_t atomic_probe_target_span =
        atomic_error_span / 2;
    constexpr std::uint64_t ptw_error_base =
        atomic_error_base + atomic_error_span;
    constexpr std::uint64_t ptw_error_span = 0x100000;
    constexpr std::uint64_t load_merge_base = cache1_base + 0xe00000;
    constexpr std::uint64_t load_merge_span = 0x4000000;
    constexpr std::uint64_t set_pressure_base =
        load_merge_base + load_merge_span;
    constexpr std::uint64_t set_pressure_span = 0x100000000ULL;
    constexpr std::uint64_t miss_burst_base =
        set_pressure_base + set_pressure_span;
    constexpr std::uint64_t miss_burst_span = 0x40000000ULL;
    // A seed-specific full-period permutation of this dedicated 512-MiB pool
    // supports more than one million three-way conflict actions without
    // reusing a line. Keep it clear of the lower hypervisor physical pools.
    constexpr std::uint64_t bank_conflict_base = 0x400000000ULL;
    constexpr std::uint64_t bank_conflict_span = 0x20000000ULL;
    constexpr std::uint64_t bank_conflict_line_bytes = 64;
    constexpr std::uint64_t bank_conflict_lines =
        bank_conflict_span / bank_conflict_line_bytes;
    static_assert((bank_conflict_lines & (bank_conflict_lines - 1U)) == 0);
    const std::uint64_t bank_conflict_line_offset =
        (options.seed * 0x9e3779b97f4a7c15ULL +
         0xd1b54a32d192ed03ULL) % bank_conflict_lines;
    const std::uint64_t bank_conflict_line_stride =
        ((options.seed * 0xbf58476d1ce4e5b9ULL) ^
         0x94d049bb133111ebULL) % bank_conflict_lines | 1U;
    // Keep the nested final G-stage walk in a different high-level subtree
    // from the implicit G-stage walks used to fetch VS page-table entries.
    constexpr std::uint64_t ptw_error_guest_base = 0x18000000000ULL;
    constexpr std::uint64_t ptw_error_stage1_root_base = 0xf0000000ULL;
    constexpr std::uint64_t ptw_error_vs_root_base = 0xf4000000ULL;
    constexpr std::uint64_t ptw_error_g_root_base = 0xf8000000ULL;
    constexpr std::uint64_t ptw_error_root_stride = 0x20000;
    constexpr std::uint64_t ptw_error_root_slots = 256;
    constexpr std::uint64_t guest_virtual = 0x60000000ULL;
    constexpr std::uint64_t guest_fault_virtual = 0xa0000000ULL;
    constexpr std::uint64_t guest_physical = 0xb0000000ULL;
    constexpr std::uint64_t host_physical = 0xd0000000ULL;
    constexpr std::uint64_t vs_root = 0x96000000ULL;
    constexpr std::uint64_t g_root = 0x97000000ULL;
    constexpr std::array<std::uint64_t, 2> random_stage1_roots{{
        0xe0000000ULL, 0xe1000000ULL}};
    constexpr std::array<std::uint64_t, 2> random_vs_roots{{
        0xe2000000ULL, 0xe3000000ULL}};
    constexpr std::array<std::uint64_t, 2> random_g_roots{{
        0xe4000000ULL, 0xe5000000ULL}};

    environment.memory().fill_incrementing(bare_base, 0x20000, 0x19);
    environment.memory().fill_incrementing(cache0_base, 0x90000, 0x43);
    environment.memory().fill_incrementing(cache1_base, 0x10000, 0x71);
    for (unsigned pbmt = 0; pbmt < hypervisor_pbmt_mappings.size(); ++pbmt) {
        for (unsigned topology = 0; topology < 4; ++topology) {
            environment.memory().fill_incrementing(
                hypervisor_physical_base(pbmt, topology), 0x1000,
                static_cast<unsigned char>(
                    0x35 + pbmt * 0x17 + topology * 0x13));
        }
    }
    for (unsigned spvp = 0; spvp < 2; ++spvp) {
        for (unsigned topology = 0; topology < 4; ++topology) {
            environment.memory().fill_incrementing(
                hypervisor_pma_device_physical_base(spvp, topology), 0x1000,
                static_cast<unsigned char>(0xb5 + spvp * 0x29 +
                                           topology * 0x13));
            environment.memory().fill_incrementing(
                hypervisor_pmp_physical_base(spvp, topology), 0x4000,
                static_cast<unsigned char>(0xd5 + spvp * 0x29 +
                                           topology * 0x13));
        }
    }
    environment.memory().fill_incrementing(nc_base, 0x1000, 0xa3);
    environment.memory().fill_incrementing(mmio_physical, 0x1000, 0xb7);
    environment.memory().fill_incrementing(atomic_base, 0x1000, 0xd3);
    environment.memory().fill_incrementing(stage1_napot_base, 0x40000, 0xe1);
    environment.memory().fill_incrementing(
        ptw_error_base, ptw_error_span, 0xf1);
    environment.memory().fill_incrementing(host_physical, 0x1000, 0xc5);
    environment.configure_backpressure(
        options.seed ^ 0x1f83d9abfb41bd6bULL, options.backpressure);
    const auto load_feedback_summary = [&](const auto &feedback) {
        std::ostringstream stream;
        stream << "load_wakeups=" << feedback.wakeups[0] << ','
               << feedback.wakeups[1] << ',' << feedback.wakeups[2]
               << " load_cancels=" << feedback.ld2_cancels[0] << ','
               << feedback.ld2_cancels[1] << ',' << feedback.ld2_cancels[2];
        return stream.str();
    };
    const auto raw_load_feedback_summary = [&]() {
        const auto &feedback = environment.scalar_load_feedback_stats();
        std::ostringstream stream;
        stream << "raw_load_wakeups=" << feedback.wakeups[0] << ','
               << feedback.wakeups[1] << ',' << feedback.wakeups[2]
               << " raw_load_cancels=" << feedback.ld2_cancels[0] << ','
               << feedback.ld2_cancels[1] << ','
               << feedback.ld2_cancels[2];
        return stream.str();
    };
    const auto total_ifetch_prefetches = [&]() {
        const auto &stats = environment.ifetch_prefetch_stats();
        return std::accumulate(
            stats.requests.begin(), stats.requests.end(), std::uint64_t{0});
    };

    auto make_load = [&](std::uint64_t address, memblock::LoadOp op,
                         unsigned lane) {
        const std::uint64_t rob = rob_offset++;
        const std::uint64_t lq = lq_offset++;
        auto transaction = memblock::LoadTransaction{
            .address = address,
            .op = op,
            .rob = memblock::rob_pointer_value(rob),
            .rob_flag = memblock::rob_pointer_flag(rob),
            .lq = memblock::lq_pointer_value(lq),
            .lq_flag = memblock::lq_pointer_flag(lq),
            .sq = memblock::sq_pointer_value(sq_offset),
            .sq_flag = memblock::sq_pointer_flag(sq_offset),
            .pdest = static_cast<std::uint8_t>(1 + random() % 255),
            .lane = lane,
        };
        transaction.predecode_rvc = (rob % 4U) == 0 || (random() & 3U) == 0;
        transaction.ftq_ptr = random() & 0x3fU;
        transaction.ftq_offset = static_cast<std::uint8_t>(random() & 7U);
        transaction.store_set_hit = (rob % 5U) == 0 || (random() % 5U) == 0;
        transaction.load_wait_bit = rob != 0 &&
            ((rob % 7U) == 0 || (random() % 7U) == 0);
        // Strict wait is coupled to the backend's older-store scheduling;
        // exercise that bit in the isolated metadata contract instead of
        // creating an unbounded wait cycle in a mixed burst.
        transaction.load_wait_strict = false;
        transaction.wait_for_rob_flag = transaction.load_wait_bit &&
            memblock::rob_pointer_flag(rob - 1);
        transaction.wait_for_rob_value = transaction.load_wait_bit
            ? memblock::rob_pointer_value(rob - 1)
            : 0;
        return transaction;
    };
    auto make_store = [&](std::uint64_t address, std::uint64_t data,
                          memblock::StoreOp op, unsigned address_lane,
                          unsigned data_lane) {
        const std::uint64_t rob = rob_offset++;
        const std::uint64_t sq = sq_offset++;
        auto transaction = memblock::StoreTransaction{
            .address = address,
            .data = data,
            .op = op,
            .rob = memblock::rob_pointer_value(rob),
            .rob_flag = memblock::rob_pointer_flag(rob),
            .sq = memblock::sq_pointer_value(sq),
            .sq_flag = memblock::sq_pointer_flag(sq),
            .address_lane = address_lane,
            .data_lane = data_lane,
        };
        return transaction;
    };
    auto make_prefetch = [&](std::uint64_t address, memblock::PrefetchOp op,
                             unsigned lane) {
        const std::uint64_t rob = rob_offset++;
        const std::uint64_t lq = lq_offset++;
        return memblock::PrefetchTransaction{
            .address = address,
            .op = op,
            .rob = memblock::rob_pointer_value(rob),
            .rob_flag = memblock::rob_pointer_flag(rob),
            .lq = memblock::lq_pointer_value(lq),
            .lq_flag = memblock::lq_pointer_flag(lq),
            .sq = memblock::sq_pointer_value(sq_offset),
            .sq_flag = memblock::sq_pointer_flag(sq_offset),
            .lane = lane,
        };
    };
    auto make_vector = [&](bool store, std::uint64_t address, unsigned eew,
                           unsigned lane,
                           memblock::VectorAddressingMode addressing =
                               memblock::VectorAddressingMode::unit_stride) {
        const std::uint64_t rob = rob_offset++;
        const std::uint64_t lq = lq_offset;
        const std::uint64_t sq = sq_offset;
        const std::uint8_t flow_num = addressing ==
                memblock::VectorAddressingMode::unit_stride
            ? 2
            : static_cast<std::uint8_t>(16U >> eew);
        if (store) {
            sq_offset += flow_num;
        } else {
            lq_offset += flow_num;
        }
        memblock::VectorMemoryTransaction transaction{
            .store = store,
            .address = address,
            .addressing = addressing,
            .eew = static_cast<std::uint8_t>(eew),
            .vl = static_cast<std::uint8_t>(16U >> eew),
            .rob = memblock::rob_pointer_value(rob),
            .rob_flag = memblock::rob_pointer_flag(rob),
            .lq = memblock::lq_pointer_value(lq),
            .lq_flag = memblock::lq_pointer_flag(lq),
            .sq = memblock::sq_pointer_value(sq),
            .sq_flag = memblock::sq_pointer_flag(sq),
            .pdest = static_cast<std::uint8_t>(1 + random() % 255),
            .lane = lane,
            .flow_num = flow_num,
        };
        for (auto &byte : transaction.data) {
            byte = static_cast<unsigned char>(random());
        }
        transaction.ftq_ptr = random() & 0x3fU;
        transaction.ftq_offset = static_cast<std::uint8_t>(random() & 7U);
        // VLSU flow_num and vtype/LMUL are coupled by the backend.  The
        // standalone MemBlock boundary has no vset* state, so keep the legal
        // baseline LMUL here; dedicated vector tests may override vlmul with
        // a matching flow shape.
        transaction.vlmul = 0;
        return transaction;
    };
    auto make_atomic = [&](std::uint64_t address, memblock::AtomicOp op,
                           std::uint64_t data) {
        const std::uint64_t rob = rob_offset++;
        return memblock::AtomicTransaction{
            .address = address,
            .op = op,
            .data = data,
            .rob = memblock::rob_pointer_value(rob),
            .rob_flag = memblock::rob_pointer_flag(rob),
            .sq = memblock::sq_pointer_value(sq_offset),
            .sq_flag = memblock::sq_pointer_flag(sq_offset),
            .pdest = static_cast<std::uint8_t>(1 + random() % 255),
            .address_lane = static_cast<unsigned>(random() % 2),
            .data_lane = static_cast<unsigned>(random() % 2),
        };
    };
    auto random_delay = [&](unsigned minimum, unsigned maximum) {
        return minimum + static_cast<unsigned>(random() % (maximum - minimum + 1));
    };
    auto random_bare_address = [&](std::uint64_t region, unsigned alignment,
                                   unsigned span) {
        const std::uint64_t offset = random() % span;
        return region + ((offset / alignment) * alignment);
    };
    auto constrained_locality_line = [&]() {
        unsigned locality = constraints.choose_locality(random());
        for (unsigned candidate = 0;
             candidate < constraint_coverage.locality.size(); ++candidate) {
            if (constraints.locality_weights[candidate] != 0 &&
                constraint_coverage.locality[candidate] == 0) {
                locality = candidate;
                break;
            }
        }
        ++constraint_coverage.locality[locality];
        std::uint64_t line = 0;
        if (locality == 0) {
            line = random() % 32;
        } else if (locality == 1) {
            line = random() % 512;
        } else {
            // Traverse a working set eight times larger than L1D without
            // making the line sequence trivially ascending.
            line = (constrained_cold_line++ * 4051U) % 8192;
        }
        return line;
    };
    auto constrained_cacheable_address = [&] (
        unsigned alignment, const TranslationContext &translation,
        const TranslationLeafTopology &leaf,
        std::optional<bool> force_misaligned = std::nullopt) {
        std::uint64_t line = constrained_locality_line();
        std::uint64_t region_base = cache0_base + 0x10000;
        if (translation.regime == RandomConstraints::translation_stage1 &&
            leaf.stage1_napot) {
            region_base = stage1_napot_base;
        } else if (translation.regime == RandomConstraints::translation_nested) {
            constexpr std::array<std::uint64_t, 4> nested_regions{{
                0,
                nested_g_napot_base,
                nested_vs_napot_base,
                nested_both_napot_base,
            }};
            const std::uint64_t nested_region =
                nested_regions[leaf.nested_index()];
            if (nested_region != 0) {
                region_base = nested_region;
            }
        }
        if (region_base != cache0_base + 0x10000) {
            // Leave 4 KiB on either side for negative strides, indexed
            // elements, and multi-uop segment address expansion.
            line = 64 + line % 896;
        }
        std::uint64_t address = region_base + line * 64;
        const bool selected_misaligned = force_misaligned
            ? *force_misaligned
            : random() % 1000 < constraints.misaligned_per_mille;
        const bool misaligned = alignment > 1 && selected_misaligned;
        if (misaligned && alignment > 1) {
            address += 1 + random() % (alignment - 1);
        } else {
            address += ((random() % 64) / alignment) * alignment;
        }
        return address;
    };
    auto constrained_hypervisor_address = [&] (
        unsigned alignment, const TranslationLeafTopology &leaf,
        unsigned spvp, unsigned pbmt, bool pma_device, bool misaligned,
        unsigned pmp_relation) {
        const unsigned topology = leaf.nested_index();
        if (pmp_relation != RandomConstraints::hypervisor_pmp_none) {
            const std::uint64_t base =
                hypervisor_pmp_virtual_base(spvp, topology);
            const std::uint64_t first = base + hypervisor_pmp_region_offset;
            const std::uint64_t above = first + hypervisor_pmp_region_size;
            switch (pmp_relation) {
            case RandomConstraints::hypervisor_pmp_first:
                return first;
            case RandomConstraints::hypervisor_pmp_last:
                return above - alignment;
            case RandomConstraints::hypervisor_pmp_below:
                return first - alignment;
            case RandomConstraints::hypervisor_pmp_above:
                return above;
            case RandomConstraints::hypervisor_pmp_cross_lower:
                return first - alignment / 2;
            case RandomConstraints::hypervisor_pmp_cross_upper:
                return above - alignment / 2;
            default:
                return std::uint64_t{0};
            }
        }
        const std::uint64_t base = pma_device
            ? hypervisor_pma_device_virtual_base(spvp, topology)
            : hypervisor_virtual_base(spvp, pbmt, topology);
        std::uint64_t address = base +
            constrained_locality_line() * 64;
        if (misaligned) {
            address += 1 + random() % (alignment - 1);
        } else {
            address += ((random() % 64) / alignment) * alignment;
        }
        return address;
    };
    auto constrained_window_address = [&](std::uint64_t base,
                                          unsigned alignment) {
        if (alignment <= 1) {
            return base + random() % 64;
        }
        const bool misaligned =
            random() % 1000 < constraints.misaligned_per_mille;
        if (!misaligned) {
            return base + ((random() % 64) / alignment) * alignment;
        }
        unsigned offset = 0;
        do {
            offset = random() % 64;
        } while ((offset & (alignment - 1)) == 0);
        return base + offset;
    };
    const unsigned constrained_completion_timeout =
        constraints.response_latency.dcache == memblock::ResponseLatencyProfile::spec ||
        constraints.response_latency.ptw == memblock::ResponseLatencyProfile::spec ||
        constraints.response_latency.uncache == memblock::ResponseLatencyProfile::spec
        ? 16384 : 2048;
    auto issue_load = [&](const memblock::LoadTransaction &transaction,
                          std::optional<std::uint64_t> expected = std::nullopt,
                          bool count_action = true) {
        if (expected) {
            environment.expect_load_data(transaction, *expected);
        } else {
            environment.expect_load(transaction);
        }
        if (!environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction) ||
            !environment.run_until_all_complete(constrained_completion_timeout)) {
            return false;
        }
        if (count_action) {
            coverage.sample(transaction);
            ++actions;
        }
        return true;
    };
    auto issue_vector = [&](
                            const memblock::VectorMemoryTransaction &transaction,
                            std::optional<std::array<unsigned char, 16>> expected =
                                std::nullopt) {
        if (expected) {
            environment.expect_vector_data(transaction, *expected);
        } else {
            environment.expect_vector(transaction);
        }
        bool requires_misaligned_head = false;
        bool requires_store_pending = false;
        const unsigned element_bytes = 1U << transaction.eew;
        const std::uint16_t active =
            memblock::active_vector_elements(transaction);
        for (unsigned element = 0;
             element < 16U / element_bytes; ++element) {
            if (((active >> element) & 1U) == 0) {
                continue;
            }
            const std::uint64_t address =
                memblock::vector_element_address(transaction, element);
            requires_misaligned_head |= !transaction.store &&
                ((address & 0xfU) + element_bytes) > 16U;
            requires_store_pending |= transaction.store &&
                (address & (element_bytes - 1)) != 0;
        }
        if (((requires_misaligned_head || requires_store_pending) &&
             !environment.set_rob_head(
                 transaction.rob, transaction.rob_flag)) ||
            !environment.enqueue_vector(transaction) ||
            !environment.issue_vector(transaction) ||
            (requires_store_pending &&
             (!environment.run_cycles(32) ||
              !environment.pulse_pending_store(
                  transaction.rob, transaction.rob_flag))) ||
            !environment.run_until_vector_complete_with_replays(
                transaction, constrained_completion_timeout,
                requires_store_pending)) {
            return false;
        }
        coverage.sample(transaction);
        ++actions;
        return true;
    };
    auto issue_store = [&](const memblock::StoreTransaction &transaction,
                           bool data_first,
                           bool hold_pending_store = false) {
        environment.expect_store(transaction);
        const bool issued = environment.enqueue_store(
                                transaction,
                                memblock::lq_pointer_value(lq_offset)) &&
                            (data_first
                                 ? environment.issue_store_data(transaction) &&
                                       environment.issue_store_address(transaction)
                                 : environment.issue_store_address(transaction) &&
                                       environment.issue_store_data(transaction));
        if (!issued || !environment.run_until_store_complete_with_replay(
                           transaction, constrained_completion_timeout,
                           hold_pending_store)) {
            return false;
        }
        coverage.sample(transaction, data_first);
        ++actions;
        return true;
    };
    auto scalar_forward_value = [](std::uint64_t data, unsigned size,
                                   bool is_unsigned) {
        const unsigned bits = size * 8;
        const std::uint64_t mask = bits == 64
            ? ~std::uint64_t{0}
            : (std::uint64_t{1} << bits) - 1;
        const std::uint64_t raw = data & mask;
        return is_unsigned ? raw : memblock::sign_extend(raw, bits);
    };
    auto atomic_writeback = [](std::uint64_t old_value, bool word) {
        return word
            ? memblock::sign_extend(static_cast<std::uint32_t>(old_value), 32)
            : old_value;
    };
    auto atomic_result = [](memblock::AtomicOp op, std::uint64_t old_value,
                            std::uint64_t operand) {
        const auto replace_word = [&](std::uint32_t value) {
            return (old_value & ~std::uint64_t{0xffffffff}) | value;
        };
        const std::uint32_t old_word = static_cast<std::uint32_t>(old_value);
        const std::uint32_t operand_word = static_cast<std::uint32_t>(operand);
        switch (op) {
        case memblock::AtomicOp::amoadd_w:
            return replace_word(old_word + operand_word);
        case memblock::AtomicOp::amoxor_w:
            return replace_word(old_word ^ operand_word);
        case memblock::AtomicOp::amoand_w:
            return replace_word(old_word & operand_word);
        case memblock::AtomicOp::amoor_w:
            return replace_word(old_word | operand_word);
        case memblock::AtomicOp::amoswap_w:
            return replace_word(operand_word);
        case memblock::AtomicOp::amomin_w:
            return replace_word(
                static_cast<std::int32_t>(old_word) <
                        static_cast<std::int32_t>(operand_word)
                    ? old_word : operand_word);
        case memblock::AtomicOp::amomax_w:
            return replace_word(
                static_cast<std::int32_t>(old_word) >
                        static_cast<std::int32_t>(operand_word)
                    ? old_word : operand_word);
        case memblock::AtomicOp::amominu_w:
            return replace_word(std::min(old_word, operand_word));
        case memblock::AtomicOp::amomaxu_w:
            return replace_word(std::max(old_word, operand_word));
        case memblock::AtomicOp::amoadd_d:
            return old_value + operand;
        case memblock::AtomicOp::amoxor_d:
            return old_value ^ operand;
        case memblock::AtomicOp::amoand_d:
            return old_value & operand;
        case memblock::AtomicOp::amoor_d:
            return old_value | operand;
        case memblock::AtomicOp::amoswap_d:
            return operand;
        case memblock::AtomicOp::amomin_d:
            return static_cast<std::int64_t>(old_value) <
                    static_cast<std::int64_t>(operand)
                ? old_value : operand;
        case memblock::AtomicOp::amomax_d:
            return static_cast<std::int64_t>(old_value) >
                    static_cast<std::int64_t>(operand)
                ? old_value : operand;
        case memblock::AtomicOp::amominu_d:
            return std::min(old_value, operand);
        case memblock::AtomicOp::amomaxu_d:
            return std::max(old_value, operand);
        default:
            return old_value;
        }
    };
    auto overlay_vector_store = [&](const memblock::VectorMemoryTransaction &store,
                                    const memblock::VectorMemoryTransaction &load) {
        auto expected = environment.memory().expected_vector_load(load);
        std::unordered_map<std::uint64_t, unsigned char> forwarded;
        const unsigned store_bytes = 1U << store.eew;
        const unsigned store_elements = 16U / store_bytes;
        const std::uint16_t store_active = memblock::active_vector_elements(store);
        for (unsigned element = 0; element < store_elements; ++element) {
            if (((store_active >> element) & 1U) == 0) {
                continue;
            }
            const std::uint64_t address =
                memblock::vector_element_address(store, element);
            for (unsigned byte = 0; byte < store_bytes; ++byte) {
                forwarded[address + byte] =
                    store.data[element * store_bytes + byte];
            }
        }
        const unsigned load_bytes = 1U << load.eew;
        const unsigned load_elements = 16U / load_bytes;
        const std::uint16_t load_active = memblock::active_vector_elements(load);
        for (unsigned element = 0; element < load_elements; ++element) {
            if (((load_active >> element) & 1U) == 0) {
                continue;
            }
            const std::uint64_t address =
                memblock::vector_element_address(load, element);
            for (unsigned byte = 0; byte < load_bytes; ++byte) {
                const auto value = forwarded.find(address + byte);
                if (value != forwarded.end()) {
                    expected[element * load_bytes + byte] = value->second;
                }
            }
        }
        return expected;
    };
    auto randomize_vector_addressing = [&](memblock::VectorMemoryTransaction &transaction) {
        const unsigned element_bytes = 1U << transaction.eew;
        const unsigned elements = 16U >> transaction.eew;
        if (transaction.addressing == memblock::VectorAddressingMode::strided) {
            if (transaction.store) {
                const std::array<std::int64_t, 5> stride_classes{{
                    -static_cast<std::int64_t>(2U * element_bytes),
                    -static_cast<std::int64_t>(element_bytes),
                    static_cast<std::int64_t>(element_bytes),
                    static_cast<std::int64_t>(2U * element_bytes),
                    static_cast<std::int64_t>(4U * element_bytes),
                }};
                transaction.stride =
                    stride_classes[random() % stride_classes.size()];
            } else {
                const std::array<std::int64_t, 6> stride_classes{{
                    -static_cast<std::int64_t>(2U * element_bytes),
                    -static_cast<std::int64_t>(element_bytes),
                    0,
                    static_cast<std::int64_t>(element_bytes),
                    static_cast<std::int64_t>(2U * element_bytes),
                    static_cast<std::int64_t>(4U * element_bytes),
                }};
                transaction.stride =
                    stride_classes[random() % stride_classes.size()];
            }
            return;
        }
        if (transaction.addressing != memblock::VectorAddressingMode::indexed_unordered &&
            transaction.addressing != memblock::VectorAddressingMode::indexed_ordered) {
            return;
        }
        std::vector<unsigned> offset_slots(256U / element_bytes);
        std::iota(offset_slots.begin(), offset_slots.end(), 0U);
        if (transaction.store) {
            std::shuffle(offset_slots.begin(), offset_slots.end(), random);
        }
        for (unsigned element = 0; element < elements; ++element) {
            const std::uint64_t slot = transaction.store
                ? offset_slots[element]
                : random() % offset_slots.size();
            const std::uint64_t offset = slot * element_bytes;
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                transaction.index[element * element_bytes + byte] =
                    static_cast<unsigned char>(offset >> (8 * byte));
            }
        }
    };

    const auto page_mode = [](unsigned index) {
        return index == 0
            ? memblock::ReferencePageMode::sv39
            : memblock::ReferencePageMode::sv48;
    };
    const auto map_stage_page = [&](unsigned mode, std::uint64_t root,
                                    std::uint64_t virtual_address,
                                    std::uint64_t physical_address,
                                    bool noncacheable = false,
                                    bool io = false,
                                    bool executable = false,
                                    bool user = false) {
        return mode == 0
            ? environment.map_sv39_4k(
                  virtual_address, physical_address, root, true, true,
                  executable, user, noncacheable, io)
            : environment.map_sv48_4k(
                  virtual_address, physical_address, root, true, true,
                  executable, user, noncacheable, io);
    };
    const auto map_g_page = [&](unsigned mode, std::uint64_t root,
                                std::uint64_t guest_physical_address,
                                std::uint64_t host_physical_address,
                                bool executable = false,
                                bool noncacheable = false,
                                bool io = false) {
        return mode == 0
            ? environment.map_sv39x4_leaf(
                  guest_physical_address, host_physical_address, 0, root,
                  true, true, executable, true, std::nullopt, true,
                  noncacheable, io)
            : environment.map_sv48x4_leaf(
                  guest_physical_address, host_physical_address, 0, root,
                  true, true, executable, true, std::nullopt, true,
                  noncacheable, io);
    };
    const auto map_stage_2m = [&](unsigned mode, std::uint64_t root,
                                  std::uint64_t address) {
        return mode == 0
            ? environment.map_sv39_2m(
                  address, address, root, true, true, true)
            : environment.map_sv48_2m(
                  address, address, root, true, true, true);
    };
    const auto map_g_2m = [&](unsigned mode, std::uint64_t root,
                              std::uint64_t address) {
        return mode == 0
            ? environment.map_sv39x4_2m(
                  address, address, root, true, true, true)
            : environment.map_sv48x4_2m(
                  address, address, root, true, true, true);
    };
    const auto map_stage_napot = [&](
                                     unsigned mode, std::uint64_t root,
                                     std::uint64_t virtual_base,
                                     std::optional<std::uint64_t> physical_base =
                                         std::nullopt,
                                     bool user = false,
                                     bool noncacheable = false,
                                     bool io = false) {
        const std::uint64_t physical = physical_base.value_or(virtual_base);
        return mode == 0
            ? environment.map_sv39_napot64k(
                  virtual_base, physical, root, true, true, true, user,
                  noncacheable, true, std::nullopt, io)
            : environment.map_sv48_napot64k(
                  virtual_base, physical, root, true, true, true, user,
                  noncacheable, true, std::nullopt, io);
    };
    const auto map_g_napot = [&](
                                 unsigned mode, std::uint64_t root,
                                 std::uint64_t guest_physical_base,
                                 std::optional<std::uint64_t> host_physical_base =
                                     std::nullopt,
                                 bool noncacheable = false,
                                 bool io = false) {
        const std::uint64_t physical =
            host_physical_base.value_or(guest_physical_base);
        return mode == 0
            ? environment.map_sv39x4_napot64k(
                  guest_physical_base, physical, root,
                  true, true, true, true, std::nullopt, true,
                  noncacheable, io)
            : environment.map_sv48x4_napot64k(
                  guest_physical_base, physical, root,
                  true, true, true, true, std::nullopt, true,
                  noncacheable, io);
    };
    const auto prepare_random_translation_contexts = [&]() {
        const auto map_all_contexts = [&](std::uint64_t virtual_address,
                                          std::uint64_t physical_address,
                                          bool noncacheable = false,
                                          bool io = false,
                                          bool executable = false) {
            for (unsigned mode = 0; mode < 2; ++mode) {
                if (!map_stage_page(
                        mode, random_stage1_roots[mode], virtual_address,
                        physical_address, noncacheable, io, executable) ||
                    !map_stage_page(
                        mode, random_vs_roots[mode], virtual_address,
                        physical_address, noncacheable, io, executable) ||
                    !map_g_page(
                        mode, random_g_roots[mode], physical_address,
                        physical_address, executable)) {
                    return false;
                }
            }
            return true;
        };

        // Include a page of padding for negative vector strides and a page
        // beyond the nominal cold set for indexed/split vector accesses.
        for (std::uint64_t page = cache0_base + 0xf000;
             page <= cache0_base + 0x92000; page += 0x1000) {
            if (!map_all_contexts(page, page, false, false, true)) {
                return false;
            }
        }
        for (std::uint64_t page = stage1_napot_base;
             page < stage1_napot_base + 0x40000; page += 0x1000) {
            if (!map_all_contexts(page, page, false, false, true)) {
                return false;
            }
        }
        for (std::uint64_t page = ptw_error_base;
             page < ptw_error_base + ptw_error_span; page += 0x1000) {
            const std::uint64_t guest_page =
                ptw_error_guest_base + (page - ptw_error_base);
            for (unsigned mode = 0; mode < 2; ++mode) {
                if (!map_stage_page(
                        mode, random_stage1_roots[mode], page, page,
                        false, false, true) ||
                    !map_stage_page(
                        mode, random_vs_roots[mode], page, guest_page,
                        false, false, true) ||
                    !map_g_page(
                        mode, random_g_roots[mode], page, page, true) ||
                    !map_g_page(
                        mode, random_g_roots[mode], guest_page, page, true)) {
                    return false;
                }
            }
        }
        for (unsigned mode = 0; mode < 2; ++mode) {
            if (!map_stage_napot(
                    mode, random_stage1_roots[mode], stage1_napot_base) ||
                !map_stage_napot(
                    mode, random_vs_roots[mode], nested_vs_napot_base) ||
                !map_stage_napot(
                    mode, random_vs_roots[mode], nested_both_napot_base) ||
                !map_g_napot(
                    mode, random_g_roots[mode], nested_g_napot_base) ||
                !map_g_napot(
                    mode, random_g_roots[mode], nested_both_napot_base)) {
                return false;
            }
        }
        for (std::uint64_t block = cmo_base;
             block < cmo_base + cmo_span; block += 0x200000) {
            for (unsigned mode = 0; mode < 2; ++mode) {
                if (!map_stage_2m(mode, random_stage1_roots[mode], block) ||
                    !map_stage_2m(mode, random_vs_roots[mode], block) ||
                    !map_g_2m(mode, random_g_roots[mode], block)) {
                    return false;
                }
            }
        }
        for (std::uint64_t block = probe_overlap_base;
             block < probe_overlap_base + probe_overlap_span;
             block += 0x200000) {
            for (unsigned mode = 0; mode < 2; ++mode) {
                if (!map_stage_2m(mode, random_stage1_roots[mode], block) ||
                    !map_stage_2m(mode, random_vs_roots[mode], block) ||
                    !map_g_2m(mode, random_g_roots[mode], block)) {
                    return false;
                }
            }
        }
        for (std::uint64_t block = dcache_error_base;
             block < dcache_error_base + dcache_error_span;
             block += 0x200000) {
            for (unsigned mode = 0; mode < 2; ++mode) {
                if (!map_stage_2m(mode, random_stage1_roots[mode], block) ||
                    !map_stage_2m(mode, random_vs_roots[mode], block) ||
                    !map_g_2m(mode, random_g_roots[mode], block)) {
                    return false;
                }
            }
        }
        for (std::uint64_t block = atomic_error_base;
             block < atomic_error_base + atomic_error_span;
             block += 0x200000) {
            for (unsigned mode = 0; mode < 2; ++mode) {
                if (!map_stage_2m(mode, random_stage1_roots[mode], block) ||
                    !map_stage_2m(mode, random_vs_roots[mode], block) ||
                    !map_g_2m(mode, random_g_roots[mode], block)) {
                    return false;
                }
            }
        }
        for (std::uint64_t block = load_merge_base;
             block < load_merge_base + load_merge_span;
             block += 0x200000) {
            for (unsigned mode = 0; mode < 2; ++mode) {
                if (!map_stage_2m(mode, random_stage1_roots[mode], block) ||
                    !map_stage_2m(mode, random_vs_roots[mode], block) ||
                    !map_g_2m(mode, random_g_roots[mode], block)) {
                    return false;
                }
            }
        }
        for (std::uint64_t block = set_pressure_base;
             block < set_pressure_base + set_pressure_span;
             block += 0x200000) {
            for (unsigned mode = 0; mode < 2; ++mode) {
                if (!map_stage_2m(mode, random_stage1_roots[mode], block) ||
                    !map_stage_2m(mode, random_vs_roots[mode], block) ||
                    !map_g_2m(mode, random_g_roots[mode], block)) {
                    return false;
                }
            }
        }
        for (std::uint64_t block = miss_burst_base;
             block < miss_burst_base + miss_burst_span;
             block += 0x200000) {
            for (unsigned mode = 0; mode < 2; ++mode) {
                if (!map_stage_2m(mode, random_stage1_roots[mode], block) ||
                    !map_stage_2m(mode, random_vs_roots[mode], block) ||
                    !map_g_2m(mode, random_g_roots[mode], block)) {
                    return false;
                }
            }
        }
        for (std::uint64_t block = bank_conflict_base;
             block < bank_conflict_base + bank_conflict_span;
             block += 0x200000) {
            for (unsigned mode = 0; mode < 2; ++mode) {
                if (!map_stage_2m(mode, random_stage1_roots[mode], block) ||
                    !map_stage_2m(mode, random_vs_roots[mode], block) ||
                    !map_g_2m(mode, random_g_roots[mode], block)) {
                    return false;
                }
            }
        }
        if (!map_all_contexts(
                cache1_base + 0xf000, cache1_base + 0xf000,
                false, false, true) ||
            !map_all_contexts(atomic_base, atomic_base, false, false, true) ||
            !map_all_contexts(nc_base, nc_base, true, false)) {
            return false;
        }
        for (unsigned spvp = 0; spvp < 2; ++spvp) {
            for (unsigned pbmt = 0; pbmt < hypervisor_pbmt_mappings.size();
                 ++pbmt) {
                const auto mapping = hypervisor_pbmt_mappings[pbmt];
                for (unsigned topology = 0; topology < 4; ++topology) {
                    const bool vs_napot = (topology & 2U) != 0;
                    const bool g_napot = (topology & 1U) != 0;
                    for (unsigned mode = 0; mode < 2; ++mode) {
                        for (std::uint64_t offset = 0;
                             offset < hypervisor_span; offset += 0x10000) {
                            const std::uint64_t virtual_base =
                                hypervisor_virtual_base(spvp, pbmt, topology) +
                                offset;
                            const std::uint64_t guest_base =
                                hypervisor_guest_base(spvp, pbmt, topology) +
                                offset;
                            const std::uint64_t physical_base =
                                hypervisor_physical_base(pbmt, topology) + offset;
                            if (vs_napot) {
                                if (!map_stage_napot(
                                        mode, random_vs_roots[mode],
                                        virtual_base, guest_base, spvp != 0,
                                        mapping.vs_nc, mapping.vs_io)) {
                                    return false;
                                }
                            } else {
                                for (std::uint64_t page = 0; page < 0x10000;
                                     page += 0x1000) {
                                    if (!map_stage_page(
                                            mode, random_vs_roots[mode],
                                            virtual_base + page,
                                            guest_base + page, mapping.vs_nc,
                                            mapping.vs_io, true, spvp != 0)) {
                                        return false;
                                    }
                                }
                            }
                            if (g_napot) {
                                if (!map_g_napot(
                                        mode, random_g_roots[mode], guest_base,
                                        physical_base, mapping.g_nc,
                                        mapping.g_io)) {
                                    return false;
                                }
                            } else {
                                for (std::uint64_t page = 0; page < 0x10000;
                                     page += 0x1000) {
                                    if (!map_g_page(
                                            mode, random_g_roots[mode],
                                            guest_base + page,
                                            physical_base + page, true,
                                            mapping.g_nc, mapping.g_io)) {
                                        return false;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        for (unsigned spvp = 0; spvp < 2; ++spvp) {
            for (unsigned topology = 0; topology < 4; ++topology) {
                const bool vs_napot = (topology & 2U) != 0;
                const bool g_napot = (topology & 1U) != 0;
                for (unsigned mode = 0; mode < 2; ++mode) {
                    for (std::uint64_t offset = 0;
                         offset < hypervisor_span; offset += 0x10000) {
                        const std::uint64_t virtual_base =
                            hypervisor_pma_device_virtual_base(spvp, topology) +
                            offset;
                        const std::uint64_t guest_base =
                            hypervisor_pma_device_guest_base(spvp, topology) +
                            offset;
                        const std::uint64_t physical_base =
                            hypervisor_pma_device_physical_base(spvp, topology) +
                            offset;
                        if (vs_napot) {
                            if (!map_stage_napot(
                                    mode, random_vs_roots[mode], virtual_base,
                                    guest_base, spvp != 0)) {
                                return false;
                            }
                        } else {
                            for (std::uint64_t page = 0; page < 0x10000;
                                 page += 0x1000) {
                                if (!map_stage_page(
                                        mode, random_vs_roots[mode],
                                        virtual_base + page, guest_base + page,
                                        false, false, true, spvp != 0)) {
                                    return false;
                                }
                            }
                        }
                        if (g_napot) {
                            if (!map_g_napot(
                                    mode, random_g_roots[mode], guest_base,
                                    physical_base)) {
                                return false;
                            }
                        } else {
                            for (std::uint64_t page = 0; page < 0x10000;
                                 page += 0x1000) {
                                if (!map_g_page(
                                        mode, random_g_roots[mode],
                                        guest_base + page, physical_base + page,
                                        true)) {
                                    return false;
                                }
                            }
                        }
                    }
                }
            }
        }
        for (unsigned spvp = 0; spvp < 2; ++spvp) {
            for (unsigned topology = 0; topology < 4; ++topology) {
                const bool vs_napot = (topology & 2U) != 0;
                const bool g_napot = (topology & 1U) != 0;
                for (unsigned mode = 0; mode < 2; ++mode) {
                    for (std::uint64_t offset = 0;
                         offset < hypervisor_span; offset += 0x10000) {
                        const std::uint64_t virtual_base =
                            hypervisor_pmp_virtual_base(spvp, topology) +
                            offset;
                        const std::uint64_t guest_base =
                            hypervisor_pmp_guest_base(spvp, topology) + offset;
                        const std::uint64_t physical_base =
                            hypervisor_pmp_physical_base(spvp, topology) +
                            offset;
                        if (vs_napot) {
                            if (!map_stage_napot(
                                    mode, random_vs_roots[mode], virtual_base,
                                    guest_base, spvp != 0)) {
                                return false;
                            }
                        } else {
                            for (std::uint64_t page = 0; page < 0x10000;
                                 page += 0x1000) {
                                if (!map_stage_page(
                                        mode, random_vs_roots[mode],
                                        virtual_base + page, guest_base + page,
                                        false, false, true, spvp != 0)) {
                                    return false;
                                }
                            }
                        }
                        if (g_napot) {
                            if (!map_g_napot(
                                    mode, random_g_roots[mode], guest_base,
                                    physical_base)) {
                                return false;
                            }
                        } else {
                            for (std::uint64_t page = 0; page < 0x10000;
                                 page += 0x1000) {
                                if (!map_g_page(
                                        mode, random_g_roots[mode],
                                        guest_base + page,
                                        physical_base + page, true)) {
                                    return false;
                                }
                            }
                        }
                    }
                }
            }
        }
        for (unsigned mode = 0; mode < 2; ++mode) {
            if (!map_stage_page(
                    mode, random_stage1_roots[mode], mmio_virtual,
                    mmio_physical, false, true) ||
                !map_stage_page(
                    mode, random_vs_roots[mode], mmio_virtual,
                    mmio_physical, false, true) ||
                !map_g_page(
                    mode, random_g_roots[mode], mmio_physical,
                    mmio_physical)) {
                return false;
            }
        }

        // G-stage must translate implicit accesses to every VS page-table
        // page. Keep these mappings at 4 KiB: this both matches ordinary
        // page-table memory and avoids making an implicit PTE access depend
        // on a separate G-stage superpage behavior.
        for (unsigned g_mode = 0; g_mode < 2; ++g_mode) {
            for (const std::uint64_t vs_page_table : random_vs_roots) {
                for (std::uint64_t page = vs_page_table;
                     page < vs_page_table + 0x200000; page += 0x1000) {
                    if (!map_g_page(
                            g_mode, random_g_roots[g_mode], page, page)) {
                        return false;
                    }
                }
            }
        }
        std::vector<std::pair<std::uint64_t, std::uint64_t>>
            reference_addresses{{
                {cache0_base + 0x106a8, cache0_base + 0x106a8},
                {cache1_base + 0xf800, cache1_base + 0xf800},
                {nc_base + 0x188, nc_base + 0x188},
                {mmio_virtual + 0x188, mmio_physical + 0x188},
                {ptw_error_base + 0x188, ptw_error_base + 0x188},
                {load_merge_base + 0x188, load_merge_base + 0x188},
                {set_pressure_base + 0x188, set_pressure_base + 0x188},
            }};
        for (unsigned spvp = 0; spvp < 2; ++spvp) {
            for (unsigned pbmt = 0; pbmt < hypervisor_pbmt_mappings.size();
                 ++pbmt) {
                for (unsigned topology = 0; topology < 4; ++topology) {
                    const std::uint64_t offset =
                        ((spvp * hypervisor_pbmt_mappings.size() + pbmt +
                          topology) % 8) *
                            0x10000ULL +
                        0x188;
                    reference_addresses.emplace_back(
                        hypervisor_virtual_base(spvp, pbmt, topology) + offset,
                        hypervisor_physical_base(pbmt, topology) + offset);
                }
            }
            for (unsigned topology = 0; topology < 4; ++topology) {
                const std::uint64_t offset =
                    ((spvp + topology) % 8) * 0x10000ULL + 0x188;
                reference_addresses.emplace_back(
                    hypervisor_pma_device_virtual_base(spvp, topology) + offset,
                    hypervisor_pma_device_physical_base(spvp, topology) + offset);
                reference_addresses.emplace_back(
                    hypervisor_pmp_virtual_base(spvp, topology) +
                        hypervisor_pmp_region_offset + 0x188,
                    hypervisor_pmp_physical_base(spvp, topology) +
                        hypervisor_pmp_region_offset + 0x188);
            }
        }
        for (unsigned vs_mode = 0; vs_mode < 2; ++vs_mode) {
            for (unsigned g_mode = 0; g_mode < 2; ++g_mode) {
                for (const auto &[virtual_address, physical_address] :
                     reference_addresses) {
                    const auto walk = memblock::reference_two_stage_walk(
                        environment.memory(), random_vs_roots[vs_mode],
                        random_g_roots[g_mode], virtual_address,
                        page_mode(vs_mode), page_mode(g_mode));
                    if (!walk.translated ||
                        walk.physical_address != physical_address) {
                        std::ostringstream detail;
                        detail << "random-translation-reference"
                               << ":vs=" << vs_mode << ":g=" << g_mode
                               << ":va=0x" << std::hex << virtual_address
                               << ":expected=0x" << physical_address
                               << ":actual=0x" << walk.physical_address;
                        phase = detail.str();
                        return false;
                    }
                }
            }
        }
        for (unsigned mode = 0; mode < 2; ++mode) {
            const auto walk = memblock::reference_page_walk(
                environment.memory(), random_stage1_roots[mode],
                stage1_napot_base + 0xa188, page_mode(mode));
            if (!walk.translated ||
                walk.physical_address != stage1_napot_base + 0xa188) {
                phase = "random-stage1-napot-reference";
                return false;
            }
        }
        constexpr std::array<std::uint64_t, 4> nested_leaf_regions{{
            cache0_base + 0x10000,
            nested_g_napot_base,
            nested_vs_napot_base,
            nested_both_napot_base,
        }};
        for (unsigned vs_mode = 0; vs_mode < 2; ++vs_mode) {
            for (unsigned g_mode = 0; g_mode < 2; ++g_mode) {
                for (unsigned topology = 0;
                     topology < nested_leaf_regions.size(); ++topology) {
                    const std::uint64_t address =
                        nested_leaf_regions[topology] + 0xa188;
                    const auto walk = memblock::reference_two_stage_walk(
                        environment.memory(), random_vs_roots[vs_mode],
                        random_g_roots[g_mode], address,
                        page_mode(vs_mode), page_mode(g_mode));
                    if (!walk.translated || walk.physical_address != address) {
                        phase = "random-nested-napot-reference";
                        return false;
                    }
                }
            }
        }
        return true;
    };
    const auto activate_translation_context = [&](const TranslationContext &context) {
        if (context.regime == RandomConstraints::translation_bare) {
            return environment.activate_bare();
        }
        if (context.regime == RandomConstraints::translation_stage1) {
            return context.stage1_mode == 0
                ? environment.activate_sv39(
                      random_stage1_roots[context.stage1_mode], 23)
                : environment.activate_sv48(
                      random_stage1_roots[context.stage1_mode], 23);
        }
        return environment.activate_two_stage_modes(
                   page_mode(context.vs_mode), page_mode(context.g_mode),
                   random_vs_roots[context.vs_mode],
                   random_g_roots[context.g_mode], 29, 31) &&
            environment.set_page_based_memory_types(true, true);
    };

    const bool completed = [&]() {
        if (!environment.reset() || !environment.enable_misaligned_accesses()) {
            return false;
        }
        if (constraints.uncache_error_per_mille != 0 ||
            constraints.dcache_load_error_per_mille != 0 ||
            constraints.atomic_error_per_mille != 0) {
            environment.configure_cache_error_enable(true);
            if (!environment.run_cycles(4)) {
                return false;
            }
        }

        phase = "bare-heterogeneous-wave";
        std::vector<memblock::LoadTransaction> scalar_wave;
        std::vector<unsigned> scalar_lanes(memblock::kScalarLoadLanes);
        std::iota(scalar_lanes.begin(), scalar_lanes.end(), 0U);
        std::shuffle(scalar_lanes.begin(), scalar_lanes.end(), random);
        for (const unsigned lane : scalar_lanes) {
            const auto op = static_cast<memblock::LoadOp>(random() % 7);
            const unsigned size = 1U << (static_cast<unsigned>(op) & 3U);
            scalar_wave.push_back(make_load(
                random_bare_address(bare_base + 0x200, size, 0x1000 - size),
                op, lane));
            environment.expect_load(scalar_wave.back());
            if (!environment.enqueue_load(scalar_wave.back())) {
                return false;
            }
        }
        std::array<memblock::VectorMemoryTransaction, 2> vector_wave{
            make_vector(false, bare_base + 0x1000 + (random() % 64) * 16,
                        random() % 4, random() % memblock::kVectorMemoryLanes),
            make_vector(false, bare_base + 0x3000 + (random() % 128) * 8,
                        random() % 4, random() % memblock::kVectorMemoryLanes),
        };
        for (auto &transaction : vector_wave) {
            const unsigned elements = 16U >> transaction.eew;
            transaction.vl = static_cast<std::uint8_t>(
                1U + random() % elements);
            transaction.vm = (random() & 1U) != 0;
            transaction.mask_bits = static_cast<std::uint16_t>(random());
        }
        for (auto &transaction : vector_wave) {
            environment.expect_vector(transaction);
            if (!environment.enqueue_vector(transaction)) {
                return false;
            }
        }
        if (!environment.issue_load_batch(scalar_wave)) {
            return false;
        }
        for (const auto &transaction : vector_wave) {
            if (!environment.issue_vector(transaction)) {
                return false;
            }
        }
        if (!environment.run_cycles(random_delay(0, 7)) ||
            !environment.run_until_all_complete(2048)) {
            return false;
        }
        for (const auto &transaction : scalar_wave) {
            coverage.sample(transaction);
        }
        for (const auto &transaction : vector_wave) {
            coverage.sample(transaction);
        }
        actions += scalar_wave.size() + vector_wave.size();
        ++coverage.heterogeneous_waves;
        coverage.max_outstanding = 5;
        ++coverage.cacheable;

        phase = "scalar-load-op-wave";
        std::vector<memblock::LoadTransaction> load_sweep;
        std::vector<unsigned> load_ops(7);
        std::iota(load_ops.begin(), load_ops.end(), 0U);
        std::shuffle(load_ops.begin(), load_ops.end(), random);
        for (const unsigned op : load_ops) {
            const unsigned size = 1U << (op & 3U);
            const std::uint64_t region = bare_base + 0x4000 +
                (random() % 28U) * 64;
            load_sweep.push_back(make_load(
                random_bare_address(region, size, 64 - size),
                static_cast<memblock::LoadOp>(op), 0));
        }
        for (unsigned begin = 0; begin < load_sweep.size(); begin += 3) {
            std::vector<memblock::LoadTransaction> batch;
            std::array<unsigned, 3> batch_lanes{{0, 1, 2}};
            std::shuffle(batch_lanes.begin(), batch_lanes.end(), random);
            for (unsigned index = begin;
                 index < std::min<unsigned>(begin + 3, load_sweep.size()); ++index) {
                load_sweep[index].lane = batch_lanes[index - begin];
                environment.expect_load(load_sweep[index]);
                if (!environment.enqueue_load(load_sweep[index])) {
                    return false;
                }
                batch.push_back(load_sweep[index]);
            }
            if (!environment.issue_load_batch(batch)) {
                return false;
            }
        }
        if (!environment.run_until_all_complete(2048)) {
            return false;
        }
        for (const auto &transaction : load_sweep) {
            coverage.sample(transaction);
        }
        actions += load_sweep.size();
        coverage.max_outstanding = std::max<std::uint64_t>(
            coverage.max_outstanding, load_sweep.size());

        phase = "multi-lane-lsq-dispatch";
        for (unsigned width = 1;
             width <= memblock::generated::kLsqEnqueueLanes; ++width) {
            std::vector<unsigned> dispatch_lanes(
                memblock::generated::kLsqEnqueueLanes);
            std::iota(dispatch_lanes.begin(), dispatch_lanes.end(), 0U);
            std::shuffle(dispatch_lanes.begin(), dispatch_lanes.end(), random);
            dispatch_lanes.resize(width);
            std::sort(dispatch_lanes.begin(), dispatch_lanes.end());

            std::vector<memblock::LoadTransaction> batch;
            batch.reserve(width);
            for (unsigned index = 0; index < width; ++index) {
                const auto op = static_cast<memblock::LoadOp>((width + index) % 7);
                const unsigned size = 1U <<
                    (static_cast<unsigned>(op) & 3U);
                batch.push_back(make_load(
                    random_bare_address(
                        bare_base + 0x5000 + width * 0x100, size, 0x80),
                    op, index % memblock::kScalarLoadLanes));
                environment.expect_load(batch.back());
            }
            if (!environment.enqueue_load_batch(batch, dispatch_lanes)) {
                return false;
            }
            for (unsigned begin = 0; begin < batch.size();
                 begin += memblock::kScalarLoadLanes) {
                const unsigned end = std::min<unsigned>(
                    begin + memblock::kScalarLoadLanes, batch.size());
                const std::vector<memblock::LoadTransaction> issue_batch(
                    batch.begin() + begin, batch.begin() + end);
                if (!environment.issue_load_batch(issue_batch)) {
                    return false;
                }
            }
            if (!environment.run_until_all_complete(2048)) {
                return false;
            }
            for (const auto &transaction : batch) {
                coverage.sample(transaction);
            }
            actions += batch.size();
            coverage.cacheable += batch.size();
            coverage.max_outstanding = std::max<std::uint64_t>(
                coverage.max_outstanding, batch.size());
        }

        phase = "vector-load-shape-wave";
        std::vector<memblock::VectorMemoryTransaction> vector_sweep;
        std::vector<unsigned> load_eews{0, 1, 2, 3};
        std::shuffle(load_eews.begin(), load_eews.end(), random);
        for (unsigned shape = 0; shape < load_eews.size(); ++shape) {
            const unsigned eew = load_eews[shape];
            const unsigned element_bytes = 1U << eew;
            auto transaction = make_vector(
                false,
                random_bare_address(bare_base + 0x8000 + random() % 0x700,
                                    element_bytes, 128) +
                    (random() % 4) * element_bytes,
                eew, random() % memblock::kVectorMemoryLanes);
            const unsigned elements = 16U >> eew;
            transaction.vm = shape == 0 ? false : (shape == 1 ? true : (random() & 1U) != 0);
            transaction.mask_bits = static_cast<std::uint16_t>(random());
            transaction.vl = static_cast<std::uint8_t>(
                shape == 1 || shape == 2
                    ? elements
                    : shape == 3 ? elements - 1U
                                 : 1U + random() % elements);
            transaction.vstart = static_cast<std::uint8_t>(
                shape == 2 && transaction.vl > 1
                    ? 1U + random() % transaction.vl
                    : 0U);
            if (!transaction.vm && shape == 0) {
                transaction.mask_bits &= static_cast<std::uint16_t>(~1U);
                if (memblock::active_vector_elements(transaction) == 0) {
                    transaction.mask_bits |= static_cast<std::uint16_t>(1U <<
                        (transaction.vl == 0 ? 0 : transaction.vl - 1));
                }
            }
            environment.expect_vector(transaction);
            if (!environment.enqueue_vector(transaction)) {
                return false;
            }
            vector_sweep.push_back(transaction);
        }
        for (const auto &transaction : vector_sweep) {
            if (!environment.issue_vector(transaction)) {
                return false;
            }
        }
        if (!environment.run_until_all_complete(2048)) {
            return false;
        }
        for (const auto &transaction : vector_sweep) {
            coverage.sample(transaction);
        }
        actions += vector_sweep.size();
        coverage.max_outstanding = std::max<std::uint64_t>(
            coverage.max_outstanding, vector_sweep.size());

        phase = "advanced-addressing-wave";
        const auto misaligned = make_load(
            random_bare_address(bare_base + 0x9000, 1, 0x80) | 0x3d,
            memblock::LoadOp::ld, random() % memblock::kScalarLoadLanes);
        if (!environment.set_rob_head(misaligned.rob, misaligned.rob_flag) ||
            !issue_load(misaligned)) {
            return false;
        }
        ++coverage.cacheable;

        const unsigned indexed_eew = 0;
        auto indexed_store = make_vector(
            true,
            bare_base + 0x7000,
            indexed_eew,
            1,
            memblock::VectorAddressingMode::indexed_unordered);
        for (unsigned element = 0; element < indexed_store.index.size(); ++element) {
            indexed_store.index[element] = static_cast<unsigned char>((element * 11) & 63);
        }
        if (!issue_vector(indexed_store)) {
            return false;
        }
        auto indexed_load = make_vector(
            false,
            indexed_store.address,
            indexed_store.eew,
            0,
            indexed_store.addressing);
        indexed_load.index = indexed_store.index;
        if (!issue_vector(
                indexed_load,
                overlay_vector_store(indexed_store, indexed_load)) ||
            !environment.commit_vector_store(indexed_store, 4096)) {
            return false;
        }
        ++coverage.vector_forwarding;
        coverage.cacheable += 2;

        auto indexed_ordered_store = make_vector(
            true, bare_base + 0x7400, 2, 0,
            memblock::VectorAddressingMode::indexed_ordered);
        for (unsigned element = 0; element < indexed_ordered_store.index.size(); ++element) {
            indexed_ordered_store.index[element] = static_cast<unsigned char>((element * 7) & 63);
        }
        if (!issue_vector(indexed_ordered_store)) {
            return false;
        }
        auto indexed_ordered_load = make_vector(
            false, indexed_ordered_store.address, indexed_ordered_store.eew, 1,
            indexed_ordered_store.addressing);
        indexed_ordered_load.index = indexed_ordered_store.index;
        if (!issue_vector(
                indexed_ordered_load,
                overlay_vector_store(indexed_ordered_store, indexed_ordered_load)) ||
            !environment.commit_vector_store(indexed_ordered_store, 4096)) {
            return false;
        }
        ++coverage.vector_forwarding;
        coverage.cacheable += 2;

        phase = "vector-stride-signs";
        constexpr std::array<std::int64_t, 3> load_strides{{-4, 0, 4}};
        for (unsigned index = 0; index < load_strides.size(); ++index) {
            auto load = make_vector(
                false, bare_base + 0x7800 + index * 0x80 + 0x30, 2,
                index % memblock::kVectorMemoryLanes,
                memblock::VectorAddressingMode::strided);
            load.stride = load_strides[index];
            if (!issue_vector(load)) {
                return false;
            }
            ++coverage.cacheable;
        }
        constexpr std::array<std::int64_t, 2> store_strides{{-4, 4}};
        for (unsigned index = 0; index < store_strides.size(); ++index) {
            auto store = make_vector(
                true, bare_base + 0x7a00 + index * 0x80 + 0x30, 2,
                index % memblock::kVectorMemoryLanes,
                memblock::VectorAddressingMode::strided);
            store.stride = store_strides[index];
            if (!issue_vector(store) || !environment.commit_vector_store(store)) {
                return false;
            }
            ++coverage.cacheable;
        }

        phase = "scalar-forwarding";
        std::vector<unsigned> store_ops{0, 1, 2, 3};
        std::shuffle(store_ops.begin(), store_ops.end(), random);
        for (const unsigned op : store_ops) {
            const unsigned size = 1U << op;
            const bool is_unsigned = op != 3 && (random() & 1U) != 0;
            const std::uint64_t region = bare_base + 0x8000 +
                (random() % 28U) * 64;
            const auto store = make_store(
                random_bare_address(region, size, 64 - size), random(),
                static_cast<memblock::StoreOp>(op), op % 2, (op + 1) % 2);
            const bool data_first = (random() & 1U) != 0;
            if (!issue_store(store, data_first)) {
                return false;
            }
            const auto load = make_load(
                store.address,
                static_cast<memblock::LoadOp>(op + (is_unsigned ? 4U : 0U)),
                op % 3);
            if (!issue_load(load, scalar_forward_value(store.data, size, is_unsigned)) ||
                !environment.commit_store(store)) {
                return false;
            }
            ++coverage.scalar_forwarding;
        }

        phase = "vector-forwarding";
        std::vector<unsigned> store_eews{0, 1, 2, 3};
        std::shuffle(store_eews.begin(), store_eews.end(), random);
        for (const unsigned eew : store_eews) {
            const unsigned element_bytes = 1U << eew;
            const std::uint64_t vector_region = bare_base + 0xc000 +
                (random() % 28U) * 64;
            auto store = make_vector(
                true,
                vector_region + (random() % (16U / element_bytes)) * element_bytes,
                eew, random() % 2, memblock::VectorAddressingMode::unit_stride);
            const unsigned elements = 16U >> eew;
            store.vm = (random() & 1U) != 0;
            store.mask_bits = static_cast<std::uint16_t>(random());
            store.vl = static_cast<std::uint8_t>(1U + random() % elements);
            store.vstart = static_cast<std::uint8_t>(
                store.vl > 1 && (random() & 1U) ? random() % store.vl : 0U);
            if (!issue_vector(store)) {
                return false;
            }
            auto load = make_vector(false, store.address, eew, random() % 2,
                                    store.addressing);
            load.vl = store.vl;
            load.vstart = store.vstart;
            load.vm = store.vm;
            load.mask_bits = store.mask_bits;
            load.vma = store.vma;
            load.vta = store.vta;
            load.stride = store.stride;
            load.index = store.index;
            const auto expected = overlay_vector_store(store, load);
            if (!issue_vector(load, expected) ||
                !environment.commit_vector_store(store)) {
                return false;
            }
            ++coverage.vector_forwarding;
        }

        phase = "vector-misaligned-store";
        auto misaligned_vector_store = make_vector(
            true,
            bare_base + 0xe005,
            3,
            0,
            memblock::VectorAddressingMode::strided);
        misaligned_vector_store.stride = 8;
        misaligned_vector_store.vl = 2;
        const std::uint64_t replays_before =
            environment.vector_replay_feedbacks();
        environment.expect_vector(misaligned_vector_store);
        if (!environment.set_rob_head(
                misaligned_vector_store.rob,
                misaligned_vector_store.rob_flag) ||
            !environment.enqueue_vector(misaligned_vector_store) ||
            !environment.issue_vector(misaligned_vector_store, 256) ||
            !environment.run_cycles(32) ||
            !environment.pulse_pending_store(
                misaligned_vector_store.rob,
                misaligned_vector_store.rob_flag) ||
            !environment.run_until_vector_complete_with_replays(
                misaligned_vector_store, 4096) ||
            !environment.commit_vector_store(
                misaligned_vector_store, 4096)) {
            return false;
        }
        coverage.sample(misaligned_vector_store);
        ++actions;
        coverage.vector_replays +=
            environment.vector_replay_feedbacks() - replays_before;
        auto misaligned_vector_load = make_vector(
            false,
            misaligned_vector_store.address,
            misaligned_vector_store.eew,
            1,
            misaligned_vector_store.addressing);
        misaligned_vector_load.stride = misaligned_vector_store.stride;
        misaligned_vector_load.vl = misaligned_vector_store.vl;
        if (!environment.set_rob_head(
                misaligned_vector_load.rob,
                misaligned_vector_load.rob_flag) ||
            !issue_vector(
                misaligned_vector_load, misaligned_vector_store.data)) {
            return false;
        }
        coverage.cacheable += 2;

        phase = "scalar-misaligned-store";
        const auto misaligned_store = make_store(
            bare_base + 0xf045,
            0xa5c33c5af00f6996ULL,
            memblock::StoreOp::sd,
            1,
            0);
        environment.expect_store(misaligned_store);
        if (!environment.set_rob_head(
                misaligned_store.rob, misaligned_store.rob_flag) ||
            !environment.enqueue_store(
                misaligned_store, memblock::lq_pointer_value(lq_offset)) ||
            !environment.issue_store_address(misaligned_store, 256) ||
            !environment.issue_store_data(misaligned_store, 256) ||
            !environment.pulse_pending_store(
                misaligned_store.rob, misaligned_store.rob_flag) ||
            !environment.run_until_all_complete(4096) ||
            !environment.commit_store(misaligned_store, 4096)) {
            return false;
        }
        coverage.sample(misaligned_store, false);
        ++actions;
        const auto misaligned_store_readback = make_load(
            misaligned_store.address, memblock::LoadOp::ld, 2);
        if (!environment.set_rob_head(
                misaligned_store_readback.rob,
                misaligned_store_readback.rob_flag) ||
            !issue_load(
                misaligned_store_readback, misaligned_store.data)) {
            return false;
        }
        coverage.cacheable += 2;

        phase = "cross-forwarding-wave";
        const auto scalar_store = make_store(
            bare_base + 0x12008, 0xfedcba9876543210ULL,
            memblock::StoreOp::sd, 0, 1);
        auto vector_store = make_vector(true, bare_base + 0x12100, 0, 1);
        vector_store.vm = true;
        environment.expect_store(scalar_store);
        environment.expect_vector(vector_store);
        if (!environment.enqueue_store(
                scalar_store, memblock::lq_pointer_value(lq_offset)) ||
            !environment.enqueue_vector(vector_store) ||
            !environment.issue_store_data(scalar_store) ||
            !environment.issue_store_address(scalar_store) ||
            !environment.issue_vector(vector_store) ||
            !environment.run_until_all_complete(1024)) {
            return false;
        }
        coverage.sample(scalar_store, true);
        coverage.sample(vector_store);
        actions += 2;
        auto vector_load = make_vector(false, bare_base + 0x12000, 0, 0);
        auto vector_expected = environment.memory().expected_vector_load(vector_load);
        for (unsigned byte = 0; byte < 8; ++byte) {
            vector_expected[8 + byte] = static_cast<unsigned char>(
                scalar_store.data >> (8 * byte));
        }
        const auto scalar_load = make_load(
            vector_store.address + 8, memblock::LoadOp::ld, 2);
        std::uint64_t scalar_expected = 0;
        for (unsigned byte = 0; byte < 8; ++byte) {
            scalar_expected |= std::uint64_t{vector_store.data[8 + byte]}
                               << (8 * byte);
        }
        environment.expect_vector_data(vector_load, vector_expected);
        environment.expect_load_data(scalar_load, scalar_expected);
        if (!environment.enqueue_vector(vector_load) ||
            !environment.enqueue_load(scalar_load) ||
            !environment.issue_load_vector_pair(scalar_load, vector_load) ||
            !environment.run_until_all_complete(2048) ||
            !environment.commit_store(scalar_store) ||
            !environment.commit_vector_store(vector_store)) {
            return false;
        }
        coverage.sample(vector_load);
        coverage.sample(scalar_load);
        actions += 2;
        ++coverage.heterogeneous_waves;
        ++coverage.simultaneous_scalar_vector;
        ++coverage.scalar_to_vector;
        ++coverage.vector_to_scalar;

        phase = "bare-queue-retirement";
        if (!environment.run_until_queues_retired(2048)) {
            return false;
        }

        phase = "two-stage-translation";
        if (!environment.map_sv39_4k(
                guest_virtual, guest_physical, vs_root) ||
            !environment.map_sv39_4k(
                guest_fault_virtual, guest_physical, vs_root) ||
            !environment.map_sv39x4_4k(vs_root, vs_root, g_root) ||
            !environment.map_sv39x4_4k(
                vs_root + 0x1000, vs_root + 0x1000, g_root) ||
            !environment.map_sv39x4_4k(
                vs_root + 0x2000, vs_root + 0x2000, g_root) ||
            !environment.map_sv39x4_4k(
                guest_physical, host_physical, g_root) ||
            !environment.activate_two_stage(vs_root, g_root, 3, 5)) {
            return false;
        }
        const auto guest_reference = memblock::reference_two_stage_walk(
            environment.memory(), vs_root, g_root, guest_virtual + 0x188);
        if (!guest_reference.translated) {
            return false;
        }
        auto guest_cold = make_load(
            guest_virtual + 0x188, memblock::LoadOp::ld, 0);
        guest_cold.oracle_address = guest_reference.physical_address;
        if (!issue_load(guest_cold)) {
            return false;
        }
        const std::uint64_t ptw_after_guest_cold = environment.ptw_requests();
        auto guest_warm = make_load(
            guest_virtual + 0x188, memblock::LoadOp::lwu, 1);
        guest_warm.oracle_address = host_physical + 0x188;
        if (!issue_load(guest_warm) ||
            environment.ptw_requests() != ptw_after_guest_cold ||
            !environment.run_until_lq_retired(2048)) {
            return false;
        }
        coverage.two_stage += 2;
        coverage.cacheable += 2;
        ++coverage.tlb_reuse;

        phase = "vector-guest-page-fault";
        const auto guest_fault_reference = memblock::reference_two_stage_walk(
            environment.memory(), vs_root, g_root, guest_fault_virtual + 0x180);
        if (guest_fault_reference.translated ||
            !guest_fault_reference.guest_page_fault ||
            !guest_fault_reference.is_for_vs_nonleaf_pte) {
            return false;
        }
        auto guest_fault = make_vector(
            false, guest_fault_virtual + 0x180, 3, 0);
        guest_fault.expected_exception_mask =
            memblock::kExceptionLoadGuestPageFault;
        if (!environment.set_rob_head(
                guest_fault.rob, guest_fault.rob_flag) ||
            !issue_vector(guest_fault) ||
            !environment.run_cycles(8) ||
            !environment.exception_is_for_vs_nonleaf_pte() ||
            environment.exception_vaddr() != guest_fault.address ||
            environment.exception_gpaddr() !=
                guest_fault_reference.faulting_guest_physical_address ||
            !environment.run_until_lq_retired(2048)) {
            if (environment.error().empty()) {
                phase += "-metadata";
            }
            return false;
        }
        ++coverage.two_stage;
        ++coverage.exceptions;

        // Boundary-hunt mode deliberately fans out the VS-non-leaf fault
        // across vector widths and 16-byte offsets.  The ordinary green
        // mixed regression keeps this disabled because the clean baseline
        // contains a known split-GPA defect; the hunt runner records that
        // architectural mismatch as evidence instead of hiding it.
        if (options.hunt_boundaries) {
            phase = "vector-guest-page-fault-boundary-hunt";
            // Constrain the generator to the VS L1 PTE page while randomizing
            // the split byte, EEW, mask, vstart, and vl.  No single historical
            // offset is required: every candidate is checked against the
            // software two-stage walk and a mismatch is a genuine oracle hit.
            const unsigned attempts = 8 + static_cast<unsigned>(random() % 9);
            for (unsigned index = 0; index < attempts; ++index) {
                const unsigned eew = static_cast<unsigned>(random() % 4);
                const unsigned element_count = 16U >> eew;
                // Pick any offset in the faulting page.  The independent
                // two-stage walk identifies the VS-non-leaf PTE GPA; the
                // vector width/mask then determines whether the internal
                // operation crosses a beat boundary.
                const std::uint64_t offset = random() % 0x1000U;
                const std::uint64_t address = guest_fault_virtual + offset;
                const auto reference = memblock::reference_two_stage_walk(
                    environment.memory(), vs_root, g_root, address);
                if (reference.translated || !reference.guest_page_fault ||
                    !reference.is_for_vs_nonleaf_pte) {
                    return false;
                }
                auto candidate = make_vector(
                    false, address, eew, random() % 2);
                candidate.vl = static_cast<std::uint8_t>(
                    element_count == 1 ? 1U : 2U + random() % (element_count - 1));
                candidate.vstart = 0;
                // A masked vector with element zero disabled creates a
                // randomized non-zero firstUnmask offset; vm=true exercises
                // the zero-offset control.  Both remain legal completed-uop
                // shapes, unlike randomizing vstart on a faulting uop.
                candidate.vm = (random() & 3U) == 0;
                candidate.mask_bits = static_cast<std::uint16_t>(random());
                if (!candidate.vm) {
                    const std::uint32_t vl_mask = candidate.vl >= 16
                        ? 0xffffU
                        : ((std::uint32_t{1} << candidate.vl) - 1U);
                    candidate.mask_bits &= static_cast<std::uint16_t>(vl_mask);
                    candidate.mask_bits &= static_cast<std::uint16_t>(~1U);
                    if (memblock::active_vector_elements(candidate) == 0) {
                        const unsigned element = 1U +
                            static_cast<unsigned>(random() % (candidate.vl - 1));
                        candidate.mask_bits |= static_cast<std::uint16_t>(1U << element);
                    }
                }
                // Vector memory faults report the VA of the first active
                // element selected by vstart/mask.  The VS-non-leaf GPA is
                // different: it remains the exact page-table byte reported
                // by the two-stage walk and is never element-offset.
                unsigned first_active_element = 0;
                while (first_active_element < element_count &&
                       ((memblock::active_vector_elements(candidate) >>
                         first_active_element) & 1U) == 0) {
                    ++first_active_element;
                }
                if (first_active_element == element_count) {
                    return false;
                }
                const std::uint64_t expected_exception_vaddr = address +
                    static_cast<std::uint64_t>(first_active_element << eew);
                candidate.expected_exception_mask =
                    memblock::kExceptionLoadGuestPageFault;
                if (!environment.set_rob_head(candidate.rob, candidate.rob_flag) ||
                    !issue_vector(candidate) ||
                    !environment.run_cycles(8) ||
                    !environment.run_until_lq_retired(2048) ||
                    !environment.exception_is_for_vs_nonleaf_pte() ||
                    environment.exception_vaddr() != expected_exception_vaddr ||
                    environment.exception_gpaddr() !=
                        reference.faulting_guest_physical_address) {
                    if (environment.error().empty()) {
                        std::cerr << "MEMBLOCK_RANDOM_MIXED_FAIL seed=" << options.seed
                                  << " transactions=" << actions
                                  << " requested=" << options.transactions
                                  << " cycle=" << environment.cycle()
                                  << " phase=" << phase
                                  << " boundary_address=0x" << std::hex << address
                                  << " expected_vaddr=0x" << expected_exception_vaddr
                                  << " actual_vaddr=0x" << environment.exception_vaddr()
                                  << " expected_gpaddr=0x"
                                  << reference.faulting_guest_physical_address
                                  << " actual_gpaddr=0x" << environment.exception_gpaddr()
                                  << std::dec << "\n";
                    }
                    return false;
                }
                ++coverage.two_stage;
                ++coverage.exceptions;
            }
        }

        phase = "sv39-configuration";
        if (!environment.configure_sv39(cache0_base, cache0_base) ||
            !environment.configure_sv39(cache1_base, cache1_base) ||
            !environment.configure_sv39_nc(nc_base, nc_base) ||
            !environment.map_sv39_4k(
                mmio_virtual, mmio_physical, sv39_root,
                true, true, false, false, false, true) ||
            !environment.activate_sv39(sv39_root)) {
            return false;
        }

        phase = "software-prefetch";
        std::vector<unsigned> prefetch_types{0, 1, 2};
        std::shuffle(prefetch_types.begin(), prefetch_types.end(), random);
        for (const unsigned type : prefetch_types) {
            const auto prefetch = make_prefetch(
                cache0_base + 0x5000 + (random() % 8) * 0x100,
                static_cast<memblock::PrefetchOp>(8 + type), type % 3);
            environment.expect_prefetch(prefetch);
            if (!environment.enqueue_prefetch(prefetch) ||
                !environment.issue_prefetch(prefetch, 256) ||
                !environment.run_until_all_complete(2048)) {
                return false;
            }
            coverage.sample(prefetch);
            ++coverage.cacheable;
            ++actions;
        }

        phase = "sv39-coissue";
        const std::uint64_t requests_before = environment.tilelink_requests();
        const auto translated_scalar = make_load(
            cache0_base + 0x1000, memblock::LoadOp::lwu, 1);
        auto translated_vector = make_vector(
            false, cache1_base + 0x1000, 2, 0);
        environment.expect_load(translated_scalar);
        environment.expect_vector(translated_vector);
        if (!environment.enqueue_load(translated_scalar) ||
            !environment.enqueue_vector(translated_vector) ||
            !environment.issue_load_vector_pair(
                translated_scalar, translated_vector, 256) ||
            !environment.run_until_all_complete(4096)) {
            return false;
        }
        coverage.sample(translated_scalar);
        coverage.sample(translated_vector);
        actions += 2;
        coverage.cacheable += 2;
        ++coverage.heterogeneous_waves;
        ++coverage.simultaneous_scalar_vector;
        if (environment.tilelink_requests() > requests_before) {
            ++coverage.dcache_misses;
        }

        phase = "dcache-tlb-hit";
        const std::uint64_t hit_requests_before = environment.tilelink_requests();
        const auto hit = make_load(
            translated_scalar.address, memblock::LoadOp::ld, 0);
        if (!issue_load(hit)) {
            return false;
        }
        ++coverage.cacheable;
        ++coverage.tlb_reuse;
        if (environment.tilelink_requests() == hit_requests_before) {
            ++coverage.dcache_hits;
        }

        phase = "exception-contracts";
        auto page_fault = make_load(
            0x50003000ULL, memblock::LoadOp::ld, 0);
        page_fault.expected_exception_mask = memblock::kExceptionLoadPageFault;
        if (!issue_load(page_fault)) {
            return false;
        }
        const auto prefetch_fault = make_prefetch(
            0x50004000ULL, memblock::PrefetchOp::read, 1);
        environment.expect_prefetch(prefetch_fault);
        if (!environment.enqueue_prefetch(prefetch_fault) ||
            !environment.issue_prefetch(prefetch_fault, 256) ||
            !environment.run_until_all_complete(4096)) {
            return false;
        }
        coverage.sample(prefetch_fault);
        ++actions;
        coverage.exceptions += 2;

        phase = "pbmt-nc-store-load";
        auto nc_store = make_store(
            nc_base + 0x180, 0x13579bdf2468ace0ULL,
            memblock::StoreOp::sd, 1, 0);
        nc_store.expected_debug_is_mmio = false;
        // PBMT=NC still targets main memory; NCIO is reserved for an
        // uncached access whose PMA class is IO.
        nc_store.expected_debug_is_ncio = false;
        environment.expect_store(nc_store);
        if (!environment.enqueue_store(
                nc_store, memblock::lq_pointer_value(lq_offset)) ||
            !environment.warm_store_translation(nc_store, 2048) ||
            !environment.issue_store_data(nc_store) ||
            !environment.run_until_all_complete(2048)) {
            return false;
        }
        coverage.sample(nc_store, false);
        ++actions;
        if (
            !environment.commit_store(nc_store, 2048)) {
            return false;
        }
        ++coverage.noncacheable;
        const auto nc_load = make_load(nc_store.address, memblock::LoadOp::ld, 2);
        if (!issue_load(nc_load, nc_store.data)) {
            return false;
        }
        ++coverage.noncacheable;

        phase = "dcache-dirty-pressure";
        constexpr unsigned dcache_sets = 128;
        constexpr unsigned dcache_ways = 8;
        constexpr unsigned line_bytes = 64;
        constexpr unsigned dirty_lines = 10;
        constexpr std::uint64_t same_set_stride = dcache_sets * line_bytes;
        constexpr std::uint64_t dirty_base = cache0_base + 0x30000;
        const std::uint64_t release_before = environment.tilelink_release_data();
        const std::uint64_t verified_release_before =
            environment.tilelink_release_data_verified();
        std::array<memblock::StoreTransaction, dirty_lines> dirty_stores{};
        for (unsigned index = 0; index < dirty_lines; ++index) {
            const std::uint64_t line = dirty_base + index * same_set_stride;
            environment.memory().fill_incrementing(
                line, line_bytes, static_cast<std::uint8_t>(0x31 + index * 9));
            dirty_stores[index] = make_store(
                line + 24, 0xc001d00d00000000ULL | index,
                memblock::StoreOp::sd, index % 2, (index + 1) % 2);
            std::vector<unsigned char> expected_line =
                environment.memory().read_beat(line, line_bytes);
            for (unsigned byte = 0; byte < sizeof(std::uint64_t); ++byte) {
                expected_line[24 + byte] = static_cast<unsigned char>(
                    dirty_stores[index].data >> (8 * byte));
            }
            // Snapshot the architectural line before the DUT can evict it.
            // The TileLink agent compares ReleaseData against this immutable
            // image before updating its backing memory model.
            environment.expect_release_line(line, expected_line);
            if (index == 0) {
                environment.expect_store(dirty_stores[index]);
                if (!environment.enqueue_store(
                        dirty_stores[index],
                        memblock::lq_pointer_value(lq_offset)) ||
                    !environment.issue_store_address(dirty_stores[index]) ||
                    !environment.run_cycles(64) ||
                    !environment.issue_store_address(dirty_stores[index]) ||
                    !environment.issue_store_data(dirty_stores[index]) ||
                    !environment.run_until_all_complete(2048)) {
                    return false;
                }
                coverage.sample(dirty_stores[index], false);
                ++actions;
            } else if (!issue_store(
                           dirty_stores[index], (index & 1U) == 0)) {
                return false;
            }
            if (!environment.commit_store(dirty_stores[index], 2048)) {
                return false;
            }
            ++coverage.cacheable;
            if ((index & 1U) != 0) {
                const auto pressure_load = make_load(
                    cache0_base + 0x18000 + index * 64,
                    static_cast<memblock::LoadOp>(index % 7), index % 3);
                if (!issue_load(pressure_load)) {
                    return false;
                }
                ++coverage.cacheable;
            }
        }
        if (!environment.run_until_release_data_count(release_before + 1, 8192) ||
            !environment.run_cycles(32)) {
            return false;
        }
        const std::uint64_t release_delta =
            environment.tilelink_release_data() - release_before;
        const std::uint64_t verified_release_delta =
            environment.tilelink_release_data_verified() - verified_release_before;
        if (release_delta < dirty_lines - dcache_ways ||
            verified_release_delta != release_delta) {
            return false;
        }
        coverage.dirty_pressure = verified_release_delta;

        // Exact snapshots belong only to the directed replacement check above.
        // The random tail may legally revisit the surviving dirty lines, so its
        // later releases must follow the live architectural reference image.
        environment.clear_release_line_expectations();

        phase = "random-translation-map";
        if (!prepare_random_translation_contexts()) {
            return false;
        }
        TranslationContext current_translation{};
        bool translation_context_valid = false;
        constexpr unsigned pending_sfence = 1U << 0;
        constexpr unsigned pending_hfence_vvma = 1U << 1;
        constexpr unsigned pending_hfence_gvma = 1U << 2;
        unsigned pending_translation_fences = 0;
        bool translation_coverage_closed = false;
        const auto first_enabled_mode = [](const std::array<unsigned, 2> &weights) {
            return weights[0] != 0 ? 0U : 1U;
        };
        const auto random_translation_context = [&]() {
            TranslationContext context;
            context.regime = constraints.choose_translation_regime(random());
            if (context.regime == RandomConstraints::translation_stage1) {
                context.stage1_mode = constraints.choose_stage1_mode(random());
            } else if (context.regime == RandomConstraints::translation_nested) {
                context.vs_mode = constraints.choose_vs_mode(random());
                context.g_mode = constraints.choose_g_mode(random());
            }
            return context;
        };
        const auto dominant_translation_context = [&]() {
            const auto dominant_index = [](const auto &weights) {
                return static_cast<unsigned>(std::distance(
                    weights.begin(),
                    std::max_element(weights.begin(), weights.end())));
            };
            TranslationContext context;
            context.regime = dominant_index(constraints.translation_weights);
            if (context.regime == RandomConstraints::translation_stage1) {
                context.stage1_mode = dominant_index(
                    constraints.stage1_mode_weights);
            } else if (context.regime == RandomConstraints::translation_nested) {
                context.vs_mode = dominant_index(constraints.vs_mode_weights);
                context.g_mode = dominant_index(constraints.g_mode_weights);
            }
            return context;
        };
        const auto translated_context = [&]() {
            TranslationContext context;
            const std::uint64_t stage1_weight = constraints.translation_weights[
                RandomConstraints::translation_stage1];
            const std::uint64_t nested_weight = constraints.translation_weights[
                RandomConstraints::translation_nested];
            if (random() % (stage1_weight + nested_weight) < stage1_weight) {
                context.regime = RandomConstraints::translation_stage1;
                context.stage1_mode = constraints.choose_stage1_mode(random());
            } else {
                context.regime = RandomConstraints::translation_nested;
                context.vs_mode = constraints.choose_vs_mode(random());
                context.g_mode = constraints.choose_g_mode(random());
            }
            return context;
        };
        const auto binary_class_enabled = [](unsigned true_per_mille,
                                             bool value) {
            return value ? true_per_mille != 0 : true_per_mille != 1000;
        };
        const auto random_translation_leaf = [&](const TranslationContext &context) {
            TranslationLeafTopology leaf;
            if (context.regime == RandomConstraints::translation_stage1) {
                leaf.stage1_napot =
                    constraints.choose_stage1_napot(random());
            } else if (context.regime == RandomConstraints::translation_nested) {
                leaf.vs_napot =
                    constraints.choose_nested_vs_napot(random());
                leaf.g_napot = constraints.choose_nested_g_napot(random());
            }
            return leaf;
        };
        const auto required_translation_leaf = [&] (
            const TranslationContext &context)
            -> std::optional<TranslationLeafTopology> {
            if (context.regime == RandomConstraints::translation_stage1) {
                for (unsigned napot = 0; napot < 2; ++napot) {
                    if (binary_class_enabled(
                            constraints.stage1_napot_per_mille, napot != 0) &&
                        constraint_coverage.stage1_leaf_types[napot] == 0) {
                        TranslationLeafTopology leaf;
                        leaf.stage1_napot = napot != 0;
                        return leaf;
                    }
                }
            } else if (context.regime == RandomConstraints::translation_nested) {
                for (unsigned vs_napot = 0; vs_napot < 2; ++vs_napot) {
                    for (unsigned g_napot = 0; g_napot < 2; ++g_napot) {
                        const unsigned topology = vs_napot * 2 + g_napot;
                        if (binary_class_enabled(
                                constraints.nested_vs_napot_per_mille,
                                vs_napot != 0) &&
                            binary_class_enabled(
                                constraints.nested_g_napot_per_mille,
                                g_napot != 0) &&
                            constraint_coverage.nested_leaf_topologies[
                                topology] == 0) {
                            TranslationLeafTopology leaf;
                            leaf.vs_napot = vs_napot != 0;
                            leaf.g_napot = g_napot != 0;
                            return leaf;
                        }
                    }
                }
            }
            return std::nullopt;
        };
        const auto choose_translation_leaf = [&](const TranslationContext &context) {
            if (const auto required = required_translation_leaf(context)) {
                return *required;
            }
            return random_translation_leaf(context);
        };
        const auto required_translation_context = [&]()
            -> std::optional<TranslationContext> {
            if (constraints.translation_weights[
                    RandomConstraints::translation_bare] != 0 &&
                constraint_coverage.translation_regimes[
                    RandomConstraints::translation_bare] == 0) {
                return TranslationContext{};
            }
            if (constraints.translation_weights[
                    RandomConstraints::translation_stage1] != 0) {
                for (unsigned mode = 0; mode < 2; ++mode) {
                    if (constraints.stage1_mode_weights[mode] != 0 &&
                        constraint_coverage.stage1_modes[mode] == 0) {
                        TranslationContext context;
                        context.regime = RandomConstraints::translation_stage1;
                        context.stage1_mode = mode;
                        return context;
                    }
                }
            }
            if (constraints.translation_weights[
                    RandomConstraints::translation_nested] != 0) {
                for (unsigned vs_mode = 0; vs_mode < 2; ++vs_mode) {
                    for (unsigned g_mode = 0; g_mode < 2; ++g_mode) {
                        if (constraints.vs_mode_weights[vs_mode] != 0 &&
                            constraints.g_mode_weights[g_mode] != 0 &&
                            constraint_coverage.nested_mode_pairs[
                                vs_mode * 2 + g_mode] == 0) {
                            TranslationContext context;
                            context.regime =
                                RandomConstraints::translation_nested;
                            context.vs_mode = vs_mode;
                            context.g_mode = g_mode;
                            return context;
                        }
                    }
                }
            }
            if (constraints.translation_weights[
                    RandomConstraints::translation_stage1] != 0) {
                TranslationContext context;
                context.regime = RandomConstraints::translation_stage1;
                context.stage1_mode = first_enabled_mode(
                    constraints.stage1_mode_weights);
                if (required_translation_leaf(context)) {
                    return context;
                }
            }
            if (constraints.translation_weights[
                    RandomConstraints::translation_nested] != 0) {
                TranslationContext context;
                context.regime = RandomConstraints::translation_nested;
                context.vs_mode = first_enabled_mode(constraints.vs_mode_weights);
                context.g_mode = first_enabled_mode(constraints.g_mode_weights);
                if (required_translation_leaf(context)) {
                    return context;
                }
            }
            if (constraints.uses_translation() &&
                constraint_coverage.translation_walk_windows != 0 &&
                constraint_coverage.translation_reuse_windows == 0 &&
                translation_context_valid &&
                current_translation.regime !=
                    RandomConstraints::translation_bare) {
                return current_translation;
            }
            if (constraints.tlb_flushes_per_mille == 0) {
                return std::nullopt;
            }
            for (unsigned kind = 0;
                 kind < RandomConstraints::fence_kind_count; ++kind) {
                const bool compatible = kind == RandomConstraints::fence_sfence
                    ? constraints.translation_weights[
                          RandomConstraints::translation_stage1] != 0
                    : constraints.translation_weights[
                          RandomConstraints::translation_nested] != 0;
                if (!compatible || constraints.fence_kind_weights[kind] == 0) {
                    continue;
                }
                for (unsigned scope = 0; scope < 2; ++scope) {
                    if (constraints.fence_scope_weights[scope] != 0 &&
                        constraint_coverage.fences[kind][scope] == 0) {
                        TranslationContext context;
                        if (kind == RandomConstraints::fence_sfence) {
                            context.regime =
                                RandomConstraints::translation_stage1;
                            context.stage1_mode = first_enabled_mode(
                                constraints.stage1_mode_weights);
                        } else {
                            context.regime =
                                RandomConstraints::translation_nested;
                            context.vs_mode = first_enabled_mode(
                                constraints.vs_mode_weights);
                            context.g_mode = first_enabled_mode(
                                constraints.g_mode_weights);
                        }
                        return context;
                    }
                }
            }
            return std::nullopt;
        };
        const auto choose_translation_context = [&]() {
            if (const auto required = required_translation_context()) {
                return *required;
            }
            if (!translation_coverage_closed) {
                translation_coverage_closed = true;
                return dominant_translation_context();
            }
            if (translation_context_valid &&
                random() % 1000 >=
                    constraints.translation_switches_per_mille) {
                return current_translation;
            }
            return random_translation_context();
        };
        const auto flush_pending_translation_fences = [&]() {
            const bool fenced =
                ((pending_translation_fences & pending_sfence) == 0 ||
                 environment.issue_sfence(
                     0, 0, true, true, false, false)) &&
                ((pending_translation_fences & pending_hfence_vvma) == 0 ||
                 environment.issue_sfence(
                     0, 0, true, true, true, false)) &&
                ((pending_translation_fences & pending_hfence_gvma) == 0 ||
                 environment.issue_sfence(
                     0, 0, true, true, false, true));
            if (fenced) {
                pending_translation_fences = 0;
            }
            return fenced;
        };
        const auto enter_translation_context =
            [&](const TranslationContext &context) {
                if (translation_context_valid &&
                    context == current_translation &&
                    pending_translation_fences == 0) {
                    return true;
                }
                if (!environment.run_until_all_complete(4096) ||
                    !environment.run_until_queues_retired(4096) ||
                    !flush_pending_translation_fences() ||
                    !activate_translation_context(context)) {
                    return false;
                }
                if (translation_context_valid) {
                    ++constraint_coverage.translation_switches;
                }
                current_translation = context;
                translation_context_valid = true;
                return true;
            };
        const auto issue_constrained_fence =
            [&](unsigned operation_kind) {
                if (current_translation.regime ==
                        RandomConstraints::translation_bare ||
                    constraints.tlb_flushes_per_mille == 0 ||
                    !constraints.has_compatible_fence_kind(
                        current_translation.regime)) {
                    return true;
                }
                std::optional<std::pair<unsigned, unsigned>> required;
                for (unsigned kind = 0;
                     kind < RandomConstraints::fence_kind_count && !required;
                     ++kind) {
                    const bool compatible =
                        (current_translation.regime ==
                             RandomConstraints::translation_stage1 &&
                         kind == RandomConstraints::fence_sfence) ||
                        (current_translation.regime ==
                             RandomConstraints::translation_nested &&
                         kind != RandomConstraints::fence_sfence);
                    if (!compatible ||
                        constraints.fence_kind_weights[kind] == 0) {
                        continue;
                    }
                    for (unsigned scope = 0; scope < 2; ++scope) {
                        if (constraints.fence_scope_weights[scope] != 0 &&
                            constraint_coverage.fences[kind][scope] == 0) {
                            required = std::make_pair(kind, scope);
                            break;
                        }
                    }
                }
                if (!required &&
                    random() % 1000 >= constraints.tlb_flushes_per_mille) {
                    return true;
                }
                const unsigned kind = required
                    ? required->first
                    : constraints.choose_fence_kind(
                          current_translation.regime, random());
                const unsigned scope = required
                    ? required->second
                    : constraints.choose_fence_scope(random());
                const bool global = scope == 0;
                std::uint64_t virtual_address = cache0_base + 0x10000;
                if (operation_kind == RandomConstraints::noncacheable) {
                    virtual_address = nc_base;
                } else if (operation_kind == RandomConstraints::mmio) {
                    virtual_address = mmio_virtual;
                } else if (operation_kind == RandomConstraints::load_merge) {
                    virtual_address = load_merge_base;
                } else if (operation_kind == RandomConstraints::set_pressure) {
                    virtual_address = set_pressure_base;
                } else if (operation_kind == RandomConstraints::miss_burst) {
                    virtual_address = miss_burst_base;
                }
                std::uint64_t fence_address = global ? 0 : virtual_address;
                std::uint16_t id = 0;
                bool hypervisor_virtual = false;
                bool hypervisor_guest = false;
                if (kind == RandomConstraints::fence_sfence) {
                    id = 23;
                } else if (kind == RandomConstraints::fence_hfence_vvma) {
                    id = 29;
                    hypervisor_virtual = true;
                } else {
                    id = 31;
                    hypervisor_guest = true;
                    const std::uint64_t guest_physical_address =
                        operation_kind == RandomConstraints::mmio
                        ? mmio_physical : virtual_address;
                    fence_address = global ? 0 : guest_physical_address >> 2;
                }
                if (!environment.issue_sfence(
                        fence_address, id, global, global,
                        hypervisor_virtual, hypervisor_guest)) {
                    return false;
                }
                ++constraint_coverage.fences[kind][scope];
                ++constraint_coverage.tlb_flushes;
                return true;
            };
        const auto mmio_access_address = [&]() {
            return current_translation.regime ==
                    RandomConstraints::translation_bare
                ? mmio_physical : mmio_virtual;
        };

        phase = "seeded-mixed-tail";
        if (options.transactions < actions + 2U) {
            phase = "short-mixed-prefix-exceeds-transaction-budget";
            return false;
        }
        const unsigned target_before_redirect = options.transactions - 2;
        if (!environment.configure_stride_prefetch(
                constraints.stride_stream_per_mille != 0)) {
            return false;
        }
        environment.configure_backpressure(
            options.seed ^ 0x1f83d9abfb41bd6bULL, options.backpressure,
            constraints.response_latency);
        const unsigned available_tail_actions = target_before_redirect - actions;
        const unsigned requested_concurrent_actions = static_cast<unsigned>(
            std::uint64_t{available_tail_actions} *
            constraints.concurrent_actions_per_mille / 1000);
        const unsigned minimum_concurrent_actions =
            constraints.concurrent_actions_per_mille == 0
            ? 0
            : std::min(
                  constraints.special_concurrent_per_mille != 0 &&
                          constraints.uses_concurrent_special_operations()
                      ? 32U : 28U,
                  available_tail_actions);
        const unsigned concurrent_action_budget = std::max(
            requested_concurrent_actions, minimum_concurrent_actions);
        const unsigned constrained_floor_actions =
            constraints.minimum_serial_actions();
        const unsigned maximum_concurrent_limit =
            target_before_redirect -
            std::min(constrained_floor_actions, available_tail_actions);
        const unsigned concurrent_action_limit = std::min(
            maximum_concurrent_limit, actions + concurrent_action_budget);
        // The tail is a constrained-random issue window. Each window contains
        // all five producer classes before any completion drain, so cache,
        // TLB, forwarding, and queue timing can interact in one simulation.
        while (actions + 7 <= concurrent_action_limit) {
            std::optional<unsigned> special_kind;
            if (actions + 8 <= concurrent_action_limit &&
                constraints.special_concurrent_per_mille != 0 &&
                constraints.uses_concurrent_special_operations()) {
                for (unsigned index = 0;
                     index < constraint_coverage.special_concurrent.size();
                     ++index) {
                    if (constraints.operation_weights[
                            RandomConstraints::noncacheable + index] != 0 &&
                        constraint_coverage.special_concurrent[index] == 0) {
                        special_kind = RandomConstraints::noncacheable + index;
                        break;
                    }
                }
                if (!special_kind &&
                    random() % 1000 <
                        constraints.special_concurrent_per_mille) {
                    special_kind =
                        constraints.choose_concurrent_special_operation(random());
                }
            }
            TranslationContext translation = choose_translation_context();
            if (special_kind &&
                (*special_kind == RandomConstraints::noncacheable ||
                 *special_kind == RandomConstraints::mmio) &&
                translation.regime == RandomConstraints::translation_bare) {
                translation = translated_context();
            }
            const TranslationLeafTopology leaf =
                choose_translation_leaf(translation);
            if (!enter_translation_context(translation) ||
                !issue_constrained_fence(
                    special_kind.value_or(RandomConstraints::scalar_load))) {
                return false;
            }
            const std::uint64_t ptw_at_window_start =
                environment.ptw_requests();
            const std::uint64_t requests_at_window_start =
                environment.tilelink_requests();
            const std::uint64_t window_base =
                constrained_cacheable_address(64, translation, leaf) &
                ~std::uint64_t{63};
            std::optional<memblock::LoadTransaction> special_load;
            if (special_kind == RandomConstraints::noncacheable) {
                special_load = make_load(
                    nc_base + (random() % 128) * 8,
                    memblock::LoadOp::ld, random() % 3);
            } else if (special_kind == RandomConstraints::mmio) {
                const std::uint64_t offset = (random() % 128) * 8;
                special_load = make_load(
                    mmio_access_address() + offset,
                    memblock::LoadOp::ld, random() % 3);
                special_load->oracle_address = mmio_physical + offset;
                special_load->expected_debug_is_mmio = true;
                special_load->expected_debug_is_ncio = false;
                special_load->expected_debug_is_perf_cnt = false;
            }
            const bool vector_corner =
                random() % 1000 < constraints.vector_corner_per_mille;
            const auto required_load_mode = static_cast<memblock::VectorAddressingMode>(
                coverage.concurrent_windows < 4
                    ? coverage.concurrent_windows
                    : vector_corner ? random() % 4 : 0);
            const auto required_store_mode = static_cast<memblock::VectorAddressingMode>(
                coverage.concurrent_windows < 4
                    ? (coverage.concurrent_windows + 1) % 4
                    : vector_corner ? random() % 4 : 0);
            const auto scalar_op =
                static_cast<memblock::LoadOp>(random() % 7);
            auto scalar = make_load(
                constrained_window_address(
                    window_base,
                    1U << (static_cast<unsigned>(scalar_op) & 3U)),
                scalar_op, random() % 3);
            auto vector_load = make_vector(
                false, window_base + 32, random() % 4, random() % 2,
                required_load_mode);
            const auto scalar_store_op =
                static_cast<memblock::StoreOp>(random() % 4);
            auto scalar_store = make_store(
                constrained_window_address(
                    window_base,
                    1U << static_cast<unsigned>(scalar_store_op)),
                random(), scalar_store_op, random() % 2, random() % 2);
            auto vector_store = make_vector(
                true, window_base + 0x100 + (random() % 8U) * 8U,
                random() % 4, random() % 2,
                required_store_mode);
            auto prefetch = make_prefetch(
                window_base + 0x180 + (random() % 8U) * 8U,
                static_cast<memblock::PrefetchOp>(8 + random() % 3), random() % 3);
            vector_load.vm = !vector_corner || (random() & 1U) != 0;
            vector_load.mask_bits = static_cast<std::uint16_t>(random());
            vector_store.vm = !vector_corner || (random() & 1U) != 0;
            vector_store.mask_bits = static_cast<std::uint16_t>(random());
            vector_load.vl = vector_corner
                ? static_cast<std::uint8_t>(
                      random() % ((16U >> vector_load.eew) + 1))
                : static_cast<std::uint8_t>(16U >> vector_load.eew);
            vector_store.vl = vector_corner
                ? static_cast<std::uint8_t>(
                      random() % ((16U >> vector_store.eew) + 1))
                : static_cast<std::uint8_t>(16U >> vector_store.eew);
            vector_load.vstart = vector_load.vl == 0
                ? 0 : static_cast<std::uint8_t>(random() % (vector_load.vl + 1));
            vector_store.vstart = vector_store.vl == 0
                ? 0 : static_cast<std::uint8_t>(random() % (vector_store.vl + 1));
            vector_load.address = window_base +
                (random() % 8U) * (1U << vector_load.eew);
            vector_store.address = window_base + 0x100 +
                (random() % 8U) * (1U << vector_store.eew);
            randomize_vector_addressing(vector_load);
            randomize_vector_addressing(vector_store);

            {
                std::ostringstream window;
                window << "seeded-mixed-tail"
                       << ":window=" << coverage.concurrent_windows
                       << ":scalar=0x" << std::hex << scalar.address
                       << "/" << static_cast<unsigned>(scalar.op)
                       << ":vload=0x" << vector_load.address << std::dec
                       << "/" << static_cast<unsigned>(vector_load.addressing)
                       << "/" << static_cast<unsigned>(vector_load.eew)
                       << ":store=0x" << std::hex << scalar_store.address
                       << std::dec << "/" << static_cast<unsigned>(scalar_store.op)
                       << ":vstore=0x" << std::hex << vector_store.address
                       << std::dec << "/"
                       << static_cast<unsigned>(vector_store.addressing)
                       << "/" << static_cast<unsigned>(vector_store.eew);
                phase = window.str();
            }

            if (special_load) {
                environment.expect_load(*special_load);
            }
            environment.expect_load(scalar);
            environment.expect_vector(vector_load);
            environment.expect_store(scalar_store);
            environment.expect_vector(vector_store);
            environment.expect_prefetch(prefetch);
            if ((special_load && !environment.enqueue_load(*special_load)) ||
                !environment.enqueue_load(scalar) ||
                !environment.enqueue_vector(vector_load) ||
                !environment.enqueue_store(
                    scalar_store, memblock::lq_pointer_value(lq_offset)) ||
                !environment.enqueue_vector(vector_store) ||
                !environment.enqueue_prefetch(prefetch)) {
                return false;
            }

            if (special_load) {
                const bool mmio = special_kind == RandomConstraints::mmio;
                const std::uint64_t uncache_before =
                    environment.uncache_requests();
                if ((mmio && !environment.set_rob_head(
                                 special_load->rob, special_load->rob_flag)) ||
                    !environment.issue_load(*special_load, 2048) ||
                    (mmio && !environment.wait_for_mmio_request(
                                 special_load->rob, special_load->rob_flag,
                                 8192)) ||
                    (mmio && !environment.run_until_uncache_requests(
                                 uncache_before + 1, 8192))) {
                    return false;
                }
            }

            std::array<unsigned, 5> issue_order{{0, 1, 2, 3, 4}};
            std::shuffle(issue_order.begin(), issue_order.end(), random);
            // Ordered indexed vector loads wait for all older LQ entries.
            // Keep the dependency legal while retaining a randomized order
            // for the remaining independent classes.
            if (vector_load.addressing ==
                    memblock::VectorAddressingMode::indexed_ordered) {
                auto scalar_position = std::find(
                    issue_order.begin(), issue_order.end(), 0U);
                auto vector_position = std::find(
                    issue_order.begin(), issue_order.end(), 1U);
                if (vector_position < scalar_position) {
                    std::iter_swap(vector_position, scalar_position);
                }
            }
            if (!environment.run_cycles(
                    special_kind ? 0 : random_delay(0, 3))) {
                return false;
            }
            bool scalar_store_data_first = false;
            for (const unsigned issue_class : issue_order) {
                switch (issue_class) {
                case 0:
                    if (!environment.set_rob_head(scalar.rob, scalar.rob_flag) ||
                        !environment.issue_load(scalar, 2048)) {
                        return false;
                    }
                    break;
                case 1:
                    if (!environment.issue_vector(vector_load, 2048)) {
                        return false;
                    }
                    break;
                case 2:
                    scalar_store_data_first = (random() & 1U) != 0;
                    if (scalar_store_data_first
                            ? (!environment.issue_store_data(scalar_store, 2048) ||
                               !environment.issue_store_address(scalar_store, 2048))
                            : (!environment.issue_store_address(scalar_store, 2048) ||
                               !environment.issue_store_data(scalar_store, 2048))) {
                        return false;
                    }
                    break;
                case 3:
                    if (!environment.issue_vector(vector_store, 2048)) {
                        return false;
                    }
                    break;
                default:
                    if (!environment.issue_prefetch(prefetch, 2048)) {
                        return false;
                    }
                    break;
                }
                if (!environment.run_cycles(random_delay(0, 5))) {
                    return false;
                }
                const std::size_t pending_scalar =
                    environment.pending_scalar_loads();
                const std::size_t pending_prefetch =
                    environment.pending_prefetches();
                const std::size_t pending_store =
                    environment.pending_scalar_stores();
                const std::size_t pending_vector_load =
                    environment.pending_vector_loads();
                const std::size_t pending_vector_store =
                    environment.pending_vector_stores();
                const std::size_t pending_total = pending_scalar + pending_prefetch +
                    pending_store + pending_vector_load + pending_vector_store;
                unsigned pending_classes = 0;
                pending_classes += pending_scalar != 0;
                pending_classes += pending_prefetch != 0;
                pending_classes += pending_store != 0;
                pending_classes += pending_vector_load != 0;
                pending_classes += pending_vector_store != 0;
                coverage.max_unresolved = std::max<std::uint64_t>(
                    coverage.max_unresolved, pending_total);
                coverage.max_unresolved_classes = std::max<std::uint64_t>(
                    coverage.max_unresolved_classes, pending_classes);
                if (pending_total >= 2 && pending_classes >= 2) {
                    ++coverage.unresolved_overlap_samples;
                }
            }
            coverage.sample(scalar);
            coverage.sample(vector_load);
            coverage.sample(scalar_store, scalar_store_data_first);
            coverage.sample(vector_store);
            coverage.sample(prefetch);
            const unsigned scalar_store_bytes =
                1U << static_cast<unsigned>(scalar_store.op);
            const bool scalar_store_crosses_page =
                (scalar_store.address & 0xfffU) + scalar_store_bytes > 0x1000U;
            const std::vector<memblock::VectorMemoryTransaction>
                window_vectors{vector_load, vector_store};
            if ((scalar_store_crosses_page &&
                 !environment.set_rob_head(
                     scalar_store.rob, scalar_store.rob_flag)) ||
                !environment.run_until_store_complete_with_replay(
                    scalar_store, constrained_completion_timeout,
                    scalar_store_crosses_page) ||
                !environment.run_until_vector_complete_with_replays(
                    window_vectors, constrained_completion_timeout) ||
                !environment.run_until_all_complete(
                    constrained_completion_timeout) ||
                !environment.run_cycles(8) ||
                !environment.commit_store(scalar_store, 4096) ||
                !environment.commit_vector_store(vector_store, 4096) ||
                !environment.set_rob_head(prefetch.rob, prefetch.rob_flag) ||
                !environment.run_until_queues_retired(4096)) {
                return false;
            }
            if (special_load) {
                coverage.sample(*special_load);
            }
            const auto scalar_readback = make_load(
                scalar_store.address,
                static_cast<memblock::LoadOp>(scalar_store.op), random() % 3);
            if (!environment.set_rob_head(
                    scalar_readback.rob, scalar_readback.rob_flag) ||
                !issue_load(
                    scalar_readback,
                    scalar_forward_value(
                        scalar_store.data, scalar_store_bytes, false)) ||
                !environment.run_until_queues_retired(4096)) {
                return false;
            }
            auto vector_readback = make_vector(
                false, vector_store.address, vector_store.eew, random() % 2,
                vector_store.addressing);
            vector_readback.vl = vector_store.vl;
            vector_readback.vstart = vector_store.vstart;
            vector_readback.vm = vector_store.vm;
            vector_readback.mask_bits = vector_store.mask_bits;
            vector_readback.vma = vector_store.vma;
            vector_readback.vta = vector_store.vta;
            vector_readback.stride = vector_store.stride;
            vector_readback.index = vector_store.index;
            if (!environment.set_rob_head(
                    vector_readback.rob, vector_readback.rob_flag) ||
                !issue_vector(
                    vector_readback,
                    environment.memory().expected_vector_load(vector_readback))) {
                return false;
            }
            if (
                // Keep the commit boundary at the last uop in this window.
                // StoreQueue treats a uop whose ROB equals pendingPtr as
                // committed; pointing at the next ROB would auto-commit the
                // first store of the following window before its explicit
                // commit operation is issued.
                !environment.set_rob_head(
                    memblock::rob_pointer_value(rob_offset - 1),
                    memblock::rob_pointer_flag(rob_offset - 1)) ||
                !environment.run_until_queues_retired(4096) ||
                // LSQ free-count updates trail the externally visible dequeue
                // pulse by a few cycles. Let enqueue readiness settle before
                // starting the next five-class window.
                !environment.run_cycles(8)) {
                return false;
            }
            ++coverage.concurrent_windows;
            coverage.concurrent_actions += special_kind ? 8 : 7;
            for (auto &count : coverage.concurrent_ops) {
                ++count;
            }
            constraint_coverage.operations[RandomConstraints::scalar_load] += 2;
            constraint_coverage.sample_dcache_load(std::nullopt);
            constraint_coverage.sample_dcache_load(std::nullopt);
            ++constraint_coverage.operations[RandomConstraints::scalar_store];
            constraint_coverage.operations[RandomConstraints::vector_load] += 2;
            ++constraint_coverage.operations[RandomConstraints::vector_store];
            ++constraint_coverage.operations[RandomConstraints::prefetch];
            constraint_coverage.actions += 7;
            if (special_kind) {
                const unsigned special_index =
                    *special_kind - RandomConstraints::noncacheable;
                ++constraint_coverage.special_concurrent[special_index];
                constraint_coverage.sample_operation(*special_kind);
                constraint_coverage.sample_uncache(
                    special_index, false, std::nullopt);
                auto &directions = *special_kind ==
                        RandomConstraints::noncacheable
                    ? constraint_coverage.nc_directions
                    : constraint_coverage.mmio_directions;
                ++directions[0];
                ++coverage.noncacheable;
                ++actions;
            }
            constraint_coverage.sample_dcache(
                requests_at_window_start, environment.tilelink_requests());
            constraint_coverage.sample_translation(
                translation, leaf, ptw_at_window_start,
                environment.ptw_requests(), special_kind ? 8 : 7, 7);
            actions += 5;
            coverage.cacheable += 7;
        }
        const std::array<memblock::AtomicOp, 9> atomic_operations_d{{
            memblock::AtomicOp::amoadd_d,
            memblock::AtomicOp::amoxor_d,
            memblock::AtomicOp::amoand_d,
            memblock::AtomicOp::amoor_d,
            memblock::AtomicOp::amoswap_d,
            memblock::AtomicOp::amomin_d,
            memblock::AtomicOp::amomax_d,
            memblock::AtomicOp::amominu_d,
            memblock::AtomicOp::amomaxu_d,
        }};
        const std::array<memblock::AtomicOp, 9> atomic_operations_w{{
            memblock::AtomicOp::amoadd_w,
            memblock::AtomicOp::amoxor_w,
            memblock::AtomicOp::amoand_w,
            memblock::AtomicOp::amoor_w,
            memblock::AtomicOp::amoswap_w,
            memblock::AtomicOp::amomin_w,
            memblock::AtomicOp::amomax_w,
            memblock::AtomicOp::amominu_w,
            memblock::AtomicOp::amomaxu_w,
        }};
        const std::array<memblock::LoadOp, 7> hypervisor_load_operations{{
            memblock::LoadOp::hlvb,
            memblock::LoadOp::hlvh,
            memblock::LoadOp::hlvw,
            memblock::LoadOp::hlvd,
            memblock::LoadOp::hlvbu,
            memblock::LoadOp::hlvhu,
            memblock::LoadOp::hlvwu,
        }};
        const std::array<memblock::LoadOp, 5>
            hypervisor_misaligned_load_operations{{
                memblock::LoadOp::hlvh,
                memblock::LoadOp::hlvw,
                memblock::LoadOp::hlvd,
                memblock::LoadOp::hlvhu,
                memblock::LoadOp::hlvwu,
            }};
        const std::array<memblock::LoadOp, 2> hypervisor_execute_operations{{
            memblock::LoadOp::hlvxhu,
            memblock::LoadOp::hlvxwu,
        }};
        const std::array<memblock::StoreOp, 4> hypervisor_store_operations{{
            memblock::StoreOp::hsvb,
            memblock::StoreOp::hsvh,
            memblock::StoreOp::hsvw,
            memblock::StoreOp::hsvd,
        }};
        std::array<std::uint64_t, 16> atomic_values{};
        for (std::size_t slot = 0; slot < atomic_values.size(); ++slot) {
            atomic_values[slot] = environment.memory().expected_load(
                atomic_base + slot * 64, memblock::LoadOp::ld);
        }
        constexpr unsigned stride_training_loads = 8;
        constexpr std::uint64_t random_stride_base = cache0_base + 0x50000;
        constexpr std::uint64_t random_stride_bytes = 128;
        constexpr unsigned random_stride_slots = 2048;
        unsigned stride_stream_loads = 0;

        struct RandomVectorShape {
            unsigned addressing;
            unsigned eew;
            unsigned vsew;
            int lmul_log2;
            int emul_log2;
            long double weight;
        };
        std::vector<RandomVectorShape> vector_shapes;
        for (unsigned addressing = 0; addressing < 4; ++addressing) {
            if (constraints.vector_addressing_weights[addressing] == 0) {
                continue;
            }
            for (unsigned eew = 0; eew < 4; ++eew) {
                if (constraints.vector_eew_weights[eew] == 0) {
                    continue;
                }
                for (unsigned vsew = 0; vsew < 4; ++vsew) {
                    if (constraints.vector_sew_weights[vsew] == 0) {
                        continue;
                    }
                    for (int lmul_log2 = -3; lmul_log2 <= 3; ++lmul_log2) {
                        if (constraints.vector_lmul_weights[lmul_log2 + 3] == 0) {
                            continue;
                        }
                        const int emul_log2 = static_cast<int>(eew) -
                            static_cast<int>(vsew) + lmul_log2;
                        if (lmul_log2 < static_cast<int>(vsew) - 3 ||
                            emul_log2 < -3 || emul_log2 > 3 ||
                            constraints.vector_emul_weights[
                                emul_log2 + 3] == 0) {
                            continue;
                        }
                        const unsigned vector_bytes = lmul_log2 < 0
                            ? 16U >> static_cast<unsigned>(-lmul_log2)
                            : 16U << static_cast<unsigned>(lmul_log2);
                        if ((vector_bytes >> vsew) <
                            constraints.vector_policy_minimum_vlmax()) {
                            continue;
                        }
                        long double weight =
                            constraints.vector_addressing_weights[addressing];
                        weight *= constraints.vector_eew_weights[eew];
                        weight *= constraints.vector_sew_weights[vsew];
                        weight *= constraints.vector_lmul_weights[lmul_log2 + 3];
                        weight *= constraints.vector_emul_weights[emul_log2 + 3];
                        vector_shapes.push_back({
                            addressing, eew, vsew, lmul_log2, emul_log2,
                            weight});
                    }
                }
            }
        }
        const auto vector_shape_deficit = [&](const RandomVectorShape &shape,
                                              unsigned direction) {
            return static_cast<unsigned>(
                (constraint_coverage.vector_shape_crosses.at(
                     ConstraintCoverage::vector_shape_cross_index(
                         direction, shape.addressing, shape.eew, shape.vsew,
                         shape.lmul_log2)) == 0) +
                (constraint_coverage.vector_addressing[shape.addressing] == 0) +
                (constraint_coverage.vector_eews[shape.eew] == 0) +
                (constraint_coverage.vector_sews[shape.vsew] == 0) +
                (constraint_coverage.vector_lmuls[shape.lmul_log2 + 3] == 0) +
                (constraint_coverage.vector_emuls[shape.emul_log2 + 3] == 0));
        };
        auto choose_vector_shape = [&](unsigned direction) -> const RandomVectorShape & {
            unsigned maximum_deficit = 0;
            for (const auto &shape : vector_shapes) {
                maximum_deficit = std::max(
                    maximum_deficit, vector_shape_deficit(shape, direction));
            }
            long double total_weight = 0;
            for (const auto &shape : vector_shapes) {
                if (vector_shape_deficit(shape, direction) == maximum_deficit) {
                    total_weight += shape.weight;
                }
            }
            const long double unit = static_cast<long double>(random() >> 11) /
                static_cast<long double>(std::uint64_t{1} << 53);
            long double selection = unit * total_weight;
            const RandomVectorShape *fallback = nullptr;
            for (const auto &shape : vector_shapes) {
                if (vector_shape_deficit(shape, direction) != maximum_deficit) {
                    continue;
                }
                fallback = &shape;
                if (selection < shape.weight) {
                    return shape;
                }
                selection -= shape.weight;
            }
            return *fallback;
        };

        struct RandomVectorSegmentShape {
            unsigned addressing;
            unsigned eew;
            unsigned vsew;
            int lmul_log2;
            int emul_log2;
            unsigned nf_index;
            long double weight;
        };
        std::vector<RandomVectorSegmentShape> vector_segment_shapes;
        for (unsigned addressing = 0; addressing < 4; ++addressing) {
            if (constraints.vector_segment_addressing_weights[addressing] == 0) {
                continue;
            }
            const bool indexed = addressing >= 2;
            for (unsigned eew = 0; eew < 4; ++eew) {
                if (constraints.vector_segment_eew_weights[eew] == 0) {
                    continue;
                }
                for (unsigned vsew = 0; vsew < 4; ++vsew) {
                    if (constraints.vector_segment_sew_weights[vsew] == 0) {
                        continue;
                    }
                    for (int lmul_log2 = -3; lmul_log2 <= 3; ++lmul_log2) {
                        if (constraints.vector_segment_lmul_weights[
                                lmul_log2 + 3] == 0) {
                            continue;
                        }
                        const int emul_log2 = static_cast<int>(eew) -
                            static_cast<int>(vsew) + lmul_log2;
                        if (lmul_log2 < static_cast<int>(vsew) - 3 ||
                            emul_log2 < -3 || emul_log2 > 3 ||
                            constraints.vector_segment_emul_weights[
                                emul_log2 + 3] == 0) {
                            continue;
                        }
                        const unsigned group_uops = 1U <<
                            static_cast<unsigned>(std::max(
                                indexed ? lmul_log2 : emul_log2, 0));
                        for (unsigned nf_index = 0; nf_index < 7; ++nf_index) {
                            if (constraints.vector_segment_nf_weights[
                                    nf_index] == 0 ||
                                group_uops * (nf_index + 2U) > 8) {
                                continue;
                            }
                            long double weight =
                                constraints.vector_segment_addressing_weights[
                                    addressing];
                            weight *= constraints.vector_segment_eew_weights[eew];
                            weight *= constraints.vector_segment_sew_weights[vsew];
                            weight *= constraints.vector_segment_lmul_weights[
                                lmul_log2 + 3];
                            weight *= constraints.vector_segment_emul_weights[
                                emul_log2 + 3];
                            weight *= constraints.vector_segment_nf_weights[
                                nf_index];
                            vector_segment_shapes.push_back({
                                addressing, eew, vsew, lmul_log2, emul_log2,
                                nf_index, weight});
                        }
                    }
                }
            }
        }
        const auto vector_segment_shape_deficit = [&] (
            const RandomVectorSegmentShape &shape) {
            return static_cast<unsigned>(
                (constraint_coverage.vector_segment_addressing[
                     shape.addressing] == 0) +
                (constraint_coverage.vector_segment_eews[shape.eew] == 0) +
                (constraint_coverage.vector_segment_sews[shape.vsew] == 0) +
                (constraint_coverage.vector_segment_lmuls[
                     shape.lmul_log2 + 3] == 0) +
                (constraint_coverage.vector_segment_emuls[
                     shape.emul_log2 + 3] == 0) +
                (constraint_coverage.vector_segment_nfs[
                     shape.nf_index] == 0));
        };
        auto choose_vector_segment_shape = [&]()
            -> const RandomVectorSegmentShape & {
            unsigned maximum_deficit = 0;
            for (const auto &shape : vector_segment_shapes) {
                maximum_deficit = std::max(
                    maximum_deficit, vector_segment_shape_deficit(shape));
            }
            long double total_weight = 0;
            for (const auto &shape : vector_segment_shapes) {
                if (vector_segment_shape_deficit(shape) == maximum_deficit) {
                    total_weight += shape.weight;
                }
            }
            const long double unit = static_cast<long double>(random() >> 11) /
                static_cast<long double>(std::uint64_t{1} << 53);
            long double selection = unit * total_weight;
            const RandomVectorSegmentShape *fallback = nullptr;
            for (const auto &shape : vector_segment_shapes) {
                if (vector_segment_shape_deficit(shape) != maximum_deficit) {
                    continue;
                }
                fallback = &shape;
                if (selection < shape.weight) {
                    return shape;
                }
                selection -= shape.weight;
            }
            return *fallback;
        };
        const auto choose_binary_class = [&] (
            unsigned true_per_mille,
            const std::array<std::uint64_t, 2> &counts) {
            if (true_per_mille != 1000 && counts[0] == 0) {
                return false;
            }
            if (true_per_mille != 0 && counts[1] == 0) {
                return true;
            }
            return random() % 1000 < true_per_mille;
        };
        auto make_random_vector_uops = [&] (
            bool store, std::uint64_t address, const RandomVectorShape &shape,
            bool vector_corner) {
            const bool indexed = shape.addressing >= 2;
            const unsigned data_eew = indexed ? shape.vsew : shape.eew;
            const unsigned data_bytes = 1U << data_eew;
            const unsigned vector_bytes = shape.lmul_log2 < 0
                ? 16U >> static_cast<unsigned>(-shape.lmul_log2)
                : 16U << static_cast<unsigned>(shape.lmul_log2);
            const unsigned vlmax = vector_bytes >> shape.vsew;
            const unsigned uop_count = 1U << static_cast<unsigned>(std::max(
                indexed ? std::max(shape.lmul_log2, shape.emul_log2)
                        : shape.emul_log2,
                0));
            unsigned flow_num = memblock::kVectorUnitStrideMaxFlows;
            if (shape.addressing == 1) {
                const unsigned bytes_per_uop = shape.emul_log2 < 0
                    ? 16U >> static_cast<unsigned>(-shape.emul_log2)
                    : 16U;
                flow_num = bytes_per_uop >> shape.eew;
            } else if (indexed) {
                const int flow_mul_log2 = shape.emul_log2 > shape.lmul_log2
                    ? shape.emul_log2 : shape.lmul_log2;
                const unsigned bytes_per_uop = flow_mul_log2 < 0
                    ? 16U >> static_cast<unsigned>(-flow_mul_log2)
                    : 16U;
                flow_num = bytes_per_uop >>
                    (shape.emul_log2 > shape.lmul_log2
                        ? shape.eew : shape.vsew);
            }
            std::int64_t stride = 0;
            if (shape.addressing == 1) {
                const std::int64_t magnitude =
                    static_cast<std::int64_t>(data_bytes *
                        (1U + random() % 4U));
                if (vector_corner && !store && random() % 6U == 0) {
                    stride = 0;
                } else {
                    stride = (random() & 1U) == 0 ? magnitude : -magnitude;
                }
                if (stride < 0) {
                    address += static_cast<std::uint64_t>(-stride) *
                        (vlmax - 1U);
                }
            }
            bool masked = choose_binary_class(
                constraints.vector_masked_per_mille,
                constraint_coverage.vector_masked);
            bool vma = choose_binary_class(
                constraints.vector_vma_per_mille,
                constraint_coverage.vector_vma);
            bool vta = choose_binary_class(
                constraints.vector_vta_per_mille,
                constraint_coverage.vector_vta);
            bool partial_vl = choose_binary_class(
                constraints.vector_partial_vl_per_mille,
                constraint_coverage.vector_partial_vl);
            bool nonzero_vstart = choose_binary_class(
                constraints.vector_nonzero_vstart_per_mille,
                constraint_coverage.vector_nonzero_vstart);
            const bool force_mask_agnostic = !store &&
                constraints.vector_vma_per_mille != 0 &&
                constraints.vector_masked_per_mille != 0 &&
                constraint_coverage.vector_mask_agnostic == 0;
            const bool force_tail_agnostic = !force_mask_agnostic && !store &&
                constraints.vector_vta_per_mille != 0 &&
                constraints.vector_partial_vl_per_mille != 0 &&
                constraint_coverage.vector_tail_agnostic == 0;
            if (force_mask_agnostic) {
                masked = true;
                vma = true;
            }
            if (force_tail_agnostic) {
                vta = true;
                partial_vl = true;
            }
            if (partial_vl && masked && nonzero_vstart && vlmax < 3U) {
                if (constraints.vector_partial_vl_per_mille != 1000) {
                    partial_vl = false;
                } else if (constraints.vector_masked_per_mille != 1000) {
                    masked = false;
                } else {
                    nonzero_vstart = false;
                }
            }
            unsigned vl = vlmax;
            if (partial_vl) {
                const unsigned minimum_vl = masked && nonzero_vstart
                    ? 2U : (masked || nonzero_vstart ? 1U : 0U);
                vl = minimum_vl + random() % (vlmax - minimum_vl);
            }
            if (masked && nonzero_vstart && vl < 2U) {
                nonzero_vstart = false;
            }
            unsigned vstart = 0;
            if (nonzero_vstart) {
                const unsigned maximum = masked
                    ? std::min(vl - 1U, 15U) : vl;
                if (maximum == 0) {
                    nonzero_vstart = false;
                } else {
                    vstart = 1U + random() % maximum;
                }
            }
            std::uint16_t mask_bits = static_cast<std::uint16_t>(random());
            if (masked) {
                mask_bits &= static_cast<std::uint16_t>(~(1U << vstart));
            }
            const std::uint64_t rob = rob_offset++;
            std::uint64_t &queue_cursor = store ? sq_offset : lq_offset;
            const std::uint8_t vlmul = static_cast<std::uint8_t>(
                shape.lmul_log2 < 0
                    ? shape.lmul_log2 + 8 : shape.lmul_log2);
            const std::uint8_t first_pdest = static_cast<std::uint8_t>(
                1U + random() % (256U - uop_count));
            const unsigned index_bytes = 1U << shape.eew;
            const unsigned index_group_bytes_per_uop = shape.emul_log2 < 0
                ? 16U >> static_cast<unsigned>(-shape.emul_log2)
                : 16U;
            const unsigned index_elements_per_vreg =
                index_group_bytes_per_uop >> shape.eew;
            const bool special_indexed =
                indexed && shape.emul_log2 > shape.lmul_log2;

            std::vector<memblock::VectorMemoryTransaction> uops;
            uops.reserve(uop_count);
            for (unsigned uop = 0; uop < uop_count; ++uop) {
                const unsigned vd_index = special_indexed
                    ? uop >> static_cast<unsigned>(
                        shape.emul_log2 - shape.lmul_log2)
                    : uop;
                uops.push_back(memblock::VectorMemoryTransaction{
                    .store = store,
                    .address = address,
                    .stride = stride,
                    .addressing = static_cast<memblock::VectorAddressingMode>(
                        shape.addressing),
                    .eew = static_cast<std::uint8_t>(shape.eew),
                    .vsew = static_cast<std::uint8_t>(shape.vsew),
                    .vl = static_cast<std::uint8_t>(vl),
                    .vstart = static_cast<std::uint8_t>(vstart),
                    .vm = !masked,
                    .mask_bits = mask_bits,
                    .vma = vma,
                    .vta = vta,
                    .rob = memblock::rob_pointer_value(rob),
                    .rob_flag = memblock::rob_pointer_flag(rob),
                    .lq = memblock::lq_pointer_value(
                        store ? lq_offset : queue_cursor),
                    .lq_flag = !store && memblock::lq_pointer_flag(queue_cursor),
                    .sq = memblock::sq_pointer_value(
                        store ? queue_cursor : sq_offset),
                    .sq_flag = store && memblock::sq_pointer_flag(queue_cursor),
                    .pdest = static_cast<std::uint8_t>(first_pdest + vd_index),
                    .lane = uop % memblock::kVectorMemoryLanes,
                    .flow_num = static_cast<std::uint8_t>(flow_num),
                    .ftq_ptr = random() & 0x3fU,
                    .ftq_offset = static_cast<std::uint8_t>(random() & 7U),
                    .vlmul = vlmul,
                    .vuop_idx = static_cast<std::uint8_t>(uop),
                    .last_uop = uop + 1U == uop_count,
                });
                auto &transaction = uops.back();
                for (auto &byte : transaction.data) {
                    byte = static_cast<unsigned char>(random());
                }
                if (indexed) {
                    const unsigned global_first = uop * flow_num;
                    const unsigned index_register_base =
                        global_first / index_elements_per_vreg *
                        index_elements_per_vreg;
                    for (unsigned element = 0;
                         element < index_elements_per_vreg; ++element) {
                        const std::uint64_t offset =
                            static_cast<std::uint64_t>(
                                index_register_base + element) *
                            data_bytes * 2U;
                        for (unsigned byte = 0; byte < index_bytes; ++byte) {
                            transaction.index[element * index_bytes + byte] =
                                static_cast<unsigned char>(offset >> (8 * byte));
                        }
                    }
                }
                queue_cursor += flow_num;
            }
            return uops;
        };
        auto issue_random_vector_instruction = [&] (
            const std::vector<memblock::VectorMemoryTransaction> &uops) {
            const bool store = uops.front().store;
            const bool reverse_issue = uops.front().addressing ==
                memblock::VectorAddressingMode::indexed_unordered &&
                (random() & 1U) != 0;
            std::size_t begin = 0;
            while (begin < uops.size()) {
                std::size_t end = begin;
                unsigned window_flows = 0;
                const unsigned capacity = store
                    ? memblock::kStoreQueueEntries - memblock::kSqEnqueueHeadroom
                    : memblock::kVirtualLoadQueueEntries -
                        memblock::kLqEnqueueHeadroom;
                while (end < uops.size() &&
                       window_flows + uops[end].flow_num <= capacity) {
                    window_flows += uops[end].flow_num;
                    ++end;
                }
                const std::vector<memblock::VectorMemoryTransaction> window(
                    uops.begin() + begin, uops.begin() + end);
                bool requires_misaligned_head = false;
                bool requires_store_pending = false;
                for (const auto &uop : window) {
                    const unsigned element_bytes =
                        1U << memblock::vector_data_eew(uop);
                    const std::uint16_t active =
                        memblock::active_vector_elements(uop);
                    for (unsigned element = 0;
                         element < 16U / element_bytes; ++element) {
                        if (((active >> element) & 1U) == 0) {
                            continue;
                        }
                        const std::uint64_t element_address =
                            memblock::vector_element_address(uop, element);
                        bool translation_valid = true;
                        if (current_translation.regime ==
                                RandomConstraints::translation_stage1) {
                            const auto reference =
                                memblock::reference_page_walk_permissions(
                                    environment.memory(),
                                    random_stage1_roots[
                                        current_translation.stage1_mode],
                                    element_address,
                                    page_mode(
                                        current_translation.stage1_mode));
                            translation_valid = reference.walk.translated &&
                                reference.walk.physical_address ==
                                    element_address &&
                                (!reference.leaf_permissions ||
                                 memblock::reference_memory_access_permitted(
                                     store
                                         ? memblock::ReferenceMemoryAccess::store
                                         : memblock::ReferenceMemoryAccess::load,
                                     *reference.leaf_permissions,
                                     memblock::ReferencePrivilegeMode::supervisor,
                                     false, false));
                        } else if (current_translation.regime ==
                                       RandomConstraints::translation_nested) {
                            const auto reference =
                                memblock::reference_two_stage_access(
                                    environment.memory(),
                                    random_vs_roots[current_translation.vs_mode],
                                    random_g_roots[current_translation.g_mode],
                                    element_address,
                                    store
                                        ? memblock::ReferenceMemoryAccess::store
                                        : memblock::ReferenceMemoryAccess::load,
                                    page_mode(current_translation.vs_mode),
                                    page_mode(current_translation.g_mode));
                            translation_valid = reference.translated &&
                                reference.physical_address == element_address;
                        }
                        if (!translation_valid) {
                            std::ostringstream detail;
                            detail << "random-vector-translation-reference"
                                   << ":store=" << store
                                   << ":regime=" << current_translation.regime
                                   << ":stage1_mode="
                                   << current_translation.stage1_mode
                                   << ":vs_mode=" << current_translation.vs_mode
                                   << ":g_mode=" << current_translation.g_mode
                                   << ":uop="
                                   << static_cast<unsigned>(uop.vuop_idx)
                                   << ":element=" << element
                                   << ":address=0x" << std::hex
                                   << element_address << std::dec;
                            phase = detail.str();
                            return false;
                        }
                        requires_misaligned_head |= !store &&
                            ((element_address & 0xfU) + element_bytes) > 16U;
                        requires_store_pending |= store &&
                            (element_address & (element_bytes - 1U)) != 0;
                    }
                }
                if (((requires_misaligned_head || requires_store_pending) &&
                     !environment.set_rob_head(
                         window.front().rob, window.front().rob_flag))) {
                    return false;
                }
                for (const auto &uop : window) {
                    environment.expect_vector(uop);
                    if (!environment.enqueue_vector(uop)) {
                        return false;
                    }
                }
                for (std::size_t offset = 0; offset < window.size(); ++offset) {
                    const std::size_t issue_index = reverse_issue
                        ? window.size() - offset - 1U : offset;
                    if (!environment.issue_vector(
                            window[issue_index], constrained_completion_timeout)) {
                        return false;
                    }
                }
                if (requires_store_pending &&
                    (!environment.run_cycles(32) ||
                     !environment.pulse_pending_store(
                         window.front().rob, window.front().rob_flag))) {
                    return false;
                }
                if (!environment.run_until_vector_complete_with_replays(
                        window, constrained_completion_timeout,
                        requires_store_pending)) {
                    return false;
                }
                if (store) {
                    for (const auto &uop : window) {
                        if (!environment.commit_vector_store(uop, 8192)) {
                            return false;
                        }
                    }
                    if (!environment.run_until_queues_retired(8192)) {
                        return false;
                    }
                } else if (!environment.run_until_lq_retired(8192)) {
                    return false;
                }
                for (const auto &uop : window) {
                    coverage.sample(uop);
                }
                begin = end;
            }
            ++actions;
            return true;
        };
        const auto probe_coverage_incomplete = [&]() {
            return constraint_coverage
                .first_missing_probe_cross(constraints).has_value();
        };
        const auto issue_random_cmo = [&] (
            unsigned operation, bool dirty, bool younger_overlap,
            std::optional<bool> error_denied, unsigned probe_depth_class) {
            const unsigned cmo_completion_timeout =
                std::max(constrained_completion_timeout, 16384U);
            phase = "random-cmo-drain";
            if (!environment.run_until_all_complete(8192) ||
                !environment.run_until_queues_retired(8192) ||
                !environment.run_until_sbuffer_empty(
                    cmo_completion_timeout)) {
                return false;
            }

            unsigned locality = constraints.choose_locality(random());
            for (unsigned candidate = 0;
                 candidate < constraint_coverage.locality.size(); ++candidate) {
                if (constraints.locality_weights[candidate] != 0 &&
                    constraint_coverage.locality[candidate] == 0) {
                    locality = candidate;
                    break;
                }
            }
            ++constraint_coverage.locality[locality];

            constexpr std::uint64_t lines_per_half_block = 0x100000 / 64;
            const std::uint64_t sequence = cmo_locality_lines[locality]++;
            const std::uint64_t block = locality == 0
                ? 0 : locality == 1 ? 1 : 2 + (sequence / lines_per_half_block) % 6;
            const std::uint64_t line_in_block = locality == 0
                ? sequence % 32
                : locality == 1
                ? sequence % 512
                : sequence % lines_per_half_block;
            const std::uint64_t line = cmo_base + block * 0x200000 +
                line_in_block * 64;
            const std::uint64_t address = line + 8 * (1 + random() % 6);

            const auto warm = make_load(
                address, memblock::LoadOp::ld,
                random() % memblock::kScalarLoadLanes);
            environment.expect_load(warm);
            phase = "random-cmo-warm";
            if (!environment.set_rob_head(warm.rob, warm.rob_flag) ||
                !environment.enqueue_load(warm) ||
                !environment.issue_load(warm, 4096) ||
                !environment.run_until_complete(
                    cmo_completion_timeout) ||
                !environment.run_until_lq_retired(8192)) {
                return false;
            }

            if (dirty) {
                phase = "random-cmo-dirty";
                const auto dirty_store = make_store(
                    address, random(), memblock::StoreOp::sd,
                    random() % memblock::kScalarStoreLanes,
                    random() % memblock::kScalarStoreLanes);
                environment.expect_store(dirty_store);
                if (!environment.set_rob_head(
                        dirty_store.rob, dirty_store.rob_flag) ||
                    !environment.enqueue_store(
                        dirty_store, memblock::lq_pointer_value(lq_offset)) ||
                    !environment.issue_store_data(dirty_store, 4096) ||
                    !environment.issue_store_address_until_tlb_hit(
                        dirty_store, cmo_completion_timeout) ||
                    !environment.run_until_store_complete(
                        cmo_completion_timeout) ||
                    !environment.commit_store(dirty_store, 8192) ||
                    environment.sbuffer_empty()) {
                    return false;
                }
            }

            const unsigned probe_depth = probe_depth_class + 1U;
            std::vector<std::uint64_t> auxiliary_probe_lines;
            if (!error_denied) {
                constexpr std::uint64_t group_bytes = 0x400;
                constexpr std::uint64_t groups =
                    cmo_auxiliary_probe_span / group_bytes;
                const std::uint64_t group =
                    cmo_auxiliary_probe_group++ % groups;
                const std::uint64_t group_base =
                    cmo_auxiliary_probe_base + group * group_bytes;
                const unsigned target_set =
                    static_cast<unsigned>((line >> 6) & 0x7fU);
                for (unsigned candidate = 0;
                     auxiliary_probe_lines.size() + 1U < probe_depth;
                     ++candidate) {
                    const std::uint64_t auxiliary_line =
                        group_base + candidate * 64U;
                    if (((auxiliary_line >> 6) & 0x7fU) == target_set) {
                        continue;
                    }
                    auxiliary_probe_lines.push_back(auxiliary_line);
                }
                for (const std::uint64_t auxiliary_line :
                     auxiliary_probe_lines) {
                    const auto auxiliary_load = make_load(
                        auxiliary_line + 8U * (random() % 8U),
                        memblock::LoadOp::ld,
                        random() % memblock::kScalarLoadLanes);
                    environment.expect_load(auxiliary_load);
                    phase = "random-cmo-auxiliary-probe-warm";
                    if (!environment.set_rob_head(
                            auxiliary_load.rob, auxiliary_load.rob_flag) ||
                        !environment.enqueue_load(auxiliary_load) ||
                        !environment.issue_load(auxiliary_load, 4096) ||
                        !environment.run_until_complete(
                            cmo_completion_timeout) ||
                        !environment.run_until_lq_retired(8192)) {
                        return false;
                    }
                }
            }

            constexpr std::array<memblock::StoreOp,
                                 RandomConstraints::cmo_operation_count>
                cmo_ops{{
                    memblock::StoreOp::cbo_clean,
                    memblock::StoreOp::cbo_flush,
                    memblock::StoreOp::cbo_inval,
                }};
            auto cmo_transaction = make_store(
                line + random() % 64, random(), cmo_ops[operation],
                random() % memblock::kScalarStoreLanes,
                random() % memblock::kScalarStoreLanes);
            cmo_transaction.oracle_address = line;
            cmo_transaction.expected_output_flush_pipe = true;
            cmo_transaction.expected_debug_is_mmio = false;
            cmo_transaction.expected_debug_is_ncio = false;
            if (error_denied) {
                cmo_transaction.expected_exception_mask = *error_denied
                    ? memblock::kExceptionStoreAccessFault
                    : memblock::kExceptionHardwareError;
            }
            const std::vector<unsigned char> expected_line =
                environment.memory().read_beat(line, 64);
            const bool retain = operation == RandomConstraints::cmo_clean;
            const std::uint8_t probe_cap = retain ? 1U : 2U;
            const std::uint8_t expected_report = dirty
                ? (retain ? 0U : 1U)
                : (retain ? 4U : 2U);
            const std::uint64_t cmo_before =
                environment.dcache_cmo_requests(cmo_transaction.op);
            const std::uint64_t probe_responses_before =
                environment.dcache_probe_responses();
            const std::uint64_t probe_data_before =
                environment.dcache_probe_data();
            const std::uint64_t store_writebacks_before =
                environment.store_writebacks();
            const std::uint64_t error_responses_before =
                environment.dcache_error_response_requests();
            const std::uint64_t denied_beats_before =
                environment.dcache_denied_d_beats();
            const std::uint64_t corrupt_beats_before =
                environment.dcache_corrupt_d_beats();
            const std::uint64_t bus_line_before =
                environment.bus_expected_load(line + 24, memblock::LoadOp::ld);
            const std::uint64_t sq_cancels_before =
                environment.sq_redirect_canceled_observed();
            environment.expect_store(cmo_transaction);
            if (error_denied) {
                environment.inject_dcache_cmo_response_error_at(
                    line, *error_denied, !*error_denied);
            }
            phase = "random-cmo-issue";
            if (!environment.enqueue_store(
                    cmo_transaction, memblock::lq_pointer_value(lq_offset)) ||
                !environment.issue_store_address_until_tlb_hit(
                    cmo_transaction, cmo_completion_timeout) ||
                !environment.issue_store_data(cmo_transaction, 4096)) {
                return false;
            }
            const unsigned cmo_response_delay = younger_overlap
                ? 512U + static_cast<unsigned>(random() % 513U)
                : 128U + static_cast<unsigned>(random() % 897U);
            environment.force_next_cmo_response_delay(cmo_response_delay);
            phase = "random-cmo-probe";
            if (!environment.wait_for_cmo_store_request(
                    cmo_transaction, cmo_before + 1, 8192) ||
                environment.dcache_last_cmo_address() != line ||
                environment.dcache_last_cmo_source() !=
                    memblock::kDcacheCmoSource) {
                return false;
            }
            if (!error_denied) {
                const std::uint64_t probes_before =
                    environment.dcache_probes();
                if (!environment.hold_dcache_c_until_probes(
                        probes_before + probe_depth)) {
                    return false;
                }
                for (const std::uint64_t auxiliary_line :
                     auxiliary_probe_lines) {
                    if (!environment.request_dcache_probe(
                            auxiliary_line, 2, false, 2)) {
                        return false;
                    }
                }
                if (!environment.request_dcache_probe(
                        line, probe_cap, false, expected_report,
                        dirty ? expected_line : std::vector<unsigned char>{}) ||
                    !environment.run_until_probe_responses(
                        probe_responses_before + probe_depth,
                        cmo_completion_timeout) ||
                    environment.dcache_probes() !=
                        probes_before + probe_depth ||
                    environment.dcache_probe_data() !=
                        probe_data_before + (dirty ? 1U : 0U) ||
                    environment.dcache_max_probe_outstanding() < probe_depth) {
                    return false;
                }
            }
            if (environment.store_writebacks() != store_writebacks_before ||
                environment.pending_scalar_stores() != 1) {
                return false;
            }

            std::optional<memblock::LoadTransaction> younger;
            std::uint64_t younger_writebacks_before = environment.writebacks();
            std::uint64_t younger_cancels_before = 0;
            unsigned younger_response_delay = 0;
            if (younger_overlap) {
                phase = "random-cmo-younger-overlap";
                const std::uint64_t younger_line =
                    cmo_base + block * 0x200000 + 0x100000 +
                    (cmo_overlap_line++ % lines_per_half_block) * 64;
                younger = make_load(
                    younger_line + 8 * (random() % 8),
                    memblock::LoadOp::ld,
                    random() % memblock::kScalarLoadLanes);
                const std::uint64_t requests_before =
                    environment.tilelink_requests();
                younger_writebacks_before = environment.writebacks();
                younger_cancels_before =
                    environment.lq_redirect_canceled_observed();
                younger_response_delay = cmo_response_delay + 512U +
                    static_cast<unsigned>(random() % 1025U);
                environment.force_next_dcache_response_delay(
                    younger_response_delay);
                if (!environment.enqueue_load(*younger) ||
                    !environment.issue_load(*younger, 4096) ||
                    !environment.run_until_dcache_requests(
                        requests_before + 1, 8192) ||
                    environment.store_writebacks() != store_writebacks_before ||
                    environment.writebacks() != younger_writebacks_before ||
                    environment.pending_scalar_stores() != 1) {
                    return false;
                }
            }

            phase = "random-cmo-complete";
            const auto fail_cmo_completion = [&](const char *check) {
                std::ostringstream detail;
                detail << "random-cmo-complete-" << check
                       << ":op=" << operation
                       << ":dirty=" << dirty
                       << ":overlap=" << younger_overlap
                       << ":error="
                       << (error_denied.has_value()
                               ? (*error_denied ? "denied" : "corrupt")
                               : "clean");
                phase = detail.str();
                return false;
            };
            if (!environment.run_until_store_complete(
                    cmo_completion_timeout)) {
                return fail_cmo_completion("store-writeback");
            }
            if (younger_overlap || error_denied.has_value()) {
                if (!environment.redirect_after(
                        cmo_transaction.rob, cmo_transaction.rob_flag,
                        error_denied.has_value())) {
                    return fail_cmo_completion("redirect");
                }
                if (environment.writebacks() != younger_writebacks_before) {
                    return fail_cmo_completion("younger-writeback-at-redirect");
                }
            }
            if (!error_denied &&
                !environment.commit_store(cmo_transaction, 8192)) {
                return fail_cmo_completion("commit");
            }
            if (!environment.run_cycles(
                    younger_overlap ? younger_response_delay + 256U
                                    : error_denied ? 96U : 16U)) {
                return fail_cmo_completion("settle");
            }
            if (!environment.run_until_dcache_idle(cmo_completion_timeout)) {
                return fail_cmo_completion("manager-drain");
            }
            if (younger_overlap &&
                environment.writebacks() != younger_writebacks_before) {
                return fail_cmo_completion("younger-writeback-after-redirect");
            }
            if (younger_overlap &&
                environment.lq_redirect_canceled_observed() !=
                    younger_cancels_before + 1) {
                return fail_cmo_completion("younger-cancel");
            }
            if (!environment.run_until_queues_retired(8192)) {
                return fail_cmo_completion("queue-retire");
            }
            if (environment.dcache_cmo_requests(cmo_transaction.op) !=
                    cmo_before + 1) {
                return fail_cmo_completion("request-count");
            }
            if (!environment.sbuffer_empty()) {
                return fail_cmo_completion("sbuffer-empty");
            }
            if (error_denied &&
                (environment.dcache_error_response_requests() !=
                     error_responses_before + 1 ||
                 environment.dcache_denied_d_beats() !=
                     denied_beats_before + (*error_denied ? 1U : 0U) ||
                 environment.dcache_corrupt_d_beats() !=
                     corrupt_beats_before + (*error_denied ? 0U : 1U) ||
                 environment.dcache_probe_responses() !=
                     probe_responses_before ||
                 environment.bus_expected_load(line + 24, memblock::LoadOp::ld) !=
                     bus_line_before)) {
                return false;
            }
            if (error_denied) {
                --rob_offset;
                const std::uint64_t canceled_sq =
                    environment.sq_redirect_canceled_observed() -
                    sq_cancels_before;
                if (canceled_sq > 1 || canceled_sq > sq_offset) {
                    return false;
                }
                sq_offset -= canceled_sq;
            }
            if (younger_overlap) {
                --rob_offset;
                --lq_offset;
            }
            phase = "seeded-mixed-tail";
            return true;
        };

        const auto recover_random_precise_error = [&] (
            std::uint8_t rob, bool rob_flag, bool store,
            std::uint64_t lq_dequeued_before,
            std::uint64_t sq_dequeued_before,
            std::uint64_t lq_cancels_before,
            std::uint64_t sq_cancels_before) {
            const char *const redirect_phase = store
                ? "random-precise-store-redirect"
                : "random-precise-load-redirect";
            phase = redirect_phase;
            if (!environment.run_cycles(8)) {
                phase += ":pre-redirect-delay";
                return false;
            }
            if (!environment.redirect_after(rob, rob_flag, true)) {
                phase += ":redirect";
                return false;
            }
            if (!environment.run_cycles(96)) {
                phase += ":post-redirect-delay";
                return false;
            }
            if (!environment.run_until_queues_retired(8192)) {
                phase += ":queue-retire";
                return false;
            }
            const std::uint64_t lq_dequeued =
                environment.lq_dequeued() - lq_dequeued_before;
            const std::uint64_t sq_dequeued =
                environment.sq_dequeued() - sq_dequeued_before;
            const std::uint64_t lq_canceled =
                environment.lq_redirect_canceled_observed() -
                lq_cancels_before;
            const std::uint64_t sq_canceled =
                environment.sq_redirect_canceled_observed() -
                sq_cancels_before;
            if (lq_dequeued + lq_canceled != (store ? 0U : 1U) ||
                sq_dequeued + sq_canceled != (store ? 1U : 0U)) {
                std::ostringstream detail;
                detail << redirect_phase << ":queue-accounting:lq="
                       << lq_dequeued << '+' << lq_canceled << ":sq="
                       << sq_dequeued << '+' << sq_canceled;
                phase = detail.str();
                return false;
            }
            --rob_offset;
            if (store) {
                sq_offset -= sq_canceled;
            } else {
                lq_offset -= lq_canceled;
            }
            ++coverage.exceptions;
            return true;
        };
        const auto retire_random_nc_store_error = [&] (
            std::uint64_t lq_dequeued_before,
            std::uint64_t sq_dequeued_before,
            std::uint64_t lq_cancels_before,
            std::uint64_t sq_cancels_before) {
            phase = "random-nc-store-error-retire";
            if (!environment.run_cycles(8) ||
                !environment.run_until_queues_retired(8192)) {
                return false;
            }
            return environment.lq_dequeued() == lq_dequeued_before &&
                environment.sq_dequeued() == sq_dequeued_before + 1 &&
                environment.lq_redirect_canceled_observed() ==
                    lq_cancels_before &&
                environment.sq_redirect_canceled_observed() ==
                    sq_cancels_before;
        };
        const auto ptw_error_outcome_enabled = [&](unsigned outcome) {
            return outcome == 0
                ? constraints.ptw_error_denied_per_mille != 0
                : outcome == 1
                ? constraints.ptw_error_denied_per_mille != 1000 &&
                    constraints.ptw_error_corrupt_first_per_mille != 0
                : constraints.ptw_error_denied_per_mille != 1000 &&
                    constraints.ptw_error_corrupt_first_per_mille != 1000;
        };
        const auto ptw_error_mode_slot_enabled = [&] (
            unsigned site, unsigned slot) {
            if (site == RandomConstraints::ptw_error_stage1) {
                return slot < 2 && constraints.stage1_mode_weights[slot] != 0;
            }
            if (site == RandomConstraints::ptw_error_gstage) {
                return slot < 2 && constraints.g_mode_weights[slot] != 0;
            }
            return slot < 4 &&
                constraints.vs_mode_weights[slot / 2] != 0 &&
                constraints.g_mode_weights[slot % 2] != 0;
        };
        const auto ptw_error_target_level_enabled = [&] (
            unsigned site, unsigned target_level) {
            if (constraints.ptw_error_site_weights[site] == 0) {
                return false;
            }
            for (unsigned mode = 0; mode < 2; ++mode) {
                const bool mode_enabled =
                    site == RandomConstraints::ptw_error_stage1
                    ? constraints.stage1_mode_weights[mode] != 0
                    : site == RandomConstraints::ptw_error_nested_vs
                    ? constraints.vs_mode_weights[mode] != 0
                    : constraints.g_mode_weights[mode] != 0;
                const unsigned levels = mode == 0 ? 3U : 4U;
                if (!mode_enabled) {
                    continue;
                }
                if ((target_level == 0 &&
                     constraints.ptw_error_level_weights[
                         RandomConstraints::ptw_error_leaf] != 0) ||
                    (target_level + 1 == levels &&
                     constraints.ptw_error_level_weights[
                         RandomConstraints::ptw_error_root] != 0) ||
                    (target_level > 0 && target_level + 1 < levels &&
                     constraints.ptw_error_level_weights[
                         RandomConstraints::ptw_error_intermediate] != 0)) {
                    return true;
                }
            }
            return false;
        };

        while (actions < target_before_redirect) {
            const bool closing_stride_stream =
                constraints.stride_stream_per_mille != 0 &&
                stride_stream_loads < stride_training_loads;
            unsigned kind = closing_stride_stream
                ? RandomConstraints::scalar_load
                : constraints.choose_operation(random());
            if (!closing_stride_stream && probe_coverage_incomplete()) {
                kind = RandomConstraints::scalar_store;
            } else if (!closing_stride_stream) {
                const auto &uncache_latency =
                    environment.uncache_response_latency_stats();
                const bool force_uncache_latency =
                    options.backpressure &&
                    constraints.response_latency.uncache ==
                        memblock::ResponseLatencyProfile::spec &&
                    uncache_latency.samples < uncache_latency.buckets.size();
                bool selected = false;
                if (force_uncache_latency) {
                    for (unsigned candidate = RandomConstraints::noncacheable;
                         candidate <= RandomConstraints::mmio; ++candidate) {
                        if (constraints.operation_weights[candidate] != 0 &&
                            !constraint_coverage.operation_complete(
                                constraints, candidate)) {
                            kind = candidate;
                            selected = true;
                            break;
                        }
                    }
                    if (!selected) {
                        kind = constraints.choose_concurrent_special_operation(
                            random());
                        selected = true;
                    }
                }
                for (unsigned candidate = 0;
                     candidate < RandomConstraints::operation_count &&
                         !selected;
                     ++candidate) {
                    if (constraints.operation_weights[candidate] != 0 &&
                        !constraint_coverage.operation_complete(
                            constraints, candidate)) {
                        kind = candidate;
                        selected = true;
                    }
                }
            }
            constexpr std::array<const char *,
                                 RandomConstraints::operation_count>
                operation_names{{
                    "scalar-load", "scalar-store", "vector-load",
                    "vector-store", "vector-segment", "prefetch", "atomic",
                    "nc", "mmio", "hypervisor", "cmo", "ptw-error",
                    "load-merge", "set-pressure", "miss-burst",
                }};
            {
                std::ostringstream detail;
                detail << "random-action:" << operation_names[kind]
                       << ":index=" << actions;
                phase = detail.str();
            }
            bool nc_store = kind == RandomConstraints::noncacheable &&
                random() % 1000 < constraints.nc_stores_per_mille;
            if (kind == RandomConstraints::noncacheable) {
                if (constraints.nc_stores_per_mille != 1000 &&
                    constraint_coverage.nc_directions[0] == 0) {
                    nc_store = false;
                } else if (constraints.nc_stores_per_mille != 0 &&
                           constraint_coverage.nc_directions[1] == 0) {
                    nc_store = true;
                }
            }
            bool mmio_store = kind == RandomConstraints::mmio &&
                random() % 1000 < constraints.mmio_stores_per_mille;
            if (kind == RandomConstraints::mmio) {
                if (constraints.mmio_stores_per_mille != 1000 &&
                    constraint_coverage.mmio_directions[0] == 0) {
                    mmio_store = false;
                } else if (constraints.mmio_stores_per_mille != 0 &&
                           constraint_coverage.mmio_directions[1] == 0) {
                    mmio_store = true;
                }
            }
            std::optional<bool> dcache_load_error_denied;
            if (kind == RandomConstraints::scalar_load &&
                !closing_stride_stream) {
                for (unsigned outcome = 0; outcome < 3; ++outcome) {
                    const bool outcome_enabled = outcome == 0
                        ? constraints.dcache_load_error_per_mille != 1000
                        : outcome == 1
                        ? constraints.dcache_load_error_per_mille != 0 &&
                            constraints.dcache_load_error_denied_per_mille !=
                                1000
                        : constraints.dcache_load_error_per_mille != 0 &&
                            constraints.dcache_load_error_denied_per_mille != 0;
                    if (outcome_enabled &&
                        constraint_coverage.dcache_load_outcomes[outcome] == 0) {
                        if (outcome != 0) {
                            dcache_load_error_denied = outcome == 2;
                        }
                        break;
                    }
                }
                const bool missing_forced_outcome =
                    (!dcache_load_error_denied &&
                     constraint_coverage.dcache_load_outcomes[0] == 0 &&
                     constraints.dcache_load_error_per_mille != 1000) ||
                    dcache_load_error_denied.has_value();
                if (!missing_forced_outcome) {
                    const bool error = choose_binary_class(
                        constraints.dcache_load_error_per_mille,
                        constraint_coverage.dcache_load_errors);
                    if (error) {
                        dcache_load_error_denied = choose_binary_class(
                            constraints.dcache_load_error_denied_per_mille,
                            constraint_coverage.dcache_load_error_kinds);
                    }
                }
            }
            bool bank_conflict_action = false;
            unsigned bank_conflict_depth_index = random() % 2U;
            unsigned bank_conflict_bank = random() % 8U;
            std::optional<unsigned> bank_conflict_forced_regime;
            if (kind == RandomConstraints::scalar_load &&
                !closing_stride_stream && !dcache_load_error_denied &&
                constraints.bank_conflict_per_mille != 0) {
                for (unsigned depth = 0;
                     depth < constraint_coverage.bank_conflict_crosses.size() &&
                         !bank_conflict_forced_regime;
                     ++depth) {
                    for (unsigned bank = 0;
                         bank < constraint_coverage
                                    .bank_conflict_crosses[depth].size() &&
                             !bank_conflict_forced_regime;
                         ++bank) {
                        for (unsigned regime = 0;
                             regime < RandomConstraints::translation_regime_count;
                             ++regime) {
                            if (constraints.translation_weights[regime] != 0 &&
                                constraint_coverage.bank_conflict_crosses
                                    [depth][bank][regime] == 0) {
                                bank_conflict_action = true;
                                bank_conflict_depth_index = depth;
                                bank_conflict_bank = bank;
                                bank_conflict_forced_regime = regime;
                                break;
                            }
                        }
                    }
                }
                if (!bank_conflict_action) {
                    bank_conflict_action = random() % 1000U <
                        constraints.bank_conflict_per_mille;
                }
            }
            unsigned atomic_family = 0;
            unsigned atomic_width = 0;
            unsigned atomic_probe_depth = 0;
            std::optional<bool> atomic_error_denied;
            bool atomic_outcome_forced = false;
            if (kind == RandomConstraints::atomic) {
                atomic_family = constraints.choose_atomic_family(random());
                atomic_width = constraints.choose_atomic_width(random());
                if (constraints.atomic_error_per_mille != 1000) {
                    atomic_probe_depth =
                        constraints.choose_atomic_probe_depth(random());
                }
                for (unsigned family = 0;
                     family < RandomConstraints::atomic_family_count &&
                         !atomic_outcome_forced;
                     ++family) {
                    if (constraints.atomic_family_weights[family] == 0) {
                        continue;
                    }
                    for (unsigned width = 0;
                         width < 2 && !atomic_outcome_forced; ++width) {
                        if (constraints.atomic_width_weights[width] == 0) {
                            continue;
                        }
                        for (unsigned outcome = 0; outcome < 3; ++outcome) {
                            const bool enabled = outcome == 0
                                ? constraints.atomic_error_per_mille != 1000
                                : outcome == 1
                                ? constraints.atomic_error_per_mille != 0 &&
                                    constraints.
                                        atomic_error_denied_per_mille != 1000
                                : constraints.atomic_error_per_mille != 0 &&
                                    constraints.
                                        atomic_error_denied_per_mille != 0;
                            if (enabled &&
                                constraint_coverage.atomic_outcomes[
                                    family][width][outcome] == 0) {
                                atomic_family = family;
                                atomic_width = width;
                                atomic_outcome_forced = true;
                                if (outcome != 0) {
                                    atomic_error_denied = outcome == 2;
                                }
                                break;
                            }
                        }
                    }
                }
                const std::optional<unsigned> missing_atomic_probe_cross =
                    constraint_coverage.first_missing_atomic_probe_cross(
                        constraints);
                if (missing_atomic_probe_cross) {
                    constexpr unsigned depth_count =
                        RandomConstraints::atomic_probe_depth_count;
                    constexpr unsigned family_stride = 2U * depth_count;
                    atomic_family =
                        *missing_atomic_probe_cross / family_stride;
                    const unsigned remainder =
                        *missing_atomic_probe_cross % family_stride;
                    atomic_width = remainder / depth_count;
                    atomic_probe_depth = remainder % depth_count;
                    atomic_error_denied.reset();
                    atomic_outcome_forced = true;
                }
                if (!atomic_outcome_forced && choose_binary_class(
                        constraints.atomic_error_per_mille,
                        constraint_coverage.atomic_errors)) {
                    atomic_error_denied = choose_binary_class(
                        constraints.atomic_error_denied_per_mille,
                        constraint_coverage.atomic_error_kinds);
                }
            }
            std::optional<bool> uncache_error_denied;
            bool uncache_outcome_forced = false;
            if (kind == RandomConstraints::noncacheable ||
                kind == RandomConstraints::mmio) {
                const unsigned memory_type =
                    kind == RandomConstraints::mmio ? 1U : 0U;
                bool &store = kind == RandomConstraints::mmio
                    ? mmio_store : nc_store;
                const unsigned store_per_mille =
                    kind == RandomConstraints::mmio
                    ? constraints.mmio_stores_per_mille
                    : constraints.nc_stores_per_mille;
                for (unsigned direction = 0;
                     direction < 2 && !uncache_outcome_forced;
                     ++direction) {
                    const bool direction_enabled = direction == 0
                        ? store_per_mille != 1000 : store_per_mille != 0;
                    if (!direction_enabled) {
                        continue;
                    }
                    for (unsigned outcome = 0; outcome < 3; ++outcome) {
                        const bool outcome_enabled =
                            outcome == 0
                            ? constraints.uncache_error_per_mille != 1000
                            : outcome == 1
                            ? constraints.uncache_error_per_mille != 0 &&
                                direction == 0 &&
                                constraints.
                                    uncache_load_error_denied_per_mille != 1000
                            : constraints.uncache_error_per_mille != 0 &&
                                (direction == 1 ||
                                 constraints.
                                    uncache_load_error_denied_per_mille != 0);
                        if (outcome_enabled &&
                            constraint_coverage.uncache_outcomes[
                                memory_type][direction][outcome] == 0) {
                            store = direction == 1;
                            uncache_outcome_forced = true;
                            if (outcome != 0) {
                                uncache_error_denied = outcome == 2;
                            }
                            break;
                        }
                    }
                }
                if (!uncache_outcome_forced) {
                    const bool error = choose_binary_class(
                        constraints.uncache_error_per_mille,
                        constraint_coverage.uncache_errors);
                    if (error) {
                        uncache_error_denied = store ? true :
                            choose_binary_class(
                                constraints.
                                    uncache_load_error_denied_per_mille,
                                constraint_coverage.uncache_error_kinds);
                    }
                }
            }
            bool vector_segment_store = kind == RandomConstraints::vector_segment &&
                random() % 1000 <
                    constraints.vector_segment_stores_per_mille;
            if (kind == RandomConstraints::vector_segment) {
                if (constraints.vector_segment_stores_per_mille != 1000 &&
                    constraint_coverage.vector_segment_directions[0] == 0) {
                    vector_segment_store = false;
                } else if (
                    constraints.vector_segment_stores_per_mille != 0 &&
                    constraint_coverage.vector_segment_directions[1] == 0) {
                    vector_segment_store = true;
                }
            }
            unsigned cmo_operation = 0;
            bool cmo_dirty = false;
            bool cmo_younger_overlap = false;
            unsigned cmo_probe_depth_class = 0;
            std::optional<bool> cmo_error_denied;
            if (kind == RandomConstraints::cmo) {
                cmo_operation = constraints.choose_cmo_operation(random());
                if (constraints.cmo_error_per_mille != 1000) {
                    cmo_probe_depth_class =
                        constraints.choose_cmo_probe_depth(random());
                }
                for (unsigned candidate = 0;
                     candidate < RandomConstraints::cmo_operation_count;
                     ++candidate) {
                    if (constraints.cmo_operation_weights[candidate] != 0 &&
                        constraint_coverage.cmo_operations[candidate] == 0) {
                        cmo_operation = candidate;
                        break;
                    }
                }
                if (constraints.cmo_error_per_mille != 0) {
                    for (unsigned candidate = 0;
                         candidate < RandomConstraints::cmo_operation_count;
                         ++candidate) {
                        if (constraints.cmo_operation_weights[candidate] == 0) {
                            continue;
                        }
                        if (constraints.cmo_error_denied_per_mille != 1000 &&
                            constraint_coverage.cmo_operation_errors[
                                candidate][0] == 0) {
                            cmo_operation = candidate;
                            cmo_error_denied = false;
                            break;
                        }
                        if (constraints.cmo_error_denied_per_mille != 0 &&
                            constraint_coverage.cmo_operation_errors[
                                candidate][1] == 0) {
                            cmo_operation = candidate;
                            cmo_error_denied = true;
                            break;
                        }
                    }
                }
                if (!cmo_error_denied) {
                    const bool cmo_error = choose_binary_class(
                        constraints.cmo_error_per_mille,
                        constraint_coverage.cmo_errors);
                    if (cmo_error) {
                        cmo_error_denied = choose_binary_class(
                            constraints.cmo_error_denied_per_mille,
                            constraint_coverage.cmo_error_kinds);
                    }
                }
                cmo_dirty = choose_binary_class(
                    constraints.cmo_dirty_per_mille,
                    constraint_coverage.cmo_line_states);
                cmo_younger_overlap = choose_binary_class(
                    constraints.cmo_younger_overlap_per_mille,
                    constraint_coverage.cmo_younger_overlaps);
                const std::optional<unsigned> missing_cmo_probe_cross =
                    constraint_coverage.first_missing_cmo_probe_cross(
                        constraints);
                if (missing_cmo_probe_cross) {
                    constexpr unsigned state_stride =
                        RandomConstraints::cmo_probe_depth_count;
                    constexpr unsigned operation_stride = 2U * state_stride;
                    cmo_operation = *missing_cmo_probe_cross / operation_stride;
                    const unsigned remainder =
                        *missing_cmo_probe_cross % operation_stride;
                    cmo_dirty = remainder / state_stride != 0;
                    cmo_probe_depth_class = remainder % state_stride;
                    cmo_error_denied.reset();
                }
            }
            std::optional<unsigned> hypervisor_family;
            bool hypervisor_spvp_user = false;
            bool hypervisor_misaligned = false;
            bool hypervisor_pma_device = false;
            unsigned hypervisor_pmp_relation =
                RandomConstraints::hypervisor_pmp_none;
            unsigned hypervisor_pbmt_pair =
                RandomConstraints::hypervisor_pbmt_pma_pma;
            if (kind == RandomConstraints::hypervisor) {
                hypervisor_family =
                    constraints.choose_hypervisor_family(random());
                hypervisor_spvp_user = choose_binary_class(
                    constraints.hypervisor_spvp_user_per_mille,
                    constraint_coverage.hypervisor_spvp);
                hypervisor_misaligned = choose_binary_class(
                    constraints.misaligned_per_mille,
                    constraint_coverage.hypervisor_alignments);
                hypervisor_pma_device = choose_binary_class(
                    constraints.hypervisor_pma_device_per_mille,
                    constraint_coverage.hypervisor_pma_devices);
                hypervisor_pmp_relation =
                    constraints.choose_hypervisor_pmp_relation(random());
                if (hypervisor_pmp_relation !=
                    RandomConstraints::hypervisor_pmp_none) {
                    hypervisor_pbmt_pair =
                        RandomConstraints::hypervisor_pbmt_pma_pma;
                    hypervisor_pma_device = false;
                    hypervisor_misaligned =
                        hypervisor_pmp_relation >= RandomConstraints::
                            hypervisor_pmp_cross_lower;
                } else if (hypervisor_pma_device) {
                    hypervisor_pbmt_pair =
                        RandomConstraints::hypervisor_pbmt_pma_pma;
                    hypervisor_misaligned = false;
                } else {
                    hypervisor_pbmt_pair =
                        constraints.choose_hypervisor_pbmt_pair(random());
                    if (hypervisor_pbmt_pair !=
                        RandomConstraints::hypervisor_pbmt_pma_pma) {
                        hypervisor_misaligned = false;
                    }
                }
                bool cross_forced = false;
                for (unsigned candidate = 0;
                     candidate < RandomConstraints::hypervisor_family_count &&
                         !cross_forced;
                     ++candidate) {
                    if (constraints.hypervisor_family_weights[candidate] == 0) {
                        continue;
                    }
                    for (unsigned spvp = 0;
                         spvp < 2 && !cross_forced; ++spvp) {
                        const bool spvp_enabled = spvp == 0
                            ? constraints.hypervisor_spvp_user_per_mille != 1000
                            : constraints.hypervisor_spvp_user_per_mille != 0;
                        if (!spvp_enabled) {
                            continue;
                        }
                        for (unsigned relation = 0;
                             relation < RandomConstraints::
                                 hypervisor_pmp_relation_count;
                             ++relation) {
                            if (constraints.hypervisor_pmp_relation_weights[
                                    relation] == 0 ||
                                constraint_coverage.
                                    hypervisor_pmp_relation_crosses[
                                        candidate][spvp][relation] != 0) {
                                continue;
                            }
                            hypervisor_family = candidate;
                            hypervisor_spvp_user = spvp != 0;
                            hypervisor_pmp_relation = relation;
                            if (relation !=
                                RandomConstraints::hypervisor_pmp_none) {
                                hypervisor_pbmt_pair = RandomConstraints::
                                    hypervisor_pbmt_pma_pma;
                                hypervisor_pma_device = false;
                                hypervisor_misaligned = relation >=
                                    RandomConstraints::
                                        hypervisor_pmp_cross_lower;
                            }
                            cross_forced = true;
                            break;
                        }
                    }
                }
                for (unsigned candidate = 0;
                     candidate < RandomConstraints::hypervisor_family_count &&
                         !cross_forced;
                     ++candidate) {
                    if (constraints.hypervisor_family_weights[candidate] == 0) {
                        continue;
                    }
                    for (unsigned spvp = 0;
                         spvp < 2 && !cross_forced; ++spvp) {
                        const bool enabled = spvp == 0
                            ? constraints.hypervisor_spvp_user_per_mille != 1000
                            : constraints.hypervisor_spvp_user_per_mille != 0;
                        if (!enabled) {
                            continue;
                        }
                        for (unsigned pbmt = 0;
                             pbmt < RandomConstraints::hypervisor_pbmt_pair_count;
                             ++pbmt) {
                            if (constraints.hypervisor_pbmt_pair_weights[pbmt] ==
                                    0 ||
                                constraint_coverage.hypervisor_pbmt_crosses[
                                    candidate][spvp][pbmt] != 0) {
                                continue;
                            }
                            hypervisor_family = candidate;
                            hypervisor_spvp_user = spvp != 0;
                            hypervisor_pmp_relation =
                                RandomConstraints::hypervisor_pmp_none;
                            hypervisor_pbmt_pair = pbmt;
                            const bool pma_pair = pbmt == RandomConstraints::
                                hypervisor_pbmt_pma_pma;
                            hypervisor_pma_device = pma_pair &&
                                constraints.hypervisor_pma_device_per_mille !=
                                    0 &&
                                constraint_coverage.
                                    hypervisor_pma_device_crosses[
                                        candidate][spvp][1] == 0;
                            hypervisor_misaligned = pma_pair &&
                                !hypervisor_pma_device &&
                                constraints.misaligned_per_mille != 0 &&
                                constraint_coverage.
                                    hypervisor_alignment_crosses[
                                        candidate][spvp][1] == 0;
                            cross_forced = true;
                            break;
                        }
                        if (cross_forced) {
                            break;
                        }
                    }
                }
                for (unsigned candidate = 0;
                     candidate < RandomConstraints::hypervisor_family_count &&
                         !cross_forced;
                     ++candidate) {
                    if (constraints.hypervisor_family_weights[candidate] == 0) {
                        continue;
                    }
                    for (unsigned spvp = 0;
                         spvp < 2 && !cross_forced; ++spvp) {
                        const bool spvp_enabled = spvp == 0
                            ? constraints.hypervisor_spvp_user_per_mille != 1000
                            : constraints.hypervisor_spvp_user_per_mille != 0;
                        if (!spvp_enabled) {
                            continue;
                        }
                        for (unsigned pma_device = 0; pma_device < 2;
                             ++pma_device) {
                            const bool address_enabled = pma_device == 0
                                ? constraints.hypervisor_pma_device_per_mille !=
                                      1000
                                : constraints.hypervisor_pma_device_per_mille !=
                                      0;
                            if (!address_enabled ||
                                constraint_coverage.
                                    hypervisor_pma_device_crosses[
                                        candidate][spvp][pma_device] != 0) {
                                continue;
                            }
                            hypervisor_family = candidate;
                            hypervisor_spvp_user = spvp != 0;
                            hypervisor_pmp_relation =
                                RandomConstraints::hypervisor_pmp_none;
                            hypervisor_pma_device = pma_device != 0;
                            if (hypervisor_pma_device) {
                                hypervisor_pbmt_pair = RandomConstraints::
                                    hypervisor_pbmt_pma_pma;
                                hypervisor_misaligned = false;
                            } else {
                                hypervisor_pbmt_pair =
                                    constraints.choose_hypervisor_pbmt_pair(
                                        random());
                                hypervisor_misaligned =
                                    hypervisor_pbmt_pair == RandomConstraints::
                                        hypervisor_pbmt_pma_pma &&
                                    constraints.misaligned_per_mille != 0 &&
                                    constraint_coverage.
                                        hypervisor_alignment_crosses[
                                            candidate][spvp][1] == 0;
                            }
                            cross_forced = true;
                            break;
                        }
                    }
                }
                for (unsigned candidate = 0;
                     candidate < RandomConstraints::hypervisor_family_count &&
                         !cross_forced;
                     ++candidate) {
                    if (constraints.hypervisor_family_weights[candidate] == 0) {
                        continue;
                    }
                    for (unsigned spvp = 0;
                         spvp < 2 && !cross_forced; ++spvp) {
                        const bool enabled = spvp == 0
                            ? constraints.hypervisor_spvp_user_per_mille != 1000
                            : constraints.hypervisor_spvp_user_per_mille != 0;
                        if (!enabled) {
                            continue;
                        }
                        for (unsigned alignment = 0; alignment < 2;
                             ++alignment) {
                            const bool alignment_enabled = alignment == 0
                                ? constraints.misaligned_per_mille != 1000
                                : constraints.misaligned_per_mille != 0;
                            const std::uint64_t observed =
                                constraint_coverage.
                                    hypervisor_alignment_crosses[
                                        candidate][spvp][alignment];
                            if (alignment_enabled && observed == 0) {
                                hypervisor_family = candidate;
                                hypervisor_spvp_user = spvp != 0;
                                hypervisor_pmp_relation =
                                    RandomConstraints::hypervisor_pmp_none;
                                hypervisor_misaligned = alignment != 0;
                                if (hypervisor_misaligned) {
                                    hypervisor_pbmt_pair = RandomConstraints::
                                        hypervisor_pbmt_pma_pma;
                                    hypervisor_pma_device = false;
                                } else if (
                                    constraints.hypervisor_pma_device_per_mille ==
                                    1000) {
                                    hypervisor_pbmt_pair = RandomConstraints::
                                        hypervisor_pbmt_pma_pma;
                                    hypervisor_pma_device = true;
                                }
                                cross_forced = true;
                                break;
                            }
                        }
                    }
                }
            }
            unsigned ptw_error_site = RandomConstraints::ptw_error_stage1;
            unsigned ptw_error_level_class = RandomConstraints::ptw_error_root;
            unsigned ptw_error_outcome = 0;
            unsigned ptw_error_stage1_mode = 0;
            unsigned ptw_error_vs_mode = 0;
            unsigned ptw_error_g_mode = 0;
            unsigned ptw_error_target_level = 0;
            bool ptw_error_store = false;
            std::uint64_t ptw_error_address = 0;
            std::uint64_t ptw_error_guest_address = 0;
            std::uint64_t ptw_error_stage1_root = 0;
            std::uint64_t ptw_error_vs_root = 0;
            std::uint64_t ptw_error_g_root = 0;
            unsigned set_pressure_depth_index = 0;
            unsigned set_pressure_width = 0;
            unsigned set_pressure_set = 0;
            bool set_pressure_dirty = false;
            bool set_pressure_refill_overlap = false;
            bool set_pressure_release_backpressure = false;
            unsigned set_pressure_window_class =
                RandomConstraints::set_pressure_single_window;
            unsigned miss_burst_depth_index = 0;
            unsigned miss_burst_issue_width_index = 0;
            if (kind == RandomConstraints::set_pressure) {
                set_pressure_depth_index =
                    constraints.choose_set_pressure_depth(random());
                set_pressure_width =
                    constraints.choose_set_pressure_width(random());
                set_pressure_set =
                    constraints.choose_set_pressure_set(random());
                set_pressure_dirty = choose_binary_class(
                    constraints.set_pressure_dirty_per_mille,
                    constraint_coverage.set_pressure_line_states);
                set_pressure_refill_overlap = choose_binary_class(
                    constraints.set_pressure_refill_overlap_per_mille,
                    constraint_coverage.set_pressure_refill_overlaps);
                set_pressure_release_backpressure = choose_binary_class(
                    constraints.set_pressure_release_backpressure_per_mille,
                    constraint_coverage.set_pressure_release_backpressures);
                set_pressure_window_class =
                    constraints.choose_set_pressure_window(random());
                for (unsigned set = 0;
                     set < constraint_coverage.set_pressure_sets.size(); ++set) {
                    if (constraints.set_pressure_set_weights[set] != 0 &&
                        constraint_coverage.set_pressure_sets[set] == 0) {
                        set_pressure_set = set;
                        break;
                    }
                }
            } else if (kind == RandomConstraints::miss_burst) {
                miss_burst_depth_index =
                    constraints.choose_miss_burst_depth(random());
                miss_burst_issue_width_index =
                    constraints.choose_miss_burst_issue_width(
                        miss_burst_depth_index, random());
            }
            if (kind == RandomConstraints::ptw_error) {
                ptw_error_site = constraints.choose_ptw_error_site(random());
                ptw_error_level_class =
                    constraints.choose_ptw_error_level(random());
                ptw_error_store = random() % 1000 <
                    constraints.ptw_error_stores_per_mille;
                if (random() % 1000 < constraints.ptw_error_denied_per_mille) {
                    ptw_error_outcome = 0;
                } else {
                    ptw_error_outcome = random() % 1000 <
                            constraints.ptw_error_corrupt_first_per_mille
                        ? 1U : 2U;
                }

                bool forced_cross = false;
                for (unsigned site = 0;
                     site < RandomConstraints::ptw_error_site_count &&
                         !forced_cross;
                     ++site) {
                    if (constraints.ptw_error_site_weights[site] == 0) {
                        continue;
                    }
                    for (unsigned direction = 0;
                         direction < 2 && !forced_cross; ++direction) {
                        const bool direction_enabled = direction == 0
                            ? constraints.ptw_error_stores_per_mille != 1000
                            : constraints.ptw_error_stores_per_mille != 0;
                        if (!direction_enabled) {
                            continue;
                        }
                        for (unsigned level = 0;
                             level < RandomConstraints::ptw_error_level_count &&
                                 !forced_cross;
                             ++level) {
                            if (constraints.ptw_error_level_weights[level] == 0) {
                                continue;
                            }
                            for (unsigned outcome = 0; outcome < 3; ++outcome) {
                                if (ptw_error_outcome_enabled(outcome) &&
                                    constraint_coverage.ptw_error_outcomes[
                                        site][direction][level][outcome] == 0) {
                                    ptw_error_site = site;
                                    ptw_error_store = direction == 1;
                                    ptw_error_level_class = level;
                                    ptw_error_outcome = outcome;
                                    forced_cross = true;
                                    break;
                                }
                            }
                        }
                    }
                }

                std::optional<unsigned> forced_mode_slot;
                for (unsigned slot = 0; slot < 4; ++slot) {
                    if (ptw_error_mode_slot_enabled(ptw_error_site, slot) &&
                        constraint_coverage.ptw_error_modes[
                            ptw_error_site][slot] == 0) {
                        forced_mode_slot = slot;
                        break;
                    }
                }
                if (!forced_cross && !forced_mode_slot) {
                    for (unsigned site = 0;
                         site < RandomConstraints::ptw_error_site_count &&
                             !forced_mode_slot;
                         ++site) {
                        if (constraints.ptw_error_site_weights[site] == 0) {
                            continue;
                        }
                        for (unsigned slot = 0; slot < 4; ++slot) {
                            if (ptw_error_mode_slot_enabled(site, slot) &&
                                constraint_coverage.ptw_error_modes[site][slot] ==
                                    0) {
                                ptw_error_site = site;
                                forced_mode_slot = slot;
                                break;
                            }
                        }
                    }
                }

                std::optional<unsigned> forced_target_level;
                std::optional<unsigned> forced_governing_mode;
                if (!forced_cross && !forced_mode_slot) {
                    for (unsigned site = 0;
                         site < RandomConstraints::ptw_error_site_count &&
                             !forced_target_level;
                         ++site) {
                        for (unsigned target = 0; target < 4; ++target) {
                            if (!ptw_error_target_level_enabled(site, target) ||
                                constraint_coverage.ptw_error_target_levels[
                                    site][target] != 0) {
                                continue;
                            }
                            for (unsigned mode = 0; mode < 2; ++mode) {
                                const bool mode_enabled =
                                    site == RandomConstraints::ptw_error_stage1
                                    ? constraints.stage1_mode_weights[mode] != 0
                                    : site == RandomConstraints::ptw_error_nested_vs
                                    ? constraints.vs_mode_weights[mode] != 0
                                    : constraints.g_mode_weights[mode] != 0;
                                const unsigned levels = mode == 0 ? 3U : 4U;
                                if (!mode_enabled) {
                                    continue;
                                }
                                for (unsigned level_class = 0;
                                     level_class <
                                         RandomConstraints::ptw_error_level_count;
                                     ++level_class) {
                                    const bool compatible =
                                        level_class ==
                                            RandomConstraints::ptw_error_leaf
                                        ? target == 0
                                        : level_class ==
                                              RandomConstraints::ptw_error_root
                                        ? target + 1 == levels
                                        : target > 0 && target + 1 < levels;
                                    if (compatible &&
                                        constraints.ptw_error_level_weights[
                                            level_class] != 0) {
                                        ptw_error_site = site;
                                        ptw_error_level_class = level_class;
                                        forced_target_level = target;
                                        forced_governing_mode = mode;
                                        break;
                                    }
                                }
                                if (forced_target_level) {
                                    break;
                                }
                            }
                            if (forced_target_level) {
                                break;
                            }
                        }
                    }
                }

                if (ptw_error_site == RandomConstraints::ptw_error_stage1) {
                    ptw_error_stage1_mode =
                        constraints.choose_stage1_mode(random());
                } else if (ptw_error_site ==
                           RandomConstraints::ptw_error_gstage) {
                    ptw_error_g_mode = constraints.choose_g_mode(random());
                } else {
                    ptw_error_vs_mode = constraints.choose_vs_mode(random());
                    ptw_error_g_mode = constraints.choose_g_mode(random());
                }
                if (forced_mode_slot) {
                    if (ptw_error_site == RandomConstraints::ptw_error_stage1) {
                        ptw_error_stage1_mode = *forced_mode_slot;
                    } else if (ptw_error_site ==
                               RandomConstraints::ptw_error_gstage) {
                        ptw_error_g_mode = *forced_mode_slot;
                    } else {
                        ptw_error_vs_mode = *forced_mode_slot / 2;
                        ptw_error_g_mode = *forced_mode_slot % 2;
                    }
                }
                if (forced_governing_mode) {
                    if (ptw_error_site == RandomConstraints::ptw_error_stage1) {
                        ptw_error_stage1_mode = *forced_governing_mode;
                    } else if (ptw_error_site ==
                               RandomConstraints::ptw_error_nested_vs) {
                        ptw_error_vs_mode = *forced_governing_mode;
                    } else {
                        ptw_error_g_mode = *forced_governing_mode;
                    }
                }
                const unsigned governing_mode =
                    ptw_error_site == RandomConstraints::ptw_error_stage1
                    ? ptw_error_stage1_mode
                    : ptw_error_site == RandomConstraints::ptw_error_nested_vs
                    ? ptw_error_vs_mode
                    : ptw_error_g_mode;
                const unsigned levels = governing_mode == 0 ? 3U : 4U;
                if (forced_target_level) {
                    ptw_error_target_level = *forced_target_level;
                } else if (ptw_error_level_class ==
                           RandomConstraints::ptw_error_leaf) {
                    ptw_error_target_level = 0;
                } else if (ptw_error_level_class ==
                           RandomConstraints::ptw_error_root) {
                    ptw_error_target_level = levels - 1;
                } else {
                    ptw_error_target_level = 1U + random() % (levels - 2U);
                }
                constexpr std::uint64_t ptw_error_lines =
                    ptw_error_span / 64;
                const std::uint64_t sequence = ptw_error_line++;
                const std::uint64_t line =
                    (sequence ^ (sequence >> 1)) % ptw_error_lines;
                ptw_error_address = ptw_error_base + line * 64 + 0x18;
                ptw_error_guest_address =
                    ptw_error_guest_base + line * 64 + 0x18;
                const std::uint64_t root_offset =
                    (sequence % ptw_error_root_slots) * ptw_error_root_stride;
                ptw_error_stage1_root =
                    ptw_error_stage1_root_base + root_offset;
                ptw_error_vs_root = ptw_error_vs_root_base + root_offset;
                ptw_error_g_root = ptw_error_g_root_base + root_offset;
            }
            TranslationContext translation = closing_stride_stream
                ? dominant_translation_context()
                : choose_translation_context();
            if (kind == RandomConstraints::scalar_load &&
                bank_conflict_action && bank_conflict_forced_regime) {
                translation = TranslationContext{};
                translation.regime = *bank_conflict_forced_regime;
                if (translation.regime ==
                    RandomConstraints::translation_stage1) {
                    translation.stage1_mode =
                        constraints.choose_stage1_mode(random());
                } else if (translation.regime ==
                           RandomConstraints::translation_nested) {
                    translation.vs_mode =
                        constraints.choose_vs_mode(random());
                    translation.g_mode =
                        constraints.choose_g_mode(random());
                }
            } else if (kind == RandomConstraints::hypervisor) {
                translation.regime = RandomConstraints::translation_nested;
                translation.vs_mode = constraints.choose_vs_mode(random());
                translation.g_mode = constraints.choose_g_mode(random());
            } else if ((kind == RandomConstraints::noncacheable ||
                        kind == RandomConstraints::mmio) &&
                translation.regime == RandomConstraints::translation_bare) {
                translation = translated_context();
            } else if (kind == RandomConstraints::ptw_error) {
                if (ptw_error_site == RandomConstraints::ptw_error_stage1) {
                    translation.regime = RandomConstraints::translation_stage1;
                    translation.stage1_mode = ptw_error_stage1_mode;
                } else if (ptw_error_site !=
                           RandomConstraints::ptw_error_gstage) {
                    translation.regime = RandomConstraints::translation_nested;
                    translation.vs_mode = ptw_error_vs_mode;
                    translation.g_mode = ptw_error_g_mode;
                }
            } else if (kind == RandomConstraints::load_merge) {
                for (unsigned regime = 0;
                     regime < RandomConstraints::translation_regime_count;
                     ++regime) {
                    if (constraints.translation_weights[regime] == 0 ||
                        constraint_coverage.load_merge_translations[regime] !=
                            0) {
                        continue;
                    }
                    translation = TranslationContext{};
                    translation.regime = regime;
                    if (regime == RandomConstraints::translation_stage1) {
                        translation.stage1_mode = first_enabled_mode(
                            constraints.stage1_mode_weights);
                    } else if (regime ==
                               RandomConstraints::translation_nested) {
                        translation.vs_mode = first_enabled_mode(
                            constraints.vs_mode_weights);
                        translation.g_mode = first_enabled_mode(
                            constraints.g_mode_weights);
                    }
                    break;
                }
            } else if (kind == RandomConstraints::set_pressure) {
                bool forced_cross = false;
                for (unsigned state = 0;
                     state < constraint_coverage.set_pressure_crosses.size() &&
                         !forced_cross;
                     ++state) {
                    const bool state_enabled = state == 0
                        ? constraints.set_pressure_dirty_per_mille != 1000
                        : constraints.set_pressure_dirty_per_mille != 0;
                    if (!state_enabled) {
                        continue;
                    }
                    for (unsigned overlap = 0;
                         overlap < constraint_coverage
                                       .set_pressure_crosses[state].size() &&
                             !forced_cross;
                         ++overlap) {
                        const bool overlap_enabled = overlap == 0
                            ? constraints
                                      .set_pressure_refill_overlap_per_mille !=
                                  1000
                            : constraints
                                      .set_pressure_refill_overlap_per_mille != 0;
                        if (!overlap_enabled) {
                            continue;
                        }
                        for (unsigned backpressure = 0;
                             backpressure < constraint_coverage
                                                .set_pressure_crosses[state]
                                                                     [overlap]
                                                .size() &&
                                 !forced_cross;
                             ++backpressure) {
                            const bool backpressure_enabled = backpressure == 0
                                ? constraints
                                          .set_pressure_release_backpressure_per_mille !=
                                      1000
                                : constraints
                                          .set_pressure_release_backpressure_per_mille !=
                                      0;
                            if (!backpressure_enabled) {
                                continue;
                            }
                            for (unsigned window_class = 0;
                                 window_class < constraint_coverage
                                                    .set_pressure_crosses[state]
                                                                         [overlap]
                                                                         [backpressure]
                                                    .size() &&
                                     !forced_cross;
                                 ++window_class) {
                                if (!constraints.set_pressure_window_enabled(
                                        window_class)) {
                                    continue;
                                }
                                for (unsigned depth = 0;
                                     depth < constraint_coverage
                                                 .set_pressure_crosses[state]
                                                                      [overlap]
                                                                      [backpressure]
                                                                      [window_class]
                                                 .size() &&
                                         !forced_cross;
                                     ++depth) {
                                    if (constraints.set_pressure_depth_weights
                                            [depth] == 0) {
                                        continue;
                                    }
                                    for (unsigned width = 0;
                                         width < constraint_coverage
                                                     .set_pressure_crosses[state]
                                                                         [overlap]
                                                                         [backpressure]
                                                                         [window_class]
                                                                          [depth]
                                                     .size() &&
                                             !forced_cross;
                                         ++width) {
                                        if (constraints
                                                .set_pressure_width_weights
                                                    [width] == 0) {
                                            continue;
                                        }
                                        for (unsigned regime = 0;
                                             regime < RandomConstraints::
                                                 translation_regime_count;
                                             ++regime) {
                                            if (constraints.translation_weights
                                                        [regime] != 0 &&
                                                constraint_coverage
                                                        .set_pressure_crosses
                                                            [state][overlap]
                                                            [backpressure]
                                                            [window_class]
                                                            [depth][width]
                                                            [regime] == 0) {
                                                set_pressure_dirty = state != 0;
                                                set_pressure_refill_overlap =
                                                    overlap != 0;
                                                set_pressure_release_backpressure =
                                                    backpressure != 0;
                                                set_pressure_window_class =
                                                    window_class;
                                                set_pressure_depth_index = depth;
                                                set_pressure_width = width;
                                                translation =
                                                    TranslationContext{};
                                                translation.regime = regime;
                                                if (regime == RandomConstraints::
                                                        translation_stage1) {
                                                    translation.stage1_mode =
                                                        first_enabled_mode(
                                                            constraints.
                                                                stage1_mode_weights);
                                                } else if (regime ==
                                                           RandomConstraints::
                                                               translation_nested) {
                                                    translation.vs_mode =
                                                        first_enabled_mode(
                                                            constraints.
                                                                vs_mode_weights);
                                                    translation.g_mode =
                                                        first_enabled_mode(
                                                            constraints.
                                                                g_mode_weights);
                                                }
                                                forced_cross = true;
                                                break;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            } else if (kind == RandomConstraints::miss_burst) {
                bool forced_cross = false;
                for (unsigned depth = 0;
                     depth < constraint_coverage.miss_burst_crosses.size() &&
                         !forced_cross;
                     ++depth) {
                    for (unsigned width = 0;
                         width < constraint_coverage
                                     .miss_burst_crosses[depth].size() &&
                             !forced_cross;
                         ++width) {
                        if (!constraints.miss_burst_cross_enabled(
                                depth, width)) {
                            continue;
                        }
                        for (unsigned regime = 0;
                             regime < RandomConstraints::translation_regime_count;
                             ++regime) {
                            if (constraints.translation_weights[regime] == 0 ||
                                constraint_coverage.miss_burst_crosses
                                    [depth][width][regime] != 0) {
                                continue;
                            }
                            miss_burst_depth_index = depth;
                            miss_burst_issue_width_index = width;
                            translation = TranslationContext{};
                            translation.regime = regime;
                            if (regime ==
                                RandomConstraints::translation_stage1) {
                                translation.stage1_mode = first_enabled_mode(
                                    constraints.stage1_mode_weights);
                            } else if (regime ==
                                       RandomConstraints::translation_nested) {
                                translation.vs_mode = first_enabled_mode(
                                    constraints.vs_mode_weights);
                                translation.g_mode = first_enabled_mode(
                                    constraints.g_mode_weights);
                            }
                            forced_cross = true;
                            break;
                        }
                    }
                }
            }
            TranslationLeafTopology leaf =
                choose_translation_leaf(translation);
            // The conflict address pool is mapped as ordinary 2-MiB leaves
            // in every translated context. Keep this stress action out of
            // the separately located NAPOT pools so its failure mode remains
            // bank/replay behavior rather than an accidental page fault.
            if (bank_conflict_action) {
                leaf = TranslationLeafTopology{};
            }
            if (kind != RandomConstraints::ptw_error) {
                std::ostringstream detail;
                detail << phase << ":regime=" << translation.regime
                       << ":stage1_mode=" << translation.stage1_mode
                       << ":vs_mode=" << translation.vs_mode
                       << ":g_mode=" << translation.g_mode
                       << ":leaf=" << leaf.nested_index();
                phase = detail.str();
            }
            if (kind == RandomConstraints::ptw_error) {
                phase = "random-ptw-error-context";
                if (!environment.run_until_all_complete(
                        constrained_completion_timeout) ||
                    !environment.run_until_queues_retired(
                        constrained_completion_timeout) ||
                    !environment.run_until_dcache_idle(
                        constrained_completion_timeout) ||
                    !flush_pending_translation_fences()) {
                    return false;
                }
                const std::uint64_t ptw_error_page =
                    ptw_error_address & ~std::uint64_t{0xfff};
                const std::uint64_t ptw_error_guest_page =
                    ptw_error_guest_address & ~std::uint64_t{0xfff};
                bool mapped = false;
                if (ptw_error_site == RandomConstraints::ptw_error_stage1) {
                    mapped = map_stage_page(
                        ptw_error_stage1_mode, ptw_error_stage1_root,
                        ptw_error_page, ptw_error_page, false, false, true);
                } else if (ptw_error_site ==
                           RandomConstraints::ptw_error_gstage) {
                    mapped = map_g_page(
                        ptw_error_g_mode, ptw_error_g_root, ptw_error_page,
                        ptw_error_page, true);
                } else {
                    mapped = map_stage_page(
                        ptw_error_vs_mode, ptw_error_vs_root, ptw_error_page,
                        ptw_error_guest_page, false, false, true);
                    const unsigned vs_levels = ptw_error_vs_mode == 0 ? 3U : 4U;
                    for (unsigned level = 0; mapped && level < vs_levels;
                         ++level) {
                        const auto vs_pte =
                            memblock::reference_pte_address_at_level(
                                environment.memory(), ptw_error_vs_root,
                                ptw_error_address,
                                page_mode(ptw_error_vs_mode), level);
                        mapped = vs_pte && map_g_page(
                            ptw_error_g_mode, ptw_error_g_root,
                            *vs_pte & ~std::uint64_t{0xfff},
                            *vs_pte & ~std::uint64_t{0xfff});
                    }
                    mapped = mapped && map_g_page(
                        ptw_error_g_mode, ptw_error_g_root,
                        ptw_error_guest_page, ptw_error_page, true);
                }
                bool reference_valid = false;
                if (mapped && ptw_error_site ==
                                  RandomConstraints::ptw_error_stage1) {
                    const auto reference = memblock::reference_page_walk(
                        environment.memory(), ptw_error_stage1_root,
                        ptw_error_address,
                        page_mode(ptw_error_stage1_mode));
                    reference_valid = reference.translated &&
                        reference.physical_address == ptw_error_address;
                } else if (mapped && ptw_error_site ==
                                         RandomConstraints::ptw_error_gstage) {
                    const auto reference = memblock::reference_page_walk(
                        environment.memory(), ptw_error_g_root,
                        ptw_error_address, page_mode(ptw_error_g_mode), true);
                    reference_valid = reference.translated &&
                        reference.physical_address == ptw_error_address;
                } else if (mapped) {
                    const auto reference =
                        memblock::reference_two_stage_walk(
                            environment.memory(), ptw_error_vs_root,
                            ptw_error_g_root, ptw_error_address,
                            page_mode(ptw_error_vs_mode),
                            page_mode(ptw_error_g_mode));
                    reference_valid = reference.translated &&
                        reference.physical_address == ptw_error_address;
                }
                if (!reference_valid) {
                    std::ostringstream detail;
                    detail << "random-ptw-error-reference"
                           << ":site=" << ptw_error_site
                           << ":stage1_mode=" << ptw_error_stage1_mode
                           << ":vs_mode=" << ptw_error_vs_mode
                           << ":g_mode=" << ptw_error_g_mode
                           << ":va=0x" << std::hex << ptw_error_address
                           << ":gpa=0x" << ptw_error_guest_address
                           << ":s1root=0x" << ptw_error_stage1_root
                           << ":vsroot=0x" << ptw_error_vs_root
                           << ":groot=0x" << ptw_error_g_root << std::dec;
                    if (ptw_error_site !=
                            RandomConstraints::ptw_error_stage1 &&
                        ptw_error_site !=
                            RandomConstraints::ptw_error_gstage) {
                        const auto reference =
                            memblock::reference_two_stage_walk(
                                environment.memory(), ptw_error_vs_root,
                                ptw_error_g_root, ptw_error_address,
                                page_mode(ptw_error_vs_mode),
                                page_mode(ptw_error_g_mode));
                        detail << ":translated=" << reference.translated
                               << ":stage1_pf="
                               << reference.stage1_page_fault
                               << ":gpf=" << reference.guest_page_fault
                               << ":af=" << reference.access_fault
                               << ":fault_gpa=0x" << std::hex
                               << reference.faulting_guest_physical_address
                               << ":pa=0x" << reference.physical_address
                               << std::dec;
                    }
                    phase = detail.str();
                    return false;
                }
                bool activated = false;
                if (ptw_error_site == RandomConstraints::ptw_error_stage1) {
                    activated = ptw_error_stage1_mode == 0
                        ? environment.activate_sv39(
                              ptw_error_stage1_root, 23)
                        : environment.activate_sv48(
                              ptw_error_stage1_root, 23);
                } else if (ptw_error_site ==
                           RandomConstraints::ptw_error_gstage) {
                    activated = environment.activate_two_stage_modes(
                        memblock::ReferencePageMode::bare,
                        page_mode(ptw_error_g_mode), 0,
                        ptw_error_g_root, 0, 31);
                } else {
                    activated = environment.activate_two_stage_modes(
                        page_mode(ptw_error_vs_mode),
                        page_mode(ptw_error_g_mode),
                        ptw_error_vs_root, ptw_error_g_root, 29, 31);
                }
                if (!mapped || !activated ||
                    ((ptw_error_site == RandomConstraints::ptw_error_stage1)
                         ? !environment.issue_sfence(
                               0, 0, true, true, false, false)
                         : ptw_error_site == RandomConstraints::ptw_error_gstage
                         ? !environment.issue_sfence(
                               0, 0, true, true, false, true)
                         : (!environment.issue_sfence(
                                0, 0, true, true, true, false) ||
                            !environment.issue_sfence(
                                0, 0, true, true, false, true)))) {
                    return false;
                }
                translation_context_valid = false;
                pending_translation_fences =
                    ptw_error_site == RandomConstraints::ptw_error_stage1
                    ? pending_sfence
                    : ptw_error_site == RandomConstraints::ptw_error_gstage
                    ? pending_hfence_gvma
                    : pending_hfence_vvma | pending_hfence_gvma;
            } else if (!enter_translation_context(translation) ||
                       (!closing_stride_stream &&
                        !issue_constrained_fence(kind))) {
                return false;
            }

            std::uint64_t ptw_requests_before =
                environment.ptw_requests();
            const std::uint64_t requests_before = environment.tilelink_requests();
            bool sample_dcache = true;
            bool sample_translation_action =
                kind != RandomConstraints::ptw_error;
            bool leaf_addressed = false;
            bool ordinary_leaf_addressed = false;
            const auto action_cacheable_address = [&] (
                unsigned alignment,
                std::optional<bool> force_misaligned = std::nullopt) {
                leaf_addressed = true;
                return constrained_cacheable_address(
                    alignment, translation, leaf, force_misaligned);
            };
            std::optional<std::uint64_t> probe_candidate;
            if (kind == RandomConstraints::scalar_load) {
              if (bank_conflict_action) {
                phase = "random-bank-conflict";
                const unsigned depth = bank_conflict_depth_index + 2U;
                if (bank_conflict_line + depth > bank_conflict_lines) {
                    phase = "random-bank-conflict-address-space-exhausted";
                    return false;
                }
                if (!environment.run_until_all_complete(
                        constrained_completion_timeout) ||
                    !environment.run_until_queues_retired(
                        constrained_completion_timeout) ||
                    !environment.run_until_dcache_idle(
                        constrained_completion_timeout)) {
                    return false;
                }
                std::vector<std::uint64_t> lines;
                lines.reserve(depth);
                for (unsigned index = 0; index < depth; ++index) {
                    const std::uint64_t line_index =
                        (bank_conflict_line_offset +
                         (bank_conflict_line + index) *
                             bank_conflict_line_stride) %
                        bank_conflict_lines;
                    const std::uint64_t line = bank_conflict_base +
                        line_index * bank_conflict_line_bytes;
                    const std::uint64_t address =
                        line + bank_conflict_bank * sizeof(std::uint64_t);
                    environment.memory().fill_incrementing(
                        line, bank_conflict_line_bytes,
                        static_cast<std::uint8_t>(
                            0x37U + bank_conflict_bank * 11U + index * 17U));
                    lines.push_back(line);
                }
                for (unsigned index = 0; index < depth; ++index) {
                    const std::uint64_t line = lines[index];
                    const std::uint64_t address =
                        line + bank_conflict_bank * sizeof(std::uint64_t);
                    const auto preload = make_load(
                        address, memblock::LoadOp::ld,
                        index % memblock::kScalarLoadLanes);
                    if (!issue_load(preload, std::nullopt, false)) {
                        return false;
                    }
                }
                bank_conflict_line += depth;
                if (!environment.run_until_queues_retired(
                        constrained_completion_timeout) ||
                    !environment.run_until_dcache_idle(
                        constrained_completion_timeout)) {
                    return false;
                }
                // Setup fills are not constrained-random actions, but they are
                // real translated walk windows. Record that observation, then
                // classify the resident target wave from a fresh baseline so
                // translation reuse remains visible without inferring cache or
                // replay behavior.
                if (translation.regime !=
                        RandomConstraints::translation_bare &&
                    environment.ptw_requests() > ptw_requests_before) {
                    ++constraint_coverage.translation_walk_windows;
                }
                ptw_requests_before = environment.ptw_requests();

                std::vector<memblock::LoadTransaction> transactions;
                transactions.reserve(depth);
                for (unsigned index = 0; index < depth; ++index) {
                    transactions.push_back(make_load(
                        lines[index] +
                            bank_conflict_bank * sizeof(std::uint64_t),
                        memblock::LoadOp::ld, index));
                    environment.expect_load(transactions.back());
                }
                const std::uint64_t writebacks_before_conflict =
                    environment.writebacks();
                const std::uint64_t lq_before_conflict =
                    environment.lq_dequeued();
                std::vector<unsigned> dispatch_lanes(depth);
                std::iota(
                    dispatch_lanes.begin(), dispatch_lanes.end(), 0U);
                if (!environment.set_rob_head(
                        transactions.front().rob,
                        transactions.front().rob_flag) ||
                    !environment.enqueue_load_batch(
                        transactions, dispatch_lanes) ||
                    !environment.issue_load_batch(
                        transactions, constrained_completion_timeout, true) ||
                    !environment.run_until_all_complete(
                        constrained_completion_timeout) ||
                    !environment.run_until_queues_retired(
                        constrained_completion_timeout)) {
                    return false;
                }
                const std::array<std::uint64_t, 4> terminal_delta{{
                    1,
                    depth,
                    environment.writebacks() - writebacks_before_conflict,
                    environment.lq_dequeued() - lq_before_conflict,
                }};
                if (terminal_delta[2] != depth ||
                    terminal_delta[3] != depth) {
                    std::ostringstream detail;
                    detail << "random-bank-conflict-terminal"
                           << ":depth=" << depth
                           << ":bank=" << bank_conflict_bank
                           << ":terminal=";
                    for (unsigned index = 0; index < terminal_delta.size();
                         ++index) {
                        detail << (index == 0 ? "" : ",")
                               << terminal_delta[index];
                    }
                    phase = detail.str();
                    return false;
                }
                for (const auto &transaction : transactions) {
                    coverage.sample(transaction);
                }
                constraint_coverage.sample_bank_conflict(
                    bank_conflict_depth_index, bank_conflict_bank,
                    translation.regime, terminal_delta);
                constraint_coverage.sample_dcache_load(std::nullopt);
                sample_dcache = false;
                ++constraint_coverage.locality[0];
                ordinary_leaf_addressed = true;
                ++actions;
                coverage.cacheable += depth;
              } else {
                const auto op = static_cast<memblock::LoadOp>(random() % 7);
                const unsigned size =
                    1U << (static_cast<unsigned>(op) & 3U);
                const bool error_response =
                    dcache_load_error_denied.has_value();
                const bool stride_shaped = !error_response &&
                    (closing_stride_stream ||
                     (constraints.stride_stream_per_mille != 0 &&
                      random() % 1000 < constraints.stride_stream_per_mille));
                std::uint64_t address = 0;
                if (error_response) {
                    constexpr std::uint64_t error_lines =
                        dcache_error_span / 64;
                    if (dcache_error_line >= error_lines) {
                        phase = "random-dcache-load-error-address-space-exhausted";
                        return false;
                    }
                    const std::uint64_t sequence = dcache_error_line++;
                    const std::uint64_t line = sequence ^ (sequence >> 1);
                    address = dcache_error_base + line * 64 +
                        ((sequence & 1U) != 0 ? 0x38 : 0x18);
                    ordinary_leaf_addressed = true;
                } else if (stride_shaped) {
                    address = random_stride_base +
                        (stride_stream_loads % random_stride_slots) *
                            random_stride_bytes;
                    ordinary_leaf_addressed = true;
                } else {
                    address = action_cacheable_address(size);
                }
                auto transaction = make_load(
                    address, op, random() % 3);
                if (stride_shaped) {
                    const std::uint64_t epoch =
                        stride_stream_loads / random_stride_slots;
                    transaction.pc = 0x6000 + epoch * 4;
                    ++stride_stream_loads;
                    ++constraint_coverage.locality[2];
                }
                if (error_response) {
                    phase = *dcache_load_error_denied
                        ? "random-dcache-load-denied"
                        : "random-dcache-load-corrupt";
                    if (!environment.run_until_dcache_idle(
                            constrained_completion_timeout) ||
                        !environment.run_until_queues_retired(
                            constrained_completion_timeout)) {
                        return false;
                    }
                    transaction.expected_exception_mask =
                        *dcache_load_error_denied
                        ? memblock::kExceptionLoadAccessFault
                        : memblock::kExceptionHardwareError;
                    const std::uint64_t requests_before_error =
                        environment.tilelink_requests();
                    const std::uint64_t refills_before =
                        environment.dcache_error_response_refills();
                    const std::uint64_t error_responses_before =
                        environment.dcache_error_response_requests();
                    const std::uint64_t denied_beats_before =
                        environment.dcache_denied_d_beats();
                    const std::uint64_t corrupt_beats_before =
                        environment.dcache_corrupt_d_beats();
                    const std::uint64_t grant_acks_before =
                        environment.dcache_error_response_grant_acks();
                    const std::uint64_t bus_data_before =
                        environment.bus_expected_load(transaction.address, op);
                    const std::uint64_t lq_dequeued_before =
                        environment.lq_dequeued();
                    const std::uint64_t sq_dequeued_before =
                        environment.sq_dequeued();
                    const std::uint64_t lq_cancels_before =
                        environment.lq_redirect_canceled_observed();
                    const std::uint64_t sq_cancels_before =
                        environment.sq_redirect_canceled_observed();
                    environment.inject_dcache_response_error_at(
                        transaction.address & ~std::uint64_t{63},
                        *dcache_load_error_denied,
                        !*dcache_load_error_denied);
                    if (!environment.set_rob_head(
                            transaction.rob, transaction.rob_flag) ||
                        !issue_load(transaction) ||
                        !recover_random_precise_error(
                            transaction.rob, transaction.rob_flag, false,
                            lq_dequeued_before, sq_dequeued_before,
                            lq_cancels_before, sq_cancels_before) ||
                        !environment.run_until_dcache_idle(
                            constrained_completion_timeout)) {
                        return false;
                    }
                    const std::array<std::uint64_t, 5> manager_delta{{
                        environment.dcache_error_response_requests() -
                            error_responses_before,
                        environment.dcache_denied_d_beats() -
                            denied_beats_before,
                        environment.dcache_corrupt_d_beats() -
                            corrupt_beats_before,
                        environment.dcache_error_response_grant_acks() -
                            grant_acks_before,
                        environment.dcache_error_response_refills() -
                            refills_before,
                    }};
                    if (environment.tilelink_requests() <
                            requests_before_error + 1 ||
                        manager_delta != std::array<std::uint64_t, 5>{
                            1, *dcache_load_error_denied ? 2U : 0U,
                            2, 1, 1} ||
                        (environment.dcache_last_error_response_address() &
                         ~std::uint64_t{63}) !=
                            (transaction.address & ~std::uint64_t{63}) ||
                        environment.dcache_last_error_response_keyword() !=
                            ((transaction.address & 32U) != 0) ||
                        environment.bus_expected_load(transaction.address, op) !=
                            bus_data_before) {
                        std::ostringstream detail;
                        detail << phase << ":manager-accounting"
                               << ":requests="
                               << environment.tilelink_requests() -
                                      requests_before_error
                               << ":manager=" << manager_delta[0] << ','
                               << manager_delta[1] << ',' << manager_delta[2]
                               << ',' << manager_delta[3] << ','
                               << manager_delta[4]
                               << ":last_address=0x" << std::hex
                               << environment.dcache_last_error_response_address()
                               << ":expected_address=0x"
                               << transaction.address << std::dec
                               << ":last_keyword="
                               << environment.dcache_last_error_response_keyword()
                               << ":expected_keyword="
                               << ((transaction.address & 32U) != 0);
                        phase = detail.str();
                        return false;
                    }
                    constraint_coverage.sample_dcache_load(
                        dcache_load_error_denied, manager_delta);
                } else {
                    if (!issue_load(transaction)) {
                        return false;
                    }
                    constraint_coverage.sample_dcache_load(std::nullopt);
                }
                ++coverage.cacheable;
              }
            } else if (kind == RandomConstraints::load_merge) {
                phase = "random-load-merge";
                unsigned depth_index =
                    constraints.choose_load_merge_depth(random());
                unsigned pattern =
                    constraints.choose_load_merge_pattern(random());
                unsigned critical_beat = random() & 1U;
                bool shape_forced = false;
                for (unsigned candidate_depth = 0;
                     candidate_depth < 2 && !shape_forced;
                     ++candidate_depth) {
                    if (constraints.load_merge_depth_weights[
                            candidate_depth] == 0) {
                        continue;
                    }
                    for (unsigned candidate_pattern = 0;
                         candidate_pattern <
                             RandomConstraints::load_merge_pattern_count &&
                             !shape_forced;
                         ++candidate_pattern) {
                        if (constraints.load_merge_pattern_weights[
                                candidate_pattern] == 0) {
                            continue;
                        }
                        for (unsigned candidate_beat = 0;
                             candidate_beat < 2; ++candidate_beat) {
                            if (constraint_coverage.load_merge_shapes[
                                    candidate_depth][candidate_pattern]
                                    [candidate_beat] == 0) {
                                depth_index = candidate_depth;
                                pattern = candidate_pattern;
                                critical_beat = candidate_beat;
                                shape_forced = true;
                                break;
                            }
                        }
                    }
                }

                constexpr std::uint64_t merge_lines = load_merge_span / 64;
                if (load_merge_line >= merge_lines) {
                    phase = "random-load-merge-address-space-exhausted";
                    return false;
                }
                const std::uint64_t sequence = load_merge_line++;
                const std::uint64_t line_index =
                    sequence ^ (sequence >> 1);
                const std::uint64_t line =
                    load_merge_base + line_index * 64;
                const unsigned depth = depth_index + 2;
                std::array<unsigned, 3> offsets{};
                const unsigned leader_offset = critical_beat ? 40U : 8U;
                offsets[0] = leader_offset;
                if (pattern == RandomConstraints::load_merge_same_address) {
                    offsets[1] = leader_offset;
                    offsets[2] = leader_offset;
                } else if (pattern ==
                           RandomConstraints::load_merge_same_beat) {
                    offsets[1] = critical_beat ? 48U : 16U;
                    offsets[2] = critical_beat ? 56U : 24U;
                } else {
                    offsets[1] = critical_beat ? 8U : 40U;
                    offsets[2] = critical_beat ? 16U : 48U;
                }

                if (!environment.run_until_all_complete(
                        constrained_completion_timeout) ||
                    !environment.run_until_queues_retired(
                        constrained_completion_timeout) ||
                    !environment.run_until_dcache_idle(
                        constrained_completion_timeout)) {
                    return false;
                }
                std::vector<memblock::LoadTransaction> transactions;
                transactions.reserve(depth);
                for (unsigned index = 0; index < depth; ++index) {
                    const auto op = static_cast<memblock::LoadOp>(random() % 7);
                    const std::uint64_t address = line + offsets[index];
                    environment.memory().write_byte(
                        address,
                        static_cast<std::uint8_t>(1U + random() % 255U));
                    transactions.push_back(make_load(address, op, index));
                }
                for (const auto &transaction : transactions) {
                    environment.expect_load(transaction);
                }

                const std::uint64_t merge_requests_before =
                    environment.tilelink_requests();
                const std::uint64_t refills_before =
                    environment.dcache_refills();
                const std::uint64_t grant_acks_before =
                    environment.dcache_grant_acks();
                const std::uint64_t writebacks_before =
                    environment.writebacks();
                std::vector<unsigned> dispatch_lanes(depth);
                std::iota(
                    dispatch_lanes.begin(), dispatch_lanes.end(), 0U);
                if (!environment.set_rob_head(
                        transactions.front().rob,
                        transactions.front().rob_flag) ||
                    !environment.enqueue_load_batch(
                        transactions, dispatch_lanes) ||
                    !environment.issue_load_batch(
                        transactions, 4096, true) ||
                    !environment.run_until_all_complete(
                        constrained_completion_timeout) ||
                    !environment.run_until_queues_retired(
                        constrained_completion_timeout) ||
                    !environment.run_until_dcache_idle(
                        constrained_completion_timeout)) {
                    return false;
                }
                const std::array<std::uint64_t, 4> manager_delta{{
                    environment.dcache_requests_covering_since(
                        line, merge_requests_before),
                    environment.dcache_refills() - refills_before,
                    environment.dcache_grant_acks() - grant_acks_before,
                    environment.writebacks() - writebacks_before,
                }};
                if (manager_delta[0] != 1 || manager_delta[1] < 1 ||
                    manager_delta[2] != manager_delta[1] ||
                    manager_delta[3] != depth) {
                    std::ostringstream detail;
                    detail << "random-load-merge-manager"
                           << ":depth=" << depth
                           << ":pattern=" << pattern
                           << ":critical_beat=" << critical_beat
                           << ":manager=" << manager_delta[0] << ','
                           << manager_delta[1] << ',' << manager_delta[2]
                           << ',' << manager_delta[3];
                    phase = detail.str();
                    return false;
                }
                for (const auto &transaction : transactions) {
                    coverage.sample(transaction);
                }
                constraint_coverage.sample_load_merge(
                    depth_index, pattern, critical_beat,
                    translation.regime, depth, manager_delta);
                ordinary_leaf_addressed = true;
                ++actions;
                coverage.cacheable += depth;
            } else if (kind == RandomConstraints::set_pressure) {
                phase = "random-set-pressure-drain";
                if (!environment.run_until_all_complete(
                        constrained_completion_timeout) ||
                    !environment.run_until_queues_retired(8192) ||
                    !environment.run_until_sbuffer_empty(
                        constrained_completion_timeout) ||
                    !environment.run_until_dcache_idle(
                        constrained_completion_timeout)) {
                    return false;
                }

                constexpr unsigned dcache_sets = 128;
                constexpr unsigned dcache_ways = 8;
                constexpr unsigned line_bytes = 64;
                constexpr unsigned sets_per_quartile = dcache_sets / 4;
                constexpr std::uint64_t same_set_stride =
                    dcache_sets * line_bytes;
                constexpr std::uint64_t group_stride = 0x20000;
                constexpr std::uint64_t groups =
                    set_pressure_span / group_stride;
                constexpr std::uint64_t sequences_per_quartile =
                    groups * sets_per_quartile;
                struct SetPressureWindow {
                    std::uint64_t sequence;
                    std::uint64_t first_line;
                    unsigned set_index;
                };
                const unsigned window_count =
                    set_pressure_window_class + 1U;
                const unsigned depth = set_pressure_depth_index + 9U;
                const std::array<unsigned,
                                 RandomConstraints::set_pressure_window_class_count>
                    window_quartiles{{
                    set_pressure_set,
                    (set_pressure_set + 2U) % 4U,
                    (set_pressure_set + 1U) % 4U,
                    (set_pressure_set + 3U) % 4U,
                    set_pressure_set,
                    (set_pressure_set + 2U) % 4U,
                    (set_pressure_set + 1U) % 4U,
                    (set_pressure_set + 3U) % 4U,
                }};
                std::vector<SetPressureWindow> pressure_windows;
                pressure_windows.reserve(window_count);
                for (unsigned window = 0; window < window_count; ++window) {
                    const unsigned quartile = window_quartiles[window];
                    std::optional<SetPressureWindow> fresh_window;
                    while (set_pressure_sequences.at(quartile) <
                           sequences_per_quartile) {
                        const std::uint64_t sequence =
                            set_pressure_sequences.at(quartile)++;
                        const std::uint64_t group =
                            sequence / sets_per_quartile;
                        const unsigned set_index =
                            quartile * sets_per_quartile +
                            static_cast<unsigned>(
                                sequence % sets_per_quartile);
                        const std::uint64_t first_line =
                            set_pressure_base + group * group_stride +
                            set_index * line_bytes;
                        bool fresh = true;
                        for (unsigned index = 0; index < depth; ++index) {
                            const std::uint64_t line =
                                first_line + index * same_set_stride;
                            if (environment.dcache_requests_covering_since(
                                    line, 0) != 0) {
                                fresh = false;
                                break;
                            }
                        }
                        if (fresh) {
                            fresh_window = SetPressureWindow{
                                sequence, first_line, set_index};
                            break;
                        }
                    }
                    if (!fresh_window) {
                        phase =
                            "random-set-pressure-target-address-exhausted";
                        return false;
                    }
                    pressure_windows.push_back(*fresh_window);
                }
                const std::uint64_t sequence =
                    pressure_windows.front().sequence;
                const unsigned set_index =
                    pressure_windows.front().set_index;
                std::vector<std::uint64_t> target_lines;
                target_lines.reserve(window_count * depth);
                for (const auto &window : pressure_windows) {
                    for (unsigned index = 0; index < depth; ++index) {
                        target_lines.push_back(
                            window.first_line + index * same_set_stride);
                    }
                }
                const unsigned width_bytes = 1U << set_pressure_width;
                const unsigned bank = static_cast<unsigned>(random() % 8U);
                const unsigned offset = bank * 8U +
                    static_cast<unsigned>(random() % (8U / width_bytes)) *
                        width_bytes;

                environment.clear_release_line_expectations();
                environment.begin_dcache_release_observation();
                std::vector<memblock::LoadTransaction> overlap_loads;
                std::vector<std::uint64_t> overlap_lines;
                std::uint64_t overlap_requests_before = 0;
                std::uint64_t overlap_writebacks_before = 0;
                std::uint64_t overlap_lq_before = 0;
                std::uint64_t releases_while_overlap_pending = 0;
                constexpr unsigned forced_release_stall_cycles = 16;
                std::uint64_t target_stalled_releases_before = 0;
                std::uint64_t target_release_stall_cycles_before = 0;
                std::uint64_t c_stability_checks_before = 0;
                bool release_ready_window_started = false;
                std::array<std::uint64_t, 5> backpressure_manager_delta{};
                const unsigned set_pressure_completion_timeout =
                    std::max(constrained_completion_timeout, 8192U);
                const auto window_target_releases = [&] (
                    const SetPressureWindow &window, bool data_only) {
                    std::uint64_t count = 0;
                    for (unsigned index = 0; index < depth; ++index) {
                        const std::uint64_t line =
                            window.first_line + index * same_set_stride;
                        count += data_only
                            ? environment
                                  .observed_dcache_release_data_covering(line)
                            : environment.observed_dcache_releases_covering(line);
                    }
                    return count;
                };
                const auto target_releases = [&](bool data_only) {
                    std::uint64_t count = 0;
                    for (const auto &window : pressure_windows) {
                        count += window_target_releases(window, data_only);
                    }
                    return count;
                };
                const auto target_stalled_releases = [&](bool data_only) {
                    std::uint64_t count = 0;
                    for (const std::uint64_t line : target_lines) {
                        count += environment
                            .observed_dcache_stalled_releases_covering(
                                line, data_only);
                    }
                    return count;
                };
                const auto target_release_stall_cycles = [&](bool data_only) {
                    std::uint64_t count = 0;
                    for (const std::uint64_t line : target_lines) {
                        count += environment
                            .observed_dcache_release_stall_cycles_covering(
                                line, data_only);
                    }
                    return count;
                };
                const auto begin_release_backpressure = [&](bool data_only) {
                    target_stalled_releases_before =
                        target_stalled_releases(data_only);
                    target_release_stall_cycles_before =
                        target_release_stall_cycles(data_only);
                    c_stability_checks_before =
                        environment
                            .dcache_release_target_payload_stability_checks();
                    release_ready_window_started =
                        environment.begin_dcache_release_ready_window(
                            target_lines,
                            set_pressure_release_backpressure
                                ? forced_release_stall_cycles
                                : 0);
                    return release_ready_window_started;
                };
                const auto finish_release_backpressure = [&](bool data_only) {
                    if (!release_ready_window_started) {
                        return false;
                    }
                    const std::uint64_t stalled_releases =
                        target_stalled_releases(data_only) -
                        target_stalled_releases_before;
                    const std::uint64_t stall_cycles =
                        target_release_stall_cycles(data_only) -
                        target_release_stall_cycles_before;
                    const std::uint64_t stability_checks =
                        environment
                            .dcache_release_target_payload_stability_checks() -
                        c_stability_checks_before;
                    if (!environment.end_dcache_release_ready_window()) {
                        return false;
                    }
                    const std::uint64_t expected_stalled =
                        set_pressure_release_backpressure ? 1U : 0U;
                    const std::uint64_t expected_cycles =
                        set_pressure_release_backpressure
                        ? forced_release_stall_cycles
                        : 0U;
                    if (stalled_releases != expected_stalled ||
                        stall_cycles != expected_cycles ||
                        stability_checks != expected_cycles) {
                        std::ostringstream detail;
                        detail << "random-set-pressure-release-backpressure"
                               << ":selected="
                               << set_pressure_release_backpressure
                               << ":stalled=" << stalled_releases
                               << ":cycles=" << stall_cycles
                               << ":stable=" << stability_checks;
                        phase = detail.str();
                        return false;
                    }
                    backpressure_manager_delta = {{
                        set_pressure_release_backpressure ? 1U : 0U,
                        stalled_releases,
                        stall_cycles,
                        stability_checks,
                        1,
                    }};
                    return true;
                };
                const auto start_refill_overlap = [&](bool load_at_rob_head) {
                    if (!set_pressure_refill_overlap) {
                        return true;
                    }
                    overlap_requests_before = environment.tilelink_requests();
                    overlap_writebacks_before = environment.writebacks();
                    overlap_lq_before = environment.lq_dequeued();
                    overlap_loads.clear();
                    overlap_lines.clear();
                    overlap_loads.reserve(window_count);
                    overlap_lines.reserve(window_count);
                    std::vector<unsigned> overlap_sets;
                    overlap_sets.reserve(window_count);
                    for (const auto &window : pressure_windows) {
                        const std::uint64_t group_base = window.first_line -
                            window.set_index * line_bytes;
                        std::optional<std::uint64_t> fresh_overlap_line;
                        std::optional<unsigned> fresh_overlap_set;
                        for (unsigned delta = 1; delta < dcache_sets &&
                             !fresh_overlap_line; ++delta) {
                            const unsigned candidate_set =
                                (window.set_index + delta) % dcache_sets;
                            const bool aliases_target = std::any_of(
                                pressure_windows.begin(), pressure_windows.end(),
                                [&](const SetPressureWindow &target) {
                                    return target.set_index == candidate_set;
                                });
                            if (aliases_target ||
                                std::find(
                                    overlap_sets.begin(), overlap_sets.end(),
                                    candidate_set) != overlap_sets.end()) {
                                continue;
                            }
                            for (unsigned tag = depth; tag < 16U; ++tag) {
                                const std::uint64_t candidate = group_base +
                                    tag * same_set_stride +
                                    candidate_set * line_bytes;
                                if (environment.dcache_requests_covering_since(
                                        candidate, 0) == 0 &&
                                    std::find(
                                        overlap_lines.begin(),
                                        overlap_lines.end(), candidate) ==
                                        overlap_lines.end()) {
                                    fresh_overlap_line = candidate;
                                    fresh_overlap_set = candidate_set;
                                    break;
                                }
                            }
                        }
                        if (!fresh_overlap_line || !fresh_overlap_set) {
                            phase =
                                "random-set-pressure-overlap-address-exhausted";
                            return false;
                        }
                        const std::uint64_t overlap_line = *fresh_overlap_line;
                        overlap_sets.push_back(*fresh_overlap_set);
                        environment.memory().fill_incrementing(
                            overlap_line, line_bytes,
                            static_cast<std::uint8_t>(
                                0xa7U + window.set_index));
                        overlap_lines.push_back(overlap_line);
                        overlap_loads.push_back(make_load(
                            overlap_line + 8U * (random() % 8U),
                            memblock::LoadOp::ld,
                            random() % memblock::kScalarLoadLanes));
                        overlap_loads.back().oracle_address =
                            overlap_loads.back().address;
                        environment.expect_load(overlap_loads.back());
                        environment.hold_dcache_response_at(overlap_line);
                    }
                    if (load_at_rob_head &&
                        !environment.set_rob_head(
                            overlap_loads.front().rob,
                            overlap_loads.front().rob_flag)) {
                        return false;
                    }
                    for (const auto &load : overlap_loads) {
                        if (!environment.enqueue_load(load) ||
                            !environment.issue_load(load, 4096)) {
                            return false;
                        }
                    }
                    const auto all_overlap_responses_held = [&]() {
                        for (unsigned window = 0; window < window_count;
                             ++window) {
                            if (environment.dcache_requests_covering_since(
                                    overlap_lines[window],
                                    overlap_requests_before) != 1 ||
                                !environment.dcache_response_held_at(
                                    overlap_lines[window]) ||
                                !environment.scalar_load_pending(
                                    overlap_loads[window])) {
                                return false;
                            }
                        }
                        return environment.dcache_held_response_count() ==
                            window_count;
                    };
                    for (unsigned cycle = 0;
                         cycle < set_pressure_completion_timeout &&
                         !all_overlap_responses_held();
                         ++cycle) {
                        if (!environment.run_cycles(1)) {
                            return false;
                        }
                    }
                    return all_overlap_responses_held();
                };
                const auto prove_release_overlap = [&](bool data_only,
                                                       const auto &before) {
                    if (!set_pressure_refill_overlap) {
                        return true;
                    }
                    const std::uint64_t required = depth - dcache_ways;
                    const auto all_windows_released = [&]() {
                        for (unsigned window = 0; window < window_count;
                             ++window) {
                            if (window_target_releases(
                                    pressure_windows[window], data_only) -
                                    before[window] < required) {
                                return false;
                            }
                        }
                        return true;
                    };
                    for (unsigned cycle = 0;
                         cycle < set_pressure_completion_timeout &&
                         !all_windows_released();
                         ++cycle) {
                        for (const auto &load : overlap_loads) {
                            if (!environment.scalar_load_pending(load)) {
                                return false;
                            }
                        }
                        if (!environment.run_cycles(1)) {
                            return false;
                        }
                    }
                    releases_while_overlap_pending = 0;
                    if (!all_windows_released() ||
                        environment.dcache_held_response_count() !=
                            window_count) {
                        return false;
                    }
                    for (unsigned window = 0; window < window_count; ++window) {
                        releases_while_overlap_pending +=
                            window_target_releases(
                                pressure_windows[window], data_only) -
                            before[window];
                        if (!environment.dcache_response_held_at(
                                overlap_lines[window]) ||
                            !environment.scalar_load_pending(
                                overlap_loads[window])) {
                            return false;
                        }
                    }
                    if (data_only &&
                        !environment.set_rob_head(
                            overlap_loads.front().rob,
                            overlap_loads.front().rob_flag)) {
                        return false;
                    }
                    for (const std::uint64_t line : overlap_lines) {
                        if (!environment.release_held_dcache_response_at(line)) {
                            return false;
                        }
                    }
                    return true;
                };
                const auto overlap_manager_delta = [&]() {
                    if (!set_pressure_refill_overlap) {
                        return std::array<std::uint64_t, 5>{};
                    }
                    std::uint64_t requests = 0;
                    for (const std::uint64_t line : overlap_lines) {
                        requests += environment.dcache_requests_covering_since(
                            line, overlap_requests_before);
                    }
                    return std::array<std::uint64_t, 5>{{
                        window_count,
                        requests,
                        releases_while_overlap_pending,
                        window_count,
                        window_count,
                    }};
                };

                if (!set_pressure_dirty) {
                    const std::uint64_t requests_before_pressure =
                        environment.tilelink_requests();
                    const std::uint64_t all_releases_before_pressure =
                        environment.tilelink_releases();
                    const std::uint64_t releases_before_pressure =
                        environment.tilelink_release_data();
                    const std::uint64_t verified_before_pressure =
                        environment.tilelink_release_data_verified();
                    const std::uint64_t writebacks_before_pressure =
                        environment.writebacks();
                    const std::uint64_t lq_before_pressure =
                        environment.lq_dequeued();
                    constexpr std::array<memblock::LoadOp, 4>
                        unsigned_load_ops{{
                            memblock::LoadOp::lbu, memblock::LoadOp::lhu,
                            memblock::LoadOp::lwu, memblock::LoadOp::ld,
                    }};
                    std::vector<memblock::LoadTransaction> pressure_loads;
                    pressure_loads.reserve(target_lines.size());
                    for (unsigned index = 0; index < target_lines.size();
                         ++index) {
                        const std::uint64_t line = target_lines[index];
                        const unsigned target_set = static_cast<unsigned>(
                            (line / line_bytes) % dcache_sets);
                        environment.memory().fill_incrementing(
                            line, line_bytes,
                            static_cast<std::uint8_t>(
                                0x31U + target_set + index * 17U));
                        environment.expect_release_line(
                            line,
                            environment.memory().read_beat(line, line_bytes));
                    }
                    std::vector<std::uint64_t>
                        target_releases_before_overlap;
                    target_releases_before_overlap.reserve(window_count);
                    const auto issue_pressure_load = [&](unsigned index) {
                        const std::uint64_t line = target_lines[index];
                        auto load = make_load(
                            line + offset,
                            unsigned_load_ops[set_pressure_width],
                            (index + set_pressure_set) %
                                memblock::kScalarLoadLanes);
                        load.oracle_address = load.address;
                        bool issued = false;
                        if (set_pressure_refill_overlap) {
                            environment.expect_load(load);
                            issued = environment.enqueue_load(load) &&
                                environment.issue_load(load, 4096);
                        } else {
                            issued = issue_load(load, std::nullopt, false);
                        }
                        if (!issued) {
                            phase = "random-set-pressure-clean-fill";
                            return false;
                        }
                        coverage.sample(load);
                        pressure_loads.push_back(load);
                        return true;
                    };
                    if (set_pressure_refill_overlap) {
                        phase = "random-set-pressure-clean-baseline-fill";
                        for (unsigned window = 0; window < window_count;
                             ++window) {
                            for (unsigned tag = 0; tag < dcache_ways; ++tag) {
                                if (!issue_pressure_load(
                                        window * depth + tag)) {
                                    return false;
                                }
                            }
                        }
                        phase =
                            "random-set-pressure-clean-baseline-fill-complete";
                        if (!environment.run_until_all_complete(
                                set_pressure_completion_timeout) ||
                            !environment.run_until_queues_retired(8192) ||
                            !environment.run_until_dcache_idle(
                                set_pressure_completion_timeout)) {
                            return false;
                        }
                    }
                    for (const auto &window : pressure_windows) {
                        target_releases_before_overlap.push_back(
                            window_target_releases(window, false));
                    }
                    if (set_pressure_refill_overlap) {
                        phase = "random-set-pressure-clean-refill-overlap";
                        if (!start_refill_overlap(true)) {
                            return false;
                        }
                    }
                    phase = "random-set-pressure-clean-release-backpressure";
                    if (!begin_release_backpressure(false)) {
                        return false;
                    }
                    if (set_pressure_refill_overlap) {
                        for (unsigned window = 0; window < window_count;
                             ++window) {
                            for (unsigned tag = dcache_ways; tag < depth;
                                 ++tag) {
                                if (!issue_pressure_load(
                                        window * depth + tag)) {
                                    return false;
                                }
                            }
                        }
                    } else {
                        for (unsigned index = 0; index < target_lines.size();
                             ++index) {
                            if (!issue_pressure_load(index)) {
                                return false;
                            }
                        }
                    }
                    phase = "random-set-pressure-clean-fill-complete";
                    if (set_pressure_refill_overlap
                            ? !environment.run_until_load_writebacks(
                                  writebacks_before_pressure +
                                      target_lines.size(),
                                  set_pressure_completion_timeout)
                            : (!environment.run_until_queues_retired(8192) ||
                               !environment.run_until_dcache_idle(
                                   constrained_completion_timeout))) {
                        return false;
                    }

                    std::uint64_t initial_target_requests = 0;
                    for (const auto &load : pressure_loads) {
                        const std::uint64_t target_requests =
                            environment.dcache_requests_covering_since(
                                load.address & ~std::uint64_t{63},
                                requests_before_pressure);
                        if (target_requests != 1) {
                            phase = "random-set-pressure-clean-initial-request";
                            return false;
                        }
                        initial_target_requests += target_requests;
                    }
                    phase = "random-set-pressure-clean-release-overlap";
                    if (!prove_release_overlap(
                            false, target_releases_before_overlap)) {
                        return false;
                    }
                    if (set_pressure_refill_overlap &&
                        (!environment.run_until_all_complete(
                             set_pressure_completion_timeout) ||
                         !environment.run_until_queues_retired(8192) ||
                         !environment.run_until_dcache_idle(
                             set_pressure_completion_timeout))) {
                        phase = "random-set-pressure-clean-fill-retire";
                        return false;
                    }
                    const std::uint64_t requests_before_revisit =
                        environment.tilelink_requests();
                    for (unsigned reverse = pressure_loads.size();
                         reverse != 0; --reverse) {
                        const unsigned index = reverse - 1U;
                        auto revisit = make_load(
                            pressure_loads[index].address,
                            pressure_loads[index].op,
                            (index + set_pressure_set + 1U) %
                                memblock::kScalarLoadLanes);
                        revisit.oracle_address = revisit.address;
                        if (!issue_load(revisit, std::nullopt, false)) {
                            phase = "random-set-pressure-clean-revisit";
                            return false;
                        }
                        coverage.sample(revisit);
                    }
                    phase = "random-set-pressure-clean-revisit-complete";
                    if (!environment.run_until_all_complete(
                            set_pressure_completion_timeout) ||
                        !environment.run_until_queues_retired(8192) ||
                        !environment.run_until_dcache_idle(
                            set_pressure_completion_timeout)) {
                        return false;
                    }
                    phase = "random-set-pressure-clean-release-window";
                    if (!finish_release_backpressure(false)) {
                        return false;
                    }

                    std::uint64_t revisit_target_requests = 0;
                    std::uint64_t target_releases = 0;
                    std::uint64_t target_release_data = 0;
                    for (const auto &load : pressure_loads) {
                        revisit_target_requests +=
                            environment.dcache_requests_covering_since(
                                load.address & ~std::uint64_t{63},
                                requests_before_revisit);
                        target_releases +=
                            environment.observed_dcache_releases_covering(
                                load.address & ~std::uint64_t{63});
                        target_release_data +=
                            environment.observed_dcache_release_data_covering(
                                load.address & ~std::uint64_t{63});
                    }
                    for (const auto &window : pressure_windows) {
                        if (window_target_releases(window, false) <
                            depth - dcache_ways) {
                            phase =
                                "random-set-pressure-clean-window-release";
                            return false;
                        }
                    }
                    const std::array<std::uint64_t, 12> manager_delta{{
                        1,
                        target_lines.size(),
                        initial_target_requests,
                        target_lines.size(),
                        revisit_target_requests,
                        target_releases,
                        target_release_data,
                        environment.tilelink_releases() -
                            all_releases_before_pressure,
                        environment.tilelink_release_data() -
                            releases_before_pressure,
                        environment.tilelink_release_data_verified() -
                            verified_before_pressure,
                        environment.writebacks() - writebacks_before_pressure,
                        environment.lq_dequeued() - lq_before_pressure,
                    }};
                    const std::uint64_t minimum_target_releases =
                        window_count * (depth - dcache_ways);
                    const std::uint64_t overlap_load_count =
                        set_pressure_refill_overlap ? window_count : 0U;
                    if (manager_delta[2] != target_lines.size() ||
                        manager_delta[4] < minimum_target_releases ||
                        manager_delta[5] < minimum_target_releases ||
                        manager_delta[6] != 0 ||
                        manager_delta[7] < manager_delta[5] ||
                        manager_delta[9] != manager_delta[8] ||
                        manager_delta[10] !=
                            target_lines.size() * 2U + overlap_load_count ||
                        manager_delta[11] !=
                            target_lines.size() * 2U + overlap_load_count) {
                        std::ostringstream detail;
                        detail << "random-set-pressure-clean-manager"
                               << ":depth=" << depth
                               << ":width=" << width_bytes
                               << ":set=" << set_index << ":manager=";
                        for (unsigned index = 0; index < manager_delta.size();
                             ++index) {
                            detail << (index == 0 ? "" : ",")
                                   << manager_delta[index];
                        }
                        phase = detail.str();
                        return false;
                    }
                    environment.end_dcache_release_observation();
                    environment.clear_release_line_expectations();
                    constraint_coverage.sample_set_pressure(
                        false, set_pressure_refill_overlap,
                        set_pressure_release_backpressure,
                        set_pressure_window_class,
                        set_pressure_depth_index, set_pressure_width,
                        translation.regime, set_pressure_set, {}, {},
                        manager_delta, overlap_manager_delta(),
                        backpressure_manager_delta);
                    ordinary_leaf_addressed = true;
                    ++actions;
                    coverage.cacheable += target_lines.size() * 2U;
                } else {
                  const std::uint64_t requests_before_pressure =
                      environment.tilelink_requests();
                  const std::uint64_t expected_releases_before =
                      environment.tilelink_expected_release_data_verified();
                  const std::uint64_t releases_before_pressure =
                      environment.tilelink_release_data();
                  const std::uint64_t verified_before_pressure =
                      environment.tilelink_release_data_verified();
                  const std::uint64_t writebacks_before_pressure =
                      environment.store_writebacks();
                  const std::uint64_t sq_before_pressure =
                      environment.sq_dequeued();
                  std::vector<std::uint64_t> store_lines;
                  store_lines.reserve(target_lines.size());
                  for (unsigned index = 0; index < dcache_ways; ++index) {
                    for (const auto &window : pressure_windows) {
                      store_lines.push_back(
                          window.first_line + index * same_set_stride);
                    }
                  }
                  for (unsigned index = dcache_ways; index < depth; ++index) {
                    for (const auto &window : pressure_windows) {
                      store_lines.push_back(
                          window.first_line + index * same_set_stride);
                    }
                  }
                  std::vector<memblock::StoreTransaction> pressure_stores;
                  std::vector<bool> pressure_store_data_first;
                  pressure_stores.reserve(store_lines.size());
                  pressure_store_data_first.reserve(store_lines.size());
                  std::array<std::uint64_t, 2> issue_orders{};
                  for (unsigned index = 0; index < store_lines.size();
                       ++index) {
                    const std::uint64_t line = store_lines[index];
                    const unsigned target_set = static_cast<unsigned>(
                        (line / line_bytes) % dcache_sets);
                    environment.memory().fill_incrementing(
                        line, line_bytes,
                        static_cast<std::uint8_t>(0x31U + target_set +
                                                  index * 17U));
                    std::vector<unsigned char> expected_line =
                        environment.memory().read_beat(line, line_bytes);
                    std::uint64_t data = random();
                    for (unsigned byte = 0; byte < width_bytes; ++byte) {
                      const auto stored = static_cast<unsigned char>(
                          expected_line[offset + byte] ^ 0xffU);
                      data &= ~(std::uint64_t{0xff} << (8U * byte));
                      data |= std::uint64_t{stored} << (8U * byte);
                      expected_line[offset + byte] = stored;
                    }
                    pressure_stores.push_back(make_store(
                        line + offset, data,
                        static_cast<memblock::StoreOp>(set_pressure_width),
                        (index + set_pressure_set) %
                            memblock::kScalarStoreLanes,
                        (index + set_pressure_set + 1U) %
                            memblock::kScalarStoreLanes));
                    const auto &store = pressure_stores.back();
                    environment.expect_release_line(line, expected_line);
                    const bool data_first =
                        ((index + (sequence & 1U)) & 1U) != 0;
                    pressure_store_data_first.push_back(data_first);
                    ++issue_orders[data_first ? 1U : 0U];
                  }
                  const auto issue_store_range = [&](std::size_t first,
                                                     std::size_t last) {
                    for (std::size_t index = first; index < last; ++index) {
                      const auto &store = pressure_stores[index];
                      const bool data_first =
                          pressure_store_data_first[index];
                      environment.expect_store(store);
                      const bool issued =
                          (index != 0 ||
                           environment.set_rob_head(
                               store.rob, store.rob_flag)) &&
                          environment.enqueue_store(
                              store, memblock::lq_pointer_value(lq_offset)) &&
                          (data_first
                               ? environment.issue_store_data(store, 4096) &&
                                     environment
                                         .issue_store_address_until_tlb_hit(
                                             store,
                                             constrained_completion_timeout)
                               : environment
                                         .issue_store_address_until_tlb_hit(
                                             store,
                                             constrained_completion_timeout) &&
                                     environment.issue_store_data(store, 4096));
                      if (!issued) {
                        return false;
                      }
                      coverage.sample(store, data_first);
                    }
                    return true;
                  };
                  const std::size_t baseline_stores =
                      window_count * dcache_ways;
                  // Commit each four-way half before the next one so an
                  // eight-window action stays below the SQ dispatch watermark.
                  const std::size_t baseline_batch_stores = window_count * 4U;
                  for (std::size_t first = 0; first < baseline_stores;
                       first += baseline_batch_stores) {
                    const std::size_t last = std::min(
                        first + baseline_batch_stores, baseline_stores);
                    phase = "random-set-pressure-dirty-baseline-issue";
                    if (!issue_store_range(first, last) ||
                        !environment.run_until_store_complete(
                            constrained_completion_timeout)) {
                      return false;
                    }
                    phase = "random-set-pressure-dirty-baseline-commit";
                    for (std::size_t index = first; index < last; ++index) {
                      if (!environment.commit_store(
                              pressure_stores[index], 8192)) {
                        return false;
                      }
                    }
                    if (!environment.run_until_queues_retired(8192) ||
                        !environment.run_until_sbuffer_empty(
                            set_pressure_completion_timeout)) {
                      return false;
                    }
                  }
                  std::vector<std::uint64_t>
                      target_releases_before_overlap;
                  target_releases_before_overlap.reserve(window_count);
                  for (const auto &window : pressure_windows) {
                    target_releases_before_overlap.push_back(
                        window_target_releases(window, true));
                  }
                  phase = "random-set-pressure-dirty-refill-overlap";
                  if (!start_refill_overlap(false)) {
                    return false;
                  }
                  phase = "random-set-pressure-dirty-release-backpressure";
                  if (!begin_release_backpressure(true)) {
                    return false;
                  }
                  phase = "random-set-pressure-dirty-overflow-issue";
                  if (!issue_store_range(
                          baseline_stores, pressure_stores.size()) ||
                      !environment.run_until_store_complete(
                          constrained_completion_timeout)) {
                    return false;
                  }
                  phase = "random-set-pressure-dirty-overflow-commit";
                  for (std::size_t index = baseline_stores;
                       index < pressure_stores.size(); ++index) {
                    if (!environment.commit_store(
                            pressure_stores[index], 8192)) {
                      return false;
                    }
                  }
                  phase = "random-set-pressure-dirty-store-issue-complete";
                  if (!environment.run_until_store_complete(
                          constrained_completion_timeout)) {
                      return false;
                  }
                  phase = "random-set-pressure-dirty-release-overlap";
                  if (!prove_release_overlap(
                          true, target_releases_before_overlap)) {
                    return false;
                  }
                  phase = "random-set-pressure-drain-complete";
                  if (!environment.run_until_all_complete(
                          set_pressure_completion_timeout) ||
                      !environment.run_until_queues_retired(8192) ||
                      !environment.run_until_sbuffer_empty(
                          set_pressure_completion_timeout) ||
                      !environment.run_until_expected_release_data_count(
                          expected_releases_before +
                              window_count * (depth - dcache_ways),
                          set_pressure_completion_timeout) ||
                      !environment.run_until_dcache_idle(
                          set_pressure_completion_timeout)) {
                    return false;
                  }
                  phase = "random-set-pressure-dirty-release-window";
                  if (!finish_release_backpressure(true)) {
                    return false;
                  }

                  std::uint64_t target_requests = 0;
                  std::uint64_t preserved_stores = 0;
                  constexpr std::array<memblock::LoadOp, 4> unsigned_load_ops{{
                      memblock::LoadOp::lbu,
                      memblock::LoadOp::lhu,
                      memblock::LoadOp::lwu,
                      memblock::LoadOp::ld,
                  }};
                  const std::uint64_t value_mask =
                      width_bytes == 8
                          ? ~std::uint64_t{0}
                          : (std::uint64_t{1} << (width_bytes * 8U)) - 1U;
                  for (const auto &store : pressure_stores) {
                    const std::uint64_t store_requests =
                        environment.dcache_requests_covering_since(
                            store.address & ~std::uint64_t{63},
                            requests_before_pressure);
                    if (store_requests != 1) {
                      phase = "random-set-pressure-dirty-target-request";
                      return false;
                    }
                    target_requests += store_requests;
                    if (environment.bus_expected_load(
                            store.address,
                            unsigned_load_ops[set_pressure_width]) ==
                        (store.data & value_mask)) {
                      ++preserved_stores;
                    }
                  }
                  for (const auto &window : pressure_windows) {
                    if (window_target_releases(window, true) <
                        depth - dcache_ways) {
                      phase = "random-set-pressure-dirty-window-release";
                      return false;
                    }
                  }
                  const std::array<std::uint64_t, 9> manager_delta{{
                      1,
                      target_lines.size(),
                      target_requests,
                      environment.tilelink_expected_release_data_verified() -
                          expected_releases_before,
                      environment.tilelink_release_data() -
                          releases_before_pressure,
                      environment.tilelink_release_data_verified() -
                          verified_before_pressure,
                      environment.store_writebacks() -
                          writebacks_before_pressure,
                      environment.sq_dequeued() - sq_before_pressure,
                      preserved_stores,
                  }};
                  const auto overlap_delta = overlap_manager_delta();
                  if (manager_delta[2] != target_lines.size() ||
                      manager_delta[3] <
                          window_count * (depth - dcache_ways) ||
                      manager_delta[4] < manager_delta[3] ||
                      manager_delta[5] != manager_delta[4] ||
                      manager_delta[6] != target_lines.size() ||
                      manager_delta[7] != target_lines.size() ||
                      manager_delta[8] != manager_delta[3] ||
                      (set_pressure_refill_overlap &&
                       (environment.writebacks() - overlap_writebacks_before !=
                            window_count ||
                        environment.lq_dequeued() - overlap_lq_before !=
                            window_count))) {
                    std::ostringstream detail;
                    detail << "random-set-pressure-manager"
                           << ":depth=" << depth << ":width=" << width_bytes
                           << ":set=" << set_index << ":manager=";
                    for (unsigned index = 0; index < manager_delta.size();
                         ++index) {
                      detail << (index == 0 ? "" : ",") << manager_delta[index];
                    }
                    phase = detail.str();
                    return false;
                  }
                  environment.end_dcache_release_observation();
                  environment.clear_release_line_expectations();
                  constraint_coverage.sample_set_pressure(
                      true, set_pressure_refill_overlap,
                      set_pressure_release_backpressure,
                      set_pressure_window_class,
                      set_pressure_depth_index, set_pressure_width,
                      translation.regime, set_pressure_set, issue_orders,
                      manager_delta, {}, overlap_delta,
                      backpressure_manager_delta);
                  ordinary_leaf_addressed = true;
                  ++actions;
                  coverage.cacheable += target_lines.size();
                }
            } else if (kind == RandomConstraints::miss_burst) {
                phase = "random-miss-burst";
                const unsigned depth = miss_burst_depth_index + 2U;
                const unsigned issue_width =
                    miss_burst_issue_width_index + 1U;
                constexpr std::uint64_t line_bytes = 64;
                constexpr std::uint64_t lines = miss_burst_span / line_bytes;
                if (miss_burst_line + depth > lines) {
                    phase = "random-miss-burst-address-space-exhausted";
                    return false;
                }
                if (!environment.run_until_all_complete(
                        constrained_completion_timeout) ||
                    !environment.run_until_queues_retired(
                        constrained_completion_timeout) ||
                    !environment.run_until_dcache_idle(
                        constrained_completion_timeout)) {
                    return false;
                }
                std::vector<memblock::LoadTransaction> transactions;
                std::vector<std::uint64_t> burst_lines;
                transactions.reserve(depth);
                burst_lines.reserve(depth);
                const std::uint64_t burst_first_line = miss_burst_line;
                miss_burst_line += depth;
                for (unsigned index = 0; index < depth; ++index) {
                    const std::uint64_t line_index =
                        burst_first_line + index;
                    const std::uint64_t line =
                        miss_burst_base + line_index * line_bytes;
                    const std::uint64_t address = line +
                        ((index & 1U) != 0 ? 8U : 40U);
                    const auto op = static_cast<memblock::LoadOp>(
                        (index + random()) % 4U);
                    environment.memory().fill_incrementing(
                        line, line_bytes,
                        static_cast<unsigned char>(0x61U + index));
                    transactions.push_back(make_load(
                        address, op, index % memblock::kScalarLoadLanes));
                    burst_lines.push_back(line);
                    environment.expect_load(transactions.back());
                    environment.hold_dcache_response_at(line);
                }
                const std::uint64_t requests_before_burst =
                    environment.tilelink_requests();
                const std::uint64_t refills_before_burst =
                    environment.dcache_refills();
                const std::uint64_t acks_before_burst =
                    environment.dcache_grant_acks();
                const std::uint64_t writebacks_before_burst =
                    environment.writebacks();
                const std::uint64_t lq_before_burst =
                    environment.lq_dequeued();
                if (!environment.set_rob_head(
                        transactions.front().rob,
                        transactions.front().rob_flag)) {
                    return false;
                }
                for (unsigned begin = 0; begin < transactions.size();
                     begin += memblock::kScalarLoadLanes) {
                    const unsigned end = std::min<unsigned>(
                        begin + memblock::kScalarLoadLanes,
                        transactions.size());
                    std::vector<memblock::LoadTransaction> batch(
                        transactions.begin() + begin,
                        transactions.begin() + end);
                    std::vector<unsigned> dispatch_lanes(batch.size());
                    std::iota(dispatch_lanes.begin(), dispatch_lanes.end(), 0U);
                    if (!environment.enqueue_load_batch(batch, dispatch_lanes)) {
                        return false;
                    }
                }
                std::vector<memblock::LoadTransaction> first_batch(
                    transactions.begin(), transactions.begin() + issue_width);
                if (!environment.issue_load_batch(first_batch, 4096, true)) {
                    return false;
                }
                for (unsigned index = issue_width; index < transactions.size();
                     ++index) {
                    if (!environment.issue_load(transactions[index], 4096)) {
                        return false;
                    }
                }
                const auto held_burst_responses = [&]() {
                    for (unsigned index = 0; index < depth; ++index) {
                        if (!environment.dcache_response_held_at(
                                burst_lines[index]) ||
                            !environment.scalar_load_pending(
                                transactions[index])) {
                            return false;
                        }
                    }
                    return environment.dcache_held_response_count() == depth &&
                        environment.dcache_outstanding_requests() == depth;
                };
                for (unsigned cycle = 0;
                     cycle < constrained_completion_timeout &&
                         !held_burst_responses(); ++cycle) {
                    if (!environment.run_cycles(1)) {
                        return false;
                    }
                }
                if (!held_burst_responses()) {
                    phase = "random-miss-burst-outstanding-depth";
                    return false;
                }
                const std::uint64_t observed_outstanding =
                    environment.dcache_outstanding_requests();
                for (const auto line : burst_lines) {
                    if (!environment.release_held_dcache_response_at(line)) {
                        return false;
                    }
                }
                if (!environment.run_until_all_complete(
                        constrained_completion_timeout) ||
                    !environment.run_until_queues_retired(
                        constrained_completion_timeout) ||
                    !environment.run_until_dcache_idle(
                        constrained_completion_timeout)) {
                    return false;
                }
                std::uint64_t target_requests = 0;
                for (const auto line : burst_lines) {
                    const std::uint64_t line_requests =
                        environment.dcache_requests_covering_since(
                            line, requests_before_burst);
                    if (line_requests != 1) {
                        phase = "random-miss-burst-request-accounting";
                        return false;
                    }
                    target_requests += line_requests;
                }
                const std::array<std::uint64_t, 7> manager_delta{{
                    1,
                    depth,
                    target_requests,
                    environment.dcache_refills() - refills_before_burst,
                    environment.dcache_grant_acks() - acks_before_burst,
                    environment.writebacks() - writebacks_before_burst,
                    environment.lq_dequeued() - lq_before_burst,
                }};
                if (manager_delta[2] != depth ||
                    manager_delta[3] < depth ||
                    manager_delta[4] != manager_delta[3] ||
                    manager_delta[5] != depth ||
                    manager_delta[6] != depth) {
                    phase = "random-miss-burst-manager-accounting";
                    return false;
                }
                constraint_coverage.sample_miss_burst(
                    miss_burst_depth_index, miss_burst_issue_width_index,
                    translation.regime, observed_outstanding, manager_delta);
                ordinary_leaf_addressed = true;
                ++actions;
                coverage.cacheable += depth;
            } else if (kind == RandomConstraints::scalar_store) {
                const auto op = static_cast<memblock::StoreOp>(random() % 4);
                const unsigned size = 1U << static_cast<unsigned>(op);
                const auto transaction = make_store(
                    action_cacheable_address(size), random(), op,
                    random() % 2, random() % 2);
                if (!issue_store(transaction, (random() & 1U) != 0) ||
                    !environment.commit_store(transaction, 8192) ||
                    !environment.run_until_queues_retired(8192)) {
                    return false;
                }
                probe_candidate = transaction.address & ~std::uint64_t{63};
                ++coverage.cacheable;
            } else if (kind == RandomConstraints::ptw_error) {
                phase = "random-ptw-error-target";
                const std::uint64_t address = ptw_error_address;
                std::optional<std::uint64_t> target_pte;
                if (ptw_error_site == RandomConstraints::ptw_error_stage1) {
                    target_pte = memblock::reference_pte_address_at_level(
                        environment.memory(), ptw_error_stage1_root, address,
                        page_mode(ptw_error_stage1_mode),
                        ptw_error_target_level);
                } else if (ptw_error_site ==
                           RandomConstraints::ptw_error_gstage) {
                    target_pte = memblock::reference_pte_address_at_level(
                        environment.memory(), ptw_error_g_root, address,
                        page_mode(ptw_error_g_mode), ptw_error_target_level,
                        true);
                } else if (ptw_error_site ==
                           RandomConstraints::ptw_error_nested_g_implicit) {
                    const auto vs_root_pte =
                        memblock::reference_pte_address_at_level(
                            environment.memory(), ptw_error_vs_root, address,
                            page_mode(ptw_error_vs_mode),
                            memblock::reference_page_levels(
                                page_mode(ptw_error_vs_mode)) - 1U);
                    if (vs_root_pte) {
                        target_pte = memblock::reference_pte_address_at_level(
                            environment.memory(), ptw_error_g_root,
                            *vs_root_pte,
                            page_mode(ptw_error_g_mode),
                            ptw_error_target_level, true);
                    }
                } else if (ptw_error_site ==
                           RandomConstraints::ptw_error_nested_vs) {
                    const auto vs_pte =
                        memblock::reference_pte_address_at_level(
                            environment.memory(), ptw_error_vs_root, address,
                            page_mode(ptw_error_vs_mode),
                            ptw_error_target_level);
                    if (vs_pte) {
                        const auto translated_pte =
                            memblock::reference_page_walk(
                                environment.memory(), ptw_error_g_root,
                                *vs_pte,
                                page_mode(ptw_error_g_mode), true);
                        if (translated_pte.translated) {
                            target_pte = translated_pte.physical_address;
                        }
                    }
                } else {
                    const auto vs_walk = memblock::reference_page_walk(
                        environment.memory(), ptw_error_vs_root, address,
                        page_mode(ptw_error_vs_mode));
                    if (vs_walk.translated) {
                        target_pte = memblock::reference_pte_address_at_level(
                            environment.memory(), ptw_error_g_root,
                            vs_walk.physical_address,
                            page_mode(ptw_error_g_mode),
                            ptw_error_target_level, true);
                    }
                }
                if (!target_pte) {
                    phase += ":reference-walk";
                    return false;
                }
                {
                    std::ostringstream detail;
                    detail << "random-ptw-error"
                           << ":site=" << ptw_error_site
                           << ":direction=" << ptw_error_store
                           << ":level_class=" << ptw_error_level_class
                           << ":target_level=" << ptw_error_target_level
                           << ":stage1_mode=" << ptw_error_stage1_mode
                           << ":vs_mode=" << ptw_error_vs_mode
                           << ":g_mode=" << ptw_error_g_mode
                           << ":outcome=" << ptw_error_outcome
                           << ":target=0x" << std::hex << *target_pte
                           << std::dec;
                    phase = detail.str();
                }
                const std::string ptw_error_phase = phase;

                const bool denied = ptw_error_outcome == 0;
                const auto corrupt_beat = denied
                    ? memblock::PtwCorruptBeat::all
                    : ptw_error_outcome == 1
                    ? memblock::PtwCorruptBeat::first
                    : memblock::PtwCorruptBeat::last;
                const std::uint64_t error_responses_before =
                    environment.ptw_error_response_requests();
                const std::uint64_t denied_beats_before =
                    environment.ptw_denied_d_beats();
                const std::uint64_t corrupt_beats_before =
                    environment.ptw_corrupt_d_beats();
                const std::uint64_t uncache_before =
                    environment.uncache_requests();
                const std::uint64_t bus_data_before =
                    environment.bus_expected_load(
                        address, memblock::LoadOp::ld);
                const std::uint64_t lq_dequeued_before =
                    environment.lq_dequeued();
                const std::uint64_t sq_dequeued_before =
                    environment.sq_dequeued();
                const std::uint64_t lq_cancels_before =
                    environment.lq_redirect_canceled_observed();
                const std::uint64_t sq_cancels_before =
                    environment.sq_redirect_canceled_observed();
                environment.inject_persistent_ptw_response_error_at(
                    *target_pte, denied, !denied, corrupt_beat);

                bool issued = false;
                std::uint8_t fault_rob = 0;
                bool fault_rob_flag = false;
                if (ptw_error_store) {
                    auto transaction = make_store(
                        address, random(), memblock::StoreOp::sd,
                        random() % memblock::kScalarStoreLanes,
                        random() % memblock::kScalarStoreLanes);
                    transaction.expected_exception_mask =
                        memblock::kExceptionStoreAccessFault;
                    fault_rob = transaction.rob;
                    fault_rob_flag = transaction.rob_flag;
                    issued = environment.set_rob_head(
                                 transaction.rob, transaction.rob_flag) &&
                        issue_store(transaction, (random() & 1U) != 0);
                } else {
                    auto transaction = make_load(
                        address, memblock::LoadOp::ld,
                        random() % memblock::kScalarLoadLanes);
                    transaction.expected_exception_mask =
                        memblock::kExceptionLoadAccessFault;
                    fault_rob = transaction.rob;
                    fault_rob_flag = transaction.rob_flag;
                    issued = environment.set_rob_head(
                                 transaction.rob, transaction.rob_flag) &&
                        issue_load(transaction);
                }
                if (!issued) {
                    std::ostringstream detail;
                    detail << ptw_error_phase << ":issue-failed"
                           << ":manager="
                           << environment.ptw_error_response_requests() -
                                  error_responses_before
                           << ','
                           << environment.ptw_denied_d_beats() -
                                  denied_beats_before
                           << ','
                           << environment.ptw_corrupt_d_beats() -
                                  corrupt_beats_before
                           << ":target_requests="
                           << environment.ptw_requests_covering_since(
                                  *target_pte, ptw_requests_before)
                           << ":last=0x" << std::hex
                           << environment.ptw_last_error_response_address()
                           << std::dec;
                    phase = detail.str();
                    environment.clear_ptw_response_error();
                    return false;
                }
                if (!recover_random_precise_error(
                        fault_rob, fault_rob_flag, ptw_error_store,
                        lq_dequeued_before, sq_dequeued_before,
                        lq_cancels_before, sq_cancels_before)) {
                    environment.clear_ptw_response_error();
                    return false;
                }
                environment.clear_ptw_response_error();
                if (!environment.run_until_ptw_idle(
                        constrained_completion_timeout)) {
                    phase = ptw_error_phase + ":ptw-drain";
                    return false;
                }
                phase = ptw_error_phase;

                const std::array<std::uint64_t, 3> manager_delta{{
                    environment.ptw_error_response_requests() -
                        error_responses_before,
                    environment.ptw_denied_d_beats() - denied_beats_before,
                    environment.ptw_corrupt_d_beats() - corrupt_beats_before,
                }};
                const std::uint64_t bus_data_after =
                    environment.bus_expected_load(
                        address, memblock::LoadOp::ld);
                const std::uint64_t target_dcache_requests =
                    environment.dcache_requests_covering_since(
                        address, requests_before);
                const bool manager_matches = manager_delta[0] != 0 &&
                    manager_delta[1] ==
                        (denied ? manager_delta[0] * 2U : 0U) &&
                    manager_delta[2] ==
                        (denied ? manager_delta[0] * 2U : manager_delta[0]);
                if (!manager_matches ||
                    target_dcache_requests != 0 ||
                    environment.uncache_requests() != uncache_before ||
                    (environment.ptw_last_error_response_address() &
                     ~std::uint64_t{63}) !=
                        (*target_pte & ~std::uint64_t{63}) ||
                    bus_data_after != bus_data_before) {
                    std::ostringstream detail;
                    detail << "random-ptw-error-manager"
                           << ":site=" << ptw_error_site
                           << ":direction=" << ptw_error_store
                           << ":level_class=" << ptw_error_level_class
                           << ":target_level=" << ptw_error_target_level
                           << ":outcome=" << ptw_error_outcome
                           << ":manager=" << manager_delta[0] << ','
                           << manager_delta[1] << ',' << manager_delta[2]
                           << ":dcache="
                           << environment.tilelink_requests() - requests_before
                           << ":dcache_target=" << target_dcache_requests
                           << ":uncache="
                           << environment.uncache_requests() - uncache_before
                           << ":last=0x" << std::hex
                           << environment.ptw_last_error_response_address()
                           << ":target=0x" << *target_pte
                           << ":bus_before=0x" << bus_data_before
                           << ":bus_after=0x" << bus_data_after << std::dec;
                    phase = detail.str();
                    return false;
                }

                phase += ":recovery";
                bool fenced = false;
                if (ptw_error_site == RandomConstraints::ptw_error_stage1) {
                    fenced = environment.issue_sfence(
                        0, 0, true, true, false, false);
                } else if (ptw_error_site ==
                           RandomConstraints::ptw_error_gstage) {
                    fenced = environment.issue_sfence(
                        0, 0, true, true, false, true);
                } else {
                    fenced = environment.issue_sfence(
                                  0, 0, true, true, true, false) &&
                        environment.issue_sfence(
                            0, 0, true, true, false, true);
                }
                const std::uint64_t recovery_ptw_before =
                    environment.ptw_requests();
                auto recovery = make_load(
                    address, memblock::LoadOp::ld,
                    random() % memblock::kScalarLoadLanes);
                environment.expect_load(recovery);
                if (!fenced ||
                    !environment.set_rob_head(
                        recovery.rob, recovery.rob_flag) ||
                    !environment.enqueue_load(recovery) ||
                    !environment.issue_load(recovery, 4096) ||
                    !environment.run_until_complete(
                        constrained_completion_timeout) ||
                    !environment.run_until_lq_retired(8192) ||
                    environment.ptw_error_response_requests() !=
                        error_responses_before + manager_delta[0] ||
                    environment.ptw_requests_covering_since(
                        *target_pte, recovery_ptw_before) == 0) {
                    return false;
                }
                const unsigned mode_index =
                    ptw_error_site == RandomConstraints::ptw_error_stage1
                    ? ptw_error_stage1_mode
                    : ptw_error_site == RandomConstraints::ptw_error_gstage
                    ? ptw_error_g_mode
                    : ptw_error_vs_mode * 2 + ptw_error_g_mode;
                constraint_coverage.sample_ptw_error(
                    ptw_error_site, ptw_error_store,
                    ptw_error_level_class, ptw_error_outcome, mode_index,
                    ptw_error_target_level, manager_delta);
                sample_dcache = false;
                ++coverage.cacheable;
                phase = "seeded-mixed-tail";
            } else if (kind == RandomConstraints::vector_load ||
                       kind == RandomConstraints::vector_store) {
                const bool store = kind == RandomConstraints::vector_store;
                const auto &shape = choose_vector_shape(store ? 1U : 0U);
                const unsigned data_eew = shape.addressing >= 2
                    ? shape.vsew : shape.eew;
                const bool vector_corner =
                    random() % 1000 < constraints.vector_corner_per_mille;
                const auto uops = make_random_vector_uops(
                    store, action_cacheable_address(1U << data_eew),
                    shape, vector_corner);
                if (!issue_random_vector_instruction(uops)) {
                    return false;
                }
                ++constraint_coverage.vector_directions[store ? 1U : 0U];
                ++constraint_coverage.vector_addressing[shape.addressing];
                ++constraint_coverage.vector_eews[shape.eew];
                ++constraint_coverage.vector_sews[shape.vsew];
                ++constraint_coverage.vector_lmuls[shape.lmul_log2 + 3];
                ++constraint_coverage.vector_emuls[shape.emul_log2 + 3];
                constraint_coverage.sample_vector_shape(
                    store ? 1U : 0U, shape.addressing, shape.eew, shape.vsew,
                    shape.lmul_log2);
                const auto &policy = uops.front();
                const unsigned vector_bytes = shape.lmul_log2 < 0
                    ? 16U >> static_cast<unsigned>(-shape.lmul_log2)
                    : 16U << static_cast<unsigned>(shape.lmul_log2);
                const unsigned vlmax = vector_bytes >> shape.vsew;
                ++constraint_coverage.vector_masked[policy.vm ? 0U : 1U];
                ++constraint_coverage.vector_vma[policy.vma ? 1U : 0U];
                ++constraint_coverage.vector_vta[policy.vta ? 1U : 0U];
                ++constraint_coverage.vector_partial_vl[
                    policy.vl < vlmax ? 1U : 0U];
                ++constraint_coverage.vector_nonzero_vstart[
                    policy.vstart != 0 ? 1U : 0U];
                constraint_coverage.vector_mask_agnostic += !store && policy.vma &&
                    std::any_of(uops.begin(), uops.end(), [](const auto &uop) {
                        return memblock::vector_mask_inactive_elements(uop) != 0;
                    });
                constraint_coverage.vector_tail_agnostic += !store && policy.vta &&
                    std::any_of(uops.begin(), uops.end(), [](const auto &uop) {
                        return memblock::vector_tail_elements(uop) != 0;
                    });
                ++constraint_coverage.vector_shape_operations;
                constraint_coverage.vector_uops += uops.size();
                constraint_coverage.vector_multi_uop += uops.size() > 1U;
                ++coverage.cacheable;
            } else if (kind == RandomConstraints::vector_segment) {
                if (!environment.run_until_all_complete(8192) ||
                    !environment.run_until_queues_retired(8192)) {
                    return false;
                }
                const auto &shape = choose_vector_segment_shape();
                const auto addressing =
                    static_cast<memblock::VectorAddressingMode>(
                        shape.addressing);
                const bool indexed = shape.addressing >= 2;
                const unsigned fields = shape.nf_index + 2U;
                const unsigned lmul_uops = 1U << static_cast<unsigned>(
                    std::max(shape.lmul_log2, 0));
                const unsigned emul_uops = 1U << static_cast<unsigned>(
                    std::max(shape.emul_log2, 0));
                const unsigned data_group_uops = indexed
                    ? lmul_uops : emul_uops;
                const unsigned data_uops = data_group_uops * fields;
                const unsigned uop_count = indexed
                    ? std::max(data_uops, emul_uops) : data_uops;
                const unsigned data_eew = indexed ? shape.vsew : shape.eew;
                const unsigned data_bytes = 1U << data_eew;
                const unsigned vector_bytes = shape.lmul_log2 < 0
                    ? 16U >> static_cast<unsigned>(-shape.lmul_log2)
                    : 16U << static_cast<unsigned>(shape.lmul_log2);
                const unsigned vlmax = vector_bytes >> shape.vsew;
                const bool vector_corner =
                    random() % 1000 < constraints.vector_corner_per_mille;
                std::int64_t stride = 0;
                if (addressing == memblock::VectorAddressingMode::strided) {
                    const std::int64_t magnitude =
                        static_cast<std::int64_t>(
                            (fields + 1U) * data_bytes *
                            (1U + random() % 4U));
                    stride = (random() & 1U) == 0 ? magnitude : -magnitude;
                }
                std::uint64_t address = action_cacheable_address(data_bytes);
                if (stride < 0) {
                    address += static_cast<std::uint64_t>(-stride) *
                        (vlmax - 1U);
                }
                const std::uint64_t rob = rob_offset++;
                const std::uint8_t vl = static_cast<std::uint8_t>(
                    vector_corner ? 1U + random() % vlmax : vlmax);
                const bool vm = !vector_corner || (random() & 1U) != 0;
                const std::uint16_t mask_bits =
                    static_cast<std::uint16_t>(random());
                const std::uint8_t vstart = vector_corner
                    ? static_cast<std::uint8_t>(random() % (vl + 1U))
                    : 0;
                const std::uint8_t first_pdest = static_cast<std::uint8_t>(
                    1 + random() % (256 - uop_count));
                const std::uint8_t vlmul = static_cast<std::uint8_t>(
                    shape.lmul_log2 < 0
                        ? shape.lmul_log2 + 8 : shape.lmul_log2);
                const unsigned bytes_per_data_uop = data_group_uops == 1 &&
                        (indexed ? shape.lmul_log2 : shape.emul_log2) < 0
                    ? 16U >> static_cast<unsigned>(
                        -(indexed ? shape.lmul_log2 : shape.emul_log2))
                    : 16U;
                const unsigned flow_num = bytes_per_data_uop >> data_eew;
                std::array<unsigned char, 128> index_group{};
                if (indexed) {
                    const unsigned index_bytes = 1U << shape.eew;
                    for (unsigned element = 0; element < vlmax; ++element) {
                        const std::uint64_t offset =
                            static_cast<std::uint64_t>(element) * data_bytes *
                            (fields + 1U);
                        for (unsigned byte = 0; byte < index_bytes; ++byte) {
                            index_group[element * index_bytes + byte] =
                                static_cast<unsigned char>(offset >> (8 * byte));
                        }
                    }
                }
                std::vector<memblock::VectorMemoryTransaction> uops;
                uops.reserve(uop_count);
                for (unsigned uop = 0; uop < uop_count; ++uop) {
                    uops.push_back(memblock::VectorMemoryTransaction{
                        .store = vector_segment_store,
                        .segment = true,
                        .address = address,
                        .stride = stride,
                        .addressing = addressing,
                        .eew = static_cast<std::uint8_t>(shape.eew),
                        .vsew = static_cast<std::uint8_t>(shape.vsew),
                        .vl = vl,
                        .rob = memblock::rob_pointer_value(rob),
                        .rob_flag = memblock::rob_pointer_flag(rob),
                        .lq = memblock::lq_pointer_value(lq_offset),
                        .lq_flag = memblock::lq_pointer_flag(lq_offset),
                        .sq = memblock::sq_pointer_value(sq_offset),
                        .sq_flag = memblock::sq_pointer_flag(sq_offset),
                        .pdest = vector_segment_store
                            ? std::uint8_t{0}
                            : static_cast<std::uint8_t>(first_pdest + uop),
                        .lane = 0,
                        .flow_num = static_cast<std::uint8_t>(flow_num),
                        .vlmul = vlmul,
                        .vuop_idx = static_cast<std::uint8_t>(uop),
                        .last_uop = uop + 1 == uop_count,
                        .nf = static_cast<std::uint8_t>(fields - 1U),
                        .vec_wen = !vector_segment_store && uop < data_uops,
                    });
                    auto &segment_uop = uops.back();
                    segment_uop.oracle_index_group = indexed
                        ? std::optional{index_group} : std::nullopt;
                    segment_uop.vm = vm;
                    segment_uop.mask_bits = mask_bits;
                    segment_uop.vstart = vstart;
                    segment_uop.ftq_ptr = random() & 0x3fU;
                    segment_uop.ftq_offset =
                        static_cast<std::uint8_t>(random() & 7U);
                    for (auto &byte : segment_uop.data) {
                        byte = static_cast<unsigned char>(random());
                    }
                    if (indexed && uop < emul_uops) {
                        const unsigned index_group_bytes = shape.emul_log2 < 0
                            ? 16U >> static_cast<unsigned>(-shape.emul_log2)
                            : 16U;
                        const unsigned source_offset = shape.emul_log2 < 0
                            ? 0 : uop * 16U;
                        std::copy_n(
                            index_group.begin() + source_offset,
                            index_group_bytes, segment_uop.index.begin());
                    }
                    if (!vector_segment_store && uop >= data_uops) {
                        environment.expect_vector_data(
                            segment_uop, segment_uop.data);
                    } else {
                        environment.expect_vector(segment_uop);
                    }
                    if (!environment.issue_vector(segment_uop, 4096)) {
                        return false;
                    }
                }
                if (!environment.run_until_vector_complete(
                        constrained_completion_timeout)) {
                    return false;
                }
                for (unsigned uop = 0; uop < data_uops; ++uop) {
                    coverage.sample(uops[uop]);
                }
                if (vector_segment_store) {
                    if (!environment.pulse_sbuffer_flush() ||
                        !environment.run_until_sbuffer_empty(
                            constrained_completion_timeout)) {
                        return false;
                    }
                    for (unsigned uop = 0; uop < data_uops; ++uop) {
                        environment.record_committed_vector_store(uops[uop]);
                    }
                }
                ++actions;
                ++constraint_coverage.vector_segment_directions[
                    vector_segment_store ? 1 : 0];
                ++constraint_coverage.vector_segment_addressing[
                    shape.addressing];
                ++constraint_coverage.vector_segment_eews[shape.eew];
                ++constraint_coverage.vector_segment_sews[shape.vsew];
                ++constraint_coverage.vector_segment_lmuls[
                    shape.lmul_log2 + 3];
                ++constraint_coverage.vector_segment_emuls[
                    shape.emul_log2 + 3];
                ++constraint_coverage.vector_segment_nfs[shape.nf_index];
                ++coverage.cacheable;
            } else if (kind == RandomConstraints::prefetch) {
                const auto transaction = make_prefetch(
                    action_cacheable_address(8),
                    static_cast<memblock::PrefetchOp>(8 + random() % 3),
                    random() % 3);
                environment.expect_prefetch(transaction);
                if (!environment.enqueue_prefetch(transaction) ||
                    !environment.issue_prefetch(transaction, 4096) ||
                    !environment.run_until_all_complete(8192)) {
                    return false;
                }
                coverage.sample(transaction);
                ++actions;
                ++coverage.cacheable;
            } else if (kind == RandomConstraints::atomic) {
                ordinary_leaf_addressed = true;
                const std::size_t slot = random() % atomic_values.size();
                const bool word = atomic_width == 0;
                const std::uint64_t operand = random();
                const bool error_response = atomic_error_denied.has_value();
                const bool probe_composed =
                    !error_response && atomic_probe_depth != 0;
                std::uint64_t address = atomic_base + slot * 64;
                if (error_response) {
                    constexpr std::uint64_t error_lines =
                        (atomic_error_span / 2) / 64;
                    if (atomic_error_line >= error_lines) {
                        phase = "random-atomic-error-address-space-exhausted";
                        return false;
                    }
                    const std::uint64_t sequence = atomic_error_line++;
                    const std::uint64_t line = sequence ^ (sequence >> 1);
                    address = atomic_error_base + line * 64 + (word ? 4 : 0);
                } else if (probe_composed) {
                    constexpr std::uint64_t target_lines =
                        atomic_probe_target_span / 64;
                    std::optional<std::uint64_t> fresh_target_line;
                    for (std::uint64_t attempt = 0;
                         attempt < target_lines && !fresh_target_line;
                         ++attempt) {
                        const std::uint64_t sequence =
                            atomic_probe_target_line++;
                        const std::uint64_t line =
                            (sequence ^ (sequence >> 1)) % target_lines;
                        const std::uint64_t candidate =
                            atomic_probe_target_base + line * 64;
                        if (environment.dcache_requests_covering_since(
                                candidate, 0) == 0) {
                            fresh_target_line = candidate;
                        }
                    }
                    if (!fresh_target_line) {
                        phase = "random-atomic-probe-address-space-exhausted";
                        return false;
                    }
                    address = *fresh_target_line;
                }
                const memblock::LoadOp read_op = word
                    ? memblock::LoadOp::lwu : memblock::LoadOp::ld;
                std::uint64_t atomic_value = error_response
                    ? environment.memory().expected_load(address, read_op)
                    : probe_composed
                    ? environment.memory().expected_load(
                          address, memblock::LoadOp::ld)
                    : atomic_values[slot];
                const std::uint64_t original_value = word
                    ? static_cast<std::uint32_t>(atomic_value) : atomic_value;
                const auto expect_atomic_writeback = [&] (
                    const auto &transaction, std::uint64_t value,
                    std::uint32_t exception = 0) {
                    const memblock::LoadTransaction writeback{
                        .address = transaction.address,
                        .op = read_op,
                        .rob = transaction.rob,
                        .rob_flag = transaction.rob_flag,
                        .pdest = transaction.pdest,
                        .lane = 0,
                        .expected_exception_mask = exception,
                        .rf_wen = exception == 0,
                    };
                    environment.expect_load_data(writeback, value);
                };
                memblock::AtomicTransaction transaction;
                if (atomic_family == RandomConstraints::atomic_amo) {
                    const auto &operations =
                        word ? atomic_operations_w : atomic_operations_d;
                    const auto op = operations[random() % operations.size()];
                    transaction = make_atomic(address, op, operand);
                } else if (atomic_family == RandomConstraints::atomic_cas) {
                    transaction = make_atomic(
                        address,
                        word ? memblock::AtomicOp::amocas_w
                             : memblock::AtomicOp::amocas_d,
                        operand);
                    const bool compare_match = (random() & 1U) != 0;
                    transaction.compare = compare_match
                        ? original_value : original_value ^ 1U;
                } else {
                    transaction = make_atomic(
                        address,
                        word ? memblock::AtomicOp::lr_w
                             : memblock::AtomicOp::lr_d,
                        0);
                }

                std::vector<std::uint64_t> atomic_probe_lines;
                if (probe_composed) {
                    phase = "random-atomic-probe-drain";
                    if (!environment.run_until_all_complete(
                            constrained_completion_timeout) ||
                        !environment.run_until_queues_retired(
                            constrained_completion_timeout) ||
                        !environment.run_until_sbuffer_empty(
                            constrained_completion_timeout) ||
                        !environment.run_until_dcache_idle(
                            constrained_completion_timeout)) {
                        return false;
                    }
                    constexpr std::uint64_t group_bytes = 0x800;
                    constexpr std::uint64_t groups =
                        atomic_auxiliary_probe_span / group_bytes;
                    const std::uint64_t group =
                        atomic_auxiliary_probe_group++ % groups;
                    const std::uint64_t group_base =
                        atomic_auxiliary_probe_base + group * group_bytes;
                    const unsigned target_set =
                        static_cast<unsigned>((address >> 6) & 0x7fU);
                    for (unsigned candidate = 0;
                         atomic_probe_lines.size() < atomic_probe_depth;
                         ++candidate) {
                        const std::uint64_t auxiliary_line =
                            group_base + candidate * 64U;
                        if (((auxiliary_line >> 6) & 0x7fU) == target_set) {
                            continue;
                        }
                        atomic_probe_lines.push_back(auxiliary_line);
                    }
                    for (const std::uint64_t auxiliary_line :
                         atomic_probe_lines) {
                        const auto auxiliary_load = make_load(
                            auxiliary_line + 8U * (random() % 8U),
                            memblock::LoadOp::ld,
                            random() % memblock::kScalarLoadLanes);
                        environment.expect_load(auxiliary_load);
                        phase = "random-atomic-probe-warm";
                        if (!environment.set_rob_head(
                                auxiliary_load.rob, auxiliary_load.rob_flag) ||
                            !environment.enqueue_load(auxiliary_load) ||
                            !environment.issue_load(auxiliary_load, 4096) ||
                            !environment.run_until_complete(
                                constrained_completion_timeout) ||
                            !environment.run_until_lq_retired(8192)) {
                            return false;
                        }
                    }
                }

                const auto issue_atomic_success = [&] (
                    const memblock::AtomicTransaction &current) {
                    if (!probe_composed) {
                        return environment.issue_atomic(current, 4096) &&
                            environment.run_until_complete(
                                constrained_completion_timeout);
                    }
                    const std::uint64_t target_line =
                        address & ~std::uint64_t{63};
                    const std::uint64_t target_requests_before =
                        environment.tilelink_requests();
                    const std::uint64_t probes_before =
                        environment.dcache_probes();
                    const std::uint64_t probe_responses_before =
                        environment.dcache_probe_responses();
                    const std::uint64_t probe_data_before =
                        environment.dcache_probe_data();
                    const std::uint64_t writebacks_before =
                        environment.writebacks();
                    environment.hold_dcache_response_at(target_line);
                    phase = "random-atomic-probe-issue";
                    if (!environment.issue_atomic(current, 4096)) {
                        phase += ":issue";
                        return false;
                    }
                    for (unsigned cycle = 0;
                         cycle < constrained_completion_timeout &&
                         !environment.dcache_response_held_at(target_line);
                         ++cycle) {
                        if (!environment.run_cycles(1)) {
                            phase += ":wait-held";
                            return false;
                        }
                    }
                    if (!environment.dcache_response_held_at(target_line)) {
                        phase += ":held-timeout";
                        return false;
                    }
                    if (environment.dcache_requests_covering_since(
                            target_line, target_requests_before) != 1) {
                        phase += ":target-request-count";
                        return false;
                    }
                    if (environment.writebacks() != writebacks_before) {
                        phase += ":early-writeback";
                        return false;
                    }
                    if (!environment.hold_dcache_c_until_probes(
                            probes_before + atomic_probe_depth)) {
                        phase += ":hold-c";
                        return false;
                    }
                    for (const std::uint64_t auxiliary_line :
                         atomic_probe_lines) {
                        if (!environment.request_dcache_probe(
                                auxiliary_line, 2, false, 2)) {
                            phase += ":probe-request";
                            return false;
                        }
                    }
                    if (environment.dcache_probe_data() != probe_data_before) {
                        phase += ":early-probe-data";
                        return false;
                    }
                    if (environment.writebacks() != writebacks_before) {
                        phase += ":writeback-before-d";
                        return false;
                    }
                    if (!environment.dcache_response_held_at(target_line)) {
                        phase += ":lost-held-d";
                        return false;
                    }
                    if (!environment.release_held_dcache_response_at(
                            target_line)) {
                        phase += ":release-d";
                        return false;
                    }
                    if (!environment.run_until_probe_responses(
                            probe_responses_before + atomic_probe_depth,
                            constrained_completion_timeout)) {
                        phase += ":probe-responses";
                        return false;
                    }
                    if (environment.dcache_probes() !=
                            probes_before + atomic_probe_depth) {
                        phase += ":probe-count";
                        return false;
                    }
                    if (environment.dcache_probe_data() != probe_data_before) {
                        phase += ":probe-data";
                        return false;
                    }
                    if (environment.dcache_max_probe_outstanding() <
                            atomic_probe_depth) {
                        phase += ":probe-depth";
                        return false;
                    }
                    if (!environment.run_until_complete(
                            constrained_completion_timeout)) {
                        phase += ":atomic-completion";
                        return false;
                    }
                    if (environment.dcache_response_held_at(target_line)) {
                        phase += ":held-d-not-drained";
                        return false;
                    }
                    return true;
                };

                std::array<std::uint64_t, 5> manager_delta{};
                if (error_response) {
                    phase = *atomic_error_denied
                        ? "random-atomic-denied"
                        : "random-atomic-corrupt";
                    if (!environment.run_until_all_complete(
                            constrained_completion_timeout) ||
                        !environment.run_until_queues_retired(
                            constrained_completion_timeout) ||
                        !environment.run_until_dcache_idle(
                            constrained_completion_timeout)) {
                        return false;
                    }
                    const std::uint32_t expected_exception =
                        !*atomic_error_denied
                        ? memblock::kExceptionHardwareError
                        : atomic_family == RandomConstraints::atomic_lrsc
                        ? memblock::kExceptionLoadAccessFault
                        : memblock::kExceptionStoreAccessFault;
                    expect_atomic_writeback(
                        transaction, 0, expected_exception);
                    const std::uint64_t requests_before_error =
                        environment.tilelink_requests();
                    const std::uint64_t refills_before =
                        environment.dcache_error_response_refills();
                    const std::uint64_t error_responses_before =
                        environment.dcache_error_response_requests();
                    const std::uint64_t denied_beats_before =
                        environment.dcache_denied_d_beats();
                    const std::uint64_t corrupt_beats_before =
                        environment.dcache_corrupt_d_beats();
                    const std::uint64_t grant_acks_before =
                        environment.dcache_error_response_grant_acks();
                    const std::uint64_t bus_data_before =
                        environment.bus_expected_load(address, read_op);
                    environment.inject_dcache_response_error_at(
                        address & ~std::uint64_t{63},
                        *atomic_error_denied, !*atomic_error_denied);
                    if (!environment.set_rob_head(
                            transaction.rob, transaction.rob_flag) ||
                        !environment.issue_atomic(transaction, 4096) ||
                        !environment.run_until_complete(
                            constrained_completion_timeout) ||
                        !environment.run_until_dcache_idle(
                            constrained_completion_timeout)) {
                        return false;
                    }
                    manager_delta = {{
                        environment.dcache_error_response_requests() -
                            error_responses_before,
                        environment.dcache_denied_d_beats() -
                            denied_beats_before,
                        environment.dcache_corrupt_d_beats() -
                            corrupt_beats_before,
                        environment.dcache_error_response_grant_acks() -
                            grant_acks_before,
                        environment.dcache_error_response_refills() -
                            refills_before,
                    }};
                    if (environment.tilelink_requests() <
                            requests_before_error + 1 ||
                        manager_delta != std::array<std::uint64_t, 5>{
                            1, *atomic_error_denied ? 2U : 0U,
                            2, 1, 1} ||
                        (environment.dcache_last_error_response_address() &
                         ~std::uint64_t{63}) !=
                            (address & ~std::uint64_t{63}) ||
                        environment.dcache_last_error_response_keyword() ||
                        environment.bus_expected_load(address, read_op) !=
                            bus_data_before) {
                        phase += ":oracle";
                        return false;
                    }
                    // The design intentionally installs an errored refill as a
                    // poisoned line. AMO/CAS still update that private cache
                    // image, while the manager backing store stays unchanged
                    // until a possible dirty eviction. Track the expected
                    // ReleaseData without treating exceptional data as an ISA
                    // result.
                    bool cache_image_updated = false;
                    std::uint64_t cache_image_value = original_value;
                    if (atomic_family == RandomConstraints::atomic_amo) {
                        cache_image_value = atomic_result(
                            transaction.op, original_value, operand);
                        cache_image_updated = true;
                    } else if (
                        atomic_family == RandomConstraints::atomic_cas &&
                        transaction.compare == original_value) {
                        cache_image_value = operand;
                        cache_image_updated = true;
                    }
                    if (cache_image_updated) {
                        const unsigned bytes = word ? 4U : 8U;
                        for (unsigned byte = 0; byte < bytes; ++byte) {
                            environment.memory().write_reference_byte(
                                address + byte,
                                static_cast<std::uint8_t>(
                                    cache_image_value >> (8 * byte)));
                        }
                    }
                    ++coverage.exceptions;
                } else if (atomic_family == RandomConstraints::atomic_amo) {
                    expect_atomic_writeback(
                        transaction,
                        atomic_writeback(atomic_value, word));
                    if (!issue_atomic_success(transaction)) {
                        return false;
                    }
                    atomic_value = atomic_result(
                        transaction.op, atomic_value, operand);
                } else if (atomic_family == RandomConstraints::atomic_cas) {
                    const std::uint64_t old_value = atomic_value;
                    const std::uint64_t compare_value = word
                        ? static_cast<std::uint32_t>(old_value) : old_value;
                    expect_atomic_writeback(
                        transaction, atomic_writeback(old_value, word));
                    if (!issue_atomic_success(transaction)) {
                        return false;
                    }
                    if (transaction.compare == compare_value) {
                        atomic_value = word
                            ? ((old_value & ~std::uint64_t{0xffffffff}) |
                               static_cast<std::uint32_t>(operand))
                            : operand;
                    }
                } else {
                    expect_atomic_writeback(
                        transaction,
                        atomic_writeback(atomic_value, word));
                    if (!issue_atomic_success(transaction)) {
                        return false;
                    }
                    const auto sc = make_atomic(
                        address,
                        word ? memblock::AtomicOp::sc_w
                             : memblock::AtomicOp::sc_d,
                        operand);
                    expect_atomic_writeback(sc, 0);
                    if (!environment.issue_atomic(sc, 4096) ||
                        !environment.run_until_complete(
                            constrained_completion_timeout)) {
                        return false;
                    }
                    atomic_value = word
                        ? ((atomic_value & ~std::uint64_t{0xffffffff}) |
                           static_cast<std::uint32_t>(operand))
                        : operand;
                }
                if (!error_response) {
                    if (!probe_composed) {
                        atomic_values[slot] = atomic_value;
                    }
                    environment.record_atomic_result(address, atomic_value);
                    constraint_coverage.sample_atomic_probe(
                        atomic_family, atomic_width, atomic_probe_depth);
                }
                constraint_coverage.sample_atomic(
                    atomic_family, atomic_width, atomic_error_denied,
                    manager_delta);
                ++actions;
                ++coverage.cacheable;
            } else if (kind == RandomConstraints::hypervisor) {
                const unsigned spvp_index = hypervisor_spvp_user ? 1U : 0U;
                const auto spvp = hypervisor_spvp_user
                    ? memblock::ReferencePrivilegeMode::user
                    : memblock::ReferencePrivilegeMode::supervisor;
                const auto pbmt_mapping =
                    hypervisor_pbmt_mappings[hypervisor_pbmt_pair];
                const auto pbmt_value = [](bool nc, bool io) {
                    return nc ? memblock::ReferencePbmt::nc
                              : io ? memblock::ReferencePbmt::io
                                   : memblock::ReferencePbmt::pma;
                };
                const auto final_pbmt = memblock::reference_two_stage_pbmt(
                    pbmt_value(pbmt_mapping.vs_nc, pbmt_mapping.vs_io),
                    pbmt_value(pbmt_mapping.g_nc, pbmt_mapping.g_io));
                const bool final_nc = final_pbmt == memblock::ReferencePbmt::nc;
                const bool final_io = final_pbmt == memblock::ReferencePbmt::io ||
                    (final_pbmt == memblock::ReferencePbmt::pma &&
                     hypervisor_pma_device);
                const bool physical_pmp_active = hypervisor_pmp_relation !=
                    RandomConstraints::hypervisor_pmp_none;
                const bool physical_pmp_denied =
                    hypervisor_pmp_relation >=
                    RandomConstraints::hypervisor_pmp_below;
                if (((final_nc || final_io) && hypervisor_misaligned) ||
                    (hypervisor_pma_device &&
                     hypervisor_pbmt_pair != RandomConstraints::
                         hypervisor_pbmt_pma_pma) ||
                    (physical_pmp_active &&
                     (hypervisor_pma_device ||
                      hypervisor_pbmt_pair != RandomConstraints::
                          hypervisor_pbmt_pma_pma))) {
                    phase += ":hypervisor-address-class";
                    return false;
                }
                if (!environment.run_until_all_complete(8192) ||
                    !environment.run_until_queues_retired(8192) ||
                    !environment.set_hypervisor_access_permissions(
                        spvp, false, false, false,
                        physical_pmp_active
                            ? memblock::ReferencePrivilegeMode::machine
                            : memblock::ReferencePrivilegeMode::supervisor)) {
                    return false;
                }
                const unsigned topology = leaf.nested_index();
                const std::uint64_t pmp_outer_base =
                    hypervisor_pmp_physical_base(spvp_index, topology);
                constexpr std::uint8_t pmp_napot_deny = 0x18;
                constexpr std::uint8_t pmp_napot_rwx = 0x1f;
                if (physical_pmp_active) {
                    const auto napot_address = [](
                                                   std::uint64_t base,
                                                   std::uint64_t size) {
                        return (base | (size / 2 - 1)) >> 2;
                    };
                    if (!environment.configure_pmp(
                            {napot_address(
                                 pmp_outer_base +
                                     hypervisor_pmp_region_offset,
                                 hypervisor_pmp_region_size),
                             napot_address(pmp_outer_base, 0x4000),
                             ~std::uint64_t{0}},
                            {pmp_napot_rwx, pmp_napot_deny,
                             pmp_napot_rwx})) {
                        phase += ":hypervisor-pmp-configuration";
                        return false;
                    }
                }
                const auto hypervisor_address = [&](unsigned size) {
                    leaf_addressed = true;
                    return constrained_hypervisor_address(
                        size, leaf, spvp_index, hypervisor_pbmt_pair,
                        hypervisor_pma_device, hypervisor_misaligned,
                        hypervisor_pmp_relation);
                };
                const auto hypervisor_oracle_address = [&](std::uint64_t address) {
                    const std::uint64_t virtual_base = physical_pmp_active
                        ? hypervisor_pmp_virtual_base(spvp_index, topology)
                        : hypervisor_pma_device
                        ? hypervisor_pma_device_virtual_base(
                              spvp_index, topology)
                        : hypervisor_virtual_base(
                              spvp_index, hypervisor_pbmt_pair, topology);
                    const std::uint64_t physical_base = physical_pmp_active
                        ? pmp_outer_base
                        : hypervisor_pma_device
                        ? hypervisor_pma_device_physical_base(
                              spvp_index, topology)
                        : hypervisor_physical_base(
                              hypervisor_pbmt_pair, topology);
                    return physical_base + (address - virtual_base);
                };
                if (*hypervisor_family == RandomConstraints::hypervisor_hsv) {
                    const auto op = hypervisor_misaligned
                        ? hypervisor_store_operations[
                              1 + random() %
                                  (hypervisor_store_operations.size() - 1)]
                        : hypervisor_store_operations[
                              random() % hypervisor_store_operations.size()];
                    const unsigned size = memblock::scalar_store_bytes(op);
                    const std::uint64_t address = hypervisor_address(size);
                    if (((address & (size - 1U)) != 0) !=
                        hypervisor_misaligned) {
                        phase += ":hypervisor-store-alignment";
                        return false;
                    }
                    const auto reference = memblock::reference_two_stage_access(
                        environment.memory(),
                        random_vs_roots[translation.vs_mode],
                        random_g_roots[translation.g_mode], address,
                        memblock::ReferenceMemoryAccess::store,
                        page_mode(translation.vs_mode),
                        page_mode(translation.g_mode), spvp);
                    if (!reference.translated ||
                        reference.physical_address !=
                            hypervisor_oracle_address(address)) {
                        phase += ":hypervisor-store-reference";
                        return false;
                    }
                    auto transaction = make_store(
                        address,
                        random(), op, random() % 2, random() % 2);
                    transaction.oracle_address = reference.physical_address;
                    transaction.expected_debug_is_mmio = hypervisor_pma_device;
                    transaction.expected_debug_is_ncio = false;
                    transaction.expected_exception_mask = physical_pmp_denied
                        ? memblock::kExceptionStoreAccessFault : 0U;
                    const bool data_first = (random() & 1U) != 0;
                    const std::uint64_t dcache_before =
                        environment.tilelink_requests();
                    const std::uint64_t uncache_before =
                        environment.uncache_requests();
                    bool completed_store = false;
                    if (physical_pmp_denied) {
                        const std::uint64_t lq_dequeued_before =
                            environment.lq_dequeued();
                        const std::uint64_t sq_dequeued_before =
                            environment.sq_dequeued();
                        const std::uint64_t lq_cancels_before =
                            environment.lq_redirect_canceled_observed();
                        const std::uint64_t sq_cancels_before =
                            environment.sq_redirect_canceled_observed();
                        completed_store = environment.set_rob_head(
                                              transaction.rob,
                                              transaction.rob_flag) &&
                            issue_store(
                                transaction, data_first,
                                hypervisor_misaligned) &&
                            recover_random_precise_error(
                                transaction.rob, transaction.rob_flag, true,
                                lq_dequeued_before, sq_dequeued_before,
                                lq_cancels_before, sq_cancels_before);
                    } else if (final_io) {
                        environment.expect_store(transaction);
                        completed_store = environment.set_rob_head(
                                              transaction.rob,
                                              transaction.rob_flag) &&
                            environment.enqueue_store(
                                transaction,
                                memblock::lq_pointer_value(lq_offset)) &&
                            (data_first
                                 ? environment.issue_store_data(
                                       transaction, 4096) &&
                                       environment.
                                           issue_store_address_until_tlb_hit(
                                               transaction,
                                               constrained_completion_timeout)
                                 : environment.issue_store_address_until_tlb_hit(
                                       transaction,
                                       constrained_completion_timeout) &&
                                       environment.issue_store_data(
                                           transaction, 4096)) &&
                            environment.run_cycles(64) &&
                            (environment.uncache_requests() != uncache_before ||
                             environment.wait_for_mmio_store_request(
                                 transaction.rob, transaction.rob_flag, 8192)) &&
                            environment.run_until_store_complete(
                                constrained_completion_timeout) &&
                            environment.commit_stores_through(transaction, 1) &&
                            environment.run_cycles(16);
                        if (completed_store) {
                            environment.record_committed_store(transaction);
                            coverage.sample(transaction, data_first);
                            ++actions;
                        }
                    } else {
                        completed_store =
                            issue_store(transaction, data_first) &&
                            environment.commit_store(transaction, 8192);
                    }
                    const std::uint64_t expected_uncache =
                        uncache_before +
                        (!physical_pmp_denied && (final_nc || final_io)
                             ? 1U : 0U);
                    if (!completed_store ||
                        !environment.run_until_queues_retired(8192) ||
                        environment.uncache_requests() != expected_uncache ||
                        ((physical_pmp_denied || final_nc || final_io) &&
                         environment.tilelink_requests() != dcache_before)) {
                        std::ostringstream detail;
                        detail << phase << ":hypervisor-store-manager"
                               << ":pbmt=" << hypervisor_pbmt_pair
                               << ":dcache=" << dcache_before << '/'
                               << environment.tilelink_requests()
                               << ":last_dcache=0x" << std::hex
                               << environment.dcache_last_request_address()
                               << std::dec
                               << ":uncache=" << uncache_before << '/'
                               << environment.uncache_requests();
                        phase = detail.str();
                        return false;
                    }
                    if (physical_pmp_active) {
                        const auto readback_op = static_cast<memblock::LoadOp>(
                            static_cast<unsigned>(op));
                        const std::uint64_t readback_rob = rob_offset++;
                        const std::uint64_t readback_lq = lq_offset++;
                        auto readback = memblock::LoadTransaction{
                            .address = address,
                            .op = readback_op,
                            .rob = memblock::rob_pointer_value(readback_rob),
                            .rob_flag = memblock::rob_pointer_flag(readback_rob),
                            .lq = memblock::lq_pointer_value(readback_lq),
                            .lq_flag = memblock::lq_pointer_flag(readback_lq),
                            .sq = memblock::sq_pointer_value(sq_offset),
                            .sq_flag = memblock::sq_pointer_flag(sq_offset),
                            .pdest = static_cast<std::uint8_t>(
                                1U + readback_rob % 255U),
                            .lane = static_cast<std::uint8_t>(
                                readback_rob % memblock::kScalarLoadLanes),
                        };
                        readback.oracle_address = reference.physical_address;
                        readback.expected_debug_is_mmio = false;
                        readback.expected_debug_is_ncio = false;
                        readback.expected_debug_is_perf_cnt = false;
                        if (!environment.configure_pmp(
                                {~std::uint64_t{0}}, {pmp_napot_rwx}) ||
                            !issue_load(readback, std::nullopt, false) ||
                            !environment.run_until_queues_retired(8192)) {
                            phase += ":hypervisor-pmp-store-readback";
                            return false;
                        }
                    }
                    if (!physical_pmp_denied && (final_nc || final_io)) {
                        const std::array<memblock::LoadOp, 4> read_ops{{
                            memblock::LoadOp::lbu, memblock::LoadOp::lhu,
                            memblock::LoadOp::lwu, memblock::LoadOp::ld,
                        }};
                        const std::uint64_t mask = size == 8
                            ? ~std::uint64_t{0}
                            : (std::uint64_t{1} << (size * 8)) - 1;
                        if (environment.bus_expected_load(
                                reference.physical_address,
                                read_ops[static_cast<unsigned>(op) & 3U]) !=
                            (transaction.data & mask)) {
                            phase += ":hypervisor-store-physical-image";
                            return false;
                        }
                    }
                } else {
                    memblock::LoadOp op{};
                    if (*hypervisor_family ==
                        RandomConstraints::hypervisor_hlvx) {
                        op = hypervisor_execute_operations[
                            random() % hypervisor_execute_operations.size()];
                    } else if (hypervisor_misaligned) {
                        op = hypervisor_misaligned_load_operations[
                            random() %
                                hypervisor_misaligned_load_operations.size()];
                    } else {
                        op = hypervisor_load_operations[
                            random() % hypervisor_load_operations.size()];
                    }
                    const unsigned size =
                        1U << (static_cast<unsigned>(op) & 3U);
                    const std::uint64_t address = hypervisor_address(size);
                    if (((address & (size - 1U)) != 0) !=
                        hypervisor_misaligned) {
                        phase += ":hypervisor-load-alignment";
                        return false;
                    }
                    const auto reference = memblock::reference_two_stage_access(
                        environment.memory(),
                        random_vs_roots[translation.vs_mode],
                        random_g_roots[translation.g_mode], address,
                        memblock::ReferenceMemoryAccess::load,
                        page_mode(translation.vs_mode),
                        page_mode(translation.g_mode),
                        spvp, false, false,
                        *hypervisor_family ==
                            RandomConstraints::hypervisor_hlvx);
                    if (!reference.translated ||
                        reference.physical_address !=
                            hypervisor_oracle_address(address)) {
                        std::ostringstream detail;
                        detail << phase << ":hypervisor-reference"
                               << ":family=" << *hypervisor_family
                               << ":op=0x" << std::hex
                               << static_cast<unsigned>(op)
                               << ":address=0x" << address
                               << ":pa=0x" << reference.physical_address
                               << std::dec
                               << ":stage1_pf="
                               << reference.stage1_page_fault
                               << ":gpf=" << reference.guest_page_fault
                               << ":af=" << reference.access_fault;
                        phase = detail.str();
                        return false;
                    }
                    {
                        std::ostringstream detail;
                        detail << phase << ":family=" << *hypervisor_family
                               << ":op=0x" << std::hex
                               << static_cast<unsigned>(op)
                               << ":address=0x" << address << std::dec;
                        phase = detail.str();
                    }
                    auto transaction = make_load(
                        address, op, random() % 3);
                    transaction.oracle_address = reference.physical_address;
                    const bool physical_pma_execute_denied =
                        hypervisor_pma_device &&
                        *hypervisor_family ==
                            RandomConstraints::hypervisor_hlvx;
                    transaction.expected_exception_mask =
                        physical_pma_execute_denied || physical_pmp_denied
                        ? memblock::kExceptionLoadAccessFault : 0U;
                    transaction.expected_debug_is_mmio =
                        physical_pma_execute_denied || physical_pmp_denied
                        ? std::nullopt : std::optional<bool>{final_io};
                    transaction.expected_debug_is_ncio =
                        final_io && !physical_pma_execute_denied
                        ? std::optional<bool>{false} : std::nullopt;
                    transaction.expected_debug_is_perf_cnt = false;
                    const std::uint64_t dcache_before =
                        environment.tilelink_requests();
                    const std::uint64_t uncache_before =
                        environment.uncache_requests();
                    const auto feedback_before =
                        environment.scalar_load_feedback_stats();
                    bool completed_load = false;
                    if (physical_pmp_denied) {
                        const std::uint64_t lq_dequeued_before =
                            environment.lq_dequeued();
                        const std::uint64_t sq_dequeued_before =
                            environment.sq_dequeued();
                        const std::uint64_t lq_cancels_before =
                            environment.lq_redirect_canceled_observed();
                        const std::uint64_t sq_cancels_before =
                            environment.sq_redirect_canceled_observed();
                        completed_load = environment.set_rob_head(
                                             transaction.rob,
                                             transaction.rob_flag) &&
                            issue_load(transaction) &&
                            recover_random_precise_error(
                                transaction.rob, transaction.rob_flag, false,
                                lq_dequeued_before, sq_dequeued_before,
                                lq_cancels_before, sq_cancels_before);
                    } else if (physical_pma_execute_denied) {
                        completed_load = issue_load(transaction) &&
                            environment.run_until_queues_retired(8192);
                    } else if (final_io) {
                        environment.expect_load(transaction);
                        completed_load = environment.set_rob_head(
                                             transaction.rob,
                                             transaction.rob_flag) &&
                            environment.enqueue_load(transaction) &&
                            environment.issue_load(transaction, 4096) &&
                            environment.wait_for_mmio_request(
                                transaction.rob, transaction.rob_flag, 8192) &&
                            environment.run_until_complete(
                                constrained_completion_timeout) &&
                            environment.run_until_lq_retired(8192);
                        if (completed_load) {
                            coverage.sample(transaction);
                            ++actions;
                        }
                    } else {
                        completed_load = issue_load(transaction) &&
                            environment.run_until_queues_retired(8192);
                    }
                    const std::uint64_t expected_uncache =
                        uncache_before +
                        (!physical_pma_execute_denied && !physical_pmp_denied &&
                             (final_nc || final_io)
                             ? 1U : 0U);
                    const std::uint64_t dcache_after =
                        environment.tilelink_requests();
                    const std::uint64_t dcache_delta =
                        dcache_after - dcache_before;
                    const bool pmp_prefix_request_valid =
                        physical_pmp_denied &&
                        hypervisor_pmp_relation == RandomConstraints::
                            hypervisor_pmp_cross_upper &&
                        dcache_delta <= 1 &&
                        (dcache_delta == 0 ||
                         environment.dcache_last_request_address() ==
                             (reference.physical_address &
                              ~std::uint64_t{63}));
                    const bool suppressed_dcache_valid =
                        !(physical_pma_execute_denied || physical_pmp_denied ||
                          final_nc || final_io) ||
                        (physical_pmp_denied
                             ? pmp_prefix_request_valid || dcache_delta == 0
                             : dcache_delta == 0);
                    if (!completed_load ||
                        environment.uncache_requests() != expected_uncache ||
                        !suppressed_dcache_valid) {
                        std::ostringstream detail;
                        detail << phase << ":hypervisor-load-manager"
                               << ":pbmt=" << hypervisor_pbmt_pair
                               << ":pmp_relation="
                               << hypervisor_pmp_relation
                               << ":dcache=" << dcache_before << '/'
                               << environment.tilelink_requests()
                               << ":last_dcache=0x" << std::hex
                               << environment.dcache_last_request_address()
                               << std::dec
                               << ":uncache=" << uncache_before << '/'
                               << environment.uncache_requests();
                        phase = detail.str();
                        return false;
                    }
                    if (physical_pma_execute_denied || physical_pmp_denied) {
                        const auto feedback_after =
                            environment.scalar_load_feedback_stats();
                        std::uint64_t wakeups = 0;
                        std::uint64_t cancels = 0;
                        bool all_wakeups_canceled = true;
                        for (unsigned lane = 0;
                             lane < memblock::kScalarLoadLanes; ++lane) {
                            const std::uint64_t lane_wakeups =
                                feedback_after.wakeups[lane] -
                                feedback_before.wakeups[lane];
                            const std::uint64_t lane_cancels =
                                feedback_after.ld2_cancels[lane] -
                                feedback_before.ld2_cancels[lane];
                            wakeups += lane_wakeups;
                            cancels += lane_cancels;
                            all_wakeups_canceled &= lane_wakeups == 0 ||
                                (lane_cancels != 0 &&
                                 feedback_after.last_cancel_cycle[lane] >=
                                     feedback_after.last_wakeup[lane].cycle);
                        }
                        if (cancels == 0 || !all_wakeups_canceled) {
                            std::ostringstream detail;
                            detail << phase
                                   << ":hypervisor-fault-wakeup"
                                   << ":pmp_relation="
                                   << hypervisor_pmp_relation
                                   << ":wakeups=" << wakeups
                                   << ":cancels=" << cancels;
                            phase = detail.str();
                            return false;
                        }
                    }
                }
                if (!activate_translation_context(translation)) {
                    return false;
                }
                ++constraint_coverage.hypervisor_families[*hypervisor_family];
                ++constraint_coverage.hypervisor_spvp[spvp_index];
                ++constraint_coverage.hypervisor_crosses[
                    *hypervisor_family][spvp_index];
                const unsigned alignment_index =
                    hypervisor_misaligned ? 1U : 0U;
                ++constraint_coverage.hypervisor_alignments[alignment_index];
                ++constraint_coverage.hypervisor_alignment_crosses[
                    *hypervisor_family][spvp_index][alignment_index];
                ++constraint_coverage.hypervisor_pbmt_pairs[
                    hypervisor_pbmt_pair];
                ++constraint_coverage.hypervisor_pbmt_crosses[
                    *hypervisor_family][spvp_index][hypervisor_pbmt_pair];
                const unsigned pma_device_index =
                    hypervisor_pma_device ? 1U : 0U;
                ++constraint_coverage.hypervisor_pma_devices[pma_device_index];
                ++constraint_coverage.hypervisor_pma_device_crosses[
                    *hypervisor_family][spvp_index][pma_device_index];
                ++constraint_coverage.hypervisor_pmp_relations[
                    hypervisor_pmp_relation];
                ++constraint_coverage.hypervisor_pmp_relation_crosses[
                    *hypervisor_family][spvp_index][hypervisor_pmp_relation];
                if (final_nc || final_io) {
                    ++coverage.noncacheable;
                } else {
                    ++coverage.cacheable;
                }
            } else if (kind == RandomConstraints::cmo) {
                ordinary_leaf_addressed = true;
                if (!issue_random_cmo(
                        cmo_operation, cmo_dirty, cmo_younger_overlap,
                        cmo_error_denied, cmo_probe_depth_class)) {
                    return false;
                }
                ++constraint_coverage.cmo_operations[cmo_operation];
                ++constraint_coverage.cmo_line_states[cmo_dirty ? 1 : 0];
                ++constraint_coverage.cmo_younger_overlaps[
                    cmo_younger_overlap ? 1 : 0];
                ++constraint_coverage.cmo_errors[
                    cmo_error_denied ? 1 : 0];
                if (cmo_error_denied) {
                    const unsigned error_kind = *cmo_error_denied ? 1U : 0U;
                    ++constraint_coverage.cmo_error_kinds[error_kind];
                    ++constraint_coverage.cmo_operation_errors[
                        cmo_operation][error_kind];
                } else {
                    constraint_coverage.sample_cmo_probe(
                        cmo_operation, cmo_dirty, cmo_probe_depth_class);
                }
                ++actions;
                ++coverage.cacheable;
            } else if (kind == RandomConstraints::noncacheable) {
                ordinary_leaf_addressed = true;
                const std::uint64_t offset = (random() % 128) * 8;
                if (uncache_error_denied &&
                    !environment.run_until_queues_retired(8192)) {
                    return false;
                }
                if (!nc_store) {
                    phase = "random-nc-load";
                    auto transaction = make_load(
                        nc_base + offset, memblock::LoadOp::ld, random() % 3);
                    if (uncache_error_denied) {
                        transaction.expected_exception_mask =
                            *uncache_error_denied
                            ? memblock::kExceptionLoadAccessFault
                            : memblock::kExceptionHardwareError;
                    }
                    const std::uint64_t uncache_before =
                        environment.uncache_requests();
                    const std::uint64_t error_before =
                        environment.uncache_error_response_requests();
                    const std::uint64_t denied_before =
                        environment.uncache_denied_d_beats();
                    const std::uint64_t corrupt_before =
                        environment.uncache_corrupt_d_beats();
                    const std::uint64_t lq_dequeued_before =
                        environment.lq_dequeued();
                    const std::uint64_t sq_dequeued_before =
                        environment.sq_dequeued();
                    const std::uint64_t lq_cancels_before =
                        environment.lq_redirect_canceled_observed();
                    const std::uint64_t sq_cancels_before =
                        environment.sq_redirect_canceled_observed();
                    if (uncache_error_denied) {
                        environment.inject_next_uncache_response_error(
                            *uncache_error_denied,
                            !*uncache_error_denied);
                    }
                    if (!issue_load(transaction) ||
                        environment.uncache_requests() != uncache_before + 1 ||
                        (uncache_error_denied &&
                         (!recover_random_precise_error(
                              transaction.rob, transaction.rob_flag, false,
                              lq_dequeued_before, sq_dequeued_before,
                              lq_cancels_before, sq_cancels_before) ||
                          environment.uncache_error_response_requests() !=
                              error_before + 1 ||
                          environment.uncache_denied_d_beats() !=
                              denied_before +
                                  (*uncache_error_denied ? 1U : 0U) ||
                          environment.uncache_corrupt_d_beats() !=
                              corrupt_before + 1))) {
                        return false;
                    }
                } else {
                    phase = "random-nc-store";
                    auto transaction = make_store(
                        nc_base + offset, random(), memblock::StoreOp::sd,
                        random() % 2, random() % 2);
                    transaction.expected_debug_is_mmio = false;
                    transaction.expected_debug_is_ncio = false;
                    const std::uint64_t uncache_before =
                        environment.uncache_requests();
                    const std::uint64_t error_before =
                        environment.uncache_error_response_requests();
                    const std::uint64_t denied_before =
                        environment.uncache_denied_d_beats();
                    const std::uint64_t corrupt_before =
                        environment.uncache_corrupt_d_beats();
                    const std::uint64_t bus_before =
                        environment.bus_expected_load(
                            transaction.address, memblock::LoadOp::ld);
                    const auto bus_errors_before = environment.bus_error_stats();
                    const std::uint64_t lq_dequeued_before =
                        environment.lq_dequeued();
                    const std::uint64_t sq_dequeued_before =
                        environment.sq_dequeued();
                    const std::uint64_t lq_cancels_before =
                        environment.lq_redirect_canceled_observed();
                    const std::uint64_t sq_cancels_before =
                        environment.sq_redirect_canceled_observed();
                    if (uncache_error_denied) {
                        environment.inject_next_uncache_response_error(true, false);
                    }
                    bool store_completed = false;
                    if (uncache_error_denied) {
                        environment.expect_store(transaction);
                        // NC stores are already committed before the Uncache
                        // request. A denied AccessAck is reported through
                        // uncacheError and must not create a second ROB
                        // writeback; the manager-side memory remains intact.
                        const bool data_first = false;
                        store_completed =
                            environment.set_rob_head(
                                transaction.rob, transaction.rob_flag) &&
                            environment.enqueue_store(
                                transaction,
                                memblock::lq_pointer_value(lq_offset)) &&
                            (data_first
                                ? environment.issue_store_data(
                                      transaction, 4096) &&
                                    environment.
                                        issue_store_address_until_tlb_hit(
                                            transaction,
                                            constrained_completion_timeout)
                                : environment.issue_store_address_until_tlb_hit(
                                      transaction,
                                      constrained_completion_timeout) &&
                                    environment.issue_store_data(
                                        transaction, 4096)) &&
                            environment.run_cycles(64) &&
                            (environment.uncache_requests() != uncache_before ||
                             environment.wait_for_mmio_store_request(
                                 transaction.rob, transaction.rob_flag, 8192)) &&
                            environment.run_until_uncache_requests(
                                uncache_before + 1, 8192) &&
                            environment.run_until_store_complete(
                                constrained_completion_timeout);
                        if (store_completed) {
                            coverage.sample(transaction, data_first);
                            ++actions;
                        }
                    } else {
                        store_completed =
                            issue_store(transaction, (random() & 1U) != 0) &&
                            environment.commit_store(transaction, 8192) &&
                            environment.run_until_queues_retired(8192);
                    }
                    if (!store_completed ||
                        (uncache_error_denied &&
                         (!retire_random_nc_store_error(
                              lq_dequeued_before, sq_dequeued_before,
                              lq_cancels_before, sq_cancels_before) ||
                          environment.uncache_error_response_requests() !=
                              error_before + 1 ||
                          environment.uncache_denied_d_beats() !=
                              denied_before + 1 ||
                          environment.uncache_corrupt_d_beats() !=
                              corrupt_before ||
                          environment.bus_expected_load(
                              transaction.address,
                              memblock::LoadOp::ld) != bus_before ||
                          environment.bus_error_stats().uncache_reports !=
                              bus_errors_before.uncache_reports + 1 ||
                          environment.bus_error_stats().dcache_reports !=
                              bus_errors_before.dcache_reports ||
                          environment.bus_error_stats().last_uncache_address !=
                              (transaction.address & ~std::uint64_t{63}))) ||
                        environment.uncache_requests() != uncache_before + 1) {
                        return false;
                    }
                }
                ++constraint_coverage.nc_directions[nc_store ? 1 : 0];
                constraint_coverage.sample_uncache(
                    0, nc_store, uncache_error_denied);
                ++coverage.noncacheable;
            } else {
                ordinary_leaf_addressed = true;
                const std::uint64_t offset = (random() % 128) * 8;
                if (uncache_error_denied &&
                    !environment.run_until_queues_retired(8192)) {
                    return false;
                }
                if (!mmio_store) {
                    phase = "random-mmio-load";
                    auto transaction = make_load(
                        mmio_access_address() + offset,
                        memblock::LoadOp::ld, random() % 3);
                    transaction.oracle_address = mmio_physical + offset;
                    transaction.expected_debug_is_mmio = true;
                    transaction.expected_debug_is_ncio = false;
                    transaction.expected_debug_is_perf_cnt = false;
                    if (uncache_error_denied) {
                        transaction.expected_exception_mask =
                            *uncache_error_denied
                            ? memblock::kExceptionLoadAccessFault
                            : memblock::kExceptionHardwareError;
                    }
                    const std::uint64_t uncache_before =
                        environment.uncache_requests();
                    const std::uint64_t error_before =
                        environment.uncache_error_response_requests();
                    const std::uint64_t denied_before =
                        environment.uncache_denied_d_beats();
                    const std::uint64_t corrupt_before =
                        environment.uncache_corrupt_d_beats();
                    const std::uint64_t lq_dequeued_before =
                        environment.lq_dequeued();
                    const std::uint64_t sq_dequeued_before =
                        environment.sq_dequeued();
                    const std::uint64_t lq_cancels_before =
                        environment.lq_redirect_canceled_observed();
                    const std::uint64_t sq_cancels_before =
                        environment.sq_redirect_canceled_observed();
                    if (uncache_error_denied) {
                        environment.inject_next_uncache_response_error(
                            *uncache_error_denied,
                            !*uncache_error_denied);
                    }
                    environment.expect_load(transaction);
                    if (!environment.set_rob_head(
                            transaction.rob, transaction.rob_flag) ||
                        !environment.enqueue_load(transaction) ||
                        !environment.issue_load(transaction, 4096) ||
                        !environment.wait_for_mmio_request(
                            transaction.rob, transaction.rob_flag, 8192) ||
                        !environment.run_until_uncache_requests(
                            uncache_before + 1, 8192) ||
                        !environment.run_until_complete(
                            constrained_completion_timeout) ||
                        (uncache_error_denied
                            ? (!recover_random_precise_error(
                                   transaction.rob, transaction.rob_flag, false,
                                   lq_dequeued_before, sq_dequeued_before,
                                   lq_cancels_before, sq_cancels_before) ||
                               environment.uncache_error_response_requests() !=
                                   error_before + 1 ||
                               environment.uncache_denied_d_beats() !=
                                   denied_before +
                                       (*uncache_error_denied ? 1U : 0U) ||
                               environment.uncache_corrupt_d_beats() !=
                                   corrupt_before + 1)
                            : !environment.run_until_lq_retired(8192))) {
                        return false;
                    }
                    coverage.sample(transaction);
                } else {
                    phase = "random-mmio-store";
                    auto transaction = make_store(
                        mmio_access_address() + offset, random(),
                        memblock::StoreOp::sd,
                        random() % 2, random() % 2);
                    transaction.oracle_address = mmio_physical + offset;
                    transaction.expected_debug_is_mmio = false;
                    transaction.expected_debug_is_ncio = false;
                    if (uncache_error_denied) {
                        transaction.expected_exception_mask =
                            memblock::kExceptionStoreAccessFault;
                        transaction.allow_preliminary_success_writeback = true;
                    }
                    const std::uint64_t uncache_before =
                        environment.uncache_requests();
                    const std::uint64_t error_before =
                        environment.uncache_error_response_requests();
                    const std::uint64_t denied_before =
                        environment.uncache_denied_d_beats();
                    const std::uint64_t corrupt_before =
                        environment.uncache_corrupt_d_beats();
                    const std::uint64_t bus_before =
                        environment.bus_expected_load(
                            *transaction.oracle_address,
                            memblock::LoadOp::ld);
                    const auto bus_errors_before = environment.bus_error_stats();
                    const std::uint64_t lq_dequeued_before =
                        environment.lq_dequeued();
                    const std::uint64_t sq_dequeued_before =
                        environment.sq_dequeued();
                    const std::uint64_t lq_cancels_before =
                        environment.lq_redirect_canceled_observed();
                    const std::uint64_t sq_cancels_before =
                        environment.sq_redirect_canceled_observed();
                    if (uncache_error_denied) {
                        environment.inject_next_uncache_response_error(true, false);
                    }
                    environment.expect_store(transaction);
                    const bool prepared_mmio_store = environment.set_rob_head(
                        transaction.rob, transaction.rob_flag);
                    if (!prepared_mmio_store ||
                        !environment.enqueue_store(
                            transaction, memblock::lq_pointer_value(lq_offset)) ||
                        !environment.issue_store_address_until_tlb_hit(
                            transaction, constrained_completion_timeout) ||
                        !environment.issue_store_data(transaction, 4096) ||
                        !environment.run_cycles(64) ||
                        (environment.uncache_requests() == uncache_before &&
                         !environment.wait_for_mmio_store_request(
                             transaction.rob, transaction.rob_flag, 8192)) ||
                        !environment.run_until_uncache_requests(
                            uncache_before + 1, 8192) ||
                        !environment.run_until_store_complete(
                            constrained_completion_timeout) ||
                        (uncache_error_denied
                            ? (!recover_random_precise_error(
                                   transaction.rob, transaction.rob_flag, true,
                                   lq_dequeued_before, sq_dequeued_before,
                                   lq_cancels_before, sq_cancels_before) ||
                               environment.uncache_error_response_requests() !=
                                   error_before + 1 ||
                               environment.uncache_denied_d_beats() !=
                                   denied_before + 1 ||
                               environment.uncache_corrupt_d_beats() !=
                                   corrupt_before ||
                               environment.bus_expected_load(
                                   *transaction.oracle_address,
                                   memblock::LoadOp::ld) != bus_before ||
                               environment.bus_error_stats().uncache_reports !=
                                   bus_errors_before.uncache_reports + 1 ||
                               environment.bus_error_stats().dcache_reports !=
                                   bus_errors_before.dcache_reports ||
                               environment.bus_error_stats().
                                       last_uncache_address !=
                                   (*transaction.oracle_address &
                                    ~std::uint64_t{63}))
                            : (!environment.commit_stores_through(
                                   transaction, 1) ||
                               !environment.run_cycles(16)))) {
                        std::ostringstream detail;
                        detail << "mmio-store"
                               << ":tlb_feedbacks="
                               << environment.store_tlb_feedbacks()
                               << ":tlb_misses=" << environment.store_tlb_misses()
                               << ":uncache=" << environment.uncache_requests()
                               << ":store_mmio_valid="
                               << environment.store_mmio_valid();
                        phase = detail.str();
                        return false;
                    }
                    if (!uncache_error_denied) {
                        environment.record_committed_store(transaction);
                    }
                    coverage.sample(transaction, false);
                }
                ++actions;
                ++constraint_coverage.mmio_directions[mmio_store ? 1 : 0];
                constraint_coverage.sample_uncache(
                    1, mmio_store, uncache_error_denied);
                ++coverage.noncacheable;
                sample_dcache = false;
            }

            constraint_coverage.sample_operation(kind);
            const bool ordinary_leaf_enabled =
                translation.regime == RandomConstraints::translation_stage1
                ? constraints.stage1_napot_per_mille != 1000
                : translation.regime == RandomConstraints::translation_nested
                ? constraints.nested_vs_napot_per_mille != 1000 &&
                    constraints.nested_g_napot_per_mille != 1000
                : false;
            const bool sample_ordinary_leaf =
                ordinary_leaf_addressed && ordinary_leaf_enabled;
            const TranslationLeafTopology observed_leaf = leaf_addressed
                ? leaf : TranslationLeafTopology{};
            if (sample_translation_action) {
                constraint_coverage.sample_translation(
                    translation, observed_leaf, ptw_requests_before,
                    environment.ptw_requests(), 1,
                    (leaf_addressed || sample_ordinary_leaf) ? 1 : 0);
            }
            if (sample_dcache) {
                constraint_coverage.sample_dcache(
                    requests_before, environment.tilelink_requests());
            }
            if (probe_candidate && constraints.probes_per_mille != 0 &&
                (probe_coverage_incomplete() ||
                 random() % 1000 < constraints.probes_per_mille)) {
                phase = "random-probe";
                const std::optional<unsigned> missing_probe_cross =
                    constraint_coverage.first_missing_probe_cross(constraints);
                const bool to_b = missing_probe_cross
                    ? (*missing_probe_cross / 2U) % 2U != 0
                    : random() % 1000 < constraints.probe_to_b_per_mille;
                const bool need_data = missing_probe_cross
                    ? *missing_probe_cross % 2U != 0
                    : random() % 1000 < constraints.probe_need_data_per_mille;
                const unsigned probe_depth_class = missing_probe_cross
                    ? *missing_probe_cross / 4U
                    : constraints.choose_probe_depth(random());
                const unsigned probe_depth = probe_depth_class + 1U;
                const bool probe_overlap = probe_depth > 1U;

                std::ostringstream probe_phase;
                probe_phase << "random-probe:address=0x" << std::hex
                            << *probe_candidate << std::dec
                            << ":cap=" << (to_b ? "toB" : "toN")
                            << ":need_data=" << need_data
                            << ":depth=" << probe_depth;
                phase = probe_phase.str();

                const auto expected_line = environment.memory().read_beat(
                    *probe_candidate, 64);
                const std::uint64_t responses_before =
                    environment.dcache_probe_responses();
                const unsigned probe_completion_timeout =
                    std::max(constrained_completion_timeout, 8192U);
                // SQ retirement only moves the committed write into SBuffer.
                // Drain it before requiring a dirty Probe response.
                if (!environment.run_until_sbuffer_empty(
                        probe_completion_timeout)) {
                    return false;
                }

                std::optional<memblock::LoadTransaction> overlap_load;
                std::uint64_t overlap_writebacks_before = 0;
                if (probe_overlap) {
                    phase = probe_phase.str() + ":prepare-overlap";
                    constexpr std::uint64_t block_bytes = 0x200000;
                    constexpr std::uint64_t half_block_bytes = 0x100000;
                    constexpr std::uint64_t lines_per_half =
                        half_block_bytes / 64;
                    constexpr std::uint64_t overlap_blocks =
                        (probe_overlap_span / 2U) / block_bytes;
                    const std::uint64_t sequence = probe_overlap_line++;
                    const std::uint64_t block =
                        (sequence / lines_per_half) % overlap_blocks;
                    const std::uint64_t line_index = sequence % lines_per_half;
                    const unsigned candidate_set = static_cast<unsigned>(
                        (*probe_candidate >> 6) & 0x7fU);
                    std::vector<std::uint64_t> clean_lines;
                    for (std::uint64_t offset = 0;
                         clean_lines.size() < probe_depth - 1U; ++offset) {
                        const std::uint64_t clean_line_index =
                            (line_index + offset) % lines_per_half;
                        if ((clean_line_index & 0x7fU) == candidate_set) {
                            continue;
                        }
                        clean_lines.push_back(
                            probe_overlap_base + block * block_bytes +
                            clean_line_index * 64);
                    }
                    for (const std::uint64_t clean_line : clean_lines) {
                        const auto clean_load = make_load(
                            clean_line + 8 * (random() % 8),
                            memblock::LoadOp::ld,
                            random() % memblock::kScalarLoadLanes);
                        environment.expect_load(clean_load);
                        if (!environment.set_rob_head(
                                clean_load.rob, clean_load.rob_flag) ||
                            !environment.enqueue_load(clean_load) ||
                            !environment.issue_load(clean_load, 4096) ||
                            !environment.run_until_complete(
                                probe_completion_timeout) ||
                            !environment.run_until_lq_retired(8192)) {
                            return false;
                        }
                    }

                    const std::uint64_t miss_line =
                        probe_overlap_base + block * block_bytes +
                        half_block_bytes + line_index * 64;
                    overlap_load = make_load(
                        miss_line + 8 * (random() % 8),
                        memblock::LoadOp::ld,
                        random() % memblock::kScalarLoadLanes);
                    environment.expect_load(*overlap_load);
                    const std::uint64_t requests_before_overlap =
                        environment.tilelink_requests();
                    overlap_writebacks_before = environment.writebacks();
                    environment.force_next_dcache_response_delay(
                        2048U + static_cast<unsigned>(random() % 2049U));
                    if (!environment.set_rob_head(
                            overlap_load->rob, overlap_load->rob_flag) ||
                        !environment.enqueue_load(*overlap_load) ||
                        !environment.issue_load(*overlap_load, 4096) ||
                        !environment.run_until_dcache_requests(
                            requests_before_overlap + 1, 8192) ||
                        environment.writebacks() != overlap_writebacks_before) {
                        return false;
                    }
                    if (!environment.hold_dcache_c_until_probes(
                            environment.dcache_probes() + probe_depth)) {
                        return false;
                    }
                    for (const std::uint64_t clean_line : clean_lines) {
                        if (!environment.request_dcache_probe(
                                clean_line, 2, false, 2)) {
                            return false;
                        }
                    }
                }
                if (!probe_overlap && !environment.hold_dcache_c_until_probes(
                        environment.dcache_probes() + probe_depth)) {
                    return false;
                }

                phase = probe_phase.str() + ":probe";
                const std::uint64_t probe_depth_before =
                    environment.dcache_max_probe_outstanding();
                if (!environment.request_dcache_probe(
                        *probe_candidate, to_b ? 1U : 2U, need_data,
                        to_b ? 0U : 1U, expected_line)) {
                    phase = probe_phase.str() + ":primary-request";
                    return false;
                }
                if (!environment.run_until_probe_responses(
                        responses_before + probe_depth,
                        probe_completion_timeout)) {
                    phase = probe_phase.str() + ":response-drain";
                    return false;
                }
                if (probe_overlap &&
                    environment.writebacks() != overlap_writebacks_before) {
                    phase = probe_phase.str() + ":early-delayed-writeback";
                    return false;
                }
                if (probe_overlap &&
                    environment.dcache_max_probe_outstanding() <
                        std::max<std::uint64_t>(probe_depth,
                                                probe_depth_before)) {
                    phase = probe_phase.str() + ":outstanding-depth";
                    return false;
                }
                if (to_b) {
                    if (!environment.request_dcache_probe(
                            *probe_candidate, 2, false, 2)) {
                        phase = probe_phase.str() + ":cleanup-request";
                        return false;
                    }
                    if (!environment.run_until_probe_responses(
                            responses_before + probe_depth + 1U,
                            probe_completion_timeout)) {
                        phase = probe_phase.str() + ":cleanup-response";
                        return false;
                    }
                }
                if (overlap_load) {
                    phase = probe_phase.str() + ":delayed-load-complete";
                    if (!environment.run_until_complete(
                            probe_completion_timeout)) {
                        return false;
                    }
                    phase = probe_phase.str() + ":delayed-load-retire";
                    if (!environment.run_until_lq_retired(8192)) {
                        return false;
                    }
                    phase = probe_phase.str() + ":delayed-manager-drain";
                    if (!environment.run_until_dcache_idle(
                            probe_completion_timeout)) {
                        return false;
                    }
                }
                constraint_coverage.sample_probe(
                    probe_depth_class, to_b, need_data);
                phase = "seeded-mixed-tail";
            }
        }

        phase = "redirect-recovery-context";
        if (!environment.run_until_all_complete(4096) ||
            !environment.run_until_queues_retired(4096) ||
            !flush_pending_translation_fences() ||
            !environment.activate_bare()) {
            return false;
        }
        phase = "redirect-recovery";
        const std::uint64_t canceled_rob_offset = rob_offset;
        auto canceled = make_load(
            cache1_base + 0xf800, memblock::LoadOp::ld, 2);
        if (!environment.enqueue_load(canceled) || !environment.issue_load(canceled) ||
            !environment.redirect_after(
                memblock::rob_pointer_value(canceled_rob_offset - 1),
                memblock::rob_pointer_flag(canceled_rob_offset - 1), false) ||
            !environment.run_cycles(96)) {
            return false;
        }
        coverage.sample(canceled);
        ++actions;
        --lq_offset;
        const std::uint64_t survivor_lq = lq_offset++;
        const memblock::LoadTransaction survivor{
            .address = canceled.address,
            .op = canceled.op,
            .rob = canceled.rob,
            .rob_flag = canceled.rob_flag,
            .lq = memblock::lq_pointer_value(survivor_lq),
            .lq_flag = memblock::lq_pointer_flag(survivor_lq),
            .sq = memblock::sq_pointer_value(sq_offset),
            .sq_flag = memblock::sq_pointer_flag(sq_offset),
            .pdest = static_cast<std::uint8_t>(canceled.pdest + 1),
            .lane = 1,
        };
        if (!issue_load(survivor)) {
            return false;
        }
        ++coverage.redirect_recovery;
        ++coverage.cacheable;

        phase = "final-drain";
        if (!environment.run_until_all_complete(2048) ||
            !environment.run_until_queues_retired(4096) ||
            !environment.run_cycles(32)) {
            return false;
        }
        coverage.dcache_request_stalls = environment.dcache_request_stalls();
        coverage.dcache_response_delays = environment.dcache_response_delays();
        coverage.ptw_request_stalls = environment.ptw_request_stalls();
        coverage.ptw_response_delays = environment.ptw_response_delays();
        coverage.uncache_request_stalls = environment.uncache_request_stalls();
        coverage.uncache_response_delays = environment.uncache_response_delays();
        coverage.dispatch_widths =
            environment.lsq_enqueue_widths_observed();
        coverage.dispatch_lanes =
            environment.lsq_enqueue_lanes_observed();
        const std::uint64_t probe_requests = environment.dcache_probes();
        const std::uint64_t expected_probe_sources = std::min<std::uint64_t>(
            probe_requests, memblock::kDcacheProbeSourceCount);
        const std::uint64_t expected_probe_source_reuses =
            probe_requests - expected_probe_sources;
        const std::uint64_t expected_probe_source_wraps = probe_requests == 0
            ? 0
            : (probe_requests - 1U) / memblock::kDcacheProbeSourceCount;
        if (actions != options.transactions || environment.ptw_requests() < 3 ||
            environment.uncache_requests() < 2 ||
            !environment.dcache_grants_drained() ||
            environment.tilelink_release_data() <= release_before ||
            environment.lq_dequeued() + environment.lq_canceled() !=
                environment.lq_allocated() ||
            environment.sq_dequeued() + environment.sq_canceled() !=
                environment.sq_allocated() ||
            environment.redirect_cancellation_events_observed() == 0 ||
            environment.lq_redirect_canceled_observed() !=
                environment.lq_canceled() ||
            environment.sq_redirect_canceled_observed() !=
                environment.sq_canceled() ||
            environment.lq_canceled_unobserved() != 0 ||
            environment.sq_canceled_unobserved() != 0 ||
            !coverage.complete(
                constraints.concurrent_actions_per_mille != 0) ||
            !constraint_coverage.complete(
                constraints, options.backpressure,
                environment.dcache_response_latency_stats(),
                environment.ptw_response_latency_stats(),
                environment.uncache_response_latency_stats()) ||
            (constraints.probes_per_mille != 0 &&
             environment.dcache_max_probe_outstanding() <
                 constraints.maximum_enabled_probe_depth()) ||
            environment.dcache_probe_sources() != expected_probe_sources ||
            environment.dcache_probe_source_reuses() !=
                expected_probe_source_reuses ||
            environment.dcache_probe_source_wraps() !=
                expected_probe_source_wraps ||
            total_ifetch_prefetches() == 0 ||
            !coverage.backpressure_complete(options.backpressure)) {
            phase = "coverage-gates";
            return false;
        }
        return true;
    }();

    if (!completed) {
        std::cerr << "MEMBLOCK_RANDOM_MIXED_FAIL seed=" << options.seed
                  << " transactions=" << actions
                  << " requested=" << options.transactions
                  << " cycle=" << environment.cycle()
                  << " phase=" << phase
                  << " lq=" << environment.lq_dequeued() << '+'
                  << environment.lq_canceled() << '/'
                  << environment.lq_allocated()
                  << " sq=" << environment.sq_dequeued() << '+'
                  << environment.sq_canceled() << '/'
                  << environment.sq_allocated()
                  << " lsq_monitor_schema=2"
                  << " lsq_enqueued_observed="
                  << environment.lq_enqueued_observed() << ','
                  << environment.sq_enqueued_observed()
                  << " redirect_cancels_observed="
                  << environment.redirect_cancellation_events_observed() << ','
                  << environment.lq_redirect_canceled_observed() << ','
                  << environment.sq_redirect_canceled_observed()
                  << " unobserved_cancels="
                  << environment.lq_canceled_unobserved() << ','
                  << environment.sq_canceled_unobserved()
                  << " ptw=" << environment.ptw_requests()
                  << " uncache=" << environment.uncache_requests()
                  << " release_data=" << environment.tilelink_release_data()
                  << " probe_max_outstanding="
                  << environment.dcache_max_probe_outstanding()
                  << " probe_source_space="
                  << memblock::kDcacheProbeSourceCount
                  << " probe_source_lifecycle="
                  << environment.dcache_probe_sources() << ','
                  << environment.dcache_probe_source_reuses() << ','
                  << environment.dcache_probe_source_wraps()
                  << " ifetch_prefetches=" << total_ifetch_prefetches()
                  << " l2_stride_prefetches="
                  << environment.hardware_prefetch_stats().l2_source_counts[12]
                  << " reason="
                  << (environment.error().empty()
                          ? "mixed_coverage_or_accounting_gate_failed"
                          : environment.error())
                  << ' ' << load_feedback_summary(
                         environment.scalar_load_feedback_stats())
                  << ' ' << raw_load_feedback_summary()
                  << ' ' << coverage.summary() << ' '
                  << constraint_coverage.summary(
                         constraints,
                         environment.dcache_response_latency_stats(),
                         environment.ptw_response_latency_stats(),
                         environment.uncache_response_latency_stats())
                  << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_RANDOM_MIXED_PASS"
              << " seed=" << options.seed
              << " transactions=" << actions
              << " cycle=" << environment.cycle()
              << " scalar_writebacks=" << environment.writebacks()
              << " prefetch_writebacks=" << environment.prefetch_writebacks()
              << " store_writebacks=" << environment.store_writebacks()
              << " vector_load_writebacks="
              << environment.vector_load_writebacks()
              << " vector_store_writebacks="
              << environment.vector_store_writebacks()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " dcache_gets=" << environment.dcache_gets()
              << " dcache_refills=" << environment.dcache_refills()
              << " dcache_acquire_perms="
              << environment.dcache_acquire_perms()
              << " probes=" << environment.dcache_probes()
              << " grant_acks=" << environment.dcache_grant_acks()
              << " release_data=" << environment.tilelink_release_data()
              << " probe_max_outstanding="
              << environment.dcache_max_probe_outstanding()
              << " probe_source_space=" << memblock::kDcacheProbeSourceCount
              << " probe_source_lifecycle="
              << environment.dcache_probe_sources() << ','
              << environment.dcache_probe_source_reuses() << ','
              << environment.dcache_probe_source_wraps()
              << " ifetch_prefetches=" << total_ifetch_prefetches()
              << " l2_stride_prefetches="
              << environment.hardware_prefetch_stats().l2_source_counts[12]
              << " ptw_requests=" << environment.ptw_requests()
              << " uncache_requests=" << environment.uncache_requests()
              << ' ' << load_feedback_summary(
                     environment.scalar_load_feedback_stats())
              << ' ' << raw_load_feedback_summary()
              << " lq=" << environment.lq_dequeued() << '+'
              << environment.lq_canceled() << '/'
              << environment.lq_allocated()
              << " sq=" << environment.sq_dequeued() << '+'
              << environment.sq_canceled() << '/'
              << environment.sq_allocated()
              << " lsq_monitor_schema=2"
              << " lsq_enqueued_observed="
              << environment.lq_enqueued_observed() << ','
              << environment.sq_enqueued_observed()
              << " redirect_cancels_observed="
              << environment.redirect_cancellation_events_observed() << ','
              << environment.lq_redirect_canceled_observed() << ','
              << environment.sq_redirect_canceled_observed()
              << " unobserved_cancels="
              << environment.lq_canceled_unobserved() << ','
              << environment.sq_canceled_unobserved()
              << ' ' << coverage.summary() << ' '
              << constraint_coverage.summary(
                     constraints, environment.dcache_response_latency_stats(),
                     environment.ptw_response_latency_stats(),
                     environment.uncache_response_latency_stats())
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}
