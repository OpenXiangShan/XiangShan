// This file is included inside memblock_main.cpp's anonymous namespace.
// Keep scenario-local helpers here; shared infrastructure belongs in memblock_env.hpp.

int run_hypervisor_contracts(int argc, char **argv)
{
    constexpr std::uint64_t guest_virtual = 0x60000000ULL;
    constexpr std::uint64_t guest_physical = 0x9e000000ULL;
    constexpr std::uint64_t host_physical = 0xc5000000ULL;
    constexpr std::uint64_t vs_root = 0xb0000000ULL;
    constexpr std::uint64_t g_root = 0xb2000000ULL;
    constexpr std::uint8_t pmp_napot_read = 0x19;
    constexpr std::uint8_t pmp_napot_execute = 0x1c;
    constexpr std::uint8_t pmp_napot_read_write = 0x1b;
    constexpr std::uint8_t pmp_napot_read_execute = 0x1d;
    constexpr std::uint8_t pmp_napot_read_write_execute = 0x1f;
    constexpr std::uint8_t pmp_locked_napot_read = 0x99;
    constexpr std::uint8_t pmp_locked_napot_read_write_execute = 0x9f;

    auto configure = [](
                         memblock::Environment &environment,
                         const memblock::ReferencePtePermissions &vs,
                         const memblock::ReferencePtePermissions &g,
                         memblock::ReferencePrivilegeMode spvp,
                         bool mxr, bool vmxr, bool vsum,
                         memblock::ReferencePageMode vs_mode =
                             memblock::ReferencePageMode::sv39,
                         memblock::ReferencePageMode g_mode =
                             memblock::ReferencePageMode::sv39,
                         memblock::ReferencePbmt vs_pbmt =
                             memblock::ReferencePbmt::pma,
                         memblock::ReferencePbmt g_pbmt =
                             memblock::ReferencePbmt::pma,
                         memblock::ReferencePrivilegeMode current_privilege =
                             memblock::ReferencePrivilegeMode::supervisor,
                         std::uint64_t final_physical_address = 0) {
        const std::uint64_t mapped_physical = final_physical_address == 0
            ? host_physical : final_physical_address;
        const bool vs_nc = vs_pbmt == memblock::ReferencePbmt::nc;
        const bool vs_io = vs_pbmt == memblock::ReferencePbmt::io;
        const bool g_nc = g_pbmt == memblock::ReferencePbmt::nc;
        const bool g_io = g_pbmt == memblock::ReferencePbmt::io;
        bool ready = environment.reset();
        ready = ready && (vs_mode == memblock::ReferencePageMode::sv48
            ? environment.map_sv48_leaf(
                  guest_virtual, guest_physical, 0, vs_root,
                  vs.readable, vs.writable, vs.executable, vs.user, vs_nc,
                  vs.accessed, vs.dirty, vs_io)
            : environment.map_sv39_leaf(
                  guest_virtual, guest_physical, 0, vs_root,
                  vs.readable, vs.writable, vs.executable, vs.user, vs_nc,
                  vs.accessed, vs.dirty, vs_io));
        const unsigned vs_table_pages =
            vs_mode == memblock::ReferencePageMode::sv48 ? 4U : 3U;
        for (unsigned page = 0; ready && page < vs_table_pages; ++page) {
            const std::uint64_t address = vs_root + page * 0x1000ULL;
            ready = g_mode == memblock::ReferencePageMode::sv48
                ? environment.map_sv48x4_4k(
                      address, address, g_root, true, true, false)
                : environment.map_sv39x4_4k(
                      address, address, g_root, true, true, false);
        }
        ready = ready && (g_mode == memblock::ReferencePageMode::sv48
            ? environment.map_sv48x4_leaf(
                  guest_physical, mapped_physical, 0, g_root,
                  g.readable, g.writable, g.executable,
                  g.accessed, g.dirty, g.user, g_nc, g_io)
            : environment.map_sv39x4_leaf(
                  guest_physical, mapped_physical, 0, g_root,
                  g.readable, g.writable, g.executable,
                  g.accessed, g.dirty, g.user, g_nc, g_io));
        return ready && environment.set_page_based_memory_types(true, true) &&
            environment.activate_two_stage_modes(
                vs_mode, g_mode,
                vs_root, g_root, 61, 71) &&
            environment.set_hypervisor_access_permissions(
                spvp, mxr, vmxr, vsum, current_privilege) &&
            environment.enable_misaligned_accesses();
    };

    std::uint64_t total_cycles = 0;
    std::uint64_t total_ptw_requests = 0;
    std::uint64_t total_dcache_requests = 0;
    unsigned load_cases = 0;
    unsigned store_cases = 0;
    unsigned page_faults = 0;
    unsigned guest_page_faults = 0;
    unsigned access_faults = 0;
    unsigned translation_mode_pairs = 0;
    unsigned mode_family_cases = 0;
    unsigned pbmt_combinations = 0;
    unsigned pbmt_family_cases = 0;
    unsigned misaligned_family_cases = 0;
    unsigned physical_pmp_cases = 0;
    unsigned locked_physical_pmp_cases = 0;
    unsigned physical_pma_cases = 0;
    unsigned physical_pma_boundary_cases = 0;

    auto run_load_case = [&](
                             const char *name,
                             memblock::LoadOp op,
                             const memblock::ReferencePtePermissions &vs,
                             const memblock::ReferencePtePermissions &g,
                             memblock::ReferencePrivilegeMode spvp,
                             bool mxr = false,
                             bool vmxr = false,
                             bool vsum = false,
                             bool pmp_execute_denied = false,
                             memblock::ReferencePageMode vs_mode =
                                 memblock::ReferencePageMode::sv39,
                             memblock::ReferencePageMode g_mode =
                                 memblock::ReferencePageMode::sv39,
                             memblock::ReferencePbmt vs_pbmt =
                                 memblock::ReferencePbmt::pma,
                             memblock::ReferencePbmt g_pbmt =
                                 memblock::ReferencePbmt::pma,
                             std::uint64_t address_offset = 0x180,
                             std::optional<std::uint8_t> physical_pmp_config =
                                 std::nullopt,
                             memblock::ReferencePrivilegeMode current_privilege =
                                 memblock::ReferencePrivilegeMode::supervisor,
                             std::uint64_t final_physical_address = 0,
                             bool physical_pma_device = false) {
        const std::uint64_t mapped_physical = final_physical_address == 0
            ? host_physical : final_physical_address;
        const bool hlvx = op == memblock::LoadOp::hlvxhu ||
                          op == memblock::LoadOp::hlvxwu;
        const bool vs_permitted = hlvx
            ? memblock::reference_hlvx_permitted(vs, spvp, vsum)
            : memblock::reference_load_permitted(
                  vs, spvp, vsum, mxr || vmxr);
        const bool g_permitted = hlvx
            ? memblock::reference_hlvx_permitted(
                  g, memblock::ReferencePrivilegeMode::supervisor, false, true)
            : memblock::reference_load_permitted(
                  g, memblock::ReferencePrivilegeMode::supervisor, false,
                  mxr, true);
        const std::optional<std::uint8_t> active_pmp_config =
            pmp_execute_denied
                ? std::optional<std::uint8_t>{pmp_napot_read}
                : physical_pmp_config;
        const std::uint8_t required_pmp_permission = hlvx ? 0x5U : 0x1U;
        const bool physical_pmp_denied = active_pmp_config.has_value() &&
            (*active_pmp_config & required_pmp_permission) !=
                required_pmp_permission;
        const bool physical_pma_denied = physical_pma_device && hlvx;
        const std::uint32_t expected_exception = !vs_permitted
            ? memblock::kExceptionLoadPageFault
            : !g_permitted
                ? memblock::kExceptionLoadGuestPageFault
                : physical_pmp_denied || physical_pma_denied
                    ? memblock::kExceptionLoadAccessFault
                    : 0U;
        const auto final_pbmt =
            memblock::reference_two_stage_pbmt(vs_pbmt, g_pbmt);
        const bool final_nc = final_pbmt == memblock::ReferencePbmt::nc;
        const bool final_io = final_pbmt == memblock::ReferencePbmt::io ||
            (final_pbmt == memblock::ReferencePbmt::pma &&
             physical_pma_device);

        memblock::Environment environment(argc, argv);
        environment.memory().fill_incrementing(
            mapped_physical, 0x1000, 0x31);
        if (!configure(
                environment, vs, g, spvp, mxr, vmxr, vsum,
                vs_mode, g_mode, vs_pbmt, g_pbmt, current_privilege,
                mapped_physical)) {
            std::cerr << "MEMBLOCK_HYPERVISOR_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << name
                      << "-configuration reason=" << environment.error()
                      << '\n';
            return false;
        }
        if (active_pmp_config.has_value()) {
            const auto napot_address = [](std::uint64_t base, std::uint64_t size) {
                return (base | (size / 2 - 1)) >> 2;
            };
            if (!environment.configure_pmp(
                    {napot_address(host_physical, 0x1000), ~std::uint64_t{0}},
                    {*active_pmp_config,
                     pmp_napot_read_write_execute})) {
                std::cerr << "MEMBLOCK_HYPERVISOR_CONTRACTS_FAIL cycle="
                          << environment.cycle() << " phase=" << name
                          << "-pmp reason=" << environment.error() << '\n';
                return false;
            }
        }
        const memblock::LoadTransaction transaction{
            .address = guest_virtual + address_offset,
            .oracle_address = mapped_physical + address_offset,
            .op = op,
            .rob = 0,
            .lq = 0,
            .pdest = 40,
            .lane = 0,
            .expected_exception_mask = expected_exception,
            .expected_debug_is_mmio = physical_pma_denied
                ? std::nullopt : std::optional<bool>{final_io},
            .expected_debug_is_ncio = final_io && !physical_pma_denied
                ? std::optional<bool>{false} : std::nullopt,
        };
        const std::uint64_t dcache_before = environment.tilelink_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        const auto feedback_before =
            environment.scalar_load_feedback_stats();
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 4096) ||
            (expected_exception == 0 && final_io &&
             !environment.wait_for_mmio_request(
                 transaction.rob, transaction.rob_flag, 8192)) ||
            !environment.run_until_complete(32768) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_HYPERVISOR_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << name
                      << "-execution expected_exception=0x" << std::hex
                      << expected_exception << std::dec << " reason="
                      << environment.error() << '\n';
            return false;
        }
        const auto &feedback_after =
            environment.scalar_load_feedback_stats();
        std::uint64_t wakeups = 0;
        std::uint64_t cancels = 0;
        for (unsigned lane = 0; lane < memblock::kScalarLoadLanes; ++lane) {
            wakeups += feedback_after.wakeups[lane] -
                       feedback_before.wakeups[lane];
            cancels += feedback_after.ld2_cancels[lane] -
                       feedback_before.ld2_cancels[lane];
        }
        // HLVX maps to a read-execute PMP command. If R is allowed but X is
        // denied, the load pipeline raises the correct access fault through
        // pmp.instr, while its DCache kill path sees neither pmp.ld nor pmp.st.
        // Pin that unique externally visible early lookup instead of allowing
        // arbitrary DCache traffic for every physical PMP fault.
        const bool hlvx_execute_only_fault =
            expected_exception == memblock::kExceptionLoadAccessFault &&
            hlvx && active_pmp_config.has_value() &&
            ((*active_pmp_config & 0x1U) != 0) &&
            ((*active_pmp_config & 0x4U) == 0);
        const std::uint64_t expected_fault_dcache = dcache_before +
            (hlvx_execute_only_fault ? 1U : 0U);
        if (expected_exception != 0 &&
            (environment.uncache_requests() != uncache_before ||
             environment.tilelink_requests() != expected_fault_dcache ||
             cancels == 0 || wakeups != cancels)) {
            std::cerr << "MEMBLOCK_HYPERVISOR_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << name
                      << " reason=faulting-load-side-effect"
                      << " dcache_before=" << dcache_before
                      << " dcache_after=" << environment.tilelink_requests()
                      << " uncache_before=" << uncache_before
                      << " uncache_after=" << environment.uncache_requests()
                      << " wakeups=" << wakeups
                      << " cancels=" << cancels << '\n';
            return false;
        }
        if (expected_exception == 0) {
            const std::uint64_t expected_dcache =
                dcache_before + (final_nc || final_io ? 0U : 1U);
            const std::uint64_t expected_uncache =
                uncache_before + (final_nc || final_io ? 1U : 0U);
            if (environment.tilelink_requests() != expected_dcache ||
                environment.uncache_requests() != expected_uncache) {
                std::cerr << "MEMBLOCK_HYPERVISOR_CONTRACTS_FAIL cycle="
                          << environment.cycle() << " phase=" << name
                          << " reason=wrong-load-manager"
                          << " dcache=" << environment.tilelink_requests()
                          << '/' << expected_dcache
                          << " uncache=" << environment.uncache_requests()
                          << '/' << expected_uncache << '\n';
                return false;
            }
        }
        ++load_cases;
        page_faults += expected_exception == memblock::kExceptionLoadPageFault;
        guest_page_faults +=
            expected_exception == memblock::kExceptionLoadGuestPageFault;
        access_faults +=
            expected_exception == memblock::kExceptionLoadAccessFault;
        total_cycles += environment.cycle();
        total_ptw_requests += environment.ptw_requests();
        total_dcache_requests += environment.tilelink_requests();
        return true;
    };

    auto run_store_case = [&](
                              const char *name,
                              memblock::StoreOp op,
                              const memblock::ReferencePtePermissions &vs,
                              const memblock::ReferencePtePermissions &g,
                              memblock::ReferencePrivilegeMode spvp,
                              bool vsum,
                              std::uint64_t data,
                              memblock::ReferencePageMode vs_mode =
                                  memblock::ReferencePageMode::sv39,
                              memblock::ReferencePageMode g_mode =
                                  memblock::ReferencePageMode::sv39,
                              memblock::ReferencePbmt vs_pbmt =
                                  memblock::ReferencePbmt::pma,
                              memblock::ReferencePbmt g_pbmt =
                                  memblock::ReferencePbmt::pma,
                              std::uint64_t address_offset = 0x280,
                              std::optional<std::uint8_t> physical_pmp_config =
                                  std::nullopt,
                              memblock::ReferencePrivilegeMode current_privilege =
                                  memblock::ReferencePrivilegeMode::supervisor,
                              std::uint64_t final_physical_address = 0,
                              bool physical_pma_device = false) {
        const std::uint64_t mapped_physical = final_physical_address == 0
            ? host_physical : final_physical_address;
        const bool vs_permitted =
            memblock::reference_store_permitted(vs, spvp, vsum);
        const bool g_permitted = memblock::reference_store_permitted(
            g, memblock::ReferencePrivilegeMode::supervisor, false, true);
        const bool physical_pmp_denied = physical_pmp_config.has_value() &&
            (*physical_pmp_config & 0x2U) == 0;
        const std::uint32_t expected_exception = !vs_permitted
            ? memblock::kExceptionStorePageFault
            : !g_permitted
                ? memblock::kExceptionStoreGuestPageFault
                : physical_pmp_denied
                    ? memblock::kExceptionStoreAccessFault
                : 0U;
        const auto final_pbmt =
            memblock::reference_two_stage_pbmt(vs_pbmt, g_pbmt);
        const bool final_nc = final_pbmt == memblock::ReferencePbmt::nc;
        const bool final_io = final_pbmt == memblock::ReferencePbmt::io ||
            (final_pbmt == memblock::ReferencePbmt::pma &&
             physical_pma_device);
        const unsigned access_bytes =
            1U << (static_cast<unsigned>(op) & 3U);
        const bool misaligned =
            ((guest_virtual + address_offset) & (access_bytes - 1U)) != 0;

        memblock::Environment environment(argc, argv);
        environment.memory().fill_incrementing(
            mapped_physical, 0x1000, 0x71);
        if (!configure(
                environment, vs, g, spvp, false, false, vsum,
                vs_mode, g_mode, vs_pbmt, g_pbmt, current_privilege,
                mapped_physical)) {
            std::cerr << "MEMBLOCK_HYPERVISOR_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << name
                      << "-configuration reason=" << environment.error()
                      << '\n';
            return false;
        }
        if (physical_pmp_config.has_value()) {
            const auto napot_address = [](std::uint64_t base, std::uint64_t size) {
                return (base | (size / 2 - 1)) >> 2;
            };
            if (!environment.configure_pmp(
                    {napot_address(host_physical, 0x1000), ~std::uint64_t{0}},
                    {*physical_pmp_config,
                     pmp_napot_read_write_execute})) {
                std::cerr << "MEMBLOCK_HYPERVISOR_CONTRACTS_FAIL cycle="
                          << environment.cycle() << " phase=" << name
                          << "-pmp reason=" << environment.error() << '\n';
                return false;
            }
        }
        const memblock::StoreTransaction transaction{
            .address = guest_virtual + address_offset,
            .oracle_address = mapped_physical + address_offset,
            .data = data,
            .op = op,
            .rob = 0,
            .sq = 0,
            .address_lane = 0,
            .data_lane = 1,
            .expected_exception_mask = expected_exception,
            .expected_debug_is_mmio = physical_pma_device,
            .expected_debug_is_ncio = false,
        };
        const std::uint64_t dcache_before = environment.tilelink_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        environment.expect_store(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_store(transaction, 0) ||
            !environment.issue_store_address_until_tlb_hit(transaction, 16384) ||
            !environment.issue_store_data(transaction, 2048) ||
            (expected_exception == 0 && misaligned &&
             !environment.pulse_pending_store(
                 transaction.rob, transaction.rob_flag)) ||
            (expected_exception == 0 && final_io &&
             !environment.wait_for_mmio_store_request(
                 transaction.rob, transaction.rob_flag, 8192)) ||
            !environment.run_until_store_complete(32768)) {
            std::cerr << "MEMBLOCK_HYPERVISOR_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << name
                      << "-execution expected_exception=0x" << std::hex
                      << expected_exception << std::dec << " reason="
                      << environment.error() << '\n';
            return false;
        }
        if (expected_exception == 0) {
            const unsigned size =
                1U << (static_cast<unsigned>(transaction.op) & 3U);
            const std::uint64_t mask = size == 8
                ? ~std::uint64_t{0}
                : (std::uint64_t{1} << (size * 8)) - 1;
            const std::array<memblock::LoadOp, 4> read_ops{{
                memblock::LoadOp::hlvbu, memblock::LoadOp::hlvhu,
                memblock::LoadOp::hlvwu, memblock::LoadOp::hlvd,
            }};
            const bool committed = final_io
                ? environment.commit_stores_through(transaction, 1) &&
                      environment.run_cycles(16)
                : environment.commit_store(transaction, 16384) &&
                      environment.run_until_sbuffer_empty(16384);
            if (!committed) {
                std::cerr << "MEMBLOCK_HYPERVISOR_CONTRACTS_FAIL cycle="
                          << environment.cycle() << " phase=" << name
                          << "-commit reason=" << environment.error()
                          << '\n';
                return false;
            }
            if (final_io) {
                environment.record_committed_store(transaction);
            }
            const std::uint64_t expected_store_dcache =
                dcache_before + (final_nc || final_io ? 0U : 1U);
            const std::uint64_t expected_store_uncache =
                uncache_before + (final_nc || final_io ? 1U : 0U);
            if (environment.tilelink_requests() != expected_store_dcache ||
                environment.uncache_requests() != expected_store_uncache) {
                std::cerr << "MEMBLOCK_HYPERVISOR_CONTRACTS_FAIL cycle="
                          << environment.cycle() << " phase=" << name
                          << " reason=wrong-store-manager"
                          << " dcache=" << environment.tilelink_requests()
                          << '/' << expected_store_dcache
                          << " uncache=" << environment.uncache_requests()
                          << '/' << expected_store_uncache << '\n';
                return false;
            }
            const memblock::LoadTransaction readback{
                .address = transaction.address,
                .oracle_address = transaction.oracle_address,
                .op = read_ops[static_cast<unsigned>(transaction.op) & 3U],
                .rob = 1,
                .lq = 0,
                .pdest = 41,
                .lane = 1,
                .expected_debug_is_mmio = final_io,
                .expected_debug_is_ncio = final_io
                    ? std::optional<bool>{false} : std::nullopt,
            };
            const std::uint64_t readback_dcache_before =
                environment.tilelink_requests();
            const std::uint64_t readback_uncache_before =
                environment.uncache_requests();
            environment.expect_load_data(readback, data & mask);
            if (!environment.set_rob_head(readback.rob, readback.rob_flag) ||
                !environment.enqueue_load(readback) ||
                !environment.issue_load(readback, 4096) ||
                (final_io && !environment.wait_for_mmio_request(
                    readback.rob, readback.rob_flag, 8192)) ||
                !environment.run_until_complete(32768) ||
                !environment.run_until_lq_retired(8192)) {
                std::cerr << "MEMBLOCK_HYPERVISOR_CONTRACTS_FAIL cycle="
                          << environment.cycle() << " phase=" << name
                          << "-hlv-readback expected=0x" << std::hex
                          << (data & mask) << std::dec << " reason="
                          << environment.error() << '\n';
                return false;
            }
            const std::uint64_t expected_readback_uncache =
                readback_uncache_before + (final_nc || final_io ? 1U : 0U);
            if (environment.tilelink_requests() != readback_dcache_before ||
                environment.uncache_requests() != expected_readback_uncache) {
                std::cerr << "MEMBLOCK_HYPERVISOR_CONTRACTS_FAIL cycle="
                          << environment.cycle() << " phase=" << name
                          << " reason=wrong-readback-manager\n";
                return false;
            }
        } else {
            if (environment.tilelink_requests() != dcache_before ||
                environment.uncache_requests() != uncache_before ||
                !environment.account_sq_cancellation(1)) {
                std::cerr << "MEMBLOCK_HYPERVISOR_CONTRACTS_FAIL cycle="
                          << environment.cycle() << " phase=" << name
                          << " reason=faulting-store-reached-data-manager\n";
                return false;
            }
        }
        ++store_cases;
        page_faults += expected_exception == memblock::kExceptionStorePageFault;
        guest_page_faults +=
            expected_exception == memblock::kExceptionStoreGuestPageFault;
        access_faults +=
            expected_exception == memblock::kExceptionStoreAccessFault;
        total_cycles += environment.cycle();
        total_ptw_requests += environment.ptw_requests();
        total_dcache_requests += environment.tilelink_requests();
        return true;
    };

    const memblock::ReferencePtePermissions rw_supervisor{};
    const memblock::ReferencePtePermissions rw_user{.user = true};
    const memblock::ReferencePtePermissions g_rw_user{.user = true};
    const memblock::ReferencePtePermissions x_supervisor{
        .readable = false, .writable = false, .executable = true,
        .dirty = false,
    };
    const memblock::ReferencePtePermissions x_user{
        .readable = false, .writable = false, .executable = true,
        .user = true, .dirty = false,
    };
    const memblock::ReferencePtePermissions g_x_user{
        .readable = false, .writable = false, .executable = true,
        .user = true, .dirty = false,
    };

    const std::array<memblock::LoadOp, 9> load_ops{{
        memblock::LoadOp::hlvb, memblock::LoadOp::hlvh,
        memblock::LoadOp::hlvw, memblock::LoadOp::hlvd,
        memblock::LoadOp::hlvbu, memblock::LoadOp::hlvhu,
        memblock::LoadOp::hlvwu, memblock::LoadOp::hlvxhu,
        memblock::LoadOp::hlvxwu,
    }};
    for (unsigned index = 0; index < load_ops.size(); ++index) {
        const bool hlvx = index >= 7;
        if (!run_load_case(
                hlvx ? "hlvx-encoding" : "hlv-encoding", load_ops[index],
                hlvx ? x_supervisor : rw_supervisor,
                hlvx ? g_x_user : g_rw_user,
                memblock::ReferencePrivilegeMode::supervisor)) {
            return 1;
        }
    }

    if (!run_load_case(
            "spvp-user-user-page", memblock::LoadOp::hlvd,
            rw_user, g_rw_user, memblock::ReferencePrivilegeMode::user) ||
        !run_load_case(
            "spvp-user-supervisor-page", memblock::LoadOp::hlvd,
            rw_supervisor, g_rw_user,
            memblock::ReferencePrivilegeMode::user) ||
        !run_load_case(
            "spvp-supervisor-user-vsum0", memblock::LoadOp::hlvd,
            rw_user, g_rw_user,
            memblock::ReferencePrivilegeMode::supervisor) ||
        !run_load_case(
            "spvp-supervisor-user-vsum1", memblock::LoadOp::hlvd,
            rw_user, g_rw_user,
            memblock::ReferencePrivilegeMode::supervisor,
            false, false, true) ||
        !run_load_case(
            "hlv-xonly-vmxr0", memblock::LoadOp::hlvd,
            x_supervisor, g_rw_user,
            memblock::ReferencePrivilegeMode::supervisor) ||
        !run_load_case(
            "hlv-xonly-vmxr1", memblock::LoadOp::hlvd,
            x_supervisor, g_rw_user,
            memblock::ReferencePrivilegeMode::supervisor,
            false, true) ||
        !run_load_case(
            "hlvx-readonly-vs", memblock::LoadOp::hlvxwu,
            rw_supervisor, g_x_user,
            memblock::ReferencePrivilegeMode::supervisor) ||
        !run_load_case(
            "hlvx-readonly-g", memblock::LoadOp::hlvxwu,
            x_supervisor, g_rw_user,
            memblock::ReferencePrivilegeMode::supervisor) ||
        !run_load_case(
            "hlvx-pmp-execute-denied", memblock::LoadOp::hlvxwu,
            x_supervisor, g_x_user,
            memblock::ReferencePrivilegeMode::supervisor,
            false, false, false, true)) {
        return 1;
    }

    const std::array<memblock::StoreOp, 4> store_ops{{
        memblock::StoreOp::hsvb, memblock::StoreOp::hsvh,
        memblock::StoreOp::hsvw, memblock::StoreOp::hsvd,
    }};
    for (unsigned index = 0; index < store_ops.size(); ++index) {
        if (!run_store_case(
                "hsv-encoding", store_ops[index], rw_supervisor, g_rw_user,
                memblock::ReferencePrivilegeMode::supervisor, false,
                0x8877665544332211ULL + index)) {
            return 1;
        }
    }
    if (!run_store_case(
            "hsv-spvp-user-supervisor-page", memblock::StoreOp::hsvd,
            rw_supervisor, g_rw_user,
            memblock::ReferencePrivilegeMode::user, false,
            0x1020304050607080ULL) ||
        !run_store_case(
            "hsv-supervisor-user-vsum0", memblock::StoreOp::hsvd,
            rw_user, g_rw_user,
            memblock::ReferencePrivilegeMode::supervisor, false,
            0x1122334455667788ULL) ||
        !run_store_case(
            "hsv-supervisor-user-vsum1", memblock::StoreOp::hsvd,
            rw_user, g_rw_user,
            memblock::ReferencePrivilegeMode::supervisor, true,
            0x99aabbccddeeff00ULL)) {
        return 1;
    }

    struct TranslationModePair {
        memblock::ReferencePageMode vs_mode;
        memblock::ReferencePageMode g_mode;
    };
    constexpr std::array<TranslationModePair, 4> translation_modes{{
        {memblock::ReferencePageMode::sv39,
         memblock::ReferencePageMode::sv39},
        {memblock::ReferencePageMode::sv39,
         memblock::ReferencePageMode::sv48},
        {memblock::ReferencePageMode::sv48,
         memblock::ReferencePageMode::sv39},
        {memblock::ReferencePageMode::sv48,
         memblock::ReferencePageMode::sv48},
    }};
    for (unsigned index = 0; index < translation_modes.size(); ++index) {
        const auto pair = translation_modes[index];
        if (!run_load_case(
                "mode-pair-hlv", memblock::LoadOp::hlvd,
                rw_supervisor, g_rw_user,
                memblock::ReferencePrivilegeMode::supervisor,
                false, false, false, false, pair.vs_mode, pair.g_mode) ||
            !run_load_case(
                "mode-pair-hlvx", memblock::LoadOp::hlvxwu,
                x_supervisor, g_x_user,
                memblock::ReferencePrivilegeMode::supervisor,
                false, false, false, false, pair.vs_mode, pair.g_mode) ||
            !run_store_case(
                "mode-pair-hsv", memblock::StoreOp::hsvd,
                rw_supervisor, g_rw_user,
                memblock::ReferencePrivilegeMode::supervisor, false,
                0x3141592653589700ULL + index,
                pair.vs_mode, pair.g_mode)) {
            return 1;
        }
        ++translation_mode_pairs;
        mode_family_cases += 3;
    }

    struct PbmtPair {
        memblock::ReferencePbmt vs_pbmt;
        memblock::ReferencePbmt g_pbmt;
    };
    constexpr std::array<PbmtPair, 5> pbmt_pairs{{
        {memblock::ReferencePbmt::pma, memblock::ReferencePbmt::pma},
        {memblock::ReferencePbmt::pma, memblock::ReferencePbmt::nc},
        {memblock::ReferencePbmt::pma, memblock::ReferencePbmt::io},
        {memblock::ReferencePbmt::nc, memblock::ReferencePbmt::io},
        {memblock::ReferencePbmt::io, memblock::ReferencePbmt::nc},
    }};
    for (unsigned index = 0; index < pbmt_pairs.size(); ++index) {
        const auto pair = pbmt_pairs[index];
        if (!run_load_case(
                "pbmt-hlv", memblock::LoadOp::hlvd,
                rw_supervisor, g_rw_user,
                memblock::ReferencePrivilegeMode::supervisor,
                false, false, false, false,
                memblock::ReferencePageMode::sv48,
                memblock::ReferencePageMode::sv48,
                pair.vs_pbmt, pair.g_pbmt) ||
            !run_load_case(
                "pbmt-hlvx", memblock::LoadOp::hlvxwu,
                x_supervisor, g_x_user,
                memblock::ReferencePrivilegeMode::supervisor,
                false, false, false, false,
                memblock::ReferencePageMode::sv48,
                memblock::ReferencePageMode::sv48,
                pair.vs_pbmt, pair.g_pbmt) ||
            !run_store_case(
                "pbmt-hsv", memblock::StoreOp::hsvd,
                rw_supervisor, g_rw_user,
                memblock::ReferencePrivilegeMode::supervisor, false,
                0x2718281828459000ULL + index,
                memblock::ReferencePageMode::sv48,
                memblock::ReferencePageMode::sv48,
                pair.vs_pbmt, pair.g_pbmt)) {
            return 1;
        }
        ++pbmt_combinations;
        pbmt_family_cases += 3;
    }

    if (!run_load_case(
            "misaligned-hlv", memblock::LoadOp::hlvd,
            rw_supervisor, g_rw_user,
            memblock::ReferencePrivilegeMode::supervisor,
            false, false, false, false,
            memblock::ReferencePageMode::sv48,
            memblock::ReferencePageMode::sv48,
            memblock::ReferencePbmt::pma,
            memblock::ReferencePbmt::pma, 0x183) ||
        !run_load_case(
            "misaligned-hlvx", memblock::LoadOp::hlvxwu,
            x_supervisor, g_x_user,
            memblock::ReferencePrivilegeMode::supervisor,
            false, false, false, false,
            memblock::ReferencePageMode::sv48,
            memblock::ReferencePageMode::sv48,
            memblock::ReferencePbmt::pma,
            memblock::ReferencePbmt::pma, 0x181) ||
        !run_store_case(
            "misaligned-hsv", memblock::StoreOp::hsvd,
            rw_supervisor, g_rw_user,
            memblock::ReferencePrivilegeMode::supervisor, false,
            0x123456789abcdef0ULL,
            memblock::ReferencePageMode::sv48,
            memblock::ReferencePageMode::sv48,
            memblock::ReferencePbmt::pma,
            memblock::ReferencePbmt::pma, 0x283)) {
        return 1;
    }
    misaligned_family_cases += 3;

    struct HypervisorPmpCase {
        const char *name;
        std::uint8_t config;
        memblock::ReferencePrivilegeMode spvp;
        const memblock::ReferencePtePermissions *vs_load;
        const memblock::ReferencePtePermissions *vs_hlvx;
        bool locked;
    };
    const std::array<HypervisorPmpCase, 6> hypervisor_pmp_cases{{
        {"pmp-r", pmp_napot_read,
         memblock::ReferencePrivilegeMode::user, &rw_user, &x_user, false},
        {"pmp-x", pmp_napot_execute,
         memblock::ReferencePrivilegeMode::supervisor,
         &rw_supervisor, &x_supervisor, false},
        {"pmp-rw", pmp_napot_read_write,
         memblock::ReferencePrivilegeMode::user, &rw_user, &x_user, false},
        {"pmp-rx", pmp_napot_read_execute,
         memblock::ReferencePrivilegeMode::supervisor,
         &rw_supervisor, &x_supervisor, false},
        {"pmp-locked-r", pmp_locked_napot_read,
         memblock::ReferencePrivilegeMode::user, &rw_user, &x_user, true},
        {"pmp-locked-rwx", pmp_locked_napot_read_write_execute,
         memblock::ReferencePrivilegeMode::supervisor,
         &rw_supervisor, &x_supervisor, true},
    }};
    for (unsigned index = 0; index < hypervisor_pmp_cases.size(); ++index) {
        const auto &item = hypervisor_pmp_cases[index];
        const std::string hlv_name = std::string(item.name) + "-hlv";
        const std::string hlvx_name = std::string(item.name) + "-hlvx";
        const std::string hsv_name = std::string(item.name) + "-hsv";
        if (!run_load_case(
                hlv_name.c_str(), memblock::LoadOp::hlvd,
                *item.vs_load, g_rw_user, item.spvp,
                false, false, false, false,
                memblock::ReferencePageMode::sv39,
                memblock::ReferencePageMode::sv39,
                memblock::ReferencePbmt::pma,
                memblock::ReferencePbmt::pma, 0x180,
                item.config,
                memblock::ReferencePrivilegeMode::machine) ||
            !run_load_case(
                hlvx_name.c_str(), memblock::LoadOp::hlvxwu,
                *item.vs_hlvx, g_x_user, item.spvp,
                false, false, false, false,
                memblock::ReferencePageMode::sv39,
                memblock::ReferencePageMode::sv39,
                memblock::ReferencePbmt::pma,
                memblock::ReferencePbmt::pma, 0x184,
                item.config,
                memblock::ReferencePrivilegeMode::machine) ||
            !run_store_case(
                hsv_name.c_str(), memblock::StoreOp::hsvd,
                *item.vs_load, g_rw_user, item.spvp, false,
                0xface000000000000ULL + index,
                memblock::ReferencePageMode::sv39,
                memblock::ReferencePageMode::sv39,
                memblock::ReferencePbmt::pma,
                memblock::ReferencePbmt::pma, 0x280,
                item.config,
                memblock::ReferencePrivilegeMode::machine)) {
            return 1;
        }
        physical_pmp_cases += 3;
        locked_physical_pmp_cases += item.locked ? 3 : 0;
    }

    constexpr std::uint64_t pma_device_physical = 0x35000000ULL;
    if (!run_load_case(
            "pma-device-hlv", memblock::LoadOp::hlvd,
            rw_user, g_rw_user, memblock::ReferencePrivilegeMode::user,
            false, false, false, false,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePbmt::pma,
            memblock::ReferencePbmt::pma, 0x180, std::nullopt,
            memblock::ReferencePrivilegeMode::machine,
            pma_device_physical, true) ||
        !run_load_case(
            "pma-device-hlvx", memblock::LoadOp::hlvxwu,
            x_supervisor, g_x_user,
            memblock::ReferencePrivilegeMode::supervisor,
            false, false, false, false,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePbmt::pma,
            memblock::ReferencePbmt::pma, 0x184, std::nullopt,
            memblock::ReferencePrivilegeMode::machine,
            pma_device_physical, true) ||
        !run_store_case(
            "pma-device-hsv", memblock::StoreOp::hsvd,
            rw_user, g_rw_user, memblock::ReferencePrivilegeMode::user,
            false, 0x0123456789abcdefULL,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePbmt::pma,
            memblock::ReferencePbmt::pma, 0x280, std::nullopt,
            memblock::ReferencePrivilegeMode::machine,
            pma_device_physical, true)) {
        return 1;
    }
    physical_pma_cases += 3;

    constexpr std::uint64_t pma_device_boundary_page = 0x7ffff000ULL;
    constexpr std::uint64_t pma_ddr_boundary_page = 0x80000000ULL;
    if (!run_load_case(
            "pma-boundary-device-hlv", memblock::LoadOp::hlvd,
            rw_user, g_rw_user, memblock::ReferencePrivilegeMode::user,
            false, false, false, false,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePbmt::pma,
            memblock::ReferencePbmt::pma, 0xff8, std::nullopt,
            memblock::ReferencePrivilegeMode::machine,
            pma_device_boundary_page, true) ||
        !run_load_case(
            "pma-boundary-device-hlvx", memblock::LoadOp::hlvxwu,
            x_supervisor, g_x_user,
            memblock::ReferencePrivilegeMode::supervisor,
            false, false, false, false,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePbmt::pma,
            memblock::ReferencePbmt::pma, 0xffc, std::nullopt,
            memblock::ReferencePrivilegeMode::machine,
            pma_device_boundary_page, true) ||
        !run_store_case(
            "pma-boundary-device-hsv", memblock::StoreOp::hsvd,
            rw_user, g_rw_user, memblock::ReferencePrivilegeMode::user,
            false, 0xfedcba9876543210ULL,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePbmt::pma,
            memblock::ReferencePbmt::pma, 0xff8, std::nullopt,
            memblock::ReferencePrivilegeMode::machine,
            pma_device_boundary_page, true) ||
        !run_load_case(
            "pma-boundary-ddr-hlv", memblock::LoadOp::hlvd,
            rw_user, g_rw_user, memblock::ReferencePrivilegeMode::user,
            false, false, false, false,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePbmt::pma,
            memblock::ReferencePbmt::pma, 0, std::nullopt,
            memblock::ReferencePrivilegeMode::machine,
            pma_ddr_boundary_page, false) ||
        !run_load_case(
            "pma-boundary-ddr-hlvx", memblock::LoadOp::hlvxwu,
            x_supervisor, g_x_user,
            memblock::ReferencePrivilegeMode::supervisor,
            false, false, false, false,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePbmt::pma,
            memblock::ReferencePbmt::pma, 4, std::nullopt,
            memblock::ReferencePrivilegeMode::machine,
            pma_ddr_boundary_page, false) ||
        !run_store_case(
            "pma-boundary-ddr-hsv", memblock::StoreOp::hsvd,
            rw_user, g_rw_user, memblock::ReferencePrivilegeMode::user,
            false, 0x0f1e2d3c4b5a6978ULL,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePbmt::pma,
            memblock::ReferencePbmt::pma, 8, std::nullopt,
            memblock::ReferencePrivilegeMode::machine,
            pma_ddr_boundary_page, false)) {
        return 1;
    }
    physical_pma_cases += 6;
    physical_pma_boundary_cases += 6;

    std::cout << "MEMBLOCK_HYPERVISOR_CONTRACTS_PASS"
              << " cases=" << load_cases + store_cases
              << " load_cases=" << load_cases
              << " store_cases=" << store_cases
              << " page_faults=" << page_faults
              << " guest_page_faults=" << guest_page_faults
              << " access_faults=" << access_faults
              << " translation_mode_pairs=" << translation_mode_pairs
              << " mode_family_cases=" << mode_family_cases
              << " pbmt_combinations=" << pbmt_combinations
              << " pbmt_family_cases=" << pbmt_family_cases
              << " misaligned_family_cases=" << misaligned_family_cases
              << " physical_pmp_cases=" << physical_pmp_cases
              << " locked_physical_pmp_cases="
              << locked_physical_pmp_cases
              << " physical_pma_cases=" << physical_pma_cases
              << " physical_pma_boundary_cases="
              << physical_pma_boundary_cases
              << " spvp=1 vsum=1 vmxr=1 hlvx=1 hsv=1 pmp_x=1"
              << " machine_spvp_pmp=1 locked_spvp_pmp=1"
              << " cycles=" << total_cycles
              << " ptw_requests=" << total_ptw_requests
              << " dcache_a=" << total_dcache_requests
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_pointer_masking_contracts(int argc, char **argv)
{
    constexpr std::uint64_t tag7 = 0x5aULL << 57;
    constexpr std::uint64_t tag16 = 0xabcdULL << 48;
    constexpr std::uint64_t physical_base = 0xc6000000ULL;
    constexpr std::uint64_t virtual_base = 0x62000000ULL;

    unsigned cases = 0;
    unsigned faults = 0;
    std::uint64_t total_cycles = 0;
    std::uint64_t total_dcache_requests = 0;
    std::uint64_t total_ptw_requests = 0;

    auto execute_load = [&](
                            memblock::Environment &environment,
                            const memblock::LoadTransaction &transaction,
                            const char *phase) {
        const std::uint64_t dcache_before = environment.tilelink_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 4096) ||
            !environment.run_until_complete(32768) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << phase
                      << " reason=" << environment.error() << '\n';
            return false;
        }
        if (transaction.expected_exception_mask != 0 &&
            (environment.tilelink_requests() != dcache_before ||
             environment.uncache_requests() != uncache_before)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << phase
                      << " reason=unmasked-fault-reached-data-manager"
                      << " dcache_before=" << dcache_before
                      << " dcache_after=" << environment.tilelink_requests()
                      << " uncache_before=" << uncache_before
                      << " uncache_after=" << environment.uncache_requests()
                      << '\n';
            return false;
        }
        ++cases;
        faults += transaction.expected_exception_mask != 0;
        total_cycles += environment.cycle();
        total_dcache_requests += environment.tilelink_requests();
        total_ptw_requests += environment.ptw_requests();
        return true;
    };

    struct ScalarCase {
        const char *name;
        memblock::ReferencePageMode page_mode;
        memblock::PointerMaskingMode pointer_mode;
        bool mxr;
        std::uint32_t exception;
        bool high_half;
    };
    const std::array<ScalarCase, 7> scalar_cases{{
        {"m-bare-pmlen7", memblock::ReferencePageMode::bare,
         memblock::PointerMaskingMode::pmlen7, false, 0, false},
        {"m-bare-pmlen16", memblock::ReferencePageMode::bare,
         memblock::PointerMaskingMode::pmlen16, false, 0, false},
        {"m-bare-disabled", memblock::ReferencePageMode::bare,
         memblock::PointerMaskingMode::disabled, false,
         memblock::kExceptionLoadAccessFault, false},
        {"sv39-pmlen7", memblock::ReferencePageMode::sv39,
         memblock::PointerMaskingMode::pmlen7, false, 0, false},
        {"sv48-pmlen16", memblock::ReferencePageMode::sv48,
         memblock::PointerMaskingMode::pmlen16, false, 0, false},
        {"sv48-pmlen16-high", memblock::ReferencePageMode::sv48,
         memblock::PointerMaskingMode::pmlen16, false, 0, true},
        {"sv39-mxr-exempt", memblock::ReferencePageMode::sv39,
         memblock::PointerMaskingMode::pmlen16, true,
         memblock::kExceptionLoadPageFault, false},
    }};

    for (std::size_t index = 0; index < scalar_cases.size(); ++index) {
        const auto &test = scalar_cases[index];
        memblock::Environment environment(argc, argv);
        const std::uint64_t physical = physical_base + index * 0x2000ULL;
        const std::uint64_t virtual_address = test.high_half
            ? 0xffff800062000000ULL
            : virtual_base + index * 0x2000ULL;
        const std::uint64_t tag = test.pointer_mode ==
                memblock::PointerMaskingMode::pmlen7
            ? tag7
            : tag16;
        const unsigned retained_bits = test.pointer_mode ==
                memblock::PointerMaskingMode::pmlen7
            ? 57U
            : 48U;
        const std::uint64_t retained_mask =
            (std::uint64_t{1} << retained_bits) - 1U;
        const std::uint64_t untagged = test.page_mode ==
                memblock::ReferencePageMode::bare
            ? physical + 0x180
            : virtual_address + 0x180;
        const std::uint64_t tagged = tag | (untagged & retained_mask);
        const bool translated = test.page_mode !=
            memblock::ReferencePageMode::bare;
        const std::uint64_t expected_effective =
            memblock::reference_pointer_mask(
                tagged, test.pointer_mode, translated);
        if (test.exception == 0 && expected_effective != untagged) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL phase="
                      << test.name << " reason=reference-transform"
                      << " tagged=0x" << std::hex << tagged
                      << " expected=0x" << untagged
                      << " actual=0x" << expected_effective << std::dec
                      << '\n';
            return 1;
        }
        environment.memory().fill_incrementing(physical, 0x1000, 0x31 + index);
        const std::uint64_t root = 0xb4000000ULL + index * 0x10000ULL;
        bool configured = environment.reset();
        if (configured && translated) {
            configured = test.page_mode == memblock::ReferencePageMode::sv48
                ? environment.map_sv48_4k(virtual_address, physical, root)
                : environment.map_sv39_4k(virtual_address, physical, root);
            configured = configured &&
                (test.page_mode == memblock::ReferencePageMode::sv48
                     ? environment.activate_sv48(root, 80 + index)
                     : environment.activate_sv39(root, 80 + index)) &&
                environment.set_translation_permissions(
                    memblock::ReferencePrivilegeMode::supervisor,
                    test.mxr);
        }
        memblock::PointerMaskingConfig config;
        if (translated) {
            config.supervisor = test.pointer_mode;
        } else {
            config.machine = test.pointer_mode;
        }
        configured = configured && environment.set_pointer_masking(config);
        if (!configured) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << test.name
                      << "-configuration reason=" << environment.error()
                      << '\n';
            return 1;
        }
        const memblock::LoadTransaction transaction{
            .address = tagged,
            .oracle_address = physical + 0x180,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = static_cast<std::uint8_t>(40 + index),
            .lane = static_cast<unsigned>(index % memblock::kScalarLoadLanes),
            .expected_exception_mask = test.exception,
        };
        if (!execute_load(environment, transaction, test.name)) {
            return 1;
        }
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t physical = physical_base + 0xe000;
        constexpr std::uint64_t tagged = tag16 | (physical + 0x180);
        environment.memory().fill_incrementing(physical, 0x1000, 0x61);
        memblock::PointerMaskingConfig config;
        config.machine = memblock::PointerMaskingMode::pmlen16;
        if (!environment.reset() || !environment.set_pointer_masking(config)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=fp-load-configuration"
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        const memblock::LoadTransaction transaction{
            .address = tagged,
            .oracle_address = physical + 0x180,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 59,
            .lane = 0,
            .rf_wen = false,
            .fp_wen = true,
        };
        if (!execute_load(environment, transaction, "fp-load")) {
            return 1;
        }
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t physical = physical_base + 0x10000;
        constexpr std::uint64_t tagged = tag16 | (physical + 0x188);
        constexpr std::uint64_t data = 0x8877665544332211ULL;
        environment.memory().fill_incrementing(physical, 0x1000, 0x71);
        memblock::PointerMaskingConfig config;
        config.machine = memblock::PointerMaskingMode::pmlen16;
        const memblock::StoreTransaction store{
            .address = tagged,
            .oracle_address = physical + 0x188,
            .data = data,
            .op = memblock::StoreOp::sd,
            .rob = 0,
            .sq = 0,
            .address_lane = 0,
            .data_lane = 1,
        };
        if (!environment.reset() || !environment.set_pointer_masking(config)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=scalar-store-configuration"
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        environment.expect_store(store);
        if (!environment.set_rob_head(store.rob) ||
            !environment.enqueue_store(store, 0) ||
            !environment.issue_store_address(store, 2048) ||
            !environment.issue_store_data(store, 2048) ||
            !environment.run_until_store_complete(16384) ||
            !environment.commit_store(store, 16384) ||
            !environment.run_until_sbuffer_empty(16384)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=scalar-store"
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        const memblock::LoadTransaction readback{
            .address = tagged,
            .oracle_address = physical + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 1,
            .lq = 0,
            .pdest = 60,
            .lane = 1,
        };
        environment.expect_load_data(readback, data);
        if (!environment.set_rob_head(readback.rob) ||
            !environment.enqueue_load(readback) ||
            !environment.issue_load(readback, 4096) ||
            !environment.run_until_complete(32768) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=scalar-store-readback"
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        ++cases;
        total_cycles += environment.cycle();
        total_dcache_requests += environment.tilelink_requests();
        total_ptw_requests += environment.ptw_requests();
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t physical = physical_base + 0x12000;
        constexpr std::uint64_t tagged = tag16 | (physical + 0x140);
        environment.memory().fill_incrementing(physical, 0x1000, 0x91);
        memblock::PointerMaskingConfig config;
        config.machine = memblock::PointerMaskingMode::pmlen16;
        memblock::VectorMemoryTransaction transaction{
            .address = tagged,
            .oracle_address = physical + 0x140,
            .eew = 0,
            .vl = 16,
            .rob = 0,
            .lq = 0,
            .pdest = 61,
            .lane = 0,
        };
        if (!environment.reset() || !environment.set_pointer_masking(config)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=vector-load-configuration"
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        environment.expect_vector(transaction);
        if (!environment.set_rob_head(transaction.rob) ||
            !environment.enqueue_vector(transaction) ||
            !environment.issue_vector(transaction, 4096) ||
            !environment.run_until_vector_complete(32768) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=vector-load"
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        ++cases;
        total_cycles += environment.cycle();
        total_dcache_requests += environment.tilelink_requests();
        total_ptw_requests += environment.ptw_requests();
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t physical = physical_base + 0x13000;
        constexpr std::uint64_t tagged = tag16 | (physical + 0x140);
        environment.memory().fill_incrementing(physical, 0x1000, 0xa1);
        memblock::PointerMaskingConfig config;
        config.machine = memblock::PointerMaskingMode::pmlen16;
        memblock::VectorMemoryTransaction store{
            .store = true,
            .address = tagged,
            .oracle_address = physical + 0x140,
            .eew = 0,
            .vl = 16,
            .rob = 0,
            .lq = 0,
            .sq = 0,
            .lane = 1,
        };
        for (unsigned byte = 0; byte < store.data.size(); ++byte) {
            store.data[byte] = static_cast<unsigned char>(0xc1 + byte * 3);
        }
        if (!environment.reset() || !environment.set_pointer_masking(config)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle()
                      << " phase=vector-store-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        environment.expect_vector(store);
        if (!environment.set_rob_head(store.rob, store.rob_flag) ||
            !environment.enqueue_vector(store) ||
            !environment.issue_vector(store, 4096) ||
            !environment.run_until_vector_complete_with_replays(store, 32768) ||
            !environment.commit_vector_store(store, 16384)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=vector-store reason="
                      << environment.error() << '\n';
            return 1;
        }
        memblock::VectorMemoryTransaction readback{
            .address = tagged,
            .oracle_address = physical + 0x140,
            .eew = 0,
            .vl = 16,
            .rob = 1,
            .lq = 0,
            .sq = 1,
            .pdest = 62,
            .lane = 0,
        };
        environment.expect_vector_data(readback, store.data);
        if (!environment.set_rob_head(readback.rob, readback.rob_flag) ||
            !environment.enqueue_vector(readback) ||
            !environment.issue_vector(readback, 4096) ||
            !environment.run_until_vector_complete(32768) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle()
                      << " phase=vector-store-readback reason="
                      << environment.error() << '\n';
            return 1;
        }
        ++cases;
        total_cycles += environment.cycle();
        total_dcache_requests += environment.tilelink_requests();
        total_ptw_requests += environment.ptw_requests();
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t physical = physical_base + 0x14000;
        constexpr std::uint64_t tagged = tag16 | (physical + 0x180);
        constexpr std::uint64_t operand = 0x0102030405060708ULL;
        environment.memory().fill_incrementing(physical, 0x1000, 0xb1);
        memblock::PointerMaskingConfig config;
        config.machine = memblock::PointerMaskingMode::pmlen16;
        const std::uint64_t old_value = environment.memory().expected_load(
            physical + 0x180, memblock::LoadOp::ld);
        const memblock::AtomicTransaction atomic{
            .address = tagged,
            .op = memblock::AtomicOp::amoadd_d,
            .data = operand,
            .rob = 0,
            .pdest = 62,
            .address_lane = 0,
            .data_lane = 1,
        };
        const memblock::LoadTransaction writeback{
            .address = tagged,
            .oracle_address = physical + 0x180,
            .op = memblock::LoadOp::ld,
            .rob = atomic.rob,
            .pdest = atomic.pdest,
        };
        if (!environment.reset() || !environment.set_pointer_masking(config)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=atomic-configuration"
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        environment.expect_load_data(writeback, old_value);
        if (!environment.issue_atomic(atomic, 4096) ||
            !environment.run_until_complete(32768)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=atomic"
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        environment.record_atomic_result(physical + 0x180, old_value + operand);
        const memblock::LoadTransaction readback{
            .address = tagged,
            .oracle_address = physical + 0x180,
            .op = memblock::LoadOp::ld,
            .rob = 1,
            .lq = 0,
            .pdest = 63,
            .lane = 1,
        };
        environment.expect_load_data(readback, old_value + operand);
        if (!environment.set_rob_head(readback.rob) ||
            !environment.enqueue_load(readback) ||
            !environment.issue_load(readback, 4096) ||
            !environment.run_until_complete(32768) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=atomic-readback"
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        ++cases;
        total_cycles += environment.cycle();
        total_dcache_requests += environment.tilelink_requests();
        total_ptw_requests += environment.ptw_requests();
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t physical = physical_base + 0x16000;
        constexpr std::uint64_t tagged = tag16 | (physical + 0x20);
        environment.memory().fill_incrementing(physical, 64, 0xc7);
        memblock::PointerMaskingConfig config;
        config.machine = memblock::PointerMaskingMode::pmlen16;
        const memblock::StoreTransaction cbo_zero{
            .address = tagged,
            .oracle_address = physical,
            .data = 0xdeadbeefcafef00dULL,
            .op = memblock::StoreOp::cbo_zero,
            .rob = 0,
            .sq = 0,
            .address_lane = 0,
            .data_lane = 1,
        };
        if (!environment.reset() || !environment.set_pointer_masking(config)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=cbo-zero-configuration"
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        const std::uint64_t sq_target = environment.sq_dequeued() + 1;
        environment.expect_store(cbo_zero);
        if (!environment.set_rob_head(cbo_zero.rob, cbo_zero.rob_flag) ||
            !environment.enqueue_store(cbo_zero, 0) ||
            !environment.issue_store_address(cbo_zero, 4096) ||
            !environment.issue_store_data(cbo_zero, 4096) ||
            !environment.commit_stores_through(cbo_zero, 1) ||
            !environment.run_until_store_complete(32768) ||
            !environment.run_until_sq_dequeued(sq_target, 16384)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=cbo-zero reason="
                      << environment.error() << '\n';
            return 1;
        }
        const memblock::LoadTransaction readback{
            .address = tag16 | physical,
            .oracle_address = physical,
            .op = memblock::LoadOp::ld,
            .rob = 1,
            .lq = 0,
            .pdest = 64,
            .lane = 0,
        };
        environment.expect_load_data(readback, 0);
        if (!environment.set_rob_head(readback.rob, readback.rob_flag) ||
            !environment.enqueue_load(readback) ||
            !environment.issue_load(readback, 4096) ||
            !environment.run_until_complete(32768) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle()
                      << " phase=cbo-zero-readback reason="
                      << environment.error() << '\n';
            return 1;
        }
        environment.record_committed_store(cbo_zero);
        ++cases;
        total_cycles += environment.cycle();
        total_dcache_requests += environment.tilelink_requests();
        total_ptw_requests += environment.ptw_requests();
    }

    auto run_nested_case = [&](
                               const char *name,
                               memblock::ReferencePrivilegeMode privilege,
                               bool hypervisor,
                               bool hlvx,
                               bool current_user) {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t guest_virtual = 0x64000000ULL;
        constexpr std::uint64_t guest_physical = 0x9f000000ULL;
        constexpr std::uint64_t host_physical = physical_base + 0x18000;
        constexpr std::uint64_t vs_root = 0xb6000000ULL;
        constexpr std::uint64_t g_root = 0xb8000000ULL;
        constexpr std::uint64_t tagged = tag16 | (guest_virtual + 0x180);
        const bool user = privilege == memblock::ReferencePrivilegeMode::user;
        const bool executable = hlvx;
        environment.memory().fill_incrementing(host_physical, 0x1000, 0xd1);
        bool configured = environment.reset() &&
            environment.map_sv39_leaf(
                guest_virtual, guest_physical, 0, vs_root,
                !hlvx, !hlvx, executable, user, false);
        for (unsigned page = 0; configured && page < 3; ++page) {
            const std::uint64_t address = vs_root + page * 0x1000ULL;
            configured = environment.map_sv39x4_4k(
                address, address, g_root, true, true, false);
        }
        configured = configured && environment.map_sv39x4_leaf(
            guest_physical, host_physical, 0, g_root,
            !hlvx, !hlvx, executable, true, std::nullopt, true) &&
            environment.activate_two_stage_modes(
                memblock::ReferencePageMode::sv39,
                memblock::ReferencePageMode::sv39,
                vs_root, g_root, 91, 92);
        if (hypervisor) {
            configured = configured &&
                environment.set_hypervisor_access_permissions(privilege);
            if (current_user) {
                configured = configured && environment.set_instruction_privilege(
                    memblock::ReferencePrivilegeMode::user);
            }
        } else {
            configured = configured && environment.set_translation_permissions(
                privilege);
        }
        memblock::PointerMaskingConfig config;
        if (hypervisor) {
            if (current_user) {
                config.hypervisor_user = memblock::PointerMaskingMode::pmlen16;
            } else if (user) {
                config.user = memblock::PointerMaskingMode::pmlen16;
            } else {
                config.virtual_supervisor =
                    memblock::PointerMaskingMode::pmlen16;
            }
        } else if (user) {
            config.user = memblock::PointerMaskingMode::pmlen16;
        } else {
            config.virtual_supervisor = memblock::PointerMaskingMode::pmlen16;
        }
        configured = configured && environment.set_pointer_masking(config);
        if (!configured) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << name
                      << "-configuration reason=" << environment.error()
                      << '\n';
            return false;
        }
        const memblock::LoadTransaction transaction{
            .address = tagged,
            .oracle_address = host_physical + 0x180,
            .op = hypervisor
                ? (hlvx ? memblock::LoadOp::hlvxwu : memblock::LoadOp::hlvd)
                : memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 70,
            .lane = 0,
            .expected_exception_mask = hlvx
                ? memblock::kExceptionLoadPageFault
                : 0U,
            // The TLB reports the non-canonical page fault.  The later HLVX
            // physical execute check may also see an invalid PMP address and
            // append the lower-priority access-fault bit; XiangShan's trap
            // priority still selects the required load-page-fault cause.
            .allowed_additional_exception_mask = hlvx
                ? memblock::kExceptionLoadAccessFault
                : 0U,
        };
        return execute_load(environment, transaction, name);
    };

    if (!run_nested_case(
            "nested-vs-henvcfg", memblock::ReferencePrivilegeMode::supervisor,
            false, false, false) ||
        !run_nested_case(
            "nested-vu-senvcfg", memblock::ReferencePrivilegeMode::user,
            false, false, false) ||
        !run_nested_case(
            "hlv-vs-henvcfg", memblock::ReferencePrivilegeMode::supervisor,
            true, false, false) ||
        !run_nested_case(
            "hlv-vu-senvcfg", memblock::ReferencePrivilegeMode::user,
            true, false, false) ||
        !run_nested_case(
            "hlv-u-hstatus", memblock::ReferencePrivilegeMode::user,
            true, false, true) ||
        !run_nested_case(
            "hlvx-exempt", memblock::ReferencePrivilegeMode::supervisor,
            true, true, false)) {
        return 1;
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t guest_virtual = 0x66000000ULL;
        constexpr std::uint64_t guest_physical = 0x9f200000ULL;
        constexpr std::uint64_t host_physical = physical_base + 0x1a000;
        constexpr std::uint64_t vs_root = 0xba000000ULL;
        constexpr std::uint64_t g_root = 0xbc000000ULL;
        constexpr std::uint64_t tagged = tag16 | (guest_virtual + 0x280);
        constexpr std::uint64_t data = 0x5a6b7c8d9eaf1021ULL;
        environment.memory().fill_incrementing(host_physical, 0x1000, 0xe1);
        bool configured = environment.reset() &&
            environment.map_sv39_leaf(
                guest_virtual, guest_physical, 0, vs_root,
                true, true, false, false, false);
        for (unsigned page = 0; configured && page < 3; ++page) {
            const std::uint64_t address = vs_root + page * 0x1000ULL;
            configured = environment.map_sv39x4_4k(
                address, address, g_root, true, true, false);
        }
        configured = configured && environment.map_sv39x4_leaf(
            guest_physical, host_physical, 0, g_root,
            true, true, false, true, std::nullopt, true) &&
            environment.activate_two_stage_modes(
                memblock::ReferencePageMode::sv39,
                memblock::ReferencePageMode::sv39,
                vs_root, g_root, 93, 94) &&
            environment.set_hypervisor_access_permissions(
                memblock::ReferencePrivilegeMode::supervisor);
        memblock::PointerMaskingConfig config;
        config.virtual_supervisor = memblock::PointerMaskingMode::pmlen16;
        configured = configured && environment.set_pointer_masking(config);
        if (!configured) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=hsv-configuration"
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        const memblock::StoreTransaction store{
            .address = tagged,
            .oracle_address = host_physical + 0x280,
            .data = data,
            .op = memblock::StoreOp::hsvd,
            .rob = 0,
            .sq = 0,
            .address_lane = 0,
            .data_lane = 1,
        };
        environment.expect_store(store);
        if (!environment.set_rob_head(store.rob, store.rob_flag) ||
            !environment.enqueue_store(store, 0) ||
            !environment.issue_store_address_until_tlb_hit(store, 16384) ||
            !environment.issue_store_data(store, 4096) ||
            !environment.run_until_store_complete(32768) ||
            !environment.commit_store(store, 16384) ||
            !environment.run_until_sbuffer_empty(16384)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=hsv-store reason="
                      << environment.error() << '\n';
            return 1;
        }
        const memblock::LoadTransaction readback{
            .address = tagged,
            .oracle_address = host_physical + 0x280,
            .op = memblock::LoadOp::hlvd,
            .rob = 1,
            .lq = 0,
            .pdest = 71,
            .lane = 1,
        };
        environment.expect_load_data(readback, data);
        if (!environment.set_rob_head(readback.rob, readback.rob_flag) ||
            !environment.enqueue_load(readback) ||
            !environment.issue_load(readback, 4096) ||
            !environment.run_until_complete(32768) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_POINTER_MASKING_CONTRACTS_FAIL cycle="
                      << environment.cycle()
                      << " phase=hsv-hlv-readback reason="
                      << environment.error() << '\n';
            return 1;
        }
        ++cases;
        total_cycles += environment.cycle();
        total_dcache_requests += environment.tilelink_requests();
        total_ptw_requests += environment.ptw_requests();
    }

    std::cout << "MEMBLOCK_POINTER_MASKING_CONTRACTS_PASS"
              << " cases=" << cases
              << " faults=" << faults
              << " pmlen7=1 pmlen16=1 bare=1 sv39=1 sv48=1 nested=1"
              << " scalar_load=1 scalar_store=1 fp_load=1"
              << " vector_load=1 vector_store=1 atomic=1 cbo_zero=1"
              << " hlv=1 hlvx_exempt=1 hsv=1 mxr_exempt=1"
              << " cycles=" << total_cycles
              << " dcache_a=" << total_dcache_requests
              << " ptw_requests=" << total_ptw_requests
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_pmp_contracts(int argc, char **argv)
{
    constexpr std::uint8_t pmp_tor_read = 0x09;
    constexpr std::uint8_t pmp_na4_read_write = 0x13;
    constexpr std::uint8_t pmp_napot_deny = 0x18;
    constexpr std::uint8_t pmp_napot_read_write = 0x1b;
    constexpr std::uint8_t pmp_locked_napot_deny = 0x98;
    auto napot_address = [](std::uint64_t base, std::uint64_t size) {
        return (base | (size / 2 - 1)) >> 2;
    };
    auto run_load = [](
                        memblock::Environment &environment,
                        const memblock::LoadTransaction &transaction,
                        const char *phase) {
        const std::uint64_t dcache_before = environment.tilelink_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        environment.expect_load(transaction);
        const bool completed =
            environment.set_rob_head(transaction.rob, transaction.rob_flag) &&
            environment.enqueue_load(transaction) &&
            environment.issue_load(transaction, 1024) &&
            environment.run_until_complete(8192) &&
            environment.run_until_lq_retired(2048);
        if (!completed) {
            std::cerr << "MEMBLOCK_PMP_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << phase
                      << " reason=" << environment.error() << '\n';
            return false;
        }
        if (transaction.expected_exception_mask != 0 &&
            (environment.tilelink_requests() != dcache_before ||
             environment.uncache_requests() != uncache_before)) {
            std::cerr << "MEMBLOCK_PMP_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=" << phase
                      << " reason=denied-load-reached-memory"
                      << " dcache_before=" << dcache_before
                      << " dcache_after=" << environment.tilelink_requests()
                      << " uncache_before=" << uncache_before
                      << " uncache_after=" << environment.uncache_requests()
                      << '\n';
            return false;
        }
        return true;
    };
    auto configure_supervisor_bare = [](
                                         memblock::Environment &environment,
                                         const std::vector<std::uint64_t> &addresses,
                                         const std::vector<std::uint8_t> &configs) {
        return environment.reset() && environment.activate_bare() &&
            environment.configure_pmp(addresses, configs);
    };

    std::uint64_t total_cycles = 0;
    std::uint64_t total_load_writebacks = 0;
    std::uint64_t total_store_writebacks = 0;
    std::uint64_t total_dcache_requests = 0;
    unsigned allowed_cases = 0;
    unsigned denied_cases = 0;

    // TOR entry 1 uses entry 0's address as its lower bound. Entry 0 stays
    // OFF, and unmatched S-mode accesses are denied by the PMP default.
    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t lower = 0x80200000ULL;
        constexpr std::uint64_t upper = lower + 0x2000;
        environment.memory().fill_incrementing(lower - 0x1000, 0x4000, 0x31);
        if (!configure_supervisor_bare(
                environment, {lower >> 2, upper >> 2}, {0, pmp_tor_read})) {
            std::cerr << "MEMBLOCK_PMP_CONTRACTS_FAIL cycle="
                      << environment.cycle()
                      << " phase=tor-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        const std::array<memblock::LoadTransaction, 4> loads{{
            memblock::LoadTransaction{
                .address = lower,
                .op = memblock::LoadOp::lbu,
                .rob = 0, .lq = 0, .pdest = 20, .lane = 0,
            },
            memblock::LoadTransaction{
                .address = upper - 1,
                .op = memblock::LoadOp::lbu,
                .rob = 1, .lq = 1, .pdest = 21, .lane = 1,
            },
            memblock::LoadTransaction{
                .address = lower - 1,
                .op = memblock::LoadOp::lbu,
                .rob = 2, .lq = 2, .pdest = 22, .lane = 2,
                .expected_exception_mask = memblock::kExceptionLoadAccessFault,
            },
            memblock::LoadTransaction{
                .address = upper,
                .op = memblock::LoadOp::lbu,
                .rob = 3, .lq = 3, .pdest = 23, .lane = 0,
                .expected_exception_mask = memblock::kExceptionLoadAccessFault,
            },
        }};
        const std::array<const char *, 4> phases{{
            "tor-lower", "tor-upper-minus-one", "tor-below", "tor-upper",
        }};
        for (unsigned index = 0; index < loads.size(); ++index) {
            if (!run_load(environment, loads[index], phases[index])) {
                return 1;
            }
            if (loads[index].expected_exception_mask == 0) {
                ++allowed_cases;
            } else {
                ++denied_cases;
            }
        }

        const memblock::AtomicTransaction atomic{
            .address = lower + 0x180,
            .op = memblock::AtomicOp::amoadd_d,
            .data = 0x0102030405060708ULL,
            .rob = 4,
            .pdest = 24,
            .address_lane = 0,
            .data_lane = 1,
        };
        const memblock::LoadTransaction atomic_writeback{
            .address = atomic.address,
            .op = memblock::LoadOp::ld,
            .rob = atomic.rob,
            .pdest = atomic.pdest,
            .expected_exception_mask = memblock::kExceptionStoreAccessFault,
        };
        environment.expect_load_data(atomic_writeback, 0);
        const std::uint64_t dcache_before_atomic = environment.tilelink_requests();
        const std::uint64_t uncache_before_atomic = environment.uncache_requests();
        if (!environment.set_rob_head(atomic.rob) ||
            !environment.issue_atomic(atomic, 1024) ||
            !environment.run_until_complete(8192) ||
            environment.tilelink_requests() != dcache_before_atomic ||
            environment.uncache_requests() != uncache_before_atomic) {
            std::cerr << "MEMBLOCK_PMP_CONTRACTS_FAIL cycle="
                      << environment.cycle()
                      << " phase=tor-atomic-write-denied reason="
                      << environment.error() << '\n';
            return 1;
        }
        ++denied_cases;

        const memblock::StoreTransaction store{
            .address = lower + 0x280,
            .data = 0x8877665544332211ULL,
            .op = memblock::StoreOp::sd,
            .rob = 5,
            .sq = 0,
            .address_lane = 1,
            .data_lane = 0,
            .expected_exception_mask = memblock::kExceptionStoreAccessFault,
        };
        environment.expect_store(store);
        const std::uint64_t dcache_before_store = environment.tilelink_requests();
        const std::uint64_t uncache_before_store = environment.uncache_requests();
        if (!environment.set_rob_head(store.rob) ||
            !environment.enqueue_store(store, 0) ||
            !environment.issue_store_address(store, 1024) ||
            !environment.run_until_store_complete(8192) ||
            environment.tilelink_requests() != dcache_before_store ||
            environment.uncache_requests() != uncache_before_store ||
            !environment.account_sq_cancellation(1)) {
            std::cerr << "MEMBLOCK_PMP_CONTRACTS_FAIL cycle="
                      << environment.cycle()
                      << " phase=tor-store-write-denied reason="
                      << environment.error() << '\n';
            return 1;
        }
        ++denied_cases;
        total_cycles += environment.cycle();
        total_load_writebacks += environment.writebacks();
        total_store_writebacks += environment.store_writebacks();
        total_dcache_requests += environment.tilelink_requests();
    }

    // With a 4-KiB platform grain, requested NA4 is WARL-coerced to NAPOT.
    // A 4-KiB NAPOT address verifies the minimum region and both permission
    // bits at the first and last naturally aligned doubleword.
    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t base = 0x80400000ULL;
        constexpr std::uint64_t size = 0x1000;
        environment.memory().fill_incrementing(base - size, size * 3, 0x52);
        if (!configure_supervisor_bare(
                environment,
                {napot_address(base, size)},
                {pmp_na4_read_write})) {
            std::cerr << "MEMBLOCK_PMP_CONTRACTS_FAIL cycle="
                      << environment.cycle()
                      << " phase=coarse-na4-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        const memblock::LoadTransaction first{
            .address = base,
            .op = memblock::LoadOp::ld,
            .rob = 0, .lq = 0, .pdest = 30, .lane = 0,
        };
        const memblock::LoadTransaction last{
            .address = base + size - 8,
            .op = memblock::LoadOp::ld,
            .rob = 1, .lq = 1, .pdest = 31, .lane = 1,
        };
        if (!run_load(environment, first, "coarse-na4-lower") ||
            !run_load(environment, last, "coarse-na4-upper")) {
            return 1;
        }
        allowed_cases += 2;
        const memblock::StoreTransaction store{
            .address = base + 0x180,
            .data = 0xa5a55a5af00dcafeULL,
            .op = memblock::StoreOp::sd,
            .rob = 2,
            .sq = 0,
            .address_lane = 0,
            .data_lane = 1,
        };
        environment.expect_store(store);
        if (!environment.set_rob_head(store.rob) ||
            !environment.enqueue_store(store, 0) ||
            !environment.issue_store_address(store, 1024) ||
            !environment.issue_store_data(store, 1024) ||
            !environment.run_until_store_complete(8192) ||
            !environment.commit_store(store, 8192)) {
            std::cerr << "MEMBLOCK_PMP_CONTRACTS_FAIL cycle="
                      << environment.cycle()
                      << " phase=coarse-na4-store reason="
                      << environment.error() << '\n';
            return 1;
        }
        ++allowed_cases;
        const memblock::LoadTransaction readback{
            .address = store.address,
            .op = memblock::LoadOp::ld,
            .rob = 3, .lq = 2, .pdest = 32, .lane = 2,
        };
        if (!run_load(environment, readback, "coarse-na4-store-readback")) {
            return 1;
        }
        ++allowed_cases;
        const std::array<memblock::LoadTransaction, 2> outside{{
            memblock::LoadTransaction{
                .address = base - 8,
                .op = memblock::LoadOp::ld,
                .rob = 4, .lq = 3, .pdest = 33, .lane = 0,
                .expected_exception_mask = memblock::kExceptionLoadAccessFault,
            },
            memblock::LoadTransaction{
                .address = base + size,
                .op = memblock::LoadOp::ld,
                .rob = 5, .lq = 4, .pdest = 34, .lane = 1,
                .expected_exception_mask = memblock::kExceptionLoadAccessFault,
            },
        }};
        if (!run_load(environment, outside[0], "coarse-na4-below") ||
            !run_load(environment, outside[1], "coarse-na4-above")) {
            return 1;
        }
        denied_cases += 2;
        total_cycles += environment.cycle();
        total_load_writebacks += environment.writebacks();
        total_store_writebacks += environment.store_writebacks();
        total_dcache_requests += environment.tilelink_requests();
    }

    // Lower-numbered entries have priority. A deny-all 4-KiB entry shadows
    // the first half of an otherwise readable/writable 8-KiB NAPOT entry.
    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t base = 0x80600000ULL;
        environment.memory().fill_incrementing(base, 0x2000, 0x73);
        if (!configure_supervisor_bare(
                environment,
                {napot_address(base, 0x1000), napot_address(base, 0x2000)},
                {pmp_napot_deny, pmp_napot_read_write})) {
            std::cerr << "MEMBLOCK_PMP_CONTRACTS_FAIL cycle="
                      << environment.cycle()
                      << " phase=priority-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        const memblock::LoadTransaction shadowed{
            .address = base + 0x100,
            .op = memblock::LoadOp::ld,
            .rob = 0, .lq = 0, .pdest = 40, .lane = 0,
            .expected_exception_mask = memblock::kExceptionLoadAccessFault,
        };
        const memblock::LoadTransaction visible{
            .address = base + 0x1100,
            .op = memblock::LoadOp::ld,
            .rob = 1, .lq = 1, .pdest = 41, .lane = 1,
        };
        if (!run_load(environment, shadowed, "priority-shadowed") ||
            !run_load(environment, visible, "priority-visible")) {
            return 1;
        }
        ++denied_cases;
        ++allowed_cases;
        total_cycles += environment.cycle();
        total_load_writebacks += environment.writebacks();
        total_dcache_requests += environment.tilelink_requests();
    }

    // Unlocked PMP entries are bypassed in M mode. Locking the same entry
    // enforces it in M mode and makes both its address and config immutable.
    {
        memblock::Environment unlocked(argc, argv);
        constexpr std::uint64_t base = 0x80800000ULL;
        unlocked.memory().fill_incrementing(base, 0x1000, 0x94);
        if (!unlocked.reset() ||
            !unlocked.configure_pmp(
                {napot_address(base, 0x1000)}, {pmp_napot_deny})) {
            std::cerr << "MEMBLOCK_PMP_CONTRACTS_FAIL cycle="
                      << unlocked.cycle()
                      << " phase=machine-unlocked-configuration reason="
                      << unlocked.error() << '\n';
            return 1;
        }
        const memblock::LoadTransaction bypassed{
            .address = base + 0x80,
            .op = memblock::LoadOp::ld,
            .rob = 0, .lq = 0, .pdest = 50, .lane = 0,
        };
        if (!run_load(unlocked, bypassed, "machine-unlocked-bypass")) {
            return 1;
        }
        ++allowed_cases;
        total_cycles += unlocked.cycle();
        total_load_writebacks += unlocked.writebacks();
        total_dcache_requests += unlocked.tilelink_requests();

        memblock::Environment locked(argc, argv);
        constexpr std::uint64_t replacement = base + 0x2000;
        locked.memory().fill_incrementing(base, 0x3000, 0xb5);
        if (!locked.reset() ||
            !locked.configure_pmp(
                {napot_address(base, 0x1000)}, {pmp_locked_napot_deny}) ||
            !locked.configure_pmp(
                {napot_address(replacement, 0x1000)},
                {pmp_napot_read_write})) {
            std::cerr << "MEMBLOCK_PMP_CONTRACTS_FAIL cycle="
                      << locked.cycle()
                      << " phase=machine-lock-configuration reason="
                      << locked.error() << '\n';
            return 1;
        }
        const memblock::LoadTransaction original{
            .address = base + 0x80,
            .op = memblock::LoadOp::ld,
            .rob = 0, .lq = 0, .pdest = 51, .lane = 1,
            .expected_exception_mask = memblock::kExceptionLoadAccessFault,
        };
        const memblock::LoadTransaction rewritten{
            .address = replacement + 0x80,
            .op = memblock::LoadOp::ld,
            .rob = 1, .lq = 1, .pdest = 52, .lane = 2,
        };
        if (!run_load(locked, original, "machine-locked-original") ||
            !run_load(locked, rewritten, "machine-locked-rewrite-rejected")) {
            return 1;
        }
        ++denied_cases;
        ++allowed_cases;
        total_cycles += locked.cycle();
        total_load_writebacks += locked.writebacks();
        total_dcache_requests += locked.tilelink_requests();
    }

    std::cout << "MEMBLOCK_PMP_CONTRACTS_PASS"
              << " cycles=" << total_cycles
              << " cases=" << allowed_cases + denied_cases
              << " allowed=" << allowed_cases
              << " denied=" << denied_cases
              << " load_writebacks=" << total_load_writebacks
              << " store_writebacks=" << total_store_writebacks
              << " dcache_a=" << total_dcache_requests
              << " tor=1 napot=1 coarse_na4=1 priority=1 lock=1"
              << " machine_bypass=1 atomic_denied=1"
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_l2_tlb_contracts(int argc, char **argv)
{
    struct RefilledResponseExpectation {
        const char *phase = nullptr;
        std::uint64_t vaddr = 0;
        std::optional<std::uint64_t> paddr;
        std::optional<std::uint8_t> pbmt;
        bool page_fault = false;
        bool guest_page_fault = false;
        bool access_fault = false;
        bool check_protection = false;
        bool pmp_load_denied = false;
        bool pmp_mmio = false;
        bool is_prefetch = false;
    };
    const auto expect_refilled_response = [](
        memblock::Environment &candidate,
        const RefilledResponseExpectation &expected) {
        memblock::Environment::L2TlbResponse cold{};
        if (!candidate.issue_l2_tlb_request(
                expected.vaddr, 0, false, expected.is_prefetch, false, cold) ||
            !cold.miss) {
            std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                      << candidate.cycle() << " phase=" << expected.phase
                      << "-miss reason=" << candidate.error()
                      << " miss=" << cold.miss << '\n';
            return false;
        }
        if (!candidate.run_cycles(256)) {
            std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                      << candidate.cycle() << " phase=" << expected.phase
                      << "-refill-drain reason=" << candidate.error() << '\n';
            return false;
        }
        const std::uint64_t ptw_a_before_retry = candidate.ptw_requests();
        memblock::Environment::L2TlbResponse response{};
        const bool received = candidate.issue_l2_tlb_request(
            expected.vaddr, 0, false, expected.is_prefetch, false, response);
        const bool payload_mismatch =
            response.miss || response.page_fault != expected.page_fault ||
            response.guest_page_fault != expected.guest_page_fault ||
            response.access_fault != expected.access_fault ||
            (expected.paddr && response.paddr != *expected.paddr) ||
            (expected.pbmt && response.pbmt != *expected.pbmt) ||
            (expected.check_protection &&
             (response.pmp_load_denied != expected.pmp_load_denied ||
              response.pmp_mmio != expected.pmp_mmio)) ||
            candidate.ptw_requests() != ptw_a_before_retry;
        if (!received || payload_mismatch) {
            std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                      << candidate.cycle() << " phase=" << expected.phase
                      << "-hit reason=" << candidate.error()
                      << " miss=" << response.miss
                      << " paddr=0x" << std::hex << response.paddr << std::dec
                      << " pbmt=" << static_cast<unsigned>(response.pbmt)
                      << " pf=" << response.page_fault
                      << " gpf=" << response.guest_page_fault
                      << " af=" << response.access_fault
                      << " pmp_ld=" << response.pmp_load_denied
                      << " pmp_mmio=" << response.pmp_mmio
                      << " ptw_a_before=" << ptw_a_before_retry
                      << " ptw_a_after=" << candidate.ptw_requests() << '\n';
            return false;
        }
        return true;
    };

    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_base = 0x50056000ULL;
    constexpr std::uint64_t physical_base = 0xa0056000ULL;
    constexpr std::uint64_t root = 0x97024000ULL;
    environment.memory().fill_incrementing(physical_base, 0x1000, 0x71);
    if (!environment.reset() ||
        !environment.map_sv39_4k(
            virtual_base, physical_base, root, true, true, false, false, false) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    // Warm the ordinary DTLB through a real load first.  The L2-to-L1 port is
    // a separate requestor, so its first lookup must still miss.  That miss
    // also starts a PTW walk which refills the shared prefetch TLB; retrying
    // this port then exercises the real hit payload and PMP/PMA classifier.
    const memblock::LoadTransaction warm{
        .address = virtual_base + 0x18,
        .oracle_address = physical_base + 0x18,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .pdest = 154,
        .lane = 0,
    };
    environment.expect_load(warm);
    if (!environment.set_rob_head(warm.rob) ||
        !environment.enqueue_load(warm) ||
        !environment.issue_load(warm, 2048) ||
        !environment.run_until_complete(8192) ||
        !environment.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=warm-load reason="
                  << environment.error() << '\n';
        return 1;
    }

    for (std::uint8_t source_id = 0; source_id < 16; ++source_id) {
        if (!environment.pulse_l2_hint(source_id, false) ||
            !environment.pulse_l2_hint(source_id, true)) {
            std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=l2-hint source="
                      << static_cast<unsigned>(source_id) << " reason="
                      << environment.error() << '\n';
            return 1;
        }
    }

    memblock::Environment::L2TlbResponse ordinary_miss;
    if (!environment.issue_l2_tlb_request(
            warm.address, 0, false, false, false, ordinary_miss) ||
        !ordinary_miss.miss) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=l1-miss-response reason="
                  << environment.error() << " miss=" << ordinary_miss.miss
                  << " paddr=0x" << std::hex << ordinary_miss.paddr
                  << std::dec << " pf=" << ordinary_miss.page_fault
                  << " gpf=" << ordinary_miss.guest_page_fault
                  << " af=" << ordinary_miss.access_fault
                  << " pmp_ld=" << ordinary_miss.pmp_load_denied
                  << " pmp_mmio=" << ordinary_miss.pmp_mmio << '\n';
        return 1;
    }

    if (!environment.run_cycles(128)) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=l1-refill-drain reason="
                  << environment.error() << '\n';
        return 1;
    }
    const std::uint64_t ptw_requests_before_hit = environment.ptw_requests();
    memblock::Environment::L2TlbResponse warm_hit;
    if (!environment.issue_l2_tlb_request(
            warm.address, 0, false, false, false, warm_hit) ||
        warm_hit.miss || warm_hit.page_fault || warm_hit.guest_page_fault ||
        warm_hit.access_fault ||
        warm_hit.paddr != warm.oracle_address.value() || warm_hit.pbmt != 0 ||
        warm_hit.pmp_load_denied || warm_hit.pmp_mmio ||
        environment.ptw_requests() != ptw_requests_before_hit) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=l1-hit-response reason="
                  << environment.error() << " miss=" << warm_hit.miss
                  << " paddr=0x" << std::hex << warm_hit.paddr
                  << " expected=0x" << warm.oracle_address.value() << std::dec
                  << " pbmt=" << static_cast<unsigned>(warm_hit.pbmt)
                  << " pf=" << warm_hit.page_fault
                  << " gpf=" << warm_hit.guest_page_fault
                  << " af=" << warm_hit.access_fault
                  << " pmp_ld=" << warm_hit.pmp_load_denied
                  << " pmp_mmio=" << warm_hit.pmp_mmio
                  << " ptw_before=" << ptw_requests_before_hit
                  << " ptw_after=" << environment.ptw_requests() << '\n';
        return 1;
    }

    // no_translate still goes through the PMP/PMA checker.  The generated
    // MemBlock boundary does not retain the optional pmp_addr payload, so the
    // top-level request observes the documented zero physical-address input;
    // it must nevertheless complete without a translation miss or fault.
    memblock::Environment::L2TlbResponse no_translate;
    if (!environment.issue_l2_tlb_request(
            0xa0056018ULL, 0, false, false, true, no_translate) ||
        no_translate.miss || no_translate.page_fault ||
        no_translate.guest_page_fault || no_translate.access_fault ||
        no_translate.paddr != 0 || no_translate.pbmt != 0 ||
        no_translate.pmp_load_denied || !no_translate.pmp_mmio) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=no-translate reason="
                  << environment.error() << " miss=" << no_translate.miss
                  << " paddr=0x" << std::hex << no_translate.paddr
                  << std::dec << " pbmt=" << static_cast<unsigned>(no_translate.pbmt)
                  << " pf=" << no_translate.page_fault
                  << " gpf=" << no_translate.guest_page_fault
                  << " af=" << no_translate.access_fault
                  << " pmp_ld=" << no_translate.pmp_load_denied
                  << " pmp_mmio=" << no_translate.pmp_mmio << '\n';
        return 1;
    }

    if (!environment.issue_killed_l2_tlb_request(
            warm.address, 0, true, false, 128)) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=killed-request reason="
                  << environment.error() << '\n';
        return 1;
    }

    memblock::Environment::L2TlbResponse miss;
    const std::uint64_t unmapped = virtual_base + 0x2000;
    if (!environment.issue_l2_tlb_request(
            unmapped, 0, false, true, false, miss) || !miss.miss) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=cold-miss reason="
                  << environment.error() << " miss=" << miss.miss
                  << " paddr=0x" << std::hex << miss.paddr << std::dec
                  << " pf=" << miss.page_fault
                  << " gpf=" << miss.guest_page_fault
                  << " af=" << miss.access_fault
                  << " pmp_ld=" << miss.pmp_load_denied
                  << " pmp_mmio=" << miss.pmp_mmio << '\n';
        return 1;
    }

    // PMP reconfiguration does not invalidate the PFTLB entry.  Deny exactly
    // the translated 4-KiB page and re-query the resident translation so the
    // retained PMP output is checked against a real, non-MMIO physical address.
    const auto napot_address = [](std::uint64_t base, std::uint64_t size) {
        return (base | (size / 2 - 1)) >> 2;
    };
    constexpr std::uint8_t pmp_locked_napot_deny = 0x98;
    memblock::Environment::L2TlbResponse pmp_denied;
    if (!environment.configure_pmp(
            {napot_address(physical_base, 0x1000)},
            {pmp_locked_napot_deny})) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=pmp-configuration reason="
                  << environment.error() << '\n';
        return 1;
    }
    const std::uint64_t ptw_requests_before_pmp_hit =
        environment.ptw_requests();
    if (!environment.issue_l2_tlb_request(
            warm.address, 0, false, false, false, pmp_denied) ||
        pmp_denied.miss || pmp_denied.page_fault ||
        pmp_denied.guest_page_fault || pmp_denied.access_fault ||
        pmp_denied.paddr != warm.oracle_address.value() ||
        pmp_denied.pbmt != 0 || !pmp_denied.pmp_load_denied ||
        pmp_denied.pmp_mmio ||
        environment.ptw_requests() != ptw_requests_before_pmp_hit) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=pmp-denied reason="
                  << environment.error() << " miss=" << pmp_denied.miss
                  << " paddr=0x" << std::hex << pmp_denied.paddr << std::dec
                  << " pf=" << pmp_denied.page_fault
                  << " gpf=" << pmp_denied.guest_page_fault
                  << " af=" << pmp_denied.access_fault
                  << " pmp_ld=" << pmp_denied.pmp_load_denied
                  << " pmp_mmio=" << pmp_denied.pmp_mmio
                  << " ptw_before=" << ptw_requests_before_pmp_hit
                  << " ptw_after=" << environment.ptw_requests() << '\n';
        return 1;
    }

    memblock::Environment pbmt_and_pf(argc, argv);
    constexpr std::uint64_t payload_virtual = 0x51000000ULL;
    constexpr std::uint64_t payload_physical = 0xa1000000ULL;
    constexpr std::uint64_t payload_root = 0x97200000ULL;
    if (!pbmt_and_pf.reset() ||
        !pbmt_and_pf.map_sv39_4k(
            payload_virtual, payload_physical, payload_root,
            true, true, false, false, true, false) ||
        !pbmt_and_pf.map_sv39_4k(
            payload_virtual + 0x1000, payload_physical + 0x1000,
            payload_root, true, true, false, false, false, true) ||
        !pbmt_and_pf.map_sv39_4k(
            payload_virtual + 0x2000, payload_physical + 0x2000,
            payload_root, false, false, true) ||
        !pbmt_and_pf.set_page_based_memory_types(true, false) ||
        !pbmt_and_pf.activate_sv39(payload_root, 81)) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << pbmt_and_pf.cycle()
                  << " phase=payload-configuration reason="
                  << pbmt_and_pf.error() << '\n';
        return 1;
    }
    if (!expect_refilled_response(
            pbmt_and_pf,
            {.phase = "pbmt-nc",
             .vaddr = payload_virtual + 0x18,
             .paddr = payload_physical + 0x18,
             .pbmt = 1,
             .check_protection = true,
             .is_prefetch = true}) ||
        !expect_refilled_response(
            pbmt_and_pf,
            {.phase = "pbmt-io",
             .vaddr = payload_virtual + 0x1018,
             .paddr = payload_physical + 0x1018,
             .pbmt = 2,
             .check_protection = true,
             .is_prefetch = true}) ||
        !expect_refilled_response(
            pbmt_and_pf,
            {.phase = "stage1-pf",
             .vaddr = payload_virtual + 0x2018,
             .page_fault = true,
             .is_prefetch = true})) {
        return 1;
    }

    memblock::Environment nested_gpf(argc, argv);
    constexpr std::uint64_t nested_virtual = 0x52000000ULL;
    constexpr std::uint64_t nested_guest = 0x9f000000ULL;
    constexpr std::uint64_t nested_physical = 0xa2000000ULL;
    constexpr std::uint64_t nested_vs_root = 0x97400000ULL;
    constexpr std::uint64_t nested_g_root = 0x97600000ULL;
    bool nested_ready = nested_gpf.reset() &&
        nested_gpf.map_sv39_4k(
            nested_virtual, nested_guest, nested_vs_root);
    for (unsigned page = 0; nested_ready && page < 3; ++page) {
        const std::uint64_t address = nested_vs_root + page * 0x1000ULL;
        nested_ready = nested_gpf.map_sv39x4_4k(
            address, address, nested_g_root);
    }
    nested_ready = nested_ready && nested_gpf.map_sv39x4_4k(
        nested_guest, nested_physical, nested_g_root,
        false, false, true) &&
        nested_gpf.activate_two_stage(nested_vs_root, nested_g_root, 82, 83);
    if (!nested_ready) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << nested_gpf.cycle()
                  << " phase=nested-gpf-configuration reason="
                  << nested_gpf.error() << '\n';
        return 1;
    }
    if (!expect_refilled_response(
            nested_gpf,
            {.phase = "nested-gpf",
             .vaddr = nested_virtual + 0x18,
             .guest_page_fault = true,
             .is_prefetch = true})) {
        return 1;
    }

    memblock::Environment ptw_af(argc, argv);
    constexpr std::uint64_t af_virtual = 0x53000000ULL;
    constexpr std::uint64_t af_physical = 0xa3000000ULL;
    constexpr std::uint64_t af_root = 0x97800000ULL;
    if (!ptw_af.reset() ||
        !ptw_af.map_sv39_4k(af_virtual, af_physical, af_root) ||
        !ptw_af.activate_sv39(af_root, 84)) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << ptw_af.cycle() << " phase=ptw-af-configuration reason="
                  << ptw_af.error() << '\n';
        return 1;
    }
    ptw_af.inject_ptw_response_error_after(0, true, false);
    if (!expect_refilled_response(
            ptw_af,
            {.phase = "ptw-af",
             .vaddr = af_virtual + 0x18,
             .access_fault = true,
             .is_prefetch = true}) ||
        ptw_af.ptw_error_response_requests() != 1) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << ptw_af.cycle() << " phase=ptw-af-count reason="
                  << ptw_af.error() << " errors="
                  << ptw_af.ptw_error_response_requests() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_L2_TLB_CONTRACTS_PASS"
              << " cycles=" << environment.cycle() + pbmt_and_pf.cycle() +
                    nested_gpf.cycle() + ptw_af.cycle()
              << " l1_miss_response=1 l1_hit_response=1"
              << " no_translate=1 killed=1 prefetch_miss=1"
              << " pbmt_nc=1 pbmt_io=1 pf_ld=1 gpf_ld=1 af_ld=1"
              << " pmp_allow=1 pmp_deny=1 pmp_mmio=1 hints=32"
              << " hit_paddr=0x" << std::hex << warm_hit.paddr << std::dec
              << " ptw_a_requests=" << environment.ptw_requests() +
                    pbmt_and_pf.ptw_requests() + nested_gpf.ptw_requests() +
                    ptw_af.ptw_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}
