// This file is included inside memblock_main.cpp's anonymous namespace.
// Keep scenario-local helpers here; shared infrastructure belongs in memblock_env.hpp.

int run_translation_permissions(int argc, char **argv)
{
    unsigned completed = 0;
    // Keep each permission case in a fresh environment so a prior TLB entry
    // cannot mask the PTE permission being tested.
    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t virtual_address = 0x51000000ULL;
        constexpr std::uint64_t physical_address = 0xc3000000ULL;
        constexpr std::uint64_t root = 0x9e000000ULL;
        environment.memory().fill_incrementing(physical_address, 0x1000, 0x51);
        const memblock::LoadTransaction transaction{
            .address = virtual_address + 0x188,
            .oracle_address = physical_address + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 150,
            .lane = 0,
        };
        if (!environment.reset() ||
            !environment.map_sv39_4k(
                virtual_address, physical_address, root, true, false) ||
            !environment.activate_sv39(root, 31)) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=readonly-load-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 2048) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=readonly-load-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
        ++completed;
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t virtual_address = 0x53000000ULL;
        constexpr std::uint64_t physical_address = 0xc3200000ULL;
        constexpr std::uint64_t root = 0xa0000000ULL;
        const memblock::LoadTransaction transaction{
            .address = virtual_address + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 151,
            .lane = 1,
            .expected_exception_mask = memblock::kExceptionLoadPageFault,
        };
        if (!environment.reset() ||
            !environment.map_sv39_4k(
                virtual_address, physical_address, root, false, false, true) ||
            !environment.activate_sv39(root, 33)) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=execute-only-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 2048) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=execute-only-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
        ++completed;
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t guest_virtual = 0x55000000ULL;
        constexpr std::uint64_t guest_physical = 0x90000000ULL;
        constexpr std::uint64_t host_physical = 0xc3400000ULL;
        constexpr std::uint64_t vs_root = 0xa1000000ULL;
        constexpr std::uint64_t g_root = 0xa2000000ULL;
        environment.memory().fill_incrementing(host_physical, 0x1000, 0x5a);
        const memblock::LoadTransaction transaction{
            .address = guest_virtual + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 152,
            .lane = 2,
            .expected_exception_mask = memblock::kExceptionLoadGuestPageFault,
        };
        if (!environment.reset() ||
            !environment.map_sv39_4k(guest_virtual, guest_physical, vs_root) ||
            // G-stage must be able to read every VS page-table page before
            // the final execute-only data mapping is checked.
            !environment.map_sv39x4_4k(vs_root, vs_root, g_root) ||
            !environment.map_sv39x4_4k(vs_root + 0x1000, vs_root + 0x1000, g_root) ||
            !environment.map_sv39x4_4k(vs_root + 0x2000, vs_root + 0x2000, g_root) ||
            !environment.map_sv39x4_4k(
                guest_physical, host_physical, g_root, false, false, true) ||
            !environment.activate_two_stage_modes(
                memblock::ReferencePageMode::sv39,
                memblock::ReferencePageMode::sv39,
                vs_root, g_root, 35, 37)) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=execute-only-g-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 2048) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=execute-only-g-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
        ++completed;
    }

    // A read-only leaf must reject a scalar store before either cache manager
    // sees a request. Keep this in a fresh environment so the store-side
    // permission result cannot be hidden by a warm load translation.
    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t virtual_address = 0x57000000ULL;
        constexpr std::uint64_t physical_address = 0xc3600000ULL;
        constexpr std::uint64_t root = 0xa3000000ULL;
        const memblock::StoreTransaction transaction{
            .address = virtual_address + 0x188,
            .oracle_address = physical_address + 0x188,
            .data = 0x0123456789abcdefULL,
            .op = memblock::StoreOp::sd,
            .rob = 0,
            .sq = 0,
            .address_lane = 0,
            .data_lane = 1,
            .expected_exception_mask = memblock::kExceptionStorePageFault,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
        };
        const std::uint64_t dcache_before = environment.tilelink_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        if (!environment.reset() ||
            !environment.map_sv39_4k(
                virtual_address, physical_address, root, true, false) ||
            !environment.activate_sv39(root, 39)) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=readonly-store-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        environment.expect_store(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_store(transaction, 0) ||
            !environment.warm_store_translation(transaction, 4096) ||
            !environment.issue_store_data(transaction, 2048) ||
            !environment.run_until_store_complete(16384)) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=readonly-store-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
        if (environment.tilelink_requests() != dcache_before ||
            environment.uncache_requests() != uncache_before) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=readonly-store-bypass reason="
                      << "permission fault issued a memory-manager request\n";
            return 1;
        }
        if (!environment.account_sq_cancellation(1) ||
            environment.sq_dequeued() + environment.sq_canceled() !=
                environment.sq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=readonly-store-accounting reason="
                      << environment.error() << '\n';
            return 1;
        }
        ++completed;
    }

    struct StageOneLoadCase {
        const char *name;
        memblock::ReferencePageMode mode;
        memblock::ReferencePtePermissions permissions;
        memblock::ReferencePrivilegeMode privilege;
        bool sum;
        bool mxr;
        bool permitted;
    };
    const std::array<StageOneLoadCase, 14> stage_one_load_cases{{
        {
            .name = "sv39-s-user-sum0",
            .mode = memblock::ReferencePageMode::sv39,
            .permissions = {.user = true},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .permitted = false,
        },
        {
            .name = "sv39-s-user-sum1",
            .mode = memblock::ReferencePageMode::sv39,
            .permissions = {.user = true},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .sum = true,
            .permitted = true,
        },
        {
            .name = "sv39-u-user",
            .mode = memblock::ReferencePageMode::sv39,
            .permissions = {.user = true},
            .privilege = memblock::ReferencePrivilegeMode::user,
            .permitted = true,
        },
        {
            .name = "sv39-u-supervisor-page",
            .mode = memblock::ReferencePageMode::sv39,
            .permissions = {},
            .privilege = memblock::ReferencePrivilegeMode::user,
            .permitted = false,
        },
        {
            .name = "sv39-xonly-mxr0",
            .mode = memblock::ReferencePageMode::sv39,
            .permissions = {
                .readable = false,
                .writable = false,
                .executable = true,
                .dirty = false,
            },
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .permitted = false,
        },
        {
            .name = "sv39-xonly-mxr1",
            .mode = memblock::ReferencePageMode::sv39,
            .permissions = {
                .readable = false,
                .writable = false,
                .executable = true,
                .dirty = false,
            },
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .mxr = true,
            .permitted = true,
        },
        {
            .name = "sv39-accessed0",
            .mode = memblock::ReferencePageMode::sv39,
            .permissions = {.accessed = false},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .permitted = false,
        },
        {
            .name = "sv48-s-user-sum0",
            .mode = memblock::ReferencePageMode::sv48,
            .permissions = {.user = true},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .permitted = false,
        },
        {
            .name = "sv48-s-user-sum1",
            .mode = memblock::ReferencePageMode::sv48,
            .permissions = {.user = true},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .sum = true,
            .permitted = true,
        },
        {
            .name = "sv48-u-user",
            .mode = memblock::ReferencePageMode::sv48,
            .permissions = {.user = true},
            .privilege = memblock::ReferencePrivilegeMode::user,
            .permitted = true,
        },
        {
            .name = "sv48-u-supervisor-page",
            .mode = memblock::ReferencePageMode::sv48,
            .permissions = {},
            .privilege = memblock::ReferencePrivilegeMode::user,
            .permitted = false,
        },
        {
            .name = "sv48-xonly-mxr0",
            .mode = memblock::ReferencePageMode::sv48,
            .permissions = {
                .readable = false,
                .writable = false,
                .executable = true,
                .dirty = false,
            },
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .permitted = false,
        },
        {
            .name = "sv48-xonly-mxr1",
            .mode = memblock::ReferencePageMode::sv48,
            .permissions = {
                .readable = false,
                .writable = false,
                .executable = true,
                .dirty = false,
            },
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .mxr = true,
            .permitted = true,
        },
        {
            .name = "sv48-accessed0",
            .mode = memblock::ReferencePageMode::sv48,
            .permissions = {.accessed = false},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .permitted = false,
        },
    }};

    for (std::size_t index = 0; index < stage_one_load_cases.size(); ++index) {
        const auto &test = stage_one_load_cases[index];
        const bool oracle = memblock::reference_load_permitted(
            test.permissions, test.privilege, test.sum, test.mxr);
        if (oracle != test.permitted) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase="
                      << test.name << " reason=reference-oracle\n";
            return 1;
        }

        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t virtual_address = 0x59000000ULL;
        constexpr std::uint64_t physical_address = 0xc3800000ULL;
        constexpr std::uint64_t root = 0xa6000000ULL;
        environment.memory().fill_incrementing(
            physical_address, 0x1000, static_cast<unsigned char>(0x61 + index));
        const auto &permissions = test.permissions;
        const memblock::LoadTransaction transaction{
            .address = virtual_address + 0x188,
            .oracle_address = physical_address + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = static_cast<std::uint8_t>(160 + index),
            .lane = static_cast<unsigned>(index % memblock::kScalarLoadLanes),
            .expected_exception_mask = test.permitted
                ? 0U : memblock::kExceptionLoadPageFault,
        };
        if (!environment.reset() ||
            !(test.mode == memblock::ReferencePageMode::sv48
                  ? environment.map_sv48_leaf(
                        virtual_address, physical_address, 0, root,
                        permissions.readable, permissions.writable,
                        permissions.executable, permissions.user, false,
                        permissions.accessed, permissions.dirty)
                  : environment.map_sv39_leaf(
                        virtual_address, physical_address, 0, root,
                        permissions.readable, permissions.writable,
                        permissions.executable, permissions.user, false,
                        permissions.accessed, permissions.dirty)) ||
            !(test.mode == memblock::ReferencePageMode::sv48
                  ? environment.activate_sv48(
                        root, static_cast<std::uint16_t>(51 + index))
                  : environment.activate_sv39(
                        root, static_cast<std::uint16_t>(51 + index))) ||
            !environment.set_translation_permissions(
                test.privilege, test.mxr, test.sum)) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase="
                      << test.name << "-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 2048) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase="
                      << test.name << "-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
        ++completed;
    }

    struct StageOneStoreCase {
        const char *name;
        memblock::ReferencePageMode mode;
        memblock::ReferencePtePermissions permissions;
        memblock::ReferencePrivilegeMode privilege;
        bool sum;
        bool permitted;
    };
    const std::array<StageOneStoreCase, 16> stage_one_store_cases{{
        {
            .name = "sv39-store-supervisor",
            .mode = memblock::ReferencePageMode::sv39,
            .permissions = {},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .permitted = true,
        },
        {
            .name = "sv39-store-readonly",
            .mode = memblock::ReferencePageMode::sv39,
            .permissions = {.writable = false},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .permitted = false,
        },
        {
            .name = "sv39-store-accessed0",
            .mode = memblock::ReferencePageMode::sv39,
            .permissions = {.accessed = false},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .permitted = false,
        },
        {
            .name = "sv39-store-dirty0",
            .mode = memblock::ReferencePageMode::sv39,
            .permissions = {.dirty = false},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .permitted = false,
        },
        {
            .name = "sv39-store-user-sum0",
            .mode = memblock::ReferencePageMode::sv39,
            .permissions = {.user = true},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .permitted = false,
        },
        {
            .name = "sv39-store-user-sum1",
            .mode = memblock::ReferencePageMode::sv39,
            .permissions = {.user = true},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .sum = true,
            .permitted = true,
        },
        {
            .name = "sv39-store-u-user",
            .mode = memblock::ReferencePageMode::sv39,
            .permissions = {.user = true},
            .privilege = memblock::ReferencePrivilegeMode::user,
            .permitted = true,
        },
        {
            .name = "sv39-store-u-supervisor-page",
            .mode = memblock::ReferencePageMode::sv39,
            .permissions = {},
            .privilege = memblock::ReferencePrivilegeMode::user,
            .permitted = false,
        },
        {
            .name = "sv48-store-supervisor",
            .mode = memblock::ReferencePageMode::sv48,
            .permissions = {},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .permitted = true,
        },
        {
            .name = "sv48-store-readonly",
            .mode = memblock::ReferencePageMode::sv48,
            .permissions = {.writable = false},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .permitted = false,
        },
        {
            .name = "sv48-store-accessed0",
            .mode = memblock::ReferencePageMode::sv48,
            .permissions = {.accessed = false},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .permitted = false,
        },
        {
            .name = "sv48-store-dirty0",
            .mode = memblock::ReferencePageMode::sv48,
            .permissions = {.dirty = false},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .permitted = false,
        },
        {
            .name = "sv48-store-user-sum0",
            .mode = memblock::ReferencePageMode::sv48,
            .permissions = {.user = true},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .permitted = false,
        },
        {
            .name = "sv48-store-user-sum1",
            .mode = memblock::ReferencePageMode::sv48,
            .permissions = {.user = true},
            .privilege = memblock::ReferencePrivilegeMode::supervisor,
            .sum = true,
            .permitted = true,
        },
        {
            .name = "sv48-store-u-user",
            .mode = memblock::ReferencePageMode::sv48,
            .permissions = {.user = true},
            .privilege = memblock::ReferencePrivilegeMode::user,
            .permitted = true,
        },
        {
            .name = "sv48-store-u-supervisor-page",
            .mode = memblock::ReferencePageMode::sv48,
            .permissions = {},
            .privilege = memblock::ReferencePrivilegeMode::user,
            .permitted = false,
        },
    }};

    for (std::size_t index = 0; index < stage_one_store_cases.size(); ++index) {
        const auto &test = stage_one_store_cases[index];
        const bool oracle = memblock::reference_store_permitted(
            test.permissions, test.privilege, test.sum);
        if (oracle != test.permitted) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase="
                      << test.name << " reason=reference-oracle\n";
            return 1;
        }

        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t virtual_address = 0x5b000000ULL;
        constexpr std::uint64_t physical_address = 0xc3a00000ULL;
        constexpr std::uint64_t root = 0xa7000000ULL;
        environment.memory().fill_incrementing(
            physical_address, 0x1000, static_cast<unsigned char>(0x81 + index));
        const auto &permissions = test.permissions;
        if (!environment.reset() ||
            !(test.mode == memblock::ReferencePageMode::sv48
                  ? environment.map_sv48_leaf(
                        virtual_address, physical_address, 0, root,
                        permissions.readable, permissions.writable,
                        permissions.executable, permissions.user, false,
                        permissions.accessed, permissions.dirty)
                  : environment.map_sv39_leaf(
                        virtual_address, physical_address, 0, root,
                        permissions.readable, permissions.writable,
                        permissions.executable, permissions.user, false,
                        permissions.accessed, permissions.dirty)) ||
            !(test.mode == memblock::ReferencePageMode::sv48
                  ? environment.activate_sv48(root, static_cast<std::uint16_t>(71 + index))
                  : environment.activate_sv39(root, static_cast<std::uint16_t>(71 + index))) ||
            !environment.set_translation_permissions(
                test.privilege, false, test.sum)) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase="
                      << test.name << "-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        const memblock::StoreTransaction transaction{
            .address = virtual_address + 0x188,
            .oracle_address = physical_address + 0x188,
            .data = 0x1020304050607080ULL + index,
            .op = memblock::StoreOp::sd,
            .rob = 0,
            .sq = 0,
            .address_lane = static_cast<unsigned>(index % memblock::kScalarStoreLanes),
            .data_lane = static_cast<unsigned>((index + 1) % memblock::kScalarStoreLanes),
            .expected_exception_mask = test.permitted
                ? 0U : memblock::kExceptionStorePageFault,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
        };
        const std::uint64_t dcache_before = environment.tilelink_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        environment.expect_store(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_store(transaction, 0) ||
            !environment.warm_store_translation(transaction, 4096) ||
            !environment.issue_store_data(transaction, 2048) ||
            (test.permitted && !environment.pulse_pending_store(
                transaction.rob, transaction.rob_flag)) ||
            !environment.run_until_store_complete(16384)) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase="
                      << test.name << "-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
        if (test.permitted) {
            if (!environment.commit_store(transaction, 8192)) {
                std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase="
                          << test.name << "-commit reason="
                          << environment.error() << '\n';
                return 1;
            }
            const memblock::LoadTransaction readback{
                .address = transaction.address,
                .oracle_address = transaction.oracle_address,
                .op = memblock::LoadOp::ld,
                .rob = 1,
                .lq = 0,
                .sq = 1,
                .pdest = static_cast<std::uint8_t>(190 + index),
                .lane = static_cast<unsigned>(index % memblock::kScalarLoadLanes),
            };
            environment.expect_load_data(readback, transaction.data);
            if (!environment.set_rob_head(readback.rob, readback.rob_flag) ||
                !environment.enqueue_load(readback) ||
                !environment.issue_load(readback, 2048) ||
                !environment.run_until_complete(16384) ||
                !environment.run_until_lq_retired(4096)) {
                std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase="
                          << test.name << "-readback reason="
                          << environment.error() << '\n';
                return 1;
            }
        } else {
            if (environment.tilelink_requests() != dcache_before ||
                environment.uncache_requests() != uncache_before ||
                !environment.account_sq_cancellation(1)) {
                std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase="
                          << test.name << "-side-effect reason="
                          << (environment.error().empty()
                                  ? "permission fault reached memory manager"
                                  : environment.error()) << '\n';
                return 1;
            }
        }
        if (environment.sq_dequeued() + environment.sq_canceled() !=
            environment.sq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase="
                      << test.name << "-accounting reason=unbalanced-sq\n";
            return 1;
        }
        ++completed;
    }

    struct TwoStageLoadCase {
        std::string name;
        memblock::ReferencePageMode vs_mode;
        memblock::ReferencePageMode g_mode;
        memblock::ReferencePtePermissions vs_permissions;
        memblock::ReferencePtePermissions g_permissions;
        bool mxr;
        bool vmxr;
        bool vsum;
        std::uint32_t exception;
    };
    struct TwoStageLoadVariant {
        const char *name;
        memblock::ReferencePtePermissions vs_permissions;
        memblock::ReferencePtePermissions g_permissions;
        bool mxr = false;
        bool vmxr = false;
        bool vsum = false;
        std::uint32_t exception = 0;
    };
    const std::array<TwoStageLoadVariant, 8> two_stage_load_variants{{
        {
            .name = "vs-user-vsum0",
            .vs_permissions = {.user = true},
            .g_permissions = {.user = true},
            .exception = memblock::kExceptionLoadPageFault,
        },
        {
            .name = "vs-user-vsum1",
            .vs_permissions = {.user = true},
            .g_permissions = {.user = true},
            .vsum = true,
        },
        {
            .name = "vs-xonly-vmxr0",
            .vs_permissions = {
                .readable = false,
                .writable = false,
                .executable = true,
                .dirty = false,
            },
            .g_permissions = {.user = true},
            .exception = memblock::kExceptionLoadPageFault,
        },
        {
            .name = "vs-xonly-vmxr1",
            .vs_permissions = {
                .readable = false,
                .writable = false,
                .executable = true,
                .dirty = false,
            },
            .g_permissions = {.user = true},
            .vmxr = true,
        },
        {
            .name = "vs-accessed0",
            .vs_permissions = {.accessed = false},
            .g_permissions = {.user = true},
            .exception = memblock::kExceptionLoadPageFault,
        },
        {
            .name = "g-xonly-mxr0",
            .vs_permissions = {},
            .g_permissions = {
                .readable = false,
                .writable = false,
                .executable = true,
                .user = true,
                .dirty = false,
            },
            .exception = memblock::kExceptionLoadGuestPageFault,
        },
        {
            .name = "g-xonly-mxr1",
            .vs_permissions = {},
            .g_permissions = {
                .readable = false,
                .writable = false,
                .executable = true,
                .user = true,
                .dirty = false,
            },
            .mxr = true,
        },
        {
            .name = "g-accessed0",
            .vs_permissions = {},
            .g_permissions = {.user = true, .accessed = false},
            .exception = memblock::kExceptionLoadGuestPageFault,
        },
    }};
    const std::array<memblock::ReferencePageMode, 2> permission_modes{{
        memblock::ReferencePageMode::sv39,
        memblock::ReferencePageMode::sv48,
    }};
    const auto permission_mode_name = [](memblock::ReferencePageMode mode) {
        return mode == memblock::ReferencePageMode::sv48 ? "sv48" : "sv39";
    };
    std::vector<TwoStageLoadCase> two_stage_load_cases;
    two_stage_load_cases.reserve(
        permission_modes.size() * permission_modes.size() *
        two_stage_load_variants.size());
    for (const auto vs_mode : permission_modes) {
        for (const auto g_mode : permission_modes) {
            for (const auto &variant : two_stage_load_variants) {
                two_stage_load_cases.push_back({
                    .name = std::string(permission_mode_name(vs_mode)) + "-" +
                        permission_mode_name(g_mode) + "x4-" + variant.name,
                    .vs_mode = vs_mode,
                    .g_mode = g_mode,
                    .vs_permissions = variant.vs_permissions,
                    .g_permissions = variant.g_permissions,
                    .mxr = variant.mxr,
                    .vmxr = variant.vmxr,
                    .vsum = variant.vsum,
                    .exception = variant.exception,
                });
            }
        }
    }

    for (std::size_t index = 0; index < two_stage_load_cases.size(); ++index) {
        const auto &test = two_stage_load_cases[index];
        const bool vs_permitted = memblock::reference_load_permitted(
            test.vs_permissions,
            memblock::ReferencePrivilegeMode::supervisor,
            test.vsum,
            test.mxr || test.vmxr);
        const bool g_permitted = memblock::reference_load_permitted(
            test.g_permissions,
            memblock::ReferencePrivilegeMode::supervisor,
            false,
            test.mxr,
            true);
        const std::uint32_t oracle_exception = !vs_permitted
            ? memblock::kExceptionLoadPageFault
            : !g_permitted ? memblock::kExceptionLoadGuestPageFault : 0U;
        if (oracle_exception != test.exception) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase="
                      << test.name << " reason=reference-oracle\n";
            return 1;
        }

        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t guest_virtual = 0x5d000000ULL;
        constexpr std::uint64_t guest_physical = 0x98000000ULL;
        constexpr std::uint64_t host_physical = 0xc3c00000ULL;
        constexpr std::uint64_t vs_root = 0xa8000000ULL;
        constexpr std::uint64_t g_root = 0xaa000000ULL;
        environment.memory().fill_incrementing(
            host_physical, 0x1000, static_cast<unsigned char>(0xa1 + index));
        const auto &vs = test.vs_permissions;
        const auto &g = test.g_permissions;
        if (!environment.reset() ||
            !(test.vs_mode == memblock::ReferencePageMode::sv48
                  ? environment.map_sv48_leaf(
                        guest_virtual, guest_physical, 0, vs_root,
                        vs.readable, vs.writable, vs.executable, vs.user,
                        false, vs.accessed, vs.dirty)
                  : environment.map_sv39_leaf(
                        guest_virtual, guest_physical, 0, vs_root,
                        vs.readable, vs.writable, vs.executable, vs.user,
                        false, vs.accessed, vs.dirty))) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase="
                      << test.name << "-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        bool configured = true;
        const unsigned vs_table_pages =
            test.vs_mode == memblock::ReferencePageMode::sv48 ? 4U : 3U;
        for (unsigned page = 0; configured && page < vs_table_pages; ++page) {
            const std::uint64_t address = vs_root + page * 0x1000ULL;
            configured = test.g_mode == memblock::ReferencePageMode::sv48
                ? environment.map_sv48x4_4k(address, address, g_root)
                : environment.map_sv39x4_4k(address, address, g_root);
        }
        configured = configured &&
            (test.g_mode == memblock::ReferencePageMode::sv48
                 ? environment.map_sv48x4_leaf(
                       guest_physical, host_physical, 0, g_root,
                       g.readable, g.writable, g.executable, g.accessed,
                       g.dirty, g.user)
                 : environment.map_sv39x4_leaf(
                       guest_physical, host_physical, 0, g_root,
                       g.readable, g.writable, g.executable, g.accessed,
                       g.dirty, g.user));
        configured = configured && environment.activate_two_stage_modes(
            test.vs_mode, test.g_mode, vs_root, g_root,
            static_cast<std::uint16_t>(91 + index),
            static_cast<std::uint16_t>(111 + index));
        if (!configured ||
            !environment.set_translation_permissions(
                memblock::ReferencePrivilegeMode::supervisor,
                test.mxr, false, test.vmxr, test.vsum)) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase="
                      << test.name << "-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        const memblock::LoadTransaction transaction{
            .address = guest_virtual + 0x188,
            .oracle_address = host_physical + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = static_cast<std::uint8_t>(210 + index),
            .lane = static_cast<unsigned>(index % memblock::kScalarLoadLanes),
            .expected_exception_mask = test.exception,
        };
        const std::uint64_t dcache_before = environment.tilelink_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 4096) ||
            !environment.run_until_complete(32768) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase="
                      << test.name << "-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
        if (test.exception != 0 &&
            (environment.tilelink_requests() != dcache_before ||
             environment.uncache_requests() != uncache_before)) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase="
                      << test.name
                      << "-side-effect reason=permission fault reached memory manager\n";
            return 1;
        }
        ++completed;
    }

    struct TwoStageStoreCase {
        std::string name;
        memblock::ReferencePageMode vs_mode;
        memblock::ReferencePageMode g_mode;
        memblock::ReferencePtePermissions vs_permissions;
        memblock::ReferencePtePermissions g_permissions;
        bool vsum;
        std::uint32_t exception;
    };
    struct TwoStageStoreVariant {
        const char *name;
        memblock::ReferencePtePermissions vs_permissions;
        memblock::ReferencePtePermissions g_permissions;
        bool vsum = false;
        std::uint32_t exception = 0;
    };
    const std::array<TwoStageStoreVariant, 10> two_stage_store_variants{{
        {
            .name = "valid",
            .vs_permissions = {},
            .g_permissions = {.user = true},
        },
        {
            .name = "vs-readonly",
            .vs_permissions = {.writable = false},
            .g_permissions = {.user = true},
            .exception = memblock::kExceptionStorePageFault,
        },
        {
            .name = "vs-accessed0",
            .vs_permissions = {.accessed = false},
            .g_permissions = {.user = true},
            .exception = memblock::kExceptionStorePageFault,
        },
        {
            .name = "vs-dirty0",
            .vs_permissions = {.dirty = false},
            .g_permissions = {.user = true},
            .exception = memblock::kExceptionStorePageFault,
        },
        {
            .name = "vs-user-vsum0",
            .vs_permissions = {.user = true},
            .g_permissions = {.user = true},
            .exception = memblock::kExceptionStorePageFault,
        },
        {
            .name = "vs-user-vsum1",
            .vs_permissions = {.user = true},
            .g_permissions = {.user = true},
            .vsum = true,
        },
        {
            .name = "g-readonly",
            .vs_permissions = {},
            .g_permissions = {.writable = false, .user = true},
            .exception = memblock::kExceptionStoreGuestPageFault,
        },
        {
            .name = "g-accessed0",
            .vs_permissions = {},
            .g_permissions = {.user = true, .accessed = false},
            .exception = memblock::kExceptionStoreGuestPageFault,
        },
        {
            .name = "g-dirty0",
            .vs_permissions = {},
            .g_permissions = {.user = true, .dirty = false},
            .exception = memblock::kExceptionStoreGuestPageFault,
        },
        {
            .name = "g-user0",
            .vs_permissions = {},
            .g_permissions = {},
            .exception = memblock::kExceptionStoreGuestPageFault,
        },
    }};
    std::vector<TwoStageStoreCase> two_stage_store_cases;
    two_stage_store_cases.reserve(
        permission_modes.size() * permission_modes.size() *
        two_stage_store_variants.size());
    for (const auto vs_mode : permission_modes) {
        for (const auto g_mode : permission_modes) {
            for (const auto &variant : two_stage_store_variants) {
                two_stage_store_cases.push_back({
                    .name = std::string(permission_mode_name(vs_mode)) + "-" +
                        permission_mode_name(g_mode) + "x4-store-" + variant.name,
                    .vs_mode = vs_mode,
                    .g_mode = g_mode,
                    .vs_permissions = variant.vs_permissions,
                    .g_permissions = variant.g_permissions,
                    .vsum = variant.vsum,
                    .exception = variant.exception,
                });
            }
        }
    }

    for (std::size_t index = 0; index < two_stage_store_cases.size(); ++index) {
        const auto &test = two_stage_store_cases[index];
        const bool vs_permitted = memblock::reference_store_permitted(
            test.vs_permissions, memblock::ReferencePrivilegeMode::supervisor, test.vsum);
        const bool g_permitted = memblock::reference_store_permitted(
            test.g_permissions, memblock::ReferencePrivilegeMode::supervisor, false, true);
        const std::uint32_t oracle_exception = !vs_permitted ? memblock::kExceptionStorePageFault
                                               : !g_permitted
                                                   ? memblock::kExceptionStoreGuestPageFault
                                                   : 0U;
        if (oracle_exception != test.exception) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=" << test.name
                      << " reason=reference-oracle\n";
            return 1;
        }

        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t guest_virtual = 0x5f000000ULL;
        constexpr std::uint64_t guest_physical = 0x9c000000ULL;
        constexpr std::uint64_t host_physical = 0xc3e00000ULL;
        constexpr std::uint64_t vs_root = 0xac000000ULL;
        constexpr std::uint64_t g_root = 0xae000000ULL;
        const auto &vs = test.vs_permissions;
        const auto &g = test.g_permissions;
        bool configured = environment.reset();
        configured = configured &&
                     (test.vs_mode == memblock::ReferencePageMode::sv48
                          ? environment.map_sv48_leaf(guest_virtual, guest_physical, 0, vs_root,
                                                      vs.readable, vs.writable, vs.executable,
                                                      vs.user, false, vs.accessed, vs.dirty)
                          : environment.map_sv39_leaf(guest_virtual, guest_physical, 0, vs_root,
                                                      vs.readable, vs.writable, vs.executable,
                                                      vs.user, false, vs.accessed, vs.dirty));
        const unsigned vs_table_pages = test.vs_mode == memblock::ReferencePageMode::sv48 ? 4U : 3U;
        for (unsigned page = 0; configured && page < vs_table_pages; ++page) {
            const std::uint64_t address = vs_root + page * 0x1000ULL;
            configured = test.g_mode == memblock::ReferencePageMode::sv48
                             ? environment.map_sv48x4_4k(address, address, g_root)
                             : environment.map_sv39x4_4k(address, address, g_root);
        }
        configured = configured &&
                     (test.g_mode == memblock::ReferencePageMode::sv48
                          ? environment.map_sv48x4_leaf(guest_physical, host_physical, 0, g_root,
                                                        g.readable, g.writable, g.executable,
                                                        g.accessed, g.dirty, g.user)
                          : environment.map_sv39x4_leaf(guest_physical, host_physical, 0, g_root,
                                                        g.readable, g.writable, g.executable,
                                                        g.accessed, g.dirty, g.user));
        configured = configured && environment.activate_two_stage_modes(
                                       test.vs_mode, test.g_mode, vs_root, g_root,
                                       static_cast<std::uint16_t>(131 + index),
                                       static_cast<std::uint16_t>(161 + index));
        configured = configured && environment.set_translation_permissions(
                                       memblock::ReferencePrivilegeMode::supervisor, false, false,
                                       false, test.vsum);
        if (!configured) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=" << test.name
                      << "-configuration reason=" << environment.error() << '\n';
            return 1;
        }

        environment.memory().fill_incrementing(host_physical, 0x1000,
                                               static_cast<unsigned char>(0xc1 + index));
        const memblock::StoreTransaction transaction{
            .address = guest_virtual + 0x188,
            .oracle_address = host_physical + 0x188,
            .data = 0x8090a0b0c0d0e000ULL + index,
            .op = memblock::StoreOp::sd,
            .rob = 0,
            .sq = 0,
            .address_lane = static_cast<unsigned>(index % memblock::kScalarStoreLanes),
            .data_lane = static_cast<unsigned>((index + 1) % memblock::kScalarStoreLanes),
            .expected_exception_mask = test.exception,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
        };
        const std::uint64_t dcache_before = environment.tilelink_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        environment.expect_store(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_store(transaction, 0) ||
            !environment.issue_store_address_until_tlb_hit(transaction, 16384) ||
            !environment.issue_store_data(transaction, 2048) ||
            (test.exception == 0 &&
             !environment.pulse_pending_store(transaction.rob, transaction.rob_flag)) ||
            !environment.run_until_store_complete(32768)) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=" << test.name
                      << "-execution reason=" << environment.error() << '\n';
            return 1;
        }
        if (test.exception == 0) {
            if (!environment.commit_store(transaction, 16384)) {
                std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=" << test.name
                          << "-commit reason=" << environment.error() << '\n';
                return 1;
            }
            const memblock::LoadTransaction readback{
                .address = transaction.address,
                .oracle_address = transaction.oracle_address,
                .op = memblock::LoadOp::ld,
                .rob = 1,
                .lq = 0,
                .sq = 1,
                .pdest = static_cast<std::uint8_t>(1 + index),
                .lane = static_cast<unsigned>(index % memblock::kScalarLoadLanes),
            };
            environment.expect_load_data(readback, transaction.data);
            if (!environment.set_rob_head(readback.rob, readback.rob_flag) ||
                !environment.enqueue_load(readback) || !environment.issue_load(readback, 4096) ||
                !environment.run_until_complete(32768) || !environment.run_until_lq_retired(8192)) {
                std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=" << test.name
                          << "-readback reason=" << environment.error() << '\n';
                return 1;
            }
        } else {
            if (environment.tilelink_requests() != dcache_before ||
                environment.uncache_requests() != uncache_before) {
                std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=" << test.name
                          << "-side-effect reason=" << "permission fault reached memory manager\n";
                return 1;
            }
            if (environment.sq_dequeued() + environment.sq_canceled() <
                    environment.sq_allocated() &&
                !environment.account_sq_cancellation(1)) {
                std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=" << test.name
                          << "-cancellation reason=" << environment.error() << '\n';
                return 1;
            }
        }
        if (environment.sq_dequeued() + environment.sq_canceled() != environment.sq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=" << test.name
                      << "-accounting reason=unbalanced-sq\n";
            return 1;
        }
        ++completed;
    }

    struct ImplicitGPermissionVariant {
      const char *name;
      memblock::ReferencePtePermissions permissions;
      bool mxr = false;
    };
    const std::array<ImplicitGPermissionVariant, 3>
        implicit_g_permission_variants{{
            {
                .name = "xonly-mxr1",
                .permissions =
                    {
                        .readable = false,
                        .writable = false,
                        .executable = true,
                        .user = true,
                        .dirty = false,
                    },
                .mxr = true,
            },
            {
                .name = "user0",
                .permissions =
                    {
                        .writable = false,
                        .dirty = false,
                    },
            },
            {
                .name = "accessed0",
                .permissions =
                    {
                        .writable = false,
                        .user = true,
                        .accessed = false,
                        .dirty = false,
                    },
            },
        }};
    const std::array<memblock::ReferenceMemoryAccess, 2> implicit_g_accesses{{
        memblock::ReferenceMemoryAccess::load,
        memblock::ReferenceMemoryAccess::store,
    }};
    unsigned implicit_g_permission_configs = 0;
    unsigned implicit_g_permission_faults = 0;
    unsigned implicit_g_load_permission_faults = 0;
    unsigned implicit_g_store_permission_faults = 0;
    unsigned implicit_g_readonly_store_cases = 0;
    unsigned implicit_g_fault_case_index = 0;
    for (const auto vs_mode : permission_modes) {
      for (const auto g_mode : permission_modes) {
        const unsigned levels = memblock::reference_page_levels(vs_mode);
        for (unsigned target_level = 0; target_level < levels; ++target_level) {
          for (const auto &variant : implicit_g_permission_variants) {
            for (const auto access : implicit_g_accesses) {
              memblock::Environment environment(argc, argv);
              constexpr std::uint64_t guest_virtual = 0x65000000ULL;
              constexpr std::uint64_t guest_physical = 0x9a000000ULL;
              constexpr std::uint64_t host_physical = 0xc4200000ULL;
              constexpr std::uint64_t vs_root = 0xb4000000ULL;
              constexpr std::uint64_t g_root = 0xb6000000ULL;
              const std::uint64_t input_address = guest_virtual + 0x188;
              bool configured = environment.reset();
              configured = configured &&
                           (vs_mode == memblock::ReferencePageMode::sv48
                                ? environment.map_sv48_4k(
                                      guest_virtual, guest_physical, vs_root)
                                : environment.map_sv39_4k(
                                      guest_virtual, guest_physical, vs_root));
              const auto fault_pte_gpa =
                  memblock::reference_pte_address_at_level(
                      environment.memory(), vs_root, input_address, vs_mode,
                      target_level);
              for (unsigned page = 0; configured && page < levels; ++page) {
                const std::uint64_t address = vs_root + page * 0x1000ULL;
                configured = g_mode == memblock::ReferencePageMode::sv48
                                 ? environment.map_sv48x4_leaf(address, address,
                                                               0, g_root)
                                 : environment.map_sv39x4_leaf(address, address,
                                                               0, g_root);
              }
              const unsigned target_page = levels - 1 - target_level;
              const std::uint64_t target_page_address =
                  vs_root + target_page * 0x1000ULL;
              const auto &permissions = variant.permissions;
              configured =
                  configured &&
                  (g_mode == memblock::ReferencePageMode::sv48
                       ? environment.map_sv48x4_leaf(
                             target_page_address, target_page_address, 0,
                             g_root, permissions.readable, permissions.writable,
                             permissions.executable, permissions.accessed,
                             permissions.dirty, permissions.user)
                       : environment.map_sv39x4_leaf(
                             target_page_address, target_page_address, 0,
                             g_root, permissions.readable, permissions.writable,
                             permissions.executable, permissions.accessed,
                             permissions.dirty, permissions.user));
              configured =
                  configured &&
                  (g_mode == memblock::ReferencePageMode::sv48
                       ? environment.map_sv48x4_leaf(guest_physical,
                                                     host_physical, 0, g_root)
                       : environment.map_sv39x4_leaf(guest_physical,
                                                     host_physical, 0, g_root));
              const auto reference = memblock::reference_two_stage_access(
                  environment.memory(), vs_root, g_root, input_address, access,
                  vs_mode, g_mode, memblock::ReferencePrivilegeMode::supervisor,
                  false, variant.mxr);
              if (!configured || !fault_pte_gpa.has_value() ||
                  reference.translated || !reference.guest_page_fault ||
                  reference.stage1_page_fault || reference.access_fault ||
                  reference.faulting_guest_physical_address != *fault_pte_gpa ||
                  !reference.is_for_vs_nonleaf_pte) {
                std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL "
                             "phase=implicit-g-reference"
                          << " case=" << implicit_g_fault_case_index
                          << " variant=" << variant.name
                          << " access=" << static_cast<unsigned>(access)
                          << " vs_mode=" << static_cast<unsigned>(vs_mode)
                          << " g_mode=" << static_cast<unsigned>(g_mode)
                          << " level=" << target_level << '\n';
                return 1;
              }
              configured =
                  configured && environment.activate_two_stage_modes(
                                    vs_mode, g_mode, vs_root, g_root,
                                    static_cast<std::uint16_t>(
                                        211 + implicit_g_fault_case_index),
                                    static_cast<std::uint16_t>(
                                        271 + implicit_g_fault_case_index));
              configured = configured &&
                           environment.set_translation_permissions(
                               memblock::ReferencePrivilegeMode::supervisor,
                               variant.mxr, false);
              if (!configured) {
                std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL "
                             "phase=implicit-g-configuration"
                          << " case=" << implicit_g_fault_case_index
                          << " variant=" << variant.name
                          << " access=" << static_cast<unsigned>(access)
                          << " reason=" << environment.error() << '\n';
                return 1;
              }

              const std::uint64_t ptw_before = environment.ptw_requests();
              const std::uint64_t dcache_before =
                  environment.tilelink_requests();
              const std::uint64_t uncache_before =
                  environment.uncache_requests();
              bool executed = false;
              if (access == memblock::ReferenceMemoryAccess::load) {
                const memblock::LoadTransaction transaction{
                    .address = input_address,
                    .op = memblock::LoadOp::ld,
                    .rob = 0,
                    .lq = 0,
                    .pdest = static_cast<std::uint8_t>(
                        32 + implicit_g_fault_case_index),
                    .lane = static_cast<unsigned>(implicit_g_fault_case_index %
                                                  memblock::kScalarLoadLanes),
                    .expected_exception_mask =
                        memblock::kExceptionLoadGuestPageFault,
                };
                environment.expect_load(transaction);
                executed = environment.set_rob_head(transaction.rob,
                                                    transaction.rob_flag) &&
                           environment.enqueue_load(transaction) &&
                           environment.issue_load(transaction, 4096) &&
                           environment.run_until_complete(32768) &&
                           environment.run_until_lq_retired(8192);
              } else {
                const memblock::StoreTransaction transaction{
                    .address = input_address,
                    .data = 0x1020304050607080ULL ^ implicit_g_fault_case_index,
                    .op = memblock::StoreOp::sd,
                    .rob = 0,
                    .sq = 0,
                    .address_lane =
                        static_cast<unsigned>(implicit_g_fault_case_index %
                                              memblock::kScalarStoreLanes),
                    .data_lane = static_cast<unsigned>(
                        (implicit_g_fault_case_index + 1) %
                        memblock::kScalarStoreLanes),
                    .expected_exception_mask =
                        memblock::kExceptionStoreGuestPageFault,
                };
                environment.expect_store(transaction);
                environment.select_store_exception_address(true);
                executed =
                    environment.set_rob_head(transaction.rob,
                                             transaction.rob_flag) &&
                    environment.enqueue_store(transaction, 0) &&
                    environment.issue_store_address(transaction, 4096) &&
                    environment.issue_store_data(transaction, 4096) &&
                    environment.run_until_store_complete_with_replay(
                        transaction, 32768) &&
                    (environment.sq_dequeued() + environment.sq_canceled() >=
                         environment.sq_allocated() ||
                     environment.account_sq_cancellation(1)) &&
                    environment.sq_dequeued() + environment.sq_canceled() ==
                        environment.sq_allocated();
              }
              if (!executed || environment.ptw_requests() <= ptw_before ||
                  environment.ptw_requests_covering_since(*fault_pte_gpa,
                                                          ptw_before) != 0 ||
                  environment.tilelink_requests() != dcache_before ||
                  environment.uncache_requests() != uncache_before ||
                  environment.exception_vaddr() != input_address ||
                  environment.exception_gpaddr() != *fault_pte_gpa ||
                  !environment.exception_is_for_vs_nonleaf_pte()) {
                std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL "
                             "phase=implicit-g-access"
                          << " case=" << implicit_g_fault_case_index
                          << " variant=" << variant.name
                          << " access=" << static_cast<unsigned>(access)
                          << " vs_mode=" << static_cast<unsigned>(vs_mode)
                          << " g_mode=" << static_cast<unsigned>(g_mode)
                          << " level=" << target_level << " reason="
                          << (environment.error().empty()
                                  ? "wrong fault, PTE traffic, metadata, or "
                                    "data-manager side effect"
                                  : environment.error())
                          << " pte_reads="
                          << environment.ptw_requests_covering_since(
                                 *fault_pte_gpa, ptw_before)
                          << " ptw=" << ptw_before << '/'
                          << environment.ptw_requests() << " vaddr=0x"
                          << std::hex << environment.exception_vaddr() << "/0x"
                          << input_address << " gpaddr=0x"
                          << environment.exception_gpaddr() << "/0x"
                          << *fault_pte_gpa << " vs_nonleaf=" << std::dec
                          << environment.exception_is_for_vs_nonleaf_pte()
                          << '\n';
                return 1;
              }
              ++implicit_g_permission_faults;
              if (access == memblock::ReferenceMemoryAccess::load) {
                ++implicit_g_load_permission_faults;
              } else {
                ++implicit_g_store_permission_faults;
              }
              ++implicit_g_fault_case_index;
              ++completed;
            }
            ++implicit_g_permission_configs;
          }
        }
      }
    }

    unsigned implicit_g_store_case_index = 0;
    for (const auto vs_mode : permission_modes) {
      for (const auto g_mode : permission_modes) {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t guest_virtual = 0x67000000ULL;
        constexpr std::uint64_t guest_physical = 0x9b000000ULL;
        constexpr std::uint64_t host_physical = 0xc4400000ULL;
        constexpr std::uint64_t vs_root = 0xb8000000ULL;
        constexpr std::uint64_t g_root = 0xba000000ULL;
        const std::uint64_t input_address = guest_virtual + 0x188;
        bool configured = environment.reset();
        configured = configured &&
                     (vs_mode == memblock::ReferencePageMode::sv48
                          ? environment.map_sv48_4k(guest_virtual,
                                                    guest_physical, vs_root)
                          : environment.map_sv39_4k(guest_virtual,
                                                    guest_physical, vs_root));
        const unsigned levels = memblock::reference_page_levels(vs_mode);
        std::vector<std::uint64_t> vs_pte_gpas;
        vs_pte_gpas.reserve(levels);
        for (unsigned target_level = 0; target_level < levels; ++target_level) {
          const auto pte_gpa = memblock::reference_pte_address_at_level(
              environment.memory(), vs_root, input_address, vs_mode,
              target_level);
          if (!pte_gpa.has_value()) {
            configured = false;
            break;
          }
          vs_pte_gpas.push_back(*pte_gpa);
        }
        for (unsigned page = 0; configured && page < levels; ++page) {
          const std::uint64_t address = vs_root + page * 0x1000ULL;
          configured =
              g_mode == memblock::ReferencePageMode::sv48
                  ? environment.map_sv48x4_leaf(address, address, 0, g_root,
                                                true, false, false, true, false,
                                                true)
                  : environment.map_sv39x4_leaf(address, address, 0, g_root,
                                                true, false, false, true, false,
                                                true);
        }
        configured =
            configured && (g_mode == memblock::ReferencePageMode::sv48
                               ? environment.map_sv48x4_leaf(
                                     guest_physical, host_physical, 0, g_root)
                               : environment.map_sv39x4_leaf(
                                     guest_physical, host_physical, 0, g_root));
        const auto reference = memblock::reference_two_stage_access(
            environment.memory(), vs_root, g_root, input_address,
            memblock::ReferenceMemoryAccess::store, vs_mode, g_mode,
            memblock::ReferencePrivilegeMode::supervisor);
        if (!configured || !reference.translated ||
            reference.physical_address != host_physical + 0x188 ||
            reference.guest_page_fault || reference.stage1_page_fault ||
            reference.access_fault) {
          std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL "
                       "phase=implicit-g-readonly-store-reference"
                    << " case=" << implicit_g_store_case_index
                    << " vs_mode=" << static_cast<unsigned>(vs_mode)
                    << " g_mode=" << static_cast<unsigned>(g_mode) << '\n';
          return 1;
        }
        configured =
            configured &&
            environment.activate_two_stage_modes(
                vs_mode, g_mode, vs_root, g_root,
                static_cast<std::uint16_t>(311 + implicit_g_store_case_index),
                static_cast<std::uint16_t>(321 + implicit_g_store_case_index));
        configured =
            configured &&
            environment.set_translation_permissions(
                memblock::ReferencePrivilegeMode::supervisor, false, false);
        if (!configured) {
          std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL "
                       "phase=implicit-g-readonly-store-configuration"
                    << " case=" << implicit_g_store_case_index
                    << " reason=" << environment.error() << '\n';
          return 1;
        }

        environment.memory().fill_incrementing(
            host_physical, 0x1000,
            static_cast<unsigned char>(0xe1 + implicit_g_store_case_index));
        const memblock::StoreTransaction transaction{
            .address = input_address,
            .oracle_address = host_physical + 0x188,
            .data = 0xfedcba9876543210ULL ^ implicit_g_store_case_index,
            .op = memblock::StoreOp::sd,
            .rob = 0,
            .sq = 0,
            .address_lane = static_cast<unsigned>(implicit_g_store_case_index %
                                                  memblock::kScalarStoreLanes),
            .data_lane =
                static_cast<unsigned>((implicit_g_store_case_index + 1) %
                                      memblock::kScalarStoreLanes),
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
        };
        const std::uint64_t ptw_before = environment.ptw_requests();
        const std::uint64_t uncache_before = environment.uncache_requests();
        environment.expect_store(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_store(transaction, 0) ||
            !environment.issue_store_address_until_tlb_hit(transaction,
                                                           16384) ||
            !environment.issue_store_data(transaction, 2048) ||
            !environment.pulse_pending_store(transaction.rob,
                                             transaction.rob_flag) ||
            !environment.run_until_store_complete(32768) ||
            !environment.commit_store(transaction, 16384)) {
          std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL "
                       "phase=implicit-g-readonly-store-execution"
                    << " case=" << implicit_g_store_case_index
                    << " reason=" << environment.error() << '\n';
          return 1;
        }
        bool saw_all_vs_ptes = environment.ptw_requests() > ptw_before;
        for (const std::uint64_t pte_gpa : vs_pte_gpas) {
          saw_all_vs_ptes =
              saw_all_vs_ptes &&
              environment.ptw_requests_covering_since(pte_gpa, ptw_before) != 0;
        }
        if (!saw_all_vs_ptes ||
            environment.uncache_requests() != uncache_before) {
          std::cerr
              << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL "
                 "phase=implicit-g-readonly-store-traffic"
              << " case=" << implicit_g_store_case_index
              << " reason=missing VS PTE read or unexpected Uncache request"
              << " ptw=" << ptw_before << '/' << environment.ptw_requests()
              << " uncache=" << uncache_before << '/'
              << environment.uncache_requests() << '\n';
          return 1;
        }
        const memblock::LoadTransaction readback{
            .address = transaction.address,
            .oracle_address = transaction.oracle_address,
            .op = memblock::LoadOp::ld,
            .rob = 1,
            .lq = 0,
            .sq = 1,
            .pdest =
                static_cast<std::uint8_t>(80 + implicit_g_store_case_index),
            .lane = static_cast<unsigned>(implicit_g_store_case_index %
                                          memblock::kScalarLoadLanes),
        };
        environment.expect_load_data(readback, transaction.data);
        if (!environment.set_rob_head(readback.rob, readback.rob_flag) ||
            !environment.enqueue_load(readback) ||
            !environment.issue_load(readback, 4096) ||
            !environment.run_until_complete(32768) ||
            !environment.run_until_lq_retired(8192) ||
            environment.sq_dequeued() + environment.sq_canceled() !=
                environment.sq_allocated()) {
          std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL "
                       "phase=implicit-g-readonly-store-readback"
                    << " case=" << implicit_g_store_case_index << " reason="
                    << (environment.error().empty()
                            ? "readback mismatch or unbalanced SQ"
                            : environment.error())
                    << '\n';
          return 1;
        }
        ++implicit_g_readonly_store_cases;
        ++implicit_g_store_case_index;
        ++completed;
      }
    }

    constexpr unsigned expected_implicit_g_permission_configs = 42;
    constexpr unsigned expected_implicit_g_permission_faults = 84;
    constexpr unsigned expected_implicit_g_load_permission_faults = 42;
    constexpr unsigned expected_implicit_g_store_permission_faults = 42;
    constexpr unsigned expected_implicit_g_readonly_store_cases = 4;
    constexpr unsigned expected_translation_permission_cases = 194;
    if (implicit_g_permission_configs !=
            expected_implicit_g_permission_configs ||
        implicit_g_permission_faults != expected_implicit_g_permission_faults ||
        implicit_g_load_permission_faults !=
            expected_implicit_g_load_permission_faults ||
        implicit_g_store_permission_faults !=
            expected_implicit_g_store_permission_faults ||
        implicit_g_readonly_store_cases !=
            expected_implicit_g_readonly_store_cases ||
        completed != expected_translation_permission_cases) {
      std::cerr << "MEMBLOCK_TRANSLATION_PERMISSIONS_FAIL phase=coverage"
                << " cases=" << completed
                << " configs=" << implicit_g_permission_configs
                << " faults=" << implicit_g_permission_faults
                << " load_faults=" << implicit_g_load_permission_faults
                << " store_faults=" << implicit_g_store_permission_faults
                << " readonly_stores=" << implicit_g_readonly_store_cases
                << '\n';
      return 1;
    }

    std::cout
        << "MEMBLOCK_TRANSLATION_PERMISSIONS_PASS cases=" << completed
        << " stage1_load_cases=" << (2 + stage_one_load_cases.size())
        << " stage1_store_cases=" << (1 + stage_one_store_cases.size())
        << " two_stage_load_cases="
        << (1 + two_stage_load_cases.size() + implicit_g_load_permission_faults)
        << " two_stage_store_cases="
        << (two_stage_store_cases.size() + implicit_g_store_permission_faults +
            implicit_g_readonly_store_cases)
        << " implicit_g_permission_faults=" << implicit_g_permission_faults
        << " implicit_g_permission_configs=" << implicit_g_permission_configs
        << " implicit_g_load_permission_faults="
        << implicit_g_load_permission_faults
        << " implicit_g_store_permission_faults="
        << implicit_g_store_permission_faults
        << " implicit_g_readonly_store_cases="
        << implicit_g_readonly_store_cases
        << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_translation_pbmt(int argc, char **argv)
{
    constexpr std::array<memblock::ReferencePageMode, 2> modes{{
        memblock::ReferencePageMode::sv39,
        memblock::ReferencePageMode::sv48,
    }};
    constexpr std::array<memblock::ReferencePbmt, 3> pbmts{{
        memblock::ReferencePbmt::pma,
        memblock::ReferencePbmt::nc,
        memblock::ReferencePbmt::io,
    }};
    auto pbmt_name = [](memblock::ReferencePbmt pbmt) {
        switch (pbmt) {
        case memblock::ReferencePbmt::pma:
            return "pma";
        case memblock::ReferencePbmt::nc:
            return "nc";
        case memblock::ReferencePbmt::io:
            return "io";
        }
        return "invalid";
    };

    unsigned combinations = 0;
    unsigned load_transactions = 0;
    unsigned store_transactions = 0;
    std::array<unsigned, 3> final_pbmt_counts{};
    for (const auto vs_mode : modes) {
        for (const auto g_mode : modes) {
            for (const auto vs_pbmt : pbmts) {
                for (const auto g_pbmt : pbmts) {
                    const auto final_pbmt = memblock::reference_two_stage_pbmt(
                        vs_pbmt, g_pbmt);
                    const bool final_nc =
                        final_pbmt == memblock::ReferencePbmt::nc;
                    const bool final_io =
                        final_pbmt == memblock::ReferencePbmt::io;
                    const unsigned index = combinations;
                    std::ostringstream phase;
                    phase << (vs_mode == memblock::ReferencePageMode::sv48
                                  ? "sv48"
                                  : "sv39")
                          << '-'
                          << (g_mode == memblock::ReferencePageMode::sv48
                                  ? "sv48x4"
                                  : "sv39x4")
                          << "-vs-" << pbmt_name(vs_pbmt)
                          << "-g-" << pbmt_name(g_pbmt)
                          << "-final-" << pbmt_name(final_pbmt);

                    memblock::Environment environment(argc, argv);
                    constexpr std::uint64_t guest_virtual = 0x61000000ULL;
                    constexpr std::uint64_t guest_physical = 0x9e000000ULL;
                    constexpr std::uint64_t host_physical = 0xc4000000ULL;
                    constexpr std::uint64_t vs_root = 0xb0000000ULL;
                    constexpr std::uint64_t g_root = 0xb2000000ULL;
                    environment.memory().fill_incrementing(
                        host_physical, 0x1000,
                        static_cast<unsigned char>(0x31 + index));
                    const bool vs_nc = vs_pbmt == memblock::ReferencePbmt::nc;
                    const bool vs_io = vs_pbmt == memblock::ReferencePbmt::io;
                    const bool g_nc = g_pbmt == memblock::ReferencePbmt::nc;
                    const bool g_io = g_pbmt == memblock::ReferencePbmt::io;
                    bool configured = environment.reset();
                    configured = configured &&
                        (vs_mode == memblock::ReferencePageMode::sv48
                             ? environment.map_sv48_leaf(
                                   guest_virtual, guest_physical, 0, vs_root,
                                   true, true, false, false, vs_nc, true,
                                   std::nullopt, vs_io)
                             : environment.map_sv39_leaf(
                                   guest_virtual, guest_physical, 0, vs_root,
                                   true, true, false, false, vs_nc, true,
                                   std::nullopt, vs_io));
                    const unsigned vs_table_pages =
                        vs_mode == memblock::ReferencePageMode::sv48 ? 4U : 3U;
                    for (unsigned page = 0;
                         configured && page < vs_table_pages; ++page) {
                        const std::uint64_t address =
                            vs_root + page * 0x1000ULL;
                        configured =
                            g_mode == memblock::ReferencePageMode::sv48
                                ? environment.map_sv48x4_4k(
                                      address, address, g_root)
                                : environment.map_sv39x4_4k(
                                      address, address, g_root);
                    }
                    configured = configured &&
                        (g_mode == memblock::ReferencePageMode::sv48
                             ? environment.map_sv48x4_leaf(
                                   guest_physical, host_physical, 0, g_root,
                                   true, true, false, true, std::nullopt, true,
                                   g_nc, g_io)
                             : environment.map_sv39x4_leaf(
                                   guest_physical, host_physical, 0, g_root,
                                   true, true, false, true, std::nullopt, true,
                                   g_nc, g_io));
                    configured = configured &&
                        environment.activate_two_stage_modes(
                            vs_mode, g_mode, vs_root, g_root,
                            static_cast<std::uint16_t>(191 + index),
                            static_cast<std::uint16_t>(231 + index));
                    configured = configured &&
                        environment.set_page_based_memory_types(true, true);
                    configured = configured &&
                        environment.set_translation_permissions(
                            memblock::ReferencePrivilegeMode::supervisor);
                    if (!configured) {
                        std::cerr << "MEMBLOCK_TRANSLATION_PBMT_FAIL phase="
                                  << phase.str() << "-configuration reason="
                                  << environment.error() << '\n';
                        return 1;
                    }

                    const memblock::LoadTransaction load{
                        .address = guest_virtual + 0x188,
                        .oracle_address = host_physical + 0x188,
                        .op = memblock::LoadOp::ld,
                        .rob = 0,
                        .lq = 0,
                        .pdest = static_cast<std::uint8_t>(180 + index),
                        .lane = index % memblock::kScalarLoadLanes,
                        .expected_debug_is_mmio = final_io,
                        .expected_debug_is_ncio = final_io
                            ? std::optional<bool>{false} : std::nullopt,
                    };
                    const std::uint64_t load_dcache_before =
                        environment.tilelink_requests();
                    const std::uint64_t load_uncache_before =
                        environment.uncache_requests();
                    environment.expect_load(load);
                    if (!environment.set_rob_head(load.rob, load.rob_flag) ||
                        !environment.enqueue_load(load) ||
                        !environment.issue_load(load, 4096) ||
                        (final_io && !environment.wait_for_mmio_request(
                            load.rob, load.rob_flag, 16384)) ||
                        !environment.run_until_complete(32768) ||
                        !environment.run_until_lq_retired(8192)) {
                        std::cerr << "MEMBLOCK_TRANSLATION_PBMT_FAIL phase="
                                  << phase.str() << "-load reason="
                                  << environment.error() << '\n';
                        return 1;
                    }
                    const std::uint64_t expected_load_dcache =
                        load_dcache_before + (final_nc || final_io ? 0U : 1U);
                    const std::uint64_t expected_load_uncache =
                        load_uncache_before + (final_nc || final_io ? 1U : 0U);
                    if (environment.tilelink_requests() != expected_load_dcache ||
                        environment.uncache_requests() != expected_load_uncache) {
                        std::cerr << "MEMBLOCK_TRANSLATION_PBMT_FAIL phase="
                                  << phase.str()
                                  << "-load-path reason=wrong-manager"
                                  << " dcache="
                                  << environment.tilelink_requests() << '/'
                                  << expected_load_dcache << " uncache="
                                  << environment.uncache_requests() << '/'
                                  << expected_load_uncache << '\n';
                        return 1;
                    }
                    ++load_transactions;

                    const memblock::StoreTransaction store{
                        .address = guest_virtual + 0x288,
                        .oracle_address = host_physical + 0x288,
                        .data = 0x3141592653589700ULL + index,
                        .op = memblock::StoreOp::sd,
                        .rob = 1,
                        .sq = 0,
                        .address_lane = index % memblock::kScalarStoreLanes,
                        .data_lane = (index + 1) % memblock::kScalarStoreLanes,
                        .expected_debug_is_mmio = false,
                        .expected_debug_is_ncio = false,
                    };
                    const std::uint64_t store_dcache_before =
                        environment.tilelink_requests();
                    const std::uint64_t store_uncache_before =
                        environment.uncache_requests();
                    environment.expect_store(store);
                    bool store_ok =
                        environment.set_rob_head(store.rob, store.rob_flag) &&
                        environment.enqueue_store(store, 1) &&
                        environment.issue_store_address_until_tlb_hit(
                            store, 16384) &&
                        environment.issue_store_data(store, 4096);
                    if (store_ok && final_io) {
                        store_ok = environment.run_cycles(64) &&
                            environment.wait_for_mmio_store_request(
                                store.rob, store.rob_flag, 16384) &&
                            environment.run_until_uncache_requests(
                                store_uncache_before + 1, 16384) &&
                            environment.run_until_store_complete(32768) &&
                            environment.commit_stores_through(store, 1) &&
                            environment.run_cycles(16);
                        if (store_ok) {
                            environment.record_committed_store(store);
                        }
                    } else if (store_ok) {
                        store_ok = environment.run_until_store_complete(32768) &&
                            environment.commit_store(store, 16384);
                    }
                    if (!store_ok) {
                        std::cerr << "MEMBLOCK_TRANSLATION_PBMT_FAIL phase="
                                  << phase.str() << "-store reason="
                                  << environment.error() << '\n';
                        return 1;
                    }
                    if (environment.sq_dequeued() + environment.sq_canceled() !=
                        environment.sq_allocated()) {
                        std::cerr << "MEMBLOCK_TRANSLATION_PBMT_FAIL phase="
                                  << phase.str()
                                  << "-store-accounting reason=unbalanced-sq\n";
                        return 1;
                    }
                    if ((final_nc || final_io) &&
                        (environment.tilelink_requests() != store_dcache_before ||
                         environment.uncache_requests() !=
                             store_uncache_before + 1)) {
                        std::cerr << "MEMBLOCK_TRANSLATION_PBMT_FAIL phase="
                                  << phase.str()
                                  << "-store-path reason=wrong-manager\n";
                        return 1;
                    }
                    if (!final_nc && !final_io &&
                        environment.uncache_requests() != store_uncache_before) {
                        std::cerr << "MEMBLOCK_TRANSLATION_PBMT_FAIL phase="
                                  << phase.str()
                                  << "-store-path reason=PMA-used-uncache\n";
                        return 1;
                    }
                    ++store_transactions;

                    const memblock::LoadTransaction readback{
                        .address = store.address,
                        .oracle_address = store.oracle_address,
                        .op = memblock::LoadOp::ld,
                        .rob = 2,
                        .lq = 1,
                        .sq = 1,
                        .pdest = static_cast<std::uint8_t>(220 + index),
                        .lane = (index + 1) % memblock::kScalarLoadLanes,
                        .expected_debug_is_mmio = final_io,
                        .expected_debug_is_ncio = final_io
                            ? std::optional<bool>{false} : std::nullopt,
                    };
                    const std::uint64_t readback_dcache_before =
                        environment.tilelink_requests();
                    const std::uint64_t readback_uncache_before =
                        environment.uncache_requests();
                    environment.expect_load_data(readback, store.data);
                    if (!environment.set_rob_head(
                            readback.rob, readback.rob_flag) ||
                        !environment.enqueue_load(readback) ||
                        !environment.issue_load(readback, 4096) ||
                        (final_io && !environment.wait_for_mmio_request(
                            readback.rob, readback.rob_flag, 16384)) ||
                        !environment.run_until_complete(32768) ||
                        !environment.run_until_lq_retired(8192)) {
                        std::cerr << "MEMBLOCK_TRANSLATION_PBMT_FAIL phase="
                                  << phase.str() << "-readback reason="
                                  << environment.error() << '\n';
                        return 1;
                    }
                    if ((final_nc || final_io) &&
                        (environment.tilelink_requests() !=
                             readback_dcache_before ||
                         environment.uncache_requests() !=
                             readback_uncache_before + 1)) {
                        std::cerr << "MEMBLOCK_TRANSLATION_PBMT_FAIL phase="
                                  << phase.str()
                                  << "-readback-path reason=wrong-manager\n";
                        return 1;
                    }
                    if (!final_nc && !final_io &&
                        environment.uncache_requests() !=
                            readback_uncache_before) {
                        std::cerr << "MEMBLOCK_TRANSLATION_PBMT_FAIL phase="
                                  << phase.str()
                                  << "-readback-path reason=PMA-used-uncache\n";
                        return 1;
                    }
                    ++load_transactions;
                    ++final_pbmt_counts[static_cast<unsigned>(final_pbmt)];
                    ++combinations;
                }
            }
        }
    }

    std::cout << "MEMBLOCK_TRANSLATION_PBMT_PASS combinations="
              << combinations << " loads=" << load_transactions
              << " stores=" << store_transactions
              << " final_pma=" << final_pbmt_counts[0]
              << " final_nc=" << final_pbmt_counts[1]
              << " final_io=" << final_pbmt_counts[2]
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_translation_superpages(int argc, char **argv)
{
    struct StageCase {
        memblock::ReferencePageMode mode;
        unsigned level;
        const char *name;
        std::uint64_t virtual_base;
        std::uint64_t physical_base;
    };
    constexpr std::array<StageCase, 5> stage_cases{{
        {memblock::ReferencePageMode::sv39, 1, "Sv39-2MiB",
         0x20000000ULL, 0x80800000ULL},
        {memblock::ReferencePageMode::sv39, 2, "Sv39-1GiB",
         0x40000000ULL, 0xc0000000ULL},
        {memblock::ReferencePageMode::sv48, 1, "Sv48-2MiB",
         0x20000000ULL, 0x81800000ULL},
        {memblock::ReferencePageMode::sv48, 2, "Sv48-1GiB",
         0x40000000ULL, 0xc0000000ULL},
        {memblock::ReferencePageMode::sv48, 3, "Sv48-512GiB",
         0x00000000ULL, 0x8000000000ULL},
    }};
    unsigned completed = 0;
    for (unsigned index = 0; index < stage_cases.size(); ++index) {
        const auto &test = stage_cases[index];
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t root = 0x97000000ULL;
        const std::uint64_t address = test.virtual_base + 0x188;
        const std::uint64_t physical = test.physical_base + 0x188;
        environment.memory().fill_incrementing(
            test.physical_base, 0x1000, static_cast<std::uint8_t>(0x30 + index));
        environment.configure_backpressure(
            0x243f6a8885a308d3ULL ^ index, true);
        bool configured = environment.reset();
        if (configured) {
            configured = test.mode == memblock::ReferencePageMode::sv39
                ? environment.map_sv39_leaf(
                    test.virtual_base, test.physical_base, test.level, root)
                : environment.map_sv48_leaf(
                    test.virtual_base, test.physical_base, test.level, root);
        }
        if (configured) {
            configured = test.mode == memblock::ReferencePageMode::sv39
                ? environment.activate_sv39(root, static_cast<std::uint16_t>(index))
                : environment.activate_sv48(root, static_cast<std::uint16_t>(index));
        }
        const auto reference = configured
            ? memblock::reference_page_walk(
                environment.memory(), root, address, test.mode)
            : memblock::ReferenceStageWalkResult{};
        const memblock::LoadTransaction transaction{
            .address = address,
            .oracle_address = physical,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(index),
            .lq = static_cast<std::uint8_t>(index),
            .pdest = static_cast<std::uint8_t>(100 + index),
            .lane = index % memblock::kScalarLoadLanes,
        };
        if (configured && reference.translated &&
            reference.physical_address == physical) {
            environment.expect_load(transaction);
            configured = environment.set_rob_head(
                    transaction.rob, transaction.rob_flag) &&
                environment.enqueue_load(transaction) &&
                environment.issue_load(transaction, 1024) &&
                environment.run_until_complete(16384);
        } else {
            configured = false;
        }
        if (!configured) {
            std::cerr << "MEMBLOCK_TRANSLATION_SUPERPAGES_FAIL case="
                      << test.name << " cycle=" << environment.cycle()
                      << " writebacks=" << environment.writebacks()
                      << " pending=" << environment.pending_scalar_loads()
                      << " ptw_requests=" << environment.ptw_requests()
                      << " tilelink_requests=" << environment.tilelink_requests()
                      << " reason=" << (environment.error().empty()
                          ? "reference-or-RTL translation mismatch"
                          : environment.error()) << '\n';
            return 1;
        }
        ++completed;
    }

    struct GCase {
        memblock::ReferencePageMode mode;
        unsigned level;
        const char *name;
        std::uint64_t base;
        memblock::ReferencePageMode vs_mode;
    };
    constexpr std::array<GCase, 5> g_cases{{
        {memblock::ReferencePageMode::sv39, 1, "Sv39x4-2MiB",
         0x80000000ULL, memblock::ReferencePageMode::sv39},
        {memblock::ReferencePageMode::sv39, 2, "Sv39x4-1GiB",
         0x80000000ULL, memblock::ReferencePageMode::sv39},
        {memblock::ReferencePageMode::sv48, 1, "Sv48x4-2MiB",
         0x80000000ULL, memblock::ReferencePageMode::sv48},
        {memblock::ReferencePageMode::sv48, 2, "Sv48x4-1GiB",
         0x80000000ULL, memblock::ReferencePageMode::sv48},
        {memblock::ReferencePageMode::sv48, 3, "Sv48x4-512GiB",
         0x8000000000ULL, memblock::ReferencePageMode::sv48},
    }};
    for (unsigned index = 0; index < g_cases.size(); ++index) {
        const auto &test = g_cases[index];
        memblock::Environment environment(argc, argv);
        const std::uint64_t vs_root = test.base + 0x100000ULL;
        constexpr std::uint64_t g_root = 0x98000000ULL;
        const std::uint64_t guest_data = test.base + 0x180000ULL;
        const std::uint64_t guest_virtual = test.vs_mode ==
                memblock::ReferencePageMode::sv48
            ? 0x40000000ULL
            : 0x20000000ULL;
        const std::uint64_t address = guest_virtual + 0x188;
        environment.memory().fill_incrementing(
            guest_data, 0x1000, static_cast<std::uint8_t>(0x70 + index));
        environment.configure_backpressure(
            0x13198a2e03707344ULL ^ (index * 0x9e3779b97f4a7c15ULL), true);
        bool configured = environment.reset();
        if (configured) {
            configured = test.mode == memblock::ReferencePageMode::sv39
                ? environment.map_sv39x4_leaf(
                    test.base, test.base, test.level, g_root)
                : environment.map_sv48x4_leaf(
                    test.base, test.base, test.level, g_root);
        }
        if (configured) {
            configured = test.vs_mode == memblock::ReferencePageMode::sv39
                ? environment.map_sv39_4k(address & ~0xfffULL, guest_data, vs_root)
                : environment.map_sv48_4k(address & ~0xfffULL, guest_data, vs_root);
        }
        if (configured) {
            configured = environment.activate_two_stage_modes(
                test.vs_mode, test.mode, vs_root, g_root,
                static_cast<std::uint16_t>(20 + index),
                static_cast<std::uint16_t>(30 + index));
        }
        const auto reference = configured
            ? memblock::reference_two_stage_walk(
                environment.memory(), vs_root, g_root, address,
                test.vs_mode, test.mode)
            : memblock::ReferenceTwoStageWalkResult{};
        const memblock::LoadTransaction transaction{
            .address = address,
            .oracle_address = guest_data + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = static_cast<std::uint8_t>(120 + index),
            .lane = (index + 1) % memblock::kScalarLoadLanes,
        };
        if (configured && reference.translated &&
            reference.physical_address == guest_data + 0x188) {
            environment.expect_load(transaction);
            configured = environment.set_rob_head(
                    transaction.rob, transaction.rob_flag) &&
                environment.enqueue_load(transaction) &&
                environment.issue_load(transaction, 2048) &&
                environment.run_until_complete(32768);
        } else {
            configured = false;
        }
        if (!configured) {
            std::cerr << "MEMBLOCK_TRANSLATION_SUPERPAGES_FAIL case="
                      << test.name << " cycle=" << environment.cycle()
                      << " reason=" << (environment.error().empty()
                          ? "nested reference-or-RTL translation mismatch"
                          : environment.error()) << '\n';
            return 1;
        }
        ++completed;
    }

    unsigned napot_stage1_subpages = 0;
    unsigned napot_gstage_subpages = 0;
    unsigned napot_loads = 0;
    unsigned napot_stores = 0;
    unsigned napot_readbacks = 0;
    std::uint64_t napot_ptw_requests = 0;
    std::uint64_t napot_cycles = 0;
    const auto execute_napot_subpage = [&](
        memblock::Environment &environment,
        std::string_view label,
        unsigned case_index,
        unsigned page,
        std::uint64_t address,
        std::uint64_t physical) {
        const std::uint64_t initial =
            0x1020304050607080ULL ^
            (static_cast<std::uint64_t>(case_index) << 48) ^
            (static_cast<std::uint64_t>(page) * 0x0101010101010101ULL);
        const std::uint64_t stored = initial ^ 0xff00ff0000ff00ffULL;
        environment.memory().write_u64(physical, initial);

        const memblock::LoadTransaction load{
            .address = address,
            .oracle_address = physical,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(page * 3),
            .lq = static_cast<std::uint8_t>(page * 2),
            .pdest = static_cast<std::uint8_t>(80 + page),
            .lane = (page + case_index) % memblock::kScalarLoadLanes,
        };
        environment.expect_load_data(load, initial);
        if (!environment.set_rob_head(load.rob, load.rob_flag) ||
            !environment.enqueue_load(load) ||
            !environment.issue_load(load, 2048) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_SUPERPAGES_FAIL case=" << label
                      << " phase=load page=" << page
                      << " reason=" << environment.error() << '\n';
            return false;
        }
        ++napot_loads;

        const memblock::StoreTransaction store{
            .address = address,
            .oracle_address = physical,
            .data = stored,
            .op = memblock::StoreOp::sd,
            .rob = static_cast<std::uint8_t>(page * 3 + 1),
            .sq = static_cast<std::uint8_t>(page),
            .address_lane = (page + case_index) % memblock::kScalarStoreLanes,
            .data_lane = (page + case_index + 1) %
                memblock::kScalarStoreLanes,
        };
        environment.expect_store(store);
        if (!environment.set_rob_head(store.rob, store.rob_flag) ||
            !environment.enqueue_store(store, 0) ||
            !environment.issue_store_address(store, 2048) ||
            !environment.issue_store_data(store, 2048) ||
            !environment.run_until_store_complete_with_replay(store, 16384) ||
            !environment.commit_store(store, 16384) ||
            !environment.run_until_sbuffer_empty(16384)) {
            std::cerr << "MEMBLOCK_TRANSLATION_SUPERPAGES_FAIL case=" << label
                      << " phase=store page=" << page
                      << " reason=" << environment.error() << '\n';
            return false;
        }
        ++napot_stores;

        const memblock::LoadTransaction readback{
            .address = address,
            .oracle_address = physical,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(page * 3 + 2),
            .lq = static_cast<std::uint8_t>(page * 2 + 1),
            .pdest = static_cast<std::uint8_t>(120 + page),
            .lane = (page + case_index + 1) % memblock::kScalarLoadLanes,
        };
        environment.expect_load_data(readback, stored);
        if (!environment.set_rob_head(readback.rob, readback.rob_flag) ||
            !environment.enqueue_load(readback) ||
            !environment.issue_load(readback, 2048) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_SUPERPAGES_FAIL case=" << label
                      << " phase=readback page=" << page
                      << " reason=" << environment.error() << '\n';
            return false;
        }
        ++napot_readbacks;
        return true;
    };

    const auto run_napot_case = [&](
        memblock::ReferencePageMode mode,
        bool gstage,
        unsigned case_index) {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t root = 0x99000000ULL;
        const std::uint64_t input_base = gstage
            ? 0x120040000ULL + case_index * 0x20000ULL
            : mode == memblock::ReferencePageMode::sv48
                ? 0xffff800012340000ULL
                : 0x62000000ULL;
        const std::uint64_t physical_base =
            0xd1000000ULL + case_index * 0x20000ULL;
        environment.configure_backpressure(
            0x3bd39e10cb0ef593ULL ^
                (case_index * 0x9e3779b97f4a7c15ULL),
            true);
        bool configured = environment.reset();
        if (configured) {
            if (gstage) {
                configured = mode == memblock::ReferencePageMode::sv48
                    ? environment.map_sv48x4_napot64k(
                        input_base, physical_base, root)
                    : environment.map_sv39x4_napot64k(
                        input_base, physical_base, root);
            } else {
                configured = mode == memblock::ReferencePageMode::sv48
                    ? environment.map_sv48_napot64k(
                        input_base, physical_base, root)
                    : environment.map_sv39_napot64k(
                        input_base, physical_base, root);
            }
        }
        if (configured) {
            configured = gstage
                ? environment.activate_two_stage_modes(
                    memblock::ReferencePageMode::bare, mode, 0, root, 0,
                    static_cast<std::uint16_t>(40 + case_index))
                : mode == memblock::ReferencePageMode::sv48
                    ? environment.activate_sv48(
                        root, static_cast<std::uint16_t>(40 + case_index))
                    : environment.activate_sv39(
                        root, static_cast<std::uint16_t>(40 + case_index));
        }
        if (!configured) {
            std::cerr << "MEMBLOCK_TRANSLATION_SUPERPAGES_FAIL case=Svnapot-"
                      << (gstage ? "G-" : "S-")
                      << (mode == memblock::ReferencePageMode::sv48
                              ? "Sv48" : "Sv39")
                      << " phase=configuration reason=" << environment.error()
                      << '\n';
            return false;
        }
        const std::string label = std::string("Svnapot-") +
            (gstage ? "G-" : "S-") +
            (mode == memblock::ReferencePageMode::sv48 ? "Sv48" : "Sv39");

        for (unsigned page = 0; page < 16; ++page) {
            const std::uint64_t byte_offset =
                page * 0x1000ULL + 0x180ULL + page * 8ULL;
            const std::uint64_t address = input_base + byte_offset;
            const std::uint64_t physical = physical_base + byte_offset;
            memblock::ReferenceTwoStageWalkResult reference;
            if (gstage) {
                reference = memblock::reference_two_stage_walk(
                    environment.memory(), 0, root, address,
                    memblock::ReferencePageMode::bare, mode);
            } else {
                const auto stage_reference = memblock::reference_page_walk(
                    environment.memory(), root, address, mode);
                reference.translated = stage_reference.translated;
                reference.physical_address = stage_reference.physical_address;
            }
            if (!reference.translated ||
                reference.physical_address != physical) {
                std::cerr << "MEMBLOCK_TRANSLATION_SUPERPAGES_FAIL case="
                          << label
                          << " phase=reference page=" << page
                          << " expected=0x" << std::hex << physical
                          << " actual=0x" << reference.physical_address
                          << std::dec << '\n';
                return false;
            }
            if (!execute_napot_subpage(
                    environment, label, case_index, page, address, physical)) {
                return false;
            }
            ++completed;
        }
        if (gstage) {
            napot_gstage_subpages += 16;
        } else {
            napot_stage1_subpages += 16;
        }
        napot_ptw_requests += environment.ptw_requests();
        napot_cycles += environment.cycle();
        return true;
    };

    if (!run_napot_case(memblock::ReferencePageMode::sv39, false, 0) ||
        !run_napot_case(memblock::ReferencePageMode::sv48, false, 1) ||
        !run_napot_case(memblock::ReferencePageMode::sv39, true, 2) ||
        !run_napot_case(memblock::ReferencePageMode::sv48, true, 3)) {
        return 1;
    }

    struct NestedNapotPair {
        memblock::ReferencePageMode vs_mode;
        memblock::ReferencePageMode g_mode;
        const char *name;
    };
    constexpr std::array<NestedNapotPair, 4> nested_napot_pairs{{
        {memblock::ReferencePageMode::sv39,
         memblock::ReferencePageMode::sv39, "Sv39-Sv39x4"},
        {memblock::ReferencePageMode::sv39,
         memblock::ReferencePageMode::sv48, "Sv39-Sv48x4"},
        {memblock::ReferencePageMode::sv48,
         memblock::ReferencePageMode::sv39, "Sv48-Sv39x4"},
        {memblock::ReferencePageMode::sv48,
         memblock::ReferencePageMode::sv48, "Sv48-Sv48x4"},
    }};
    struct NestedNapotTopology {
        bool vs_napot;
        bool g_napot;
        const char *name;
    };
    constexpr std::array<NestedNapotTopology, 3> nested_napot_topologies{{
        {true, false, "VS-NAPOT"},
        {false, true, "G-NAPOT"},
        {true, true, "VS-G-NAPOT"},
    }};
    unsigned napot_nested_mode_cases = 0;
    unsigned napot_nested_subpages = 0;
    unsigned napot_nested_vs_subpages = 0;
    unsigned napot_nested_g_subpages = 0;
    unsigned napot_nested_both_subpages = 0;

    for (unsigned pair_index = 0;
         pair_index < nested_napot_pairs.size(); ++pair_index) {
        const auto &pair = nested_napot_pairs[pair_index];
        for (unsigned topology_index = 0;
             topology_index < nested_napot_topologies.size();
             ++topology_index) {
            const auto &topology = nested_napot_topologies[topology_index];
            const unsigned case_index =
                4 + pair_index * nested_napot_topologies.size() + topology_index;
            memblock::Environment environment(argc, argv);
            const std::uint64_t guest_virtual_base =
                pair.vs_mode == memblock::ReferencePageMode::sv48
                    ? 0xffff800020000000ULL + topology_index * 0x20000ULL
                    : 0x64000000ULL + topology_index * 0x20000ULL;
            const std::uint64_t guest_physical_base =
                0x130000000ULL + case_index * 0x20000ULL;
            const std::uint64_t host_physical_base =
                0xd4000000ULL + case_index * 0x20000ULL;
            const std::uint64_t vs_root =
                0x9a000000ULL + case_index * 0x100000ULL;
            const std::uint64_t g_root =
                0xaa000000ULL + case_index * 0x100000ULL;
            const std::string label = std::string(pair.name) + '-' + topology.name;
            environment.configure_backpressure(
                0xc0ac29b7c97c50ddULL ^
                    (case_index * 0x9e3779b97f4a7c15ULL),
                true);
            bool configured = environment.reset();

            if (configured && topology.vs_napot) {
                configured = pair.vs_mode == memblock::ReferencePageMode::sv48
                    ? environment.map_sv48_napot64k(
                        guest_virtual_base, guest_physical_base, vs_root)
                    : environment.map_sv39_napot64k(
                        guest_virtual_base, guest_physical_base, vs_root);
            }
            for (unsigned page = 0;
                 configured && !topology.vs_napot && page < 16; ++page) {
                const std::uint64_t offset = page * 0x1000ULL;
                configured = pair.vs_mode == memblock::ReferencePageMode::sv48
                    ? environment.map_sv48_4k(
                        guest_virtual_base + offset,
                        guest_physical_base + offset, vs_root)
                    : environment.map_sv39_4k(
                        guest_virtual_base + offset,
                        guest_physical_base + offset, vs_root);
            }

            const unsigned vs_table_pages =
                pair.vs_mode == memblock::ReferencePageMode::sv48 ? 4U : 3U;
            for (unsigned page = 0;
                 configured && page < vs_table_pages; ++page) {
                const std::uint64_t address = vs_root + page * 0x1000ULL;
                configured = pair.g_mode == memblock::ReferencePageMode::sv48
                    ? environment.map_sv48x4_4k(address, address, g_root)
                    : environment.map_sv39x4_4k(address, address, g_root);
            }

            if (configured && topology.g_napot) {
                configured = pair.g_mode == memblock::ReferencePageMode::sv48
                    ? environment.map_sv48x4_napot64k(
                        guest_physical_base, host_physical_base, g_root)
                    : environment.map_sv39x4_napot64k(
                        guest_physical_base, host_physical_base, g_root);
            }
            for (unsigned page = 0;
                 configured && !topology.g_napot && page < 16; ++page) {
                const std::uint64_t offset = page * 0x1000ULL;
                configured = pair.g_mode == memblock::ReferencePageMode::sv48
                    ? environment.map_sv48x4_4k(
                        guest_physical_base + offset,
                        host_physical_base + offset, g_root)
                    : environment.map_sv39x4_4k(
                        guest_physical_base + offset,
                        host_physical_base + offset, g_root);
            }
            if (configured) {
                configured = environment.activate_two_stage_modes(
                    pair.vs_mode, pair.g_mode, vs_root, g_root,
                    static_cast<std::uint16_t>(70 + case_index),
                    static_cast<std::uint16_t>(90 + case_index));
            }
            if (!configured) {
                std::cerr << "MEMBLOCK_TRANSLATION_SUPERPAGES_FAIL case="
                          << label << " phase=configuration reason="
                          << environment.error() << '\n';
                return 1;
            }

            for (unsigned page = 0; page < 16; ++page) {
                const std::uint64_t byte_offset =
                    page * 0x1000ULL + 0x180ULL + page * 8ULL;
                const std::uint64_t address =
                    guest_virtual_base + byte_offset;
                const std::uint64_t physical =
                    host_physical_base + byte_offset;
                const auto reference = memblock::reference_two_stage_walk(
                    environment.memory(), vs_root, g_root, address,
                    pair.vs_mode, pair.g_mode);
                if (!reference.translated ||
                    reference.physical_address != physical) {
                    std::cerr << "MEMBLOCK_TRANSLATION_SUPERPAGES_FAIL case="
                              << label << " phase=reference page=" << page
                              << " expected=0x" << std::hex << physical
                              << " actual=0x" << reference.physical_address
                              << std::dec << '\n';
                    return 1;
                }
                if (!execute_napot_subpage(
                        environment, label, case_index, page, address,
                        physical)) {
                    return 1;
                }
                ++completed;
            }

            ++napot_nested_mode_cases;
            napot_nested_subpages += 16;
            napot_nested_vs_subpages += topology.vs_napot ? 16 : 0;
            napot_nested_g_subpages += topology.g_napot ? 16 : 0;
            napot_nested_both_subpages +=
                topology.vs_napot && topology.g_napot ? 16 : 0;
            napot_ptw_requests += environment.ptw_requests();
            napot_cycles += environment.cycle();
        }
    }

    std::cout << "MEMBLOCK_TRANSLATION_SUPERPAGES_PASS"
              << " cases=" << completed
              << " napot_stage1_subpages=" << napot_stage1_subpages
              << " napot_gstage_subpages=" << napot_gstage_subpages
              << " napot_nested_mode_cases=" << napot_nested_mode_cases
              << " napot_nested_subpages=" << napot_nested_subpages
              << " napot_nested_vs_subpages=" << napot_nested_vs_subpages
              << " napot_nested_g_subpages=" << napot_nested_g_subpages
              << " napot_nested_both_subpages=" << napot_nested_both_subpages
              << " napot_loads=" << napot_loads
              << " napot_stores=" << napot_stores
              << " napot_readbacks=" << napot_readbacks
              << " napot_ptw_requests=" << napot_ptw_requests
              << " napot_cycles=" << napot_cycles
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}
