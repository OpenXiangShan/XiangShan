// This file is included inside memblock_main.cpp's anonymous namespace.
// Keep scenario-local helpers here; shared infrastructure belongs in memblock_env.hpp.

int run_ifetch_ptw_bridge(int argc, char **argv)
{
    enum class GlobalPtePlacement : std::uint8_t {
        none,
        leaf,
        nonleaf,
    };
    struct Case {
        const char *name;
        memblock::ReferencePageMode vs_mode;
        memblock::ReferencePageMode g_mode;
        bool nested;
        memblock::ReferencePbmt pbmt;
        bool page_fault;
        GlobalPtePlacement vs_global = GlobalPtePlacement::none;
        GlobalPtePlacement g_global = GlobalPtePlacement::none;
    };
    constexpr std::array<Case, 16> cases{{
        {"Sv39", memblock::ReferencePageMode::sv39,
         memblock::ReferencePageMode::bare, false,
         memblock::ReferencePbmt::pma, false},
        {"Sv48", memblock::ReferencePageMode::sv48,
         memblock::ReferencePageMode::bare, false,
         memblock::ReferencePbmt::pma, false},
        {"Sv39-Sv39x4", memblock::ReferencePageMode::sv39,
         memblock::ReferencePageMode::sv39, true,
         memblock::ReferencePbmt::pma, false},
        {"Sv39-Sv48x4", memblock::ReferencePageMode::sv39,
         memblock::ReferencePageMode::sv48, true,
         memblock::ReferencePbmt::pma, false},
        {"Sv48-Sv39x4", memblock::ReferencePageMode::sv48,
         memblock::ReferencePageMode::sv39, true,
         memblock::ReferencePbmt::pma, false},
        {"Sv48-Sv48x4", memblock::ReferencePageMode::sv48,
         memblock::ReferencePageMode::sv48, true,
         memblock::ReferencePbmt::pma, false},
        {"Sv39-PBMT-NC", memblock::ReferencePageMode::sv39,
         memblock::ReferencePageMode::bare, false,
         memblock::ReferencePbmt::nc, false},
        {"Sv48-PBMT-IO", memblock::ReferencePageMode::sv48,
         memblock::ReferencePageMode::bare, false,
         memblock::ReferencePbmt::io, false},
        {"Sv39-invalid-L0", memblock::ReferencePageMode::sv39,
         memblock::ReferencePageMode::bare, false,
         memblock::ReferencePbmt::pma, true},
        {"Sv48-invalid-L0", memblock::ReferencePageMode::sv48,
         memblock::ReferencePageMode::bare, false,
         memblock::ReferencePbmt::pma, true},
        {"Sv39-global-leaf", memblock::ReferencePageMode::sv39,
         memblock::ReferencePageMode::bare, false,
         memblock::ReferencePbmt::pma, false,
         GlobalPtePlacement::leaf},
        {"Sv48-global-nonleaf", memblock::ReferencePageMode::sv48,
         memblock::ReferencePageMode::bare, false,
         memblock::ReferencePbmt::pma, false,
         GlobalPtePlacement::nonleaf},
        {"Sv39-Sv39x4-global-leaves", memblock::ReferencePageMode::sv39,
         memblock::ReferencePageMode::sv39, true,
         memblock::ReferencePbmt::pma, false,
         GlobalPtePlacement::leaf, GlobalPtePlacement::leaf},
        {"Sv39-Sv48x4-global-nonleaves", memblock::ReferencePageMode::sv39,
         memblock::ReferencePageMode::sv48, true,
         memblock::ReferencePbmt::pma, false,
         GlobalPtePlacement::nonleaf, GlobalPtePlacement::nonleaf},
        {"Sv48-Sv39x4-global-mixed", memblock::ReferencePageMode::sv48,
         memblock::ReferencePageMode::sv39, true,
         memblock::ReferencePbmt::pma, false,
         GlobalPtePlacement::leaf, GlobalPtePlacement::nonleaf},
        {"Sv48-Sv48x4-global-mixed", memblock::ReferencePageMode::sv48,
         memblock::ReferencePageMode::sv48, true,
         memblock::ReferencePbmt::pma, false,
         GlobalPtePlacement::nonleaf, GlobalPtePlacement::leaf},
    }};
    constexpr std::uint64_t vs_root = 0x94000000ULL;
    constexpr std::uint64_t g_root = 0x95000000ULL;
    constexpr std::uint16_t asid = 0x35;
    constexpr std::uint16_t vmid = 0x29;
    constexpr unsigned response_stall_cycles = 5;
    std::uint64_t total_cycles = 0;
    std::uint64_t total_ptw_requests = 0;
    unsigned s1_global_reported = 0;
    unsigned s1_global_demoted = 0;
    unsigned gstage_global_reported = 0;
    unsigned gstage_global_demoted = 0;

    for (unsigned case_index = 0; case_index < cases.size(); ++case_index) {
        const auto &item = cases[case_index];
        memblock::Environment environment(argc, argv);
        const std::uint64_t virtual_page = 0x60012000ULL +
            case_index * 0x200000ULL;
        const std::uint64_t guest_page = 0x120012000ULL +
            case_index * 0x200000ULL;
        const std::uint64_t host_page = 0xc0012000ULL +
            case_index * 0x200000ULL;
        const std::uint64_t stage1_output = item.nested
            ? guest_page
            : host_page;
        environment.configure_backpressure(
            0xd1b54a32d192ed03ULL ^ case_index, true,
            memblock::ResponseLatencyProfile::spec);

        bool configured = environment.reset();
        if (configured) {
            configured = item.vs_mode == memblock::ReferencePageMode::sv48
                ? environment.map_sv48_4k(
                      virtual_page, stage1_output, vs_root, true, false,
                      true, false,
                      item.pbmt == memblock::ReferencePbmt::nc,
                      item.pbmt == memblock::ReferencePbmt::io,
                      item.vs_global == GlobalPtePlacement::leaf)
                : environment.map_sv39_4k(
                      virtual_page, stage1_output, vs_root, true, false,
                      true, false,
                      item.pbmt == memblock::ReferencePbmt::nc,
                      item.pbmt == memblock::ReferencePbmt::io,
                      item.vs_global == GlobalPtePlacement::leaf);
        }
        if (configured &&
            item.vs_global == GlobalPtePlacement::nonleaf) {
            const auto nonleaf_pte = memblock::reference_pte_address_at_level(
                environment.memory(), vs_root, virtual_page, item.vs_mode,
                memblock::reference_page_levels(item.vs_mode) - 1U);
            if (!nonleaf_pte) {
                std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case="
                          << item.name
                          << " phase=locate-vs-global-nonleaf\n";
                return 1;
            }
            environment.memory().write_u64(
                *nonleaf_pte,
                environment.memory().read_u64(*nonleaf_pte) |
                    (std::uint64_t{1} << 5));
        }
        if (configured && item.nested) {
            const unsigned vs_table_pages = item.vs_mode ==
                    memblock::ReferencePageMode::sv48
                ? 4U
                : 3U;
            for (unsigned page = 0; configured && page < vs_table_pages;
                 ++page) {
                const std::uint64_t address = vs_root + page * 0x1000ULL;
                configured = item.g_mode == memblock::ReferencePageMode::sv48
                    ? environment.map_sv48x4_4k(
                          address, address, g_root, true, true, false)
                    : environment.map_sv39x4_4k(
                          address, address, g_root, true, true, false);
            }
            if (configured) {
                configured = item.g_mode == memblock::ReferencePageMode::sv48
                    ? environment.map_sv48x4_4k(
                          guest_page, host_page, g_root, true, false, true,
                          item.g_global == GlobalPtePlacement::leaf)
                    : environment.map_sv39x4_4k(
                          guest_page, host_page, g_root, true, false, true,
                          item.g_global == GlobalPtePlacement::leaf);
            }
            if (configured &&
                item.g_global == GlobalPtePlacement::nonleaf) {
                const auto nonleaf_pte =
                    memblock::reference_pte_address_at_level(
                        environment.memory(), g_root, guest_page,
                        item.g_mode,
                        memblock::reference_page_levels(item.g_mode) - 1U,
                        true);
                if (!nonleaf_pte) {
                    std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case="
                              << item.name
                              << " phase=locate-g-global-nonleaf\n";
                    return 1;
                }
                environment.memory().write_u64(
                    *nonleaf_pte,
                    environment.memory().read_u64(*nonleaf_pte) |
                        (std::uint64_t{1} << 5));
            }
        }
        if (configured && item.page_fault) {
            const auto leaf_pte = memblock::reference_pte_address_at_level(
                environment.memory(), vs_root, virtual_page, item.vs_mode, 0);
            if (!leaf_pte) {
                configured = false;
            } else {
                environment.memory().write_u64(*leaf_pte, 0);
            }
        }
        if (configured) {
            const auto reference = memblock::reference_page_walk(
                environment.memory(), vs_root, virtual_page, item.vs_mode);
            configured = item.page_fault
                ? !reference.translated && reference.fault_level == 0
                : reference.translated &&
                    reference.physical_address == stage1_output;
        }
        if (configured) {
            configured = item.nested
                ? environment.activate_two_stage_modes(
                      item.vs_mode, item.g_mode, vs_root, g_root, asid, vmid)
                : item.vs_mode == memblock::ReferencePageMode::sv48
                    ? environment.activate_sv48(vs_root, asid)
                    : environment.activate_sv39(vs_root, asid);
        }
        if (!configured) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=" << item.name
                      << " cycle=" << environment.cycle()
                      << " phase=configuration reason=" << environment.error()
                      << '\n';
            return 1;
        }

        const std::uint64_t vpn = virtual_page >> 12;
        memblock::Environment::IFetchPtwResponse response;
        const auto translation_mode = item.nested
            ? memblock::PtwTranslationMode::all_stages
            : memblock::PtwTranslationMode::no_stage_two;
        if (!environment.issue_ifetch_ptw_request(
                vpn, translation_mode, response,
                response_stall_cycles)) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=" << item.name
                      << " cycle=" << environment.cycle()
                      << " phase=request reason=" << environment.error()
                      << '\n';
            return 1;
        }

        const unsigned sector = static_cast<unsigned>(vpn & 7U);
        const std::uint64_t stage1_ppn = stage1_output >> 12;
        const std::uint64_t reconstructed_s1_ppn =
            (response.s1_ppn << 3) | response.s1_ppn_low[sector];
        const bool s1_identity_valid =
            response.s2xlate == static_cast<std::uint8_t>(translation_mode) &&
            response.s1_tag == (vpn >> 3) && response.s1_asid == asid &&
            response.s1_n == false &&
            response.s1_pbmt == static_cast<std::uint8_t>(item.pbmt) &&
            response.s1_level == 0 &&
            response.s1_addr_low == sector &&
            response.s1_pteidx == (1U << sector) &&
            (response.s1_valididx & (1U << sector)) != 0;
        const bool s1_result_valid = item.page_fault
            ? !response.s1_d && !response.s1_a && !response.s1_g &&
                !response.s1_u && !response.s1_x && !response.s1_w &&
                !response.s1_r && !response.s1_v &&
                reconstructed_s1_ppn == 0 && response.s1_pf &&
                !response.s1_af
            // Caching a global mapping as ASID-specific is a legal,
            // conservative implementation choice. Never permit the inverse.
            : !response.s1_d && response.s1_a &&
                (item.vs_global != GlobalPtePlacement::none ||
                 !response.s1_g) &&
                !response.s1_u && response.s1_x && !response.s1_w &&
                response.s1_r && response.s1_v &&
                reconstructed_s1_ppn == stage1_ppn &&
                !response.s1_pf && !response.s1_af;
        const bool s1_valid = s1_identity_valid && s1_result_valid;
        const bool s2_valid = !item.nested ||
            (response.s1_vmid == vmid &&
             response.s2_tag == (guest_page >> 12) &&
             response.s2_vmid == vmid && !response.s2_n &&
             response.s2_pbmt == 0 && response.s2_ppn == (host_page >> 12) &&
             // G-stage G is architecturally ignored. The internal response
             // may expose or demote the raw bit, but it must not alter the
             // translation, permissions, or fault result checked here.
             !response.s2_d && response.s2_a &&
             (item.g_global != GlobalPtePlacement::none ||
              !response.s2_g) &&
             response.s2_u && response.s2_x && !response.s2_w &&
             response.s2_r && response.s2_level == 0 &&
             !response.s2_gpf && !response.s2_gaf);
        if (!s1_valid || !s2_valid || environment.ptw_requests() == 0) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=" << item.name
                      << " cycle=" << environment.cycle()
                      << " phase=response"
                      << " s2xlate=" << static_cast<unsigned>(response.s2xlate)
                      << " s1_tag=0x" << std::hex << response.s1_tag
                      << " s1_ppn=0x" << reconstructed_s1_ppn
                      << " s2_tag=0x" << response.s2_tag
                      << " s2_ppn=0x" << response.s2_ppn << std::dec
                      << " s1_asid=" << response.s1_asid
                      << " s1_vmid=" << response.s1_vmid
                      << " s2_vmid=" << response.s2_vmid
                      << " s1_pbmt=" << static_cast<unsigned>(response.s1_pbmt)
                      << " s1_v=" << response.s1_v
                      << " s1_perm=" << response.s1_d << response.s1_a
                      << response.s1_g << response.s1_u << response.s1_x
                      << response.s1_w << response.s1_r
                      << " s2_perm=" << response.s2_d << response.s2_a
                      << response.s2_g << response.s2_u << response.s2_x
                      << response.s2_w << response.s2_r
                      << " s1_pf=" << response.s1_pf
                      << " s1_af=" << response.s1_af
                      << " s2_gpf=" << response.s2_gpf
                      << " s2_gaf=" << response.s2_gaf
                      << " ptw_requests=" << environment.ptw_requests()
                      << '\n';
            return 1;
        }
        if (item.vs_global != GlobalPtePlacement::none) {
            response.s1_g ? ++s1_global_reported : ++s1_global_demoted;
        }
        if (item.g_global != GlobalPtePlacement::none) {
            response.s2_g
                ? ++gstage_global_reported
                : ++gstage_global_demoted;
        }
        total_cycles += environment.cycle();
        total_ptw_requests += environment.ptw_requests();
    }

    constexpr std::uint64_t sector_virtual_base = 0x64018000ULL;
    constexpr std::uint64_t sector_root = 0x95800000ULL;
    constexpr std::array<std::uint8_t, 8> sector_ppn_low{
        5, 0, 7, 2, 6, 1, 4, 3};
    constexpr std::array<std::uint8_t, 8> sector_group{
        0, 1, 2, 0, 2, 0, 1, 2};
    constexpr std::array<std::uint8_t, 8> sector_request_order{
        7, 0, 5, 2, 6, 1, 4, 3};
    constexpr std::array<std::uint8_t, 3> sector_valid_masks{
        0x29, 0x42, 0x94};
    constexpr std::array<std::uint64_t, 3> sector_physical_groups{
        0xc8000000ULL, 0xc8010000ULL, 0xc8020000ULL};
    memblock::Environment sector_environment(argc, argv);
    sector_environment.configure_backpressure(
        0x243f6a8885a308d3ULL, true,
        memblock::ResponseLatencyProfile::spec);
    bool sector_configured = sector_environment.reset();
    for (unsigned sector = 0;
         sector_configured && sector < sector_ppn_low.size(); ++sector) {
        const std::uint64_t virtual_page =
            sector_virtual_base + sector * 0x1000ULL;
        const std::uint64_t physical_page =
            sector_physical_groups[sector_group[sector]] +
            sector_ppn_low[sector] * 0x1000ULL;
        sector_configured = sector_environment.map_sv39_4k(
            virtual_page, physical_page, sector_root,
            true, false, true, false);
        const auto reference = memblock::reference_page_walk(
            sector_environment.memory(), sector_root, virtual_page,
            memblock::ReferencePageMode::sv39);
        sector_configured = sector_configured && reference.translated &&
            reference.physical_address == physical_page;
    }
    sector_configured = sector_configured &&
        sector_environment.activate_sv39(sector_root, asid);
    if (!sector_configured) {
        std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=Sv39-sector"
                  << " cycle=" << sector_environment.cycle()
                  << " phase=configuration reason="
                  << sector_environment.error() << '\n';
        return 1;
    }

    const std::uint64_t sector_ptw_before =
        sector_environment.ptw_requests();
    std::uint64_t sector_ptw_after_cold = 0;
    for (unsigned request_index = 0;
         request_index < sector_request_order.size(); ++request_index) {
        const unsigned sector = sector_request_order[request_index];
        const std::uint64_t virtual_page =
            sector_virtual_base + sector * 0x1000ULL;
        const std::uint64_t physical_page =
            sector_physical_groups[sector_group[sector]] +
            sector_ppn_low[sector] * 0x1000ULL;
        const std::uint64_t vpn = virtual_page >> 12;
        memblock::Environment::IFetchPtwResponse response{};
        if (!sector_environment.issue_ifetch_ptw_request(
                vpn, memblock::PtwTranslationMode::no_stage_two, response,
                response_stall_cycles)) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=Sv39-sector"
                      << " cycle=" << sector_environment.cycle()
                      << " phase=request sector=" << sector
                      << " reason=" << sector_environment.error() << '\n';
            return 1;
        }
        if (request_index == 0) {
            sector_ptw_after_cold = sector_environment.ptw_requests();
        }

        bool sibling_ppn_low_valid = true;
        const std::uint8_t valid_mask =
            sector_valid_masks[sector_group[sector]];
        for (unsigned sibling = 0;
             sibling < sector_ppn_low.size(); ++sibling) {
            if ((valid_mask & (1U << sibling)) != 0 &&
                response.s1_ppn_low[sibling] != sector_ppn_low[sibling]) {
                sibling_ppn_low_valid = false;
            }
        }
        const std::uint64_t reconstructed_ppn =
            (response.s1_ppn << 3) | response.s1_ppn_low[sector];
        const bool payload_valid =
            response.s2xlate == static_cast<std::uint8_t>(
                memblock::PtwTranslationMode::no_stage_two) &&
            response.s1_tag == (vpn >> 3) && response.s1_asid == asid &&
            !response.s1_n && response.s1_pbmt == 0 && !response.s1_d &&
            response.s1_a && !response.s1_g && !response.s1_u &&
            response.s1_x && !response.s1_w && response.s1_r &&
            response.s1_level == 0 && response.s1_v &&
            response.s1_addr_low == sector &&
            response.s1_pteidx == (1U << sector) &&
            response.s1_valididx == valid_mask &&
            sibling_ppn_low_valid &&
            reconstructed_ppn == (physical_page >> 12) &&
            !response.s1_pf && !response.s1_af;
        const bool ptw_count_valid = request_index == 0
            ? sector_ptw_after_cold == sector_ptw_before + 3
            : sector_environment.ptw_requests() == sector_ptw_after_cold;
        if (!payload_valid || !ptw_count_valid) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=Sv39-sector"
                      << " cycle=" << sector_environment.cycle()
                      << " phase=response sector=" << sector
                      << " s1_tag=0x" << std::hex << response.s1_tag
                      << " ppn=0x" << reconstructed_ppn
                      << " valididx=0x"
                      << static_cast<unsigned>(response.s1_valididx)
                      << " pteidx=0x"
                      << static_cast<unsigned>(response.s1_pteidx)
                      << std::dec << " addr_low="
                      << static_cast<unsigned>(response.s1_addr_low)
                      << " ptw=" << sector_ptw_before << '/'
                      << sector_ptw_after_cold << '/'
                      << sector_environment.ptw_requests()
                      << " reason=" << sector_environment.error() << '\n';
            return 1;
        }
    }
    total_cycles += sector_environment.cycle();
    total_ptw_requests += sector_environment.ptw_requests();

    struct DegenerateCase {
        const char *name;
        memblock::ReferencePageMode mode;
        bool only_stage_two;
    };
    constexpr std::array<DegenerateCase, 4> degenerate_cases{{
        {"Sv39x4-onlyStage2", memblock::ReferencePageMode::sv39, true},
        {"Sv48x4-onlyStage2", memblock::ReferencePageMode::sv48, true},
        {"Sv39-onlyStage1", memblock::ReferencePageMode::sv39, false},
        {"Sv48-onlyStage1", memblock::ReferencePageMode::sv48, false},
    }};
    for (unsigned case_index = 0; case_index < degenerate_cases.size();
         ++case_index) {
        const auto &item = degenerate_cases[case_index];
        memblock::Environment environment(argc, argv);
        const std::uint64_t input_page = 0xb8012000ULL +
            case_index * 0x200000ULL;
        const std::uint64_t output_page = 0xd0012000ULL +
            case_index * 0x200000ULL;
        environment.configure_backpressure(
            0x94d049bb133111ebULL ^ case_index, true,
            memblock::ResponseLatencyProfile::spec);

        bool configured = environment.reset();
        if (configured) {
            if (item.only_stage_two) {
                configured = item.mode == memblock::ReferencePageMode::sv48
                    ? environment.map_sv48x4_4k(
                          input_page, output_page, g_root, true, false, true)
                    : environment.map_sv39x4_4k(
                          input_page, output_page, g_root, true, false, true);
            } else {
                configured = item.mode == memblock::ReferencePageMode::sv48
                    ? environment.map_sv48_4k(
                          input_page, output_page, vs_root, true, false,
                          true, false)
                    : environment.map_sv39_4k(
                          input_page, output_page, vs_root, true, false,
                          true, false);
            }
        }
        if (configured) {
            const auto reference = memblock::reference_page_walk(
                environment.memory(), item.only_stage_two ? g_root : vs_root,
                input_page, item.mode, item.only_stage_two);
            configured = reference.translated &&
                reference.physical_address == output_page;
        }
        if (configured) {
            configured = environment.activate_two_stage_modes(
                item.only_stage_two
                    ? memblock::ReferencePageMode::bare
                    : item.mode,
                item.only_stage_two
                    ? item.mode
                    : memblock::ReferencePageMode::bare,
                item.only_stage_two ? 0 : vs_root,
                item.only_stage_two ? g_root : 0, asid, vmid);
        }
        if (configured) {
            configured = environment.run_cycles(64);
        }
        if (!configured) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=" << item.name
                      << " cycle=" << environment.cycle()
                      << " phase=configuration reason=" << environment.error()
                      << '\n';
            return 1;
        }

        const std::uint64_t vpn = input_page >> 12;
        const std::uint64_t ptw_before_request = environment.ptw_requests();
        memblock::Environment::IFetchPtwResponse response;
        const auto translation_mode = item.only_stage_two
            ? memblock::PtwTranslationMode::only_stage_two
            : memblock::PtwTranslationMode::only_stage_one;
        if (!environment.issue_ifetch_ptw_request(
                vpn, translation_mode, response,
                response_stall_cycles)) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=" << item.name
                      << " cycle=" << environment.cycle()
                      << " phase=request reason=" << environment.error()
                      << '\n';
            return 1;
        }

        const unsigned sector = static_cast<unsigned>(vpn & 7U);
        const std::uint64_t reconstructed_s1_ppn =
            (response.s1_ppn << 3) | response.s1_ppn_low[sector];
        const bool s1_valid = item.only_stage_two || (
            response.s1_tag == (vpn >> 3) && response.s1_asid == asid &&
            response.s1_vmid == vmid && !response.s1_n &&
            response.s1_pbmt == 0 && response.s1_addr_low == sector &&
            response.s1_pteidx == (1U << sector) &&
            (response.s1_valididx & (1U << sector)) != 0 &&
            !response.s1_d && response.s1_a && !response.s1_g &&
            !response.s1_u && response.s1_x && !response.s1_w &&
            response.s1_r && response.s1_level == 0 && response.s1_v &&
            reconstructed_s1_ppn == (output_page >> 12) &&
            !response.s1_pf && !response.s1_af);
        const bool s2_valid = !item.only_stage_two ||
            (response.s2_tag == vpn && response.s2_vmid == vmid &&
             !response.s2_n && response.s2_pbmt == 0 &&
             response.s2_ppn == (output_page >> 12) && !response.s2_d &&
             response.s2_a && !response.s2_g && response.s2_u &&
             response.s2_x && !response.s2_w && response.s2_r &&
             response.s2_level == 0 && !response.s2_gpf &&
             !response.s2_gaf);
        if (response.s2xlate != static_cast<std::uint8_t>(translation_mode) ||
            !s1_valid || !s2_valid ||
            environment.ptw_requests() <= ptw_before_request) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=" << item.name
                      << " cycle=" << environment.cycle()
                      << " phase=response"
                      << " s2xlate=" << static_cast<unsigned>(response.s2xlate)
                      << " s1_tag=0x" << std::hex << response.s1_tag
                      << " s1_ppn=0x" << reconstructed_s1_ppn
                      << " s2_tag=0x" << response.s2_tag
                      << " s2_ppn=0x" << response.s2_ppn << std::dec
                      << " s1_asid=" << response.s1_asid
                      << " s1_vmid=" << response.s1_vmid
                      << " s2_vmid=" << response.s2_vmid
                      << " s1_v=" << response.s1_v
                      << " s1_pf=" << response.s1_pf
                      << " s1_af=" << response.s1_af
                      << " s2_gpf=" << response.s2_gpf
                      << " s2_gaf=" << response.s2_gaf
                      << " ptw_requests=" << environment.ptw_requests()
                      << '\n';
            return 1;
        }
        total_cycles += environment.cycle();
        total_ptw_requests += environment.ptw_requests();
    }

    constexpr std::uint64_t concurrent_root = 0x96000000ULL;
    constexpr std::uint64_t ifetch_virtual = 0x60012000ULL;
    constexpr std::uint64_t ifetch_physical = 0xc0012000ULL;
    constexpr std::uint64_t load_virtual = 0xa0012000ULL;
    constexpr std::uint64_t load_physical = 0xd0012000ULL;
    constexpr unsigned concurrent_ptw_delay = 256;
    memblock::Environment concurrent(argc, argv);
    concurrent.configure_backpressure(
        0x8538ec6303f2f4ebULL, true,
        memblock::ResponseLatencyProfile::spec);
    concurrent.memory().fill_incrementing(load_physical, 0x1000, 0x5d);
    bool concurrent_configured = concurrent.reset() &&
        concurrent.map_sv39_4k(
            ifetch_virtual, ifetch_physical, concurrent_root, true, false,
            true, false) &&
        concurrent.map_sv39_4k(
            load_virtual, load_physical, concurrent_root, true, true,
            false, false) &&
        concurrent.activate_sv39(concurrent_root, asid);
    const auto ifetch_reference = memblock::reference_page_walk(
        concurrent.memory(), concurrent_root, ifetch_virtual,
        memblock::ReferencePageMode::sv39);
    const auto load_reference = memblock::reference_page_walk(
        concurrent.memory(), concurrent_root, load_virtual + 0x188,
        memblock::ReferencePageMode::sv39);
    const auto ifetch_root_pte = memblock::reference_pte_address_at_level(
        concurrent.memory(), concurrent_root, ifetch_virtual,
        memblock::ReferencePageMode::sv39, 2);
    const auto ifetch_leaf_pte = memblock::reference_pte_address_at_level(
        concurrent.memory(), concurrent_root, ifetch_virtual,
        memblock::ReferencePageMode::sv39, 0);
    const auto load_root_pte = memblock::reference_pte_address_at_level(
        concurrent.memory(), concurrent_root, load_virtual,
        memblock::ReferencePageMode::sv39, 2);
    const auto load_leaf_pte = memblock::reference_pte_address_at_level(
        concurrent.memory(), concurrent_root, load_virtual,
        memblock::ReferencePageMode::sv39, 0);
    concurrent_configured = concurrent_configured &&
        ifetch_reference.translated &&
        ifetch_reference.physical_address == ifetch_physical &&
        load_reference.translated &&
        load_reference.physical_address == load_physical + 0x188 &&
        ifetch_root_pte && ifetch_leaf_pte && load_root_pte && load_leaf_pte;
    if (!concurrent_configured) {
        std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=IFU-DTLB-concurrent"
                  << " cycle=" << concurrent.cycle()
                  << " phase=configuration reason=" << concurrent.error()
                  << '\n';
        return 1;
    }

    const memblock::LoadTransaction concurrent_load{
        .address = load_virtual + 0x188,
        .oracle_address = load_reference.physical_address,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .pdest = 52,
        .lane = 0,
    };
    concurrent.expect_load(concurrent_load);
    const std::uint64_t concurrent_ptw_before = concurrent.ptw_requests();
    concurrent.force_next_ptw_response_delay(concurrent_ptw_delay);
    memblock::Environment::IFetchPtwResponse concurrent_response;
    if (!concurrent.set_rob_head(
            concurrent_load.rob, concurrent_load.rob_flag) ||
        !concurrent.enqueue_load(concurrent_load) ||
        !concurrent.start_ifetch_ptw_request(
            ifetch_virtual >> 12,
            memblock::PtwTranslationMode::no_stage_two) ||
        !concurrent.run_until_ptw_requests(concurrent_ptw_before + 1, 4096) ||
        !concurrent.issue_load(concurrent_load, 512) ||
        concurrent.pending_scalar_loads() != 1 ||
        !concurrent.run_until_ptw_request_covering(
            *load_root_pte, concurrent_ptw_before, 4096) ||
        !concurrent.complete_ifetch_ptw_request(
            concurrent_response, response_stall_cycles) ||
        !concurrent.run_until_complete(16384) ||
        !concurrent.run_until_lq_retired(2048) ||
        !concurrent.run_until_ptw_request_covering(
            *ifetch_root_pte, concurrent_ptw_before, 4096,
            concurrent_ptw_delay) ||
        !concurrent.run_until_ptw_request_covering(
            *ifetch_leaf_pte, concurrent_ptw_before, 4096) ||
        !concurrent.run_until_ptw_request_covering(
            *load_leaf_pte, concurrent_ptw_before, 4096) ||
        concurrent.ptw_max_outstanding_requests() < 2 ||
        concurrent.writebacks() != 1) {
        std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=IFU-DTLB-concurrent"
                  << " cycle=" << concurrent.cycle()
                  << " phase=request"
                  << " pending_loads=" << concurrent.pending_scalar_loads()
                  << " max_ptw_outstanding="
                  << concurrent.ptw_max_outstanding_requests()
                  << " ptw_requests=" << concurrent.ptw_requests()
                  << " writebacks=" << concurrent.writebacks()
                  << " reason=" << concurrent.error() << '\n';
        return 1;
    }

    const std::uint64_t concurrent_vpn = ifetch_virtual >> 12;
    const unsigned concurrent_sector =
        static_cast<unsigned>(concurrent_vpn & 7U);
    const std::uint64_t concurrent_s1_ppn =
        (concurrent_response.s1_ppn << 3) |
        concurrent_response.s1_ppn_low[concurrent_sector];
    if (concurrent_response.s2xlate != static_cast<std::uint8_t>(
            memblock::PtwTranslationMode::no_stage_two) ||
        concurrent_response.s1_tag != (concurrent_vpn >> 3) ||
        concurrent_response.s1_asid != asid || concurrent_response.s1_n ||
        concurrent_response.s1_pbmt != 0 || concurrent_response.s1_d ||
        !concurrent_response.s1_a || concurrent_response.s1_g ||
        concurrent_response.s1_u || !concurrent_response.s1_x ||
        concurrent_response.s1_w || !concurrent_response.s1_r ||
        concurrent_response.s1_level != 0 || !concurrent_response.s1_v ||
        concurrent_s1_ppn != (ifetch_physical >> 12) ||
        concurrent_response.s1_addr_low != concurrent_sector ||
        concurrent_response.s1_pteidx != (1U << concurrent_sector) ||
        (concurrent_response.s1_valididx & (1U << concurrent_sector)) == 0 ||
        concurrent_response.s1_pf || concurrent_response.s1_af) {
        std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=IFU-DTLB-concurrent"
                  << " cycle=" << concurrent.cycle()
                  << " phase=response"
                  << " s1_tag=0x" << std::hex << concurrent_response.s1_tag
                  << " s1_ppn=0x" << concurrent_s1_ppn << std::dec
                  << " s1_asid=" << concurrent_response.s1_asid
                  << " s1_v=" << concurrent_response.s1_v
                  << " s1_pf=" << concurrent_response.s1_pf
                  << " s1_af=" << concurrent_response.s1_af << '\n';
        return 1;
    }
    total_cycles += concurrent.cycle();
    total_ptw_requests += concurrent.ptw_requests();

    constexpr std::uint64_t duplicate_root = 0x97000000ULL;
    constexpr std::uint64_t duplicate_virtual = 0x68012000ULL;
    constexpr std::uint64_t duplicate_physical = 0xe0012000ULL;
    memblock::Environment duplicate(argc, argv);
    duplicate.configure_backpressure(
        0x4f1bbcdc67621a53ULL, true,
        memblock::ResponseLatencyProfile::spec);
    bool duplicate_configured = duplicate.reset() &&
        duplicate.map_sv39_4k(
            duplicate_virtual, duplicate_physical, duplicate_root, true,
            false, true, false) &&
        duplicate.activate_sv39(duplicate_root, asid);
    const auto duplicate_reference = memblock::reference_page_walk(
        duplicate.memory(), duplicate_root, duplicate_virtual,
        memblock::ReferencePageMode::sv39);
    const auto duplicate_root_pte = memblock::reference_pte_address_at_level(
        duplicate.memory(), duplicate_root, duplicate_virtual,
        memblock::ReferencePageMode::sv39, 2);
    const auto duplicate_leaf_pte = memblock::reference_pte_address_at_level(
        duplicate.memory(), duplicate_root, duplicate_virtual,
        memblock::ReferencePageMode::sv39, 0);
    duplicate_configured = duplicate_configured &&
        duplicate_reference.translated &&
        duplicate_reference.physical_address == duplicate_physical &&
        duplicate_root_pte && duplicate_leaf_pte;
    if (!duplicate_configured) {
        std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=IFU-duplicate"
                  << " cycle=" << duplicate.cycle()
                  << " phase=configuration reason=" << duplicate.error()
                  << '\n';
        return 1;
    }

    const std::uint64_t duplicate_ptw_before = duplicate.ptw_requests();
    duplicate.force_next_ptw_response_delay(concurrent_ptw_delay);
    memblock::Environment::IFetchPtwResponse duplicate_first_response;
    memblock::Environment::IFetchPtwResponse duplicate_second_response;
    if (!duplicate.start_ifetch_ptw_request(
            duplicate_virtual >> 12,
            memblock::PtwTranslationMode::no_stage_two) ||
        !duplicate.run_until_ptw_request_covering(
            *duplicate_root_pte, duplicate_ptw_before, 4096,
            concurrent_ptw_delay) ||
        !duplicate.start_ifetch_ptw_request(
            duplicate_virtual >> 12,
            memblock::PtwTranslationMode::no_stage_two) ||
        duplicate.pending_ifetch_ptw_requests() != 2 ||
        !duplicate.complete_ifetch_ptw_request(
            duplicate_first_response, response_stall_cycles) ||
        !duplicate.complete_ifetch_ptw_request(
            duplicate_second_response, response_stall_cycles) ||
        duplicate.pending_ifetch_ptw_requests() != 0 ||
        !duplicate.run_until_ptw_request_covering(
            *duplicate_leaf_pte, duplicate_ptw_before, 4096) ||
        duplicate.ptw_requests() - duplicate_ptw_before != 3 ||
        !(duplicate_first_response == duplicate_second_response)) {
        std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=IFU-duplicate"
                  << " cycle=" << duplicate.cycle()
                  << " phase=request"
                  << " pending=" << duplicate.pending_ifetch_ptw_requests()
                  << " ptw_requests="
                  << duplicate.ptw_requests() - duplicate_ptw_before
                  << " reason=" << duplicate.error() << '\n';
        return 1;
    }

    const std::uint64_t duplicate_vpn = duplicate_virtual >> 12;
    const unsigned duplicate_sector =
        static_cast<unsigned>(duplicate_vpn & 7U);
    const std::uint64_t duplicate_s1_ppn =
        (duplicate_first_response.s1_ppn << 3) |
        duplicate_first_response.s1_ppn_low[duplicate_sector];
    if (duplicate_first_response.s2xlate != static_cast<std::uint8_t>(
            memblock::PtwTranslationMode::no_stage_two) ||
        duplicate_first_response.s1_tag != (duplicate_vpn >> 3) ||
        duplicate_first_response.s1_asid != asid ||
        duplicate_first_response.s1_n ||
        duplicate_first_response.s1_pbmt != 0 ||
        duplicate_first_response.s1_d || !duplicate_first_response.s1_a ||
        duplicate_first_response.s1_g || duplicate_first_response.s1_u ||
        !duplicate_first_response.s1_x || duplicate_first_response.s1_w ||
        !duplicate_first_response.s1_r ||
        duplicate_first_response.s1_level != 0 ||
        !duplicate_first_response.s1_v ||
        duplicate_s1_ppn != (duplicate_physical >> 12) ||
        duplicate_first_response.s1_addr_low != duplicate_sector ||
        duplicate_first_response.s1_pteidx != (1U << duplicate_sector) ||
        (duplicate_first_response.s1_valididx &
         (1U << duplicate_sector)) == 0 ||
        duplicate_first_response.s1_pf || duplicate_first_response.s1_af) {
        std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=IFU-duplicate"
                  << " cycle=" << duplicate.cycle()
                  << " phase=response"
                  << " s1_tag=0x" << std::hex
                  << duplicate_first_response.s1_tag
                  << " s1_ppn=0x" << duplicate_s1_ppn << std::dec
                  << " s1_asid=" << duplicate_first_response.s1_asid
                  << " s1_v=" << duplicate_first_response.s1_v
                  << " s1_pf=" << duplicate_first_response.s1_pf
                  << " s1_af=" << duplicate_first_response.s1_af << '\n';
        return 1;
    }
    total_cycles += duplicate.cycle();
    total_ptw_requests += duplicate.ptw_requests();

    enum class NestedFaultKind {
        vs_leaf,
        g_leaf,
        implicit_g,
    };
    struct NestedFaultCase {
        const char *name;
        memblock::ReferencePageMode vs_mode;
        memblock::ReferencePageMode g_mode;
        NestedFaultKind kind;
    };
#define MEMBLOCK_IFETCH_NESTED_FAULT_CASES(vs_name, g_name)                  \
    {#vs_name "-" #g_name "-VS-leaf",                                      \
     memblock::ReferencePageMode::vs_name,                                  \
     memblock::ReferencePageMode::g_name, NestedFaultKind::vs_leaf},        \
    {#vs_name "-" #g_name "-G-leaf",                                       \
     memblock::ReferencePageMode::vs_name,                                  \
     memblock::ReferencePageMode::g_name, NestedFaultKind::g_leaf},         \
    {#vs_name "-" #g_name "-implicit-G",                                   \
     memblock::ReferencePageMode::vs_name,                                  \
     memblock::ReferencePageMode::g_name, NestedFaultKind::implicit_g}
    constexpr std::array<NestedFaultCase, 12> nested_fault_cases{{
        MEMBLOCK_IFETCH_NESTED_FAULT_CASES(sv39, sv39),
        MEMBLOCK_IFETCH_NESTED_FAULT_CASES(sv39, sv48),
        MEMBLOCK_IFETCH_NESTED_FAULT_CASES(sv48, sv39),
        MEMBLOCK_IFETCH_NESTED_FAULT_CASES(sv48, sv48),
    }};
#undef MEMBLOCK_IFETCH_NESTED_FAULT_CASES

    for (unsigned case_index = 0; case_index < nested_fault_cases.size();
         ++case_index) {
        const auto &item = nested_fault_cases[case_index];
        const std::uint64_t fault_vs_root =
            0xa0000000ULL + case_index * 0x02000000ULL;
        const std::uint64_t fault_g_root = fault_vs_root + 0x01000000ULL;
        const std::uint64_t virtual_page =
            0x70012000ULL + case_index * 0x200000ULL;
        const std::uint64_t guest_page =
            0x130012000ULL + case_index * 0x200000ULL;
        const std::uint64_t host_page =
            0xf0012000ULL + case_index * 0x200000ULL;
        auto &environment = duplicate;
        const std::uint64_t cycle_before = environment.cycle();
        environment.configure_backpressure(
            0xda942042e4dd58b5ULL ^ case_index, true,
            memblock::ResponseLatencyProfile::spec);

        bool configured = environment.reset();
        if (configured) {
            configured = item.vs_mode == memblock::ReferencePageMode::sv48
                ? environment.map_sv48_4k(
                      virtual_page, guest_page, fault_vs_root, true, false,
                      true, false)
                : environment.map_sv39_4k(
                      virtual_page, guest_page, fault_vs_root, true, false,
                      true, false);
        }
        const unsigned vs_table_pages =
            memblock::reference_page_levels(item.vs_mode);
        for (unsigned page = 0; configured && page < vs_table_pages; ++page) {
            const std::uint64_t address = fault_vs_root + page * 0x1000ULL;
            configured = item.g_mode == memblock::ReferencePageMode::sv48
                ? environment.map_sv48x4_4k(
                      address, address, fault_g_root, true, true, false)
                : environment.map_sv39x4_4k(
                      address, address, fault_g_root, true, true, false);
        }
        if (configured) {
            configured = item.g_mode == memblock::ReferencePageMode::sv48
                ? environment.map_sv48x4_4k(
                      guest_page, host_page, fault_g_root, true, false, true)
                : environment.map_sv39x4_4k(
                      guest_page, host_page, fault_g_root, true, false, true);
        }

        const auto vs_leaf_pte = memblock::reference_pte_address_at_level(
            environment.memory(), fault_vs_root, virtual_page, item.vs_mode,
            0);
        const std::uint64_t g_fault_input =
            item.kind == NestedFaultKind::implicit_g
                ? fault_vs_root
                : guest_page;
        const auto g_leaf_pte = memblock::reference_pte_address_at_level(
            environment.memory(), fault_g_root, g_fault_input, item.g_mode,
            0, true);
        configured = configured && vs_leaf_pte && g_leaf_pte;
        if (configured) {
            environment.memory().write_u64(
                item.kind == NestedFaultKind::vs_leaf
                    ? *vs_leaf_pte
                    : *g_leaf_pte,
                0);
            configured = environment.activate_two_stage_modes(
                item.vs_mode, item.g_mode, fault_vs_root, fault_g_root, asid,
                vmid);
        }

        const auto reference = memblock::reference_two_stage_walk(
            environment.memory(), fault_vs_root, fault_g_root, virtual_page,
            item.vs_mode, item.g_mode);
        const bool reference_valid = item.kind == NestedFaultKind::vs_leaf
            ? !reference.translated && reference.stage1_page_fault &&
                !reference.guest_page_fault &&
                !reference.is_for_vs_nonleaf_pte
            : !reference.translated && !reference.stage1_page_fault &&
                reference.guest_page_fault &&
                reference.is_for_vs_nonleaf_pte ==
                    (item.kind == NestedFaultKind::implicit_g);
        configured = configured && reference_valid;
        if (!configured) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=" << item.name
                      << " cycle=" << environment.cycle()
                      << " phase=configuration"
                      << " reference_s1_pf=" << reference.stage1_page_fault
                      << " reference_gpf=" << reference.guest_page_fault
                      << " reference_implicit="
                      << reference.is_for_vs_nonleaf_pte
                      << " reason=" << environment.error() << '\n';
            return 1;
        }

        const std::uint64_t ptw_before = environment.ptw_requests();
        memblock::Environment::IFetchPtwResponse response;
        if (!environment.issue_ifetch_ptw_request(
                virtual_page >> 12,
                memblock::PtwTranslationMode::all_stages, response,
                response_stall_cycles) ||
            environment.ptw_requests() <= ptw_before) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=" << item.name
                      << " cycle=" << environment.cycle()
                      << " phase=request ptw_requests="
                      << environment.ptw_requests() - ptw_before
                      << " reason=" << environment.error() << '\n';
            return 1;
        }

        const std::uint64_t vpn = virtual_page >> 12;
        const unsigned sector = static_cast<unsigned>(vpn & 7U);
        const std::uint64_t reconstructed_s1_ppn =
            (response.s1_ppn << 3) | response.s1_ppn_low[sector];
        const bool mode_valid = response.s2xlate == static_cast<std::uint8_t>(
            memblock::PtwTranslationMode::all_stages);
        const bool stage1_fault_valid = item.kind != NestedFaultKind::vs_leaf ||
            (response.s1_tag == (vpn >> 3) && response.s1_asid == asid &&
             response.s1_vmid == vmid && !response.s1_n &&
             response.s1_pbmt == 0 && response.s1_addr_low == sector &&
             response.s1_pteidx == (1U << sector) &&
             (response.s1_valididx & (1U << sector)) != 0 &&
             !response.s1_d && !response.s1_a && !response.s1_g &&
             !response.s1_u && !response.s1_x && !response.s1_w &&
             !response.s1_r && response.s1_level == 0 && !response.s1_v &&
             reconstructed_s1_ppn == 0 && response.s1_pf &&
             !response.s1_af && !response.s2_gpf && !response.s2_gaf);
        const bool stage1_success_valid =
            item.kind != NestedFaultKind::g_leaf ||
            (response.s1_tag == (vpn >> 3) && response.s1_asid == asid &&
             response.s1_vmid == vmid && !response.s1_n &&
             response.s1_pbmt == 0 && response.s1_addr_low == sector &&
             response.s1_pteidx == (1U << sector) &&
             (response.s1_valididx & (1U << sector)) != 0 &&
             !response.s1_d && response.s1_a && !response.s1_g &&
             !response.s1_u && response.s1_x && !response.s1_w &&
             response.s1_r && response.s1_level == 0 && response.s1_v &&
             reconstructed_s1_ppn == (guest_page >> 12) &&
             !response.s1_pf && !response.s1_af);
        const std::uint64_t expected_g_fault_address =
            reference.faulting_guest_physical_address;
        const auto g_reference = item.kind == NestedFaultKind::vs_leaf
            ? memblock::ReferenceStageWalkResult{}
            : memblock::reference_page_walk(
                  environment.memory(), fault_g_root,
                  expected_g_fault_address, item.g_mode, true);
        const bool g_fault_valid = item.kind == NestedFaultKind::vs_leaf ||
            (!g_reference.translated &&
             response.s2_tag == (expected_g_fault_address >> 12) &&
             response.s2_vmid == vmid &&
             response.s2_level == g_reference.fault_level &&
             response.s2_gpf && !response.s2_gaf &&
             !response.s1_pf && !response.s1_af);
        if (!mode_valid || !stage1_fault_valid || !stage1_success_valid ||
            !g_fault_valid) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=" << item.name
                      << " cycle=" << environment.cycle()
                      << " phase=response"
                      << " s2xlate="
                      << static_cast<unsigned>(response.s2xlate)
                      << " s1_tag=0x" << std::hex << response.s1_tag
                      << " s1_ppn=0x" << reconstructed_s1_ppn
                      << " s2_tag=0x" << response.s2_tag
                      << " expected_g_fault=0x" << expected_g_fault_address
                      << std::dec
                      << " s1_asid=" << response.s1_asid
                      << " s1_vmid=" << response.s1_vmid
                      << " s2_vmid=" << response.s2_vmid
                      << " s1_level="
                      << static_cast<unsigned>(response.s1_level)
                      << " s2_level="
                      << static_cast<unsigned>(response.s2_level)
                      << " expected_s2_level=" << g_reference.fault_level
                      << " s1_v=" << response.s1_v
                      << " s1_pf=" << response.s1_pf
                      << " s1_af=" << response.s1_af
                      << " s2_gpf=" << response.s2_gpf
                      << " s2_gaf=" << response.s2_gaf << '\n';
            return 1;
        }
        total_cycles += environment.cycle() - cycle_before;
        total_ptw_requests += environment.ptw_requests() - ptw_before;
    }

    const auto stage1_success_matches = [](
        const memblock::Environment::IFetchPtwResponse &response,
        std::uint64_t vpn, std::uint64_t output_page,
        std::uint16_t expected_asid,
        std::optional<std::uint16_t> expected_vmid = std::nullopt) {
        const unsigned sector = static_cast<unsigned>(vpn & 7U);
        const std::uint64_t reconstructed_ppn =
            (response.s1_ppn << 3) | response.s1_ppn_low[sector];
        return response.s1_tag == (vpn >> 3) &&
            response.s1_asid == expected_asid &&
            (!expected_vmid || response.s1_vmid == *expected_vmid) &&
            !response.s1_n && response.s1_pbmt == 0 && !response.s1_d &&
            response.s1_a && !response.s1_g && !response.s1_u &&
            response.s1_x && !response.s1_w && response.s1_r &&
            response.s1_level == 0 && response.s1_v &&
            reconstructed_ppn == (output_page >> 12) &&
            response.s1_addr_low == sector &&
            response.s1_pteidx == (1U << sector) &&
            (response.s1_valididx & (1U << sector)) != 0 &&
            !response.s1_pf && !response.s1_af;
    };
    const auto stage2_success_matches = [](
        const memblock::Environment::IFetchPtwResponse &response,
        std::uint64_t input_page, std::uint64_t output_page,
        std::uint16_t expected_vmid) {
        return response.s2_tag == (input_page >> 12) &&
            response.s2_vmid == expected_vmid && !response.s2_n &&
            response.s2_pbmt == 0 &&
            response.s2_ppn == (output_page >> 12) && !response.s2_d &&
            response.s2_a && !response.s2_g && response.s2_u &&
            response.s2_x && !response.s2_w && response.s2_r &&
            response.s2_level == 0 && !response.s2_gpf && !response.s2_gaf;
    };

    const auto run_stage1_ifetch_race = [&](
        bool explicit_fence, bool selective_fence = false) {
        auto &environment = duplicate;
        const char *const race_name = !explicit_fence
            ? "context"
            : selective_fence ? "sfence-selective" : "sfence-global";
        const std::uint64_t cycle_before = environment.cycle();
        constexpr std::uint64_t virtual_page = 0x58012000ULL;
        const std::uint64_t old_physical = explicit_fence
            ? 0xc8012000ULL : 0xc9012000ULL;
        const std::uint64_t new_physical = explicit_fence
            ? 0xca012000ULL : 0xcb012000ULL;
        const std::uint64_t old_root = explicit_fence
            ? 0xc4000000ULL : 0xc5000000ULL;
        const std::uint64_t new_root = explicit_fence
            ? old_root : 0xc6000000ULL;
        const std::uint16_t new_asid = explicit_fence ? asid : asid + 1;
        environment.configure_backpressure(
            (explicit_fence ? 0x3c6ef372fe94f82bULL
                            : 0xa54ff53a5f1d36f1ULL) ^
                (selective_fence ? 0x9e3779b97f4a7c15ULL : 0),
            true, memblock::ResponseLatencyProfile::spec);
        bool configured = environment.reset() &&
            environment.map_sv39_4k(
                virtual_page, old_physical, old_root, true, false, true,
                false);
        if (configured && !explicit_fence) {
            configured = environment.map_sv48_4k(
                virtual_page, new_physical, new_root, true, false, true,
                false);
        }
        if (configured) {
            configured = environment.activate_sv39(old_root, asid);
        }
        const auto old_root_pte = memblock::reference_pte_address_at_level(
            environment.memory(), old_root, virtual_page,
            memblock::ReferencePageMode::sv39, 2);
        if (!configured || !old_root_pte) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=IFU-"
                      << race_name
                      << "-race phase=configuration reason="
                      << environment.error() << '\n';
            return false;
        }

        const std::uint64_t ptw_before = environment.ptw_requests();
        environment.force_next_ptw_response_delay(concurrent_ptw_delay);
        if (!environment.start_ifetch_ptw_request(
                virtual_page >> 12,
                memblock::PtwTranslationMode::no_stage_two) ||
            !environment.run_until_ptw_request_covering(
                *old_root_pte, ptw_before, 4096, concurrent_ptw_delay)) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=IFU-"
                      << race_name
                      << "-race phase=old-request reason="
                      << environment.error() << '\n';
            return false;
        }

        const bool switched = explicit_fence
            ? environment.map_sv39_4k(
                  virtual_page, new_physical, old_root, true, false, true,
                  false) &&
                (selective_fence
                    ? environment.issue_sfence(
                          virtual_page, asid, false, false)
                    : environment.issue_sfence())
            : environment.update_stage_one_context(
                  memblock::ReferencePageMode::sv48, new_root, new_asid);
        const auto new_mode = explicit_fence
            ? memblock::ReferencePageMode::sv39
            : memblock::ReferencePageMode::sv48;
        const auto new_leaf_pte = memblock::reference_pte_address_at_level(
            environment.memory(), new_root, virtual_page, new_mode, 0);
        if (!switched || !new_leaf_pte ||
            !environment.confirm_ifetch_ptw_flushed(1024)) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=IFU-"
                      << race_name
                      << "-race phase=flush reason=" << environment.error()
                      << '\n';
            return false;
        }

        const std::uint64_t ptw_after_flush = environment.ptw_requests();
        memblock::Environment::IFetchPtwResponse response;
        if (!environment.issue_ifetch_ptw_request(
                virtual_page >> 12,
                memblock::PtwTranslationMode::no_stage_two, response,
                response_stall_cycles) ||
            environment.ptw_requests() <= ptw_after_flush ||
            !environment.run_until_ptw_request_covering(
                *new_leaf_pte, ptw_after_flush, 4096) ||
            response.s2xlate != static_cast<std::uint8_t>(
                memblock::PtwTranslationMode::no_stage_two) ||
            !stage1_success_matches(
                response, virtual_page >> 12, new_physical, new_asid)) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=IFU-"
                      << race_name
                      << "-race phase=new-request ptw=" << ptw_before << '/'
                      << ptw_after_flush << '/' << environment.ptw_requests()
                      << " s1_ppn=0x" << std::hex
                      << ((response.s1_ppn << 3) |
                          response.s1_ppn_low[(virtual_page >> 12) & 7U])
                      << " expected=0x" << (new_physical >> 12) << std::dec
                      << " reason=" << environment.error() << '\n';
            return false;
        }
        total_cycles += environment.cycle() - cycle_before;
        total_ptw_requests += environment.ptw_requests() - ptw_before;
        return true;
    };

    if (!run_stage1_ifetch_race(false) ||
        !run_stage1_ifetch_race(true) ||
        !run_stage1_ifetch_race(true, true)) {
        return 1;
    }

    {
        auto &environment = duplicate;
        const std::uint64_t cycle_before = environment.cycle();
        constexpr std::uint64_t virtual_page = 0x5c012000ULL;
        constexpr std::uint64_t old_guest = 0x160012000ULL;
        constexpr std::uint64_t new_guest = 0x170012000ULL;
        constexpr std::uint64_t old_host = 0xcc012000ULL;
        constexpr std::uint64_t new_host = 0xcd012000ULL;
        constexpr std::uint64_t old_vs_root = 0xd0000000ULL;
        constexpr std::uint64_t old_g_root = 0xd1000000ULL;
        constexpr std::uint64_t new_vs_root = 0xd2000000ULL;
        constexpr std::uint64_t new_g_root = 0xd3000000ULL;
        constexpr std::uint16_t new_asid = asid + 2;
        constexpr std::uint16_t new_vmid = vmid + 2;
        environment.configure_backpressure(
            0x510e527fade682d1ULL, true,
            memblock::ResponseLatencyProfile::spec);
        bool configured = environment.reset() &&
            environment.map_sv39_4k(
                virtual_page, old_guest, old_vs_root, true, false, true,
                false) &&
            environment.map_sv48_4k(
                virtual_page, new_guest, new_vs_root, true, false, true,
                false);
        for (unsigned page = 0; configured && page < 3; ++page) {
            const std::uint64_t address = old_vs_root + page * 0x1000ULL;
            configured = environment.map_sv39x4_4k(
                address, address, old_g_root, true, true, false);
        }
        if (configured) {
            configured = environment.map_sv39x4_4k(
                old_guest, old_host, old_g_root, true, false, true);
        }
        for (unsigned page = 0; configured && page < 4; ++page) {
            const std::uint64_t address = new_vs_root + page * 0x1000ULL;
            configured = environment.map_sv48x4_4k(
                address, address, new_g_root, true, true, false);
        }
        if (configured) {
            configured = environment.map_sv48x4_4k(
                new_guest, new_host, new_g_root, true, false, true) &&
                environment.activate_two_stage_modes(
                    memblock::ReferencePageMode::sv39,
                    memblock::ReferencePageMode::sv39, old_vs_root,
                    old_g_root, asid, vmid);
        }
        const std::uint64_t old_vs_root_pte_gpa = old_vs_root +
            ((virtual_page >> 30) & 0x1ffULL) * 8;
        const auto old_first_g_pte = memblock::reference_pte_address_at_level(
            environment.memory(), old_g_root, old_vs_root_pte_gpa,
            memblock::ReferencePageMode::sv39, 2, true);
        const auto new_vs_leaf_pte = memblock::reference_pte_address_at_level(
            environment.memory(), new_vs_root, virtual_page,
            memblock::ReferencePageMode::sv48, 0);
        const auto new_g_leaf_pte = memblock::reference_pte_address_at_level(
            environment.memory(), new_g_root, new_guest,
            memblock::ReferencePageMode::sv48, 0, true);
        if (!configured || !old_first_g_pte || !new_vs_leaf_pte ||
            !new_g_leaf_pte) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL"
                      << " case=IFU-nested-context-race"
                      << " phase=configuration reason=" << environment.error()
                      << '\n';
            return 1;
        }

        const std::uint64_t ptw_before = environment.ptw_requests();
        environment.force_next_ptw_response_delay(concurrent_ptw_delay);
        if (!environment.start_ifetch_ptw_request(
                virtual_page >> 12,
                memblock::PtwTranslationMode::all_stages) ||
            !environment.run_until_ptw_request_covering(
                *old_first_g_pte, ptw_before, 4096,
                concurrent_ptw_delay) ||
            !environment.update_two_stage_context(
                memblock::ReferencePageMode::sv48,
                memblock::ReferencePageMode::sv48, new_vs_root, new_g_root,
                new_asid, new_vmid) ||
            !environment.confirm_ifetch_ptw_flushed(1024)) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL"
                      << " case=IFU-nested-context-race phase=flush reason="
                      << environment.error() << '\n';
            return 1;
        }

        const std::uint64_t ptw_after_flush = environment.ptw_requests();
        memblock::Environment::IFetchPtwResponse response;
        if (!environment.issue_ifetch_ptw_request(
                virtual_page >> 12,
                memblock::PtwTranslationMode::all_stages, response,
                response_stall_cycles) ||
            environment.ptw_requests() <= ptw_after_flush ||
            !environment.run_until_ptw_request_covering(
                *new_vs_leaf_pte, ptw_after_flush, 4096) ||
            !environment.run_until_ptw_request_covering(
                *new_g_leaf_pte, ptw_after_flush, 4096) ||
            response.s2xlate != static_cast<std::uint8_t>(
                memblock::PtwTranslationMode::all_stages) ||
            !stage1_success_matches(
                response, virtual_page >> 12, new_guest, new_asid,
                new_vmid) ||
            !stage2_success_matches(
                response, new_guest, new_host, new_vmid)) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL"
                      << " case=IFU-nested-context-race"
                      << " phase=new-request ptw=" << ptw_before << '/'
                      << ptw_after_flush << '/' << environment.ptw_requests()
                      << " s1_ppn=0x" << std::hex
                      << ((response.s1_ppn << 3) |
                          response.s1_ppn_low[(virtual_page >> 12) & 7U])
                      << " s2_ppn=0x" << response.s2_ppn << std::dec
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        total_cycles += environment.cycle() - cycle_before;
        total_ptw_requests += environment.ptw_requests() - ptw_before;
    }

    const auto run_nested_ifetch_fence_race = [&](
        bool guest_fence, bool selective_fence = false) {
        auto &environment = duplicate;
        const std::uint64_t cycle_before = environment.cycle();
        constexpr std::uint64_t virtual_page = 0x60012000ULL;
        constexpr std::uint64_t old_guest = 0x180012000ULL;
        constexpr std::uint64_t new_guest = 0x190012000ULL;
        constexpr std::uint64_t old_host = 0xce012000ULL;
        constexpr std::uint64_t new_host = 0xcf012000ULL;
        const std::uint64_t vs_root = guest_fence
            ? 0xd4000000ULL : 0xd6000000ULL;
        const std::uint64_t g_root = guest_fence
            ? 0xd5000000ULL : 0xd7000000ULL;
        environment.configure_backpressure(
            guest_fence ? 0x1f83d9abfb41bd6bULL
                        : 0x5be0cd19137e2179ULL,
            true, memblock::ResponseLatencyProfile::spec);
        bool configured = environment.reset() &&
            environment.map_sv39_4k(
                virtual_page, old_guest, vs_root, true, false, true, false);
        for (unsigned page = 0; configured && page < 3; ++page) {
            const std::uint64_t address = vs_root + page * 0x1000ULL;
            configured = environment.map_sv39x4_4k(
                address, address, g_root, true, true, false);
        }
        if (configured) {
            configured = environment.map_sv39x4_4k(
                    old_guest, old_host, g_root, true, false, true) &&
                environment.map_sv39x4_4k(
                    new_guest, new_host, g_root, true, false, true) &&
                environment.activate_two_stage_modes(
                    memblock::ReferencePageMode::sv39,
                    memblock::ReferencePageMode::sv39, vs_root, g_root,
                    asid, vmid);
        }
        const std::uint64_t vs_root_pte_gpa = vs_root +
            ((virtual_page >> 30) & 0x1ffULL) * 8;
        const auto first_g_pte = memblock::reference_pte_address_at_level(
            environment.memory(), g_root, vs_root_pte_gpa,
            memblock::ReferencePageMode::sv39, 2, true);
        if (!configured || !first_g_pte) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=IFU-HFENCE."
                      << (guest_fence ? "GVMA" : "VVMA")
                      << (selective_fence ? "-selective" : "-global")
                      << "-race phase=configuration reason="
                      << environment.error() << '\n';
            return false;
        }

        const std::uint64_t ptw_before = environment.ptw_requests();
        environment.force_next_ptw_response_delay(concurrent_ptw_delay);
        if (!environment.start_ifetch_ptw_request(
                virtual_page >> 12,
                memblock::PtwTranslationMode::all_stages) ||
            !environment.run_until_ptw_request_covering(
                *first_g_pte, ptw_before, 4096,
                concurrent_ptw_delay)) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=IFU-HFENCE."
                      << (guest_fence ? "GVMA" : "VVMA")
                      << (selective_fence ? "-selective" : "-global")
                      << "-race phase=old-request reason="
                      << environment.error() << '\n';
            return false;
        }

        const bool updated = guest_fence
            ? environment.map_sv39x4_4k(
                  old_guest, new_host, g_root, true, false, true)
            : environment.map_sv39_4k(
                  virtual_page, new_guest, vs_root, true, false, true,
                  false);
        if (!updated ||
            !environment.issue_sfence(
                selective_fence
                    ? (guest_fence ? old_guest : virtual_page)
                    : 0,
                guest_fence ? vmid : asid,
                !selective_fence, !selective_fence,
                !guest_fence, guest_fence) ||
            !environment.confirm_ifetch_ptw_flushed(1024)) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=IFU-HFENCE."
                      << (guest_fence ? "GVMA" : "VVMA")
                      << (selective_fence ? "-selective" : "-global")
                      << "-race phase=flush reason=" << environment.error()
                      << '\n';
            return false;
        }

        const std::uint64_t expected_guest =
            guest_fence ? old_guest : new_guest;
        const auto reference = memblock::reference_two_stage_walk(
            environment.memory(), vs_root, g_root, virtual_page,
            memblock::ReferencePageMode::sv39,
            memblock::ReferencePageMode::sv39);
        const auto vs_leaf_pte = memblock::reference_pte_address_at_level(
            environment.memory(), vs_root, virtual_page,
            memblock::ReferencePageMode::sv39, 0);
        const auto g_leaf_pte = memblock::reference_pte_address_at_level(
            environment.memory(), g_root, expected_guest,
            memblock::ReferencePageMode::sv39, 0, true);
        if (!reference.translated ||
            reference.physical_address != new_host || !vs_leaf_pte ||
            !g_leaf_pte) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=IFU-HFENCE."
                      << (guest_fence ? "GVMA" : "VVMA")
                      << (selective_fence ? "-selective" : "-global")
                      << "-race phase=reference\n";
            return false;
        }

        const std::uint64_t ptw_after_flush = environment.ptw_requests();
        memblock::Environment::IFetchPtwResponse response;
        if (!environment.issue_ifetch_ptw_request(
                virtual_page >> 12,
                memblock::PtwTranslationMode::all_stages, response,
                response_stall_cycles) ||
            environment.ptw_requests() <= ptw_after_flush ||
            !environment.run_until_ptw_request_covering(
                *vs_leaf_pte, ptw_after_flush, 4096) ||
            !environment.run_until_ptw_request_covering(
                *g_leaf_pte, ptw_after_flush, 4096) ||
            response.s2xlate != static_cast<std::uint8_t>(
                memblock::PtwTranslationMode::all_stages) ||
            !stage1_success_matches(
                response, virtual_page >> 12, expected_guest, asid, vmid) ||
            !stage2_success_matches(
                response, expected_guest, new_host, vmid)) {
            std::cerr << "MEMBLOCK_IFETCH_PTW_BRIDGE_FAIL case=IFU-HFENCE."
                      << (guest_fence ? "GVMA" : "VVMA")
                      << (selective_fence ? "-selective" : "-global")
                      << "-race phase=new-request ptw=" << ptw_before << '/'
                      << ptw_after_flush << '/' << environment.ptw_requests()
                      << " s1_ppn=0x" << std::hex
                      << ((response.s1_ppn << 3) |
                          response.s1_ppn_low[(virtual_page >> 12) & 7U])
                      << " s2_ppn=0x" << response.s2_ppn << std::dec
                      << " reason=" << environment.error() << '\n';
            return false;
        }
        total_cycles += environment.cycle() - cycle_before;
        total_ptw_requests += environment.ptw_requests() - ptw_before;
        return true;
    };

    if (!run_nested_ifetch_fence_race(false) ||
        !run_nested_ifetch_fence_race(true) ||
        !run_nested_ifetch_fence_race(false, true) ||
        !run_nested_ifetch_fence_race(true, true)) {
        return 1;
    }

    std::cout << "MEMBLOCK_IFETCH_PTW_BRIDGE_PASS"
              << " cases=" << cases.size() + degenerate_cases.size() +
                    nested_fault_cases.size() + 18
              << " stage1_valid=6 nested_valid=8 stage1_fault=2"
              << " pbmt=2 only_stage1=2 only_stage2=2"
              << " sector_indices=8 sector_ppn_low=8 sector_valid_masks=3"
              << " s1_global_leaf=3 s1_global_nonleaf=3"
              << " s1_global_reported=" << s1_global_reported
              << " s1_global_demoted=" << s1_global_demoted
              << " gstage_g_leaf=2 gstage_g_nonleaf=2"
              << " gstage_g_reported=" << gstage_global_reported
              << " gstage_g_demoted=" << gstage_global_demoted
              << " nested_vs_fault=4 nested_g_leaf_fault=4"
              << " nested_implicit_g_fault=4"
              << " ifu_stage1_context_race=1"
              << " ifu_sfence_global_race=1 ifu_sfence_selective_race=1"
              << " ifu_nested_context_race=1"
              << " ifu_hfence_vvma_race=2 ifu_hfence_gvma_race=2"
              << " ifu_dtlb_source_overlap=1"
              << " duplicate_requests=2 duplicate_walk_requests=3"
              << " ptw_bus_max_outstanding="
              << concurrent.ptw_max_outstanding_requests()
              << " response_stall_cycles="
              << (cases.size() + degenerate_cases.size() +
                  nested_fault_cases.size() + 19) *
                    response_stall_cycles
              << " ptw_requests=" << total_ptw_requests
              << " cycles=" << total_cycles
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_two_stage_translation(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t guest_virtual = 0x60000000ULL;
    constexpr std::uint64_t guest_physical = 0xb0000000ULL;
    constexpr std::uint64_t host_physical = 0xc0000000ULL;
    constexpr std::uint64_t vs_root = 0x94000000ULL;
    constexpr std::uint64_t g_root = 0x95000000ULL;
    environment.memory().fill_incrementing(host_physical, 0x1000, 0x67);
    environment.configure_backpressure(0x13198a2e03707344ULL, true);
    if (!environment.reset() ||
        !environment.map_sv39_4k(
            guest_virtual, guest_physical, vs_root) ||
        !environment.map_sv39x4_4k(vs_root, vs_root, g_root) ||
        !environment.map_sv39x4_4k(
            vs_root + 0x1000, vs_root + 0x1000, g_root) ||
        !environment.map_sv39x4_4k(
            vs_root + 0x2000, vs_root + 0x2000, g_root) ||
        !environment.map_sv39x4_4k(
            guest_physical, host_physical, g_root) ||
        !environment.activate_two_stage(vs_root, g_root, 3, 5)) {
        std::cerr << "MEMBLOCK_TWO_STAGE_FAIL cycle=" << environment.cycle()
                  << " phase=configuration reason=" << environment.error() << '\n';
        return 1;
    }

    const auto reference = memblock::reference_two_stage_walk(
        environment.memory(), vs_root, g_root, guest_virtual + 0x188);
    if (!reference.translated) {
        std::cerr << "MEMBLOCK_TWO_STAGE_FAIL cycle=" << environment.cycle()
                  << " phase=reference-walk reason=software page walk did not translate\n";
        return 1;
    }

    const memblock::LoadTransaction cold{
        .address = guest_virtual + 0x188,
        .oracle_address = reference.physical_address,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .pdest = 50,
        .lane = 0,
    };
    environment.expect_load(cold);
    if (!environment.set_rob_head(cold.rob, cold.rob_flag) ||
        !environment.enqueue_load(cold) ||
        !environment.issue_load(cold, 256) ||
        !environment.run_until_complete(8192) ||
        !environment.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_TWO_STAGE_FAIL cycle=" << environment.cycle()
                  << " phase=cold reason=" << environment.error() << '\n';
        return 1;
    }
    const std::uint64_t ptw_after_cold = environment.ptw_requests();

    auto warm = cold;
    warm.rob = 1;
    warm.lq = 1;
    warm.pdest = 51;
    warm.lane = 1;
    environment.expect_load(warm);
    if (!environment.enqueue_load(warm) ||
        !environment.issue_load(warm, 256) ||
        !environment.run_until_complete(2048) ||
        !environment.run_until_lq_retired(1024) ||
        environment.ptw_requests() != ptw_after_cold) {
        std::cerr << "MEMBLOCK_TWO_STAGE_FAIL cycle=" << environment.cycle()
                  << " phase=warm reason="
                  << (environment.error().empty()
                          ? "two-stage TLB entry was not reused"
                          : environment.error())
                  << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_TWO_STAGE_PASS"
              << " cycle=" << environment.cycle()
              << " writebacks=" << environment.writebacks()
              << " ptw_requests=" << environment.ptw_requests()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_translation_matrix(int argc, char **argv)
{
    struct Pair {
        memblock::ReferencePageMode vs_mode;
        memblock::ReferencePageMode g_mode;
        const char *name;
    };
    constexpr std::array<Pair, 4> pairs{{
        {memblock::ReferencePageMode::sv39,
         memblock::ReferencePageMode::sv39,
         "Sv39-Sv39x4"},
        {memblock::ReferencePageMode::sv39,
         memblock::ReferencePageMode::sv48,
         "Sv39-Sv48x4"},
        {memblock::ReferencePageMode::sv48,
         memblock::ReferencePageMode::sv39,
         "Sv48-Sv39x4"},
        {memblock::ReferencePageMode::sv48,
         memblock::ReferencePageMode::sv48,
         "Sv48-Sv48x4"},
    }};
    std::uint64_t total_ptw_requests = 0;
    std::uint64_t total_tilelink_requests = 0;
    std::uint64_t total_cycles = 0;

    for (unsigned pair_index = 0; pair_index < pairs.size(); ++pair_index) {
        const auto pair = pairs[pair_index];
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t guest_physical = 0x120000000ULL;
        constexpr std::uint64_t host_physical = 0xc0000000ULL;
        constexpr std::uint64_t vs_root = 0x94000000ULL;
        constexpr std::uint64_t g_root = 0x95000000ULL;
        const std::uint64_t guest_virtual_base = pair.vs_mode ==
                memblock::ReferencePageMode::sv48
            ? 0xffff800012340000ULL
            : 0x60000000ULL;
        const std::uint64_t guest_virtual = guest_virtual_base + 0x188;
        environment.memory().fill_incrementing(
            host_physical, 0x1000, 0x39 + pair_index * 0x11);
        environment.configure_backpressure(
            0x13198a2e03707344ULL ^ (pair_index * 0x9e3779b97f4a7c15ULL),
            true);

        const unsigned vs_table_pages = pair.vs_mode ==
                memblock::ReferencePageMode::sv48
            ? 4U
            : 3U;
        bool configured = environment.reset();
        if (configured) {
            if (pair.vs_mode == memblock::ReferencePageMode::sv48) {
                configured = environment.map_sv48_4k(
                    guest_virtual_base, guest_physical, vs_root);
            } else {
                configured = environment.map_sv39_4k(
                    guest_virtual_base, guest_physical, vs_root);
            }
        }
        for (unsigned page = 0; configured && page < vs_table_pages; ++page) {
            const std::uint64_t address = vs_root + page * 0x1000ULL;
            if (pair.g_mode == memblock::ReferencePageMode::sv48) {
                configured = environment.map_sv48x4_4k(
                    address, address, g_root);
            } else {
                configured = environment.map_sv39x4_4k(
                    address, address, g_root);
            }
        }
        if (configured) {
            if (pair.g_mode == memblock::ReferencePageMode::sv48) {
                configured = environment.map_sv48x4_4k(
                    guest_physical, host_physical, g_root);
            } else {
                configured = environment.map_sv39x4_4k(
                    guest_physical, host_physical, g_root);
            }
        }
        if (configured) {
            configured = environment.activate_two_stage_modes(
                pair.vs_mode,
                pair.g_mode,
                vs_root,
                g_root,
                static_cast<std::uint16_t>(3 + pair_index),
                static_cast<std::uint16_t>(5 + pair_index));
        }
        if (!configured) {
            std::cerr << "MEMBLOCK_TRANSLATION_MATRIX_FAIL pair=" << pair.name
                      << " cycle=" << environment.cycle()
                      << " phase=configuration reason=" << environment.error()
                      << '\n';
            return 1;
        }

        const auto reference = memblock::reference_two_stage_walk(
            environment.memory(),
            vs_root,
            g_root,
            guest_virtual,
            pair.vs_mode,
            pair.g_mode);
        if (!reference.translated ||
            reference.physical_address != host_physical + 0x188) {
            std::cerr << "MEMBLOCK_TRANSLATION_MATRIX_FAIL pair=" << pair.name
                      << " cycle=" << environment.cycle()
                      << " phase=reference-walk expected=0x" << std::hex
                      << (host_physical + 0x188) << " actual=0x"
                      << reference.physical_address << std::dec << '\n';
            return 1;
        }

        const memblock::LoadTransaction cold{
            .address = guest_virtual,
            .oracle_address = reference.physical_address,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = static_cast<std::uint8_t>(60 + pair_index * 2),
            .lane = pair_index % memblock::kScalarLoadLanes,
        };
        environment.expect_load(cold);
        if (!environment.enqueue_load(cold) ||
            !environment.issue_load(cold, 512) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_MATRIX_FAIL pair=" << pair.name
                      << " cycle=" << environment.cycle()
                      << " phase=cold reason=" << environment.error() << '\n';
            return 1;
        }
        const std::uint64_t ptw_after_cold = environment.ptw_requests();
        auto warm = cold;
        warm.rob = 1;
        warm.lq = 1;
        warm.pdest = static_cast<std::uint8_t>(61 + pair_index * 2);
        warm.lane = (pair_index + 1) % memblock::kScalarLoadLanes;
        environment.expect_load(warm);
        if (!environment.enqueue_load(warm) ||
            !environment.issue_load(warm, 512) ||
            !environment.run_until_complete(4096) ||
            !environment.run_until_lq_retired(2048) ||
            environment.ptw_requests() != ptw_after_cold) {
            std::cerr << "MEMBLOCK_TRANSLATION_MATRIX_FAIL pair=" << pair.name
                      << " cycle=" << environment.cycle()
                      << " phase=warm reason="
                      << (environment.error().empty()
                              ? "two-stage TLB entry was not reused"
                              : environment.error())
                      << '\n';
            return 1;
        }
        total_ptw_requests += environment.ptw_requests();
        total_tilelink_requests += environment.tilelink_requests();
        total_cycles += environment.cycle();
    }

    std::cout << "MEMBLOCK_TRANSLATION_MATRIX_PASS"
              << " pairs=" << pairs.size()
              << " ptw_requests=" << total_ptw_requests
              << " tilelink_requests=" << total_tilelink_requests
              << " cycles=" << total_cycles
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_translation_fence(
    int argc,
    char **argv,
    bool outstanding_selective = false,
    bool outstanding_vs_sv48 = false,
    bool outstanding_g_sv48 = false)
{
    const auto run_fenced_load = [](
        memblock::Environment &test_environment,
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        unsigned index,
        std::uint8_t pdest) {
        const memblock::LoadTransaction transaction{
            .address = virtual_address,
            .oracle_address = physical_address,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(index),
            .lq = static_cast<std::uint8_t>(index),
            .pdest = pdest,
            .lane = index % memblock::kScalarLoadLanes,
        };
        test_environment.expect_load(transaction);
        return test_environment.set_rob_head(
                   transaction.rob, transaction.rob_flag) &&
            test_environment.enqueue_load(transaction) &&
            test_environment.issue_load(transaction, 2048) &&
            test_environment.run_until_complete(16384) &&
            test_environment.run_until_lq_retired(4096);
    };

    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_base = 0x62000000ULL;
    constexpr std::uint64_t old_physical = 0xc1000000ULL;
    constexpr std::uint64_t new_physical = 0xc2000000ULL;
    constexpr std::uint64_t root = 0x92000000ULL;
    environment.memory().fill_incrementing(old_physical, 0x1000, 0x27);
    environment.memory().fill_incrementing(new_physical, 0x1000, 0xa1);
    environment.configure_backpressure(0x243f6a8885a308d3ULL, true);
    if (!environment.reset() ||
        !environment.map_sv39_4k(virtual_base, old_physical, root) ||
        !environment.activate_sv39(root, 9)) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    const auto make_load = [&](unsigned index, std::uint64_t physical) {
        return memblock::LoadTransaction{
            .address = virtual_base + 0x188,
            .oracle_address = physical + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(index),
            .lq = static_cast<std::uint8_t>(index),
            .pdest = static_cast<std::uint8_t>(70 + index),
            .lane = index % memblock::kScalarLoadLanes,
        };
    };

    const auto cold = make_load(0, old_physical);
    environment.expect_load(cold);
    if (!environment.set_rob_head(cold.rob, cold.rob_flag) ||
        !environment.enqueue_load(cold) ||
        !environment.issue_load(cold, 512) ||
        !environment.run_until_complete(8192) ||
        !environment.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << environment.cycle() << " phase=cold reason="
                  << environment.error() << '\n';
        return 1;
    }
    const std::uint64_t ptw_after_cold = environment.ptw_requests();

    const auto stale = make_load(1, old_physical);
    environment.expect_load(stale);
    if (!environment.set_rob_head(stale.rob, stale.rob_flag) ||
        !environment.enqueue_load(stale) ||
        !environment.issue_load(stale, 512) ||
        !environment.run_until_complete(4096) ||
        !environment.run_until_lq_retired(2048) ||
        environment.ptw_requests() != ptw_after_cold) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << environment.cycle() << " phase=pre-fence-hit reason="
                  << (environment.error().empty()
                          ? "unexpected PTW before fence"
                          : environment.error())
                  << '\n';
        return 1;
    }

    if (!environment.map_sv39_4k(virtual_base, new_physical, root) ||
        !environment.issue_sfence()) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << environment.cycle() << " phase=fence reason="
                  << environment.error() << '\n';
        return 1;
    }
    const auto refreshed = make_load(2, new_physical);
    environment.expect_load(refreshed);
    if (!environment.set_rob_head(refreshed.rob, refreshed.rob_flag) ||
        !environment.enqueue_load(refreshed) ||
        !environment.issue_load(refreshed, 512) ||
        !environment.run_until_complete(8192) ||
        !environment.run_until_lq_retired(2048) ||
        environment.ptw_requests() <= ptw_after_cold) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << environment.cycle() << " phase=post-fence reason="
                  << (environment.error().empty()
                          ? "translation was not refilled after SFENCE.VMA"
                          : environment.error())
                  << '\n';
        return 1;
    }

    constexpr std::uint64_t selective_physical = 0xc5000000ULL;
    environment.memory().fill_incrementing(selective_physical, 0x1000, 0x63);
    const std::uint64_t ptw_before_selective = environment.ptw_requests();
    if (!environment.map_sv39_4k(
            virtual_base, selective_physical, root) ||
        !environment.issue_sfence(
            virtual_base + 0x188, 9, false, false, false, false)) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << environment.cycle() << " phase=selective-sfence reason="
                  << environment.error() << '\n';
        return 1;
    }
    const auto selectively_refreshed = make_load(3, selective_physical);
    environment.expect_load(selectively_refreshed);
    if (!environment.set_rob_head(
            selectively_refreshed.rob, selectively_refreshed.rob_flag) ||
        !environment.enqueue_load(selectively_refreshed) ||
        !environment.issue_load(selectively_refreshed, 512) ||
        !environment.run_until_complete(8192) ||
        !environment.run_until_lq_retired(2048) ||
        environment.ptw_requests() <= ptw_before_selective) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << environment.cycle() << " phase=selective-sfence-refill reason="
                  << (environment.error().empty()
                          ? "selective SFENCE.VMA did not invalidate the leaf"
                          : environment.error()) << '\n';
        return 1;
    }

    memblock::Environment nested(argc, argv);
    constexpr std::uint64_t nested_virtual = 0x63000000ULL;
    constexpr std::uint64_t nested_guest_physical = 0xb1000000ULL;
    constexpr std::uint64_t nested_old_host = 0xc3000000ULL;
    constexpr std::uint64_t nested_new_host = 0xc4000000ULL;
    constexpr std::uint64_t nested_vs_root = 0x94000000ULL;
    constexpr std::uint64_t nested_g_root = 0x95000000ULL;
    nested.memory().fill_incrementing(nested_old_host, 0x1000, 0x4d);
    nested.memory().fill_incrementing(nested_new_host, 0x1000, 0xd2);
    nested.configure_backpressure(0x13198a2e03707344ULL, true);
    if (!nested.reset() ||
        !nested.map_sv39_4k(
            nested_virtual, nested_guest_physical, nested_vs_root) ||
        !nested.map_sv39x4_4k(
            nested_vs_root, nested_vs_root, nested_g_root) ||
        !nested.map_sv39x4_4k(
            nested_vs_root + 0x1000,
            nested_vs_root + 0x1000,
            nested_g_root) ||
        !nested.map_sv39x4_4k(
            nested_vs_root + 0x2000,
            nested_vs_root + 0x2000,
            nested_g_root) ||
        !nested.map_sv39x4_4k(
            nested_guest_physical,
            nested_old_host,
            nested_g_root) ||
        !nested.activate_two_stage(nested_vs_root, nested_g_root, 3, 5)) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << nested.cycle() << " phase=hfence-configuration reason="
                  << nested.error() << '\n';
        return 1;
    }
    const auto nested_reference = memblock::reference_two_stage_walk(
        nested.memory(),
        nested_vs_root,
        nested_g_root,
        nested_virtual + 0x188);
    if (!nested_reference.translated ||
        nested_reference.physical_address != nested_old_host + 0x188) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << nested.cycle() << " phase=hfence-reference-walk\n";
        return 1;
    }
    const auto make_nested_load = [&](unsigned index, std::uint64_t physical) {
        return memblock::LoadTransaction{
            .address = nested_virtual + 0x188,
            .oracle_address = physical + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(index),
            .lq = static_cast<std::uint8_t>(index),
            .pdest = static_cast<std::uint8_t>(80 + index),
            .lane = index % memblock::kScalarLoadLanes,
        };
    };
    const auto nested_cold = make_nested_load(0, nested_old_host);
    nested.expect_load(nested_cold);
    if (!nested.enqueue_load(nested_cold) ||
        !nested.issue_load(nested_cold, 512) ||
        !nested.run_until_complete(16384) ||
        !nested.run_until_lq_retired(4096)) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << nested.cycle() << " phase=hfence-cold reason="
                  << nested.error() << '\n';
        return 1;
    }
    const std::uint64_t nested_ptw_before_fence = nested.ptw_requests();
    const auto nested_stale = make_nested_load(1, nested_old_host);
    nested.expect_load(nested_stale);
    if (!nested.enqueue_load(nested_stale) ||
        !nested.issue_load(nested_stale, 512) ||
        !nested.run_until_complete(4096) ||
        !nested.run_until_lq_retired(2048) ||
        nested.ptw_requests() != nested_ptw_before_fence) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << nested.cycle() << " phase=hfence-pre-hit reason="
                  << nested.error() << '\n';
        return 1;
    }
    if (!nested.map_sv39x4_4k(
            nested_guest_physical,
            nested_new_host,
            nested_g_root) ||
        !nested.issue_sfence(0, 5, true, true, false, true)) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << nested.cycle() << " phase=hfence-gvma reason="
                  << nested.error() << '\n';
        return 1;
    }
    const auto nested_refreshed = make_nested_load(2, nested_new_host);
    nested.expect_load(nested_refreshed);
    if (!nested.enqueue_load(nested_refreshed) ||
        !nested.issue_load(nested_refreshed, 512) ||
        !nested.run_until_complete(16384) ||
        !nested.run_until_lq_retired(4096) ||
        nested.ptw_requests() <= nested_ptw_before_fence) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << nested.cycle() << " phase=hfence-post reason="
                  << (nested.error().empty()
                          ? "translation was not refilled after HFENCE.GVMA"
                          : nested.error())
                  << '\n';
        return 1;
    }

    memblock::Environment vvma(argc, argv);
    constexpr std::uint64_t vvma_virtual = 0x65000000ULL;
    constexpr std::uint64_t vvma_old_guest = 0xb5000000ULL;
    constexpr std::uint64_t vvma_new_guest = 0xb6000000ULL;
    constexpr std::uint64_t vvma_old_host = 0xc7000000ULL;
    constexpr std::uint64_t vvma_new_host = 0xc8000000ULL;
    constexpr std::uint64_t vvma_vs_root = 0x96000000ULL;
    constexpr std::uint64_t vvma_g_root = 0x97000000ULL;
    vvma.memory().fill_incrementing(vvma_old_host, 0x1000, 0x19);
    vvma.memory().fill_incrementing(vvma_new_host, 0x1000, 0xb8);
    vvma.configure_backpressure(0xa4093822299f31d0ULL, true);
    if (!vvma.reset() ||
        !vvma.map_sv39_4k(vvma_virtual, vvma_old_guest, vvma_vs_root) ||
        !vvma.map_sv39x4_4k(vvma_vs_root, vvma_vs_root, vvma_g_root) ||
        !vvma.map_sv39x4_4k(
            vvma_vs_root + 0x1000, vvma_vs_root + 0x1000, vvma_g_root) ||
        !vvma.map_sv39x4_4k(
            vvma_vs_root + 0x2000, vvma_vs_root + 0x2000, vvma_g_root) ||
        !vvma.map_sv39x4_4k(vvma_old_guest, vvma_old_host, vvma_g_root) ||
        !vvma.map_sv39x4_4k(vvma_new_guest, vvma_new_host, vvma_g_root) ||
        !vvma.activate_two_stage(vvma_vs_root, vvma_g_root, 13, 17)) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << vvma.cycle() << " phase=hfence-vvma-configuration reason="
                  << vvma.error() << '\n';
        return 1;
    }
    const auto vvma_load = [&](unsigned index, std::uint64_t physical) {
        return memblock::LoadTransaction{
            .address = vvma_virtual + 0x188,
            .oracle_address = physical + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(index),
            .lq = static_cast<std::uint8_t>(index),
            .pdest = static_cast<std::uint8_t>(110 + index),
            .lane = index % memblock::kScalarLoadLanes,
        };
    };
    const auto vvma_cold = vvma_load(0, vvma_old_host);
    vvma.expect_load(vvma_cold);
    if (!vvma.enqueue_load(vvma_cold) ||
        !vvma.issue_load(vvma_cold, 512) ||
        !vvma.run_until_complete(16384) ||
        !vvma.run_until_lq_retired(4096)) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << vvma.cycle() << " phase=hfence-vvma-cold reason="
                  << vvma.error() << '\n';
        return 1;
    }
    const auto vvma_stale = vvma_load(1, vvma_old_host);
    vvma.expect_load(vvma_stale);
    if (!vvma.enqueue_load(vvma_stale) ||
        !vvma.issue_load(vvma_stale, 512) ||
        !vvma.run_until_complete(4096) ||
        !vvma.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << vvma.cycle() << " phase=hfence-vvma-pre-hit reason="
                  << vvma.error() << '\n';
        return 1;
    }
    const std::uint64_t vvma_ptw_before_fence = vvma.ptw_requests();
    if (!vvma.map_sv39_4k(vvma_virtual, vvma_new_guest, vvma_vs_root) ||
        !vvma.issue_sfence(
            vvma_virtual + 0x188, 13, false, false, true, false)) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << vvma.cycle() << " phase=hfence-vvma reason="
                  << vvma.error() << '\n';
        return 1;
    }
    const auto vvma_refreshed = vvma_load(2, vvma_new_host);
    vvma.expect_load(vvma_refreshed);
    if (!vvma.enqueue_load(vvma_refreshed) ||
        !vvma.issue_load(vvma_refreshed, 512) ||
        !vvma.run_until_complete(16384) ||
        !vvma.run_until_lq_retired(4096) ||
        vvma.ptw_requests() <= vvma_ptw_before_fence) {
        std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                  << vvma.cycle() << " phase=hfence-vvma-refill reason="
                  << (vvma.error().empty()
                          ? "HFENCE.VVMA did not invalidate the VS leaf"
                          : vvma.error()) << '\n';
        return 1;
    }

    std::uint64_t same_id_cycles = 0;
    std::uint64_t same_id_ptw_requests = 0;
    std::uint64_t same_id_writebacks = 0;

    // Reuse one host ASID for a different root, then invalidate that ASID.
    {
        memblock::Environment reuse(argc, argv);
        constexpr std::uint64_t address = 0x67000188ULL;
        constexpr std::uint64_t virtual_page = 0x67000000ULL;
        constexpr std::uint64_t physical_a = 0xd2000000ULL;
        constexpr std::uint64_t physical_b = 0xd3000000ULL;
        constexpr std::uint64_t root_a = 0xb8000000ULL;
        constexpr std::uint64_t root_b = 0xba000000ULL;
        constexpr std::uint16_t asid = 47;
        reuse.memory().fill_incrementing(physical_a, 0x1000, 0x35);
        reuse.memory().fill_incrementing(physical_b, 0x1000, 0xc5);
        reuse.configure_backpressure(0x082efa98ec4e6c89ULL, true);
        if (!reuse.reset() ||
            !reuse.map_sv39_4k(virtual_page, physical_a, root_a) ||
            !reuse.map_sv39_4k(virtual_page, physical_b, root_b) ||
            !reuse.activate_sv39(root_a, asid) ||
            !run_fenced_load(reuse, address, physical_a + 0x188, 0, 112)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << reuse.cycle() << " phase=same-asid-cold reason="
                      << reuse.error() << '\n';
            return 1;
        }
        const std::uint64_t ptw_before_reuse = reuse.ptw_requests();
        if (!reuse.update_stage_one_context(
                memblock::ReferencePageMode::sv39, root_b, asid) ||
            !reuse.issue_sfence(0, asid, true, false) ||
            !run_fenced_load(reuse, address, physical_b + 0x188, 1, 113) ||
            reuse.ptw_requests() <= ptw_before_reuse) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << reuse.cycle() << " phase=same-asid-refill reason="
                      << (reuse.error().empty()
                              ? "same-ASID root reuse did not refill"
                              : reuse.error()) << '\n';
            return 1;
        }
        same_id_cycles += reuse.cycle();
        same_id_ptw_requests += reuse.ptw_requests();
        same_id_writebacks += reuse.writebacks();
    }

    // Reuse one VS ASID for another VS root while retaining the G context.
    {
        memblock::Environment reuse(argc, argv);
        constexpr std::uint64_t address = 0x69000188ULL;
        constexpr std::uint64_t virtual_page = 0x69000000ULL;
        constexpr std::uint64_t guest_a = 0x90000000ULL;
        constexpr std::uint64_t guest_b = 0x90100000ULL;
        constexpr std::uint64_t physical_a = 0xd4000000ULL;
        constexpr std::uint64_t physical_b = 0xd5000000ULL;
        constexpr std::uint64_t vs_root_a = 0xbc000000ULL;
        constexpr std::uint64_t vs_root_b = 0xbd000000ULL;
        constexpr std::uint64_t g_root = 0xbe000000ULL;
        constexpr std::uint16_t asid = 51;
        constexpr std::uint16_t vmid = 61;
        reuse.memory().fill_incrementing(physical_a, 0x1000, 0x46);
        reuse.memory().fill_incrementing(physical_b, 0x1000, 0xd6);
        reuse.configure_backpressure(0x452821e638d01377ULL, true);
        if (!reuse.reset() ||
            !reuse.map_sv39_4k(virtual_page, guest_a, vs_root_a) ||
            !reuse.map_sv39_4k(virtual_page, guest_b, vs_root_b) ||
            !reuse.map_sv39x4_4k(vs_root_a, vs_root_a, g_root) ||
            !reuse.map_sv39x4_4k(
                vs_root_a + 0x1000, vs_root_a + 0x1000, g_root) ||
            !reuse.map_sv39x4_4k(
                vs_root_a + 0x2000, vs_root_a + 0x2000, g_root) ||
            !reuse.map_sv39x4_4k(vs_root_b, vs_root_b, g_root) ||
            !reuse.map_sv39x4_4k(
                vs_root_b + 0x1000, vs_root_b + 0x1000, g_root) ||
            !reuse.map_sv39x4_4k(
                vs_root_b + 0x2000, vs_root_b + 0x2000, g_root) ||
            !reuse.map_sv39x4_4k(guest_a, physical_a, g_root) ||
            !reuse.map_sv39x4_4k(guest_b, physical_b, g_root) ||
            !reuse.activate_two_stage(vs_root_a, g_root, asid, vmid) ||
            !run_fenced_load(reuse, address, physical_a + 0x188, 0, 114)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << reuse.cycle() << " phase=same-vs-asid-cold reason="
                      << reuse.error() << '\n';
            return 1;
        }
        const std::uint64_t ptw_before_reuse = reuse.ptw_requests();
        if (!reuse.update_vs_context(
                memblock::ReferencePageMode::sv39, vs_root_b, asid) ||
            !reuse.issue_sfence(0, asid, true, false, true, false) ||
            !run_fenced_load(reuse, address, physical_b + 0x188, 1, 115) ||
            reuse.ptw_requests() <= ptw_before_reuse) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << reuse.cycle() << " phase=same-vs-asid-refill reason="
                      << (reuse.error().empty()
                              ? "same VS-ASID root reuse did not refill"
                              : reuse.error()) << '\n';
            return 1;
        }
        same_id_cycles += reuse.cycle();
        same_id_ptw_requests += reuse.ptw_requests();
        same_id_writebacks += reuse.writebacks();
    }

    // Reuse one VMID for another G-stage root while retaining the VS context.
    {
        memblock::Environment reuse(argc, argv);
        constexpr std::uint64_t address = 0x6b000188ULL;
        constexpr std::uint64_t virtual_page = 0x6b000000ULL;
        constexpr std::uint64_t guest = 0x92000000ULL;
        constexpr std::uint64_t physical_a = 0xd6000000ULL;
        constexpr std::uint64_t physical_b = 0xd7000000ULL;
        constexpr std::uint64_t vs_root = 0xcf000000ULL;
        constexpr std::uint64_t g_root_a = 0xd0000000ULL;
        constexpr std::uint64_t g_root_b = 0xd1000000ULL;
        constexpr std::uint16_t asid = 53;
        constexpr std::uint16_t vmid = 63;
        reuse.memory().fill_incrementing(physical_a, 0x1000, 0x57);
        reuse.memory().fill_incrementing(physical_b, 0x1000, 0xe7);
        reuse.configure_backpressure(0xbe5466cf34e90c6cULL, true);
        if (!reuse.reset() ||
            !reuse.map_sv39_4k(virtual_page, guest, vs_root) ||
            !reuse.map_sv39x4_4k(vs_root, vs_root, g_root_a) ||
            !reuse.map_sv39x4_4k(
                vs_root + 0x1000, vs_root + 0x1000, g_root_a) ||
            !reuse.map_sv39x4_4k(
                vs_root + 0x2000, vs_root + 0x2000, g_root_a) ||
            !reuse.map_sv39x4_4k(guest, physical_a, g_root_a) ||
            !reuse.map_sv39x4_4k(vs_root, vs_root, g_root_b) ||
            !reuse.map_sv39x4_4k(
                vs_root + 0x1000, vs_root + 0x1000, g_root_b) ||
            !reuse.map_sv39x4_4k(
                vs_root + 0x2000, vs_root + 0x2000, g_root_b) ||
            !reuse.map_sv39x4_4k(guest, physical_b, g_root_b) ||
            !reuse.activate_two_stage(vs_root, g_root_a, asid, vmid) ||
            !run_fenced_load(reuse, address, physical_a + 0x188, 0, 116)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << reuse.cycle() << " phase=same-vmid-cold reason="
                      << reuse.error() << '\n';
            return 1;
        }
        const std::uint64_t ptw_before_reuse = reuse.ptw_requests();
        if (!reuse.update_g_context(
                memblock::ReferencePageMode::sv39, g_root_b, vmid) ||
            !reuse.issue_sfence(0, vmid, true, false, false, true) ||
            !run_fenced_load(reuse, address, physical_b + 0x188, 1, 117) ||
            reuse.ptw_requests() <= ptw_before_reuse) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << reuse.cycle() << " phase=same-vmid-refill reason="
                      << (reuse.error().empty()
                              ? "same-VMID root reuse did not refill"
                              : reuse.error()) << '\n';
            return 1;
        }
        same_id_cycles += reuse.cycle();
        same_id_ptw_requests += reuse.ptw_requests();
        same_id_writebacks += reuse.writebacks();
    }

    std::uint64_t outstanding_cycles = 0;
    std::uint64_t outstanding_ptw_requests = 0;
    std::uint64_t outstanding_writebacks = 0;

    // Snapshot an old root-leaf PTE in the delayed PTW response queue, then
    // replace it and fence while the walk is still outstanding. The canceled
    // access must not write back, and reuse of its identity must observe only
    // the new mapping.
    unsigned outstanding_stage1_walks = 0;
    const auto run_outstanding_sfence = [&](bool selective) {
        memblock::Environment outstanding(argc, argv);
        constexpr std::uint64_t virtual_page = 0x40000000ULL;
        constexpr std::uint64_t address = virtual_page + 0x188;
        constexpr std::uint64_t old_physical = 0x80000000ULL;
        constexpr std::uint64_t new_physical = 0xc0000000ULL;
        constexpr std::uint64_t root = 0xbc000000ULL;
        const char *const scope = selective ? "selective" : "global";
        const char *const mode = outstanding_vs_sv48 ? "sv48" : "sv39";
        outstanding.memory().fill_incrementing(old_physical, 0x1000, 0x16);
        outstanding.memory().fill_incrementing(new_physical, 0x1000, 0xa6);
        outstanding.configure_backpressure(
            selective ? 0x13198a2e03707344ULL : 0xa4093822299f31d0ULL,
            true,
            memblock::ResponseLatencyProfiles{
                memblock::ResponseLatencyProfile::compact,
                memblock::ResponseLatencyProfile::spec,
                memblock::ResponseLatencyProfile::compact});
        const bool configured = outstanding.reset() &&
            (outstanding_vs_sv48
                ? outstanding.map_sv48_1g(
                      virtual_page, old_physical, root)
                : outstanding.map_sv39_1g(
                      virtual_page, old_physical, root)) &&
            (outstanding_vs_sv48
                ? outstanding.activate_sv48(root, 71)
                : outstanding.activate_sv39(root, 71));
        if (!configured) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << outstanding.cycle() << " phase=outstanding-stage1-"
                      << mode << '-' << scope << "-configuration reason="
                      << outstanding.error() << '\n';
            return false;
        }

        const memblock::LoadTransaction canceled{
            .address = address,
            .oracle_address = old_physical + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 20,
            .lq = 0,
            .pdest = 118,
            .lane = 2,
        };
        const std::uint64_t ptw_before_walk = outstanding.ptw_requests();
        const std::uint64_t target_pte =
            root + (outstanding_vs_sv48 ? 0x1000ULL : 0) +
            ((virtual_page >> 30) & 0x1ff) * 8;
        if (!outstanding.set_rob_head(canceled.rob, canceled.rob_flag) ||
            !outstanding.enqueue_load(canceled) ||
            !outstanding.issue_load(canceled, 512) ||
            !outstanding.run_until_ptw_request_covering(
                target_pte, ptw_before_walk, 4096, 12)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << outstanding.cycle() << " phase=outstanding-stage1-"
                      << mode << '-' << scope << "-old-pte-request reason="
                      << (outstanding.error().empty()
                              ? "PTW response was not held outstanding"
                              : outstanding.error()) << '\n';
            return false;
        }
        const std::uint64_t stale_ptw_snapshot = outstanding.ptw_requests();
        const bool remapped = outstanding_vs_sv48
            ? outstanding.map_sv48_1g(
                  virtual_page, new_physical, root)
            : outstanding.map_sv39_1g(
                  virtual_page, new_physical, root);
        if (!remapped ||
            !outstanding.issue_sfence_with_redirect(
                19,
                false,
                false,
                selective ? address : 0,
                selective ? 71 : 0,
                !selective,
                !selective) ||
            outstanding.writebacks() != 0) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << outstanding.cycle() << " phase=outstanding-stage1-"
                      << mode << '-' << scope << "-fence reason="
                      << (outstanding.error().empty()
                              ? "redirected stale walk produced a writeback"
                              : outstanding.error()) << '\n';
            return false;
        }
        if (outstanding.lq_dequeued() + outstanding.lq_canceled() <
                outstanding.lq_allocated() &&
            !outstanding.account_lq_cancellation(1)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << outstanding.cycle() << " phase=outstanding-stage1-"
                      << mode << '-' << scope << "-cancel-accounting reason="
                      << outstanding.error() << '\n';
            return false;
        }

        auto survivor = canceled;
        survivor.oracle_address = new_physical + 0x188;
        survivor.pdest = 119;
        survivor.lane = 1;
        outstanding.expect_load(survivor);
        if (!outstanding.enqueue_load(survivor) ||
            !outstanding.issue_load(survivor, 2048) ||
            !outstanding.run_until_complete(16384) ||
            !outstanding.run_until_lq_retired(4096) ||
            outstanding.ptw_requests() <= stale_ptw_snapshot ||
            outstanding.lq_dequeued() + outstanding.lq_canceled() !=
                outstanding.lq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << outstanding.cycle() << " phase=outstanding-stage1-"
                      << mode << '-' << scope << "-refill reason="
                      << (outstanding.error().empty()
                              ? "post-fence access reused the stale walk"
                              : outstanding.error()) << '\n';
            return false;
        }
        outstanding_cycles += outstanding.cycle();
        outstanding_ptw_requests += outstanding.ptw_requests();
        outstanding_writebacks += outstanding.writebacks();
        ++outstanding_stage1_walks;
        return true;
    };

    if (!run_outstanding_sfence(outstanding_selective)) {
        return 1;
    }

    unsigned outstanding_vs_walks = 0;
    unsigned outstanding_g_walks = 0;
    const auto run_outstanding_hfence = [&](bool vs_stage, bool selective) {
        memblock::Environment outstanding(argc, argv);
        constexpr std::uint64_t virtual_page = 0x40000000ULL;
        constexpr std::uint64_t address = virtual_page + 0x188;
        constexpr std::uint64_t old_physical = 0x80000000ULL;
        constexpr std::uint64_t new_physical = 0xc0000000ULL;
        constexpr std::uint64_t root = 0xbc000000ULL;
        const char *const stage = vs_stage ? "vs" : "g";
        const char *const scope = selective ? "selective" : "global";
        const bool stage_sv48 =
            vs_stage ? outstanding_vs_sv48 : outstanding_g_sv48;
        const char *const mode = stage_sv48
            ? (vs_stage ? "sv48" : "sv48x4")
            : (vs_stage ? "sv39" : "sv39x4");
        outstanding.memory().fill_incrementing(old_physical, 0x1000, 0x2b);
        outstanding.memory().fill_incrementing(new_physical, 0x1000, 0xbb);
        outstanding.configure_backpressure(
            vs_stage ? 0x299f31d0a4093822ULL : 0x082efa98ec4e6c89ULL,
            true,
            memblock::ResponseLatencyProfiles{
                memblock::ResponseLatencyProfile::compact,
                memblock::ResponseLatencyProfile::spec,
                memblock::ResponseLatencyProfile::compact});
        const bool configured = outstanding.reset() &&
            (vs_stage
                ? (stage_sv48
                    ? outstanding.map_sv48_1g(
                          virtual_page, old_physical, root)
                    : outstanding.map_sv39_1g(
                          virtual_page, old_physical, root))
                : (stage_sv48
                    ? outstanding.map_sv48x4_1g(
                          virtual_page, old_physical, root)
                    : outstanding.map_sv39x4_1g(
                          virtual_page, old_physical, root))) &&
            outstanding.activate_two_stage_modes(
                vs_stage
                    ? (stage_sv48
                        ? memblock::ReferencePageMode::sv48
                        : memblock::ReferencePageMode::sv39)
                    : memblock::ReferencePageMode::bare,
                vs_stage
                    ? memblock::ReferencePageMode::bare
                    : (stage_sv48
                        ? memblock::ReferencePageMode::sv48
                        : memblock::ReferencePageMode::sv39),
                vs_stage ? root : 0,
                vs_stage ? 0 : root,
                73,
                79);
        if (!configured) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << outstanding.cycle() << " phase=outstanding-"
                      << stage << '-' << mode << '-' << scope
                      << "-configuration reason="
                      << outstanding.error() << '\n';
            return false;
        }

        const memblock::LoadTransaction canceled{
            .address = address,
            .oracle_address = old_physical + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 24,
            .lq = 0,
            .pdest = 120,
            .lane = 0,
        };
        const std::uint64_t ptw_before_walk = outstanding.ptw_requests();
        const std::uint64_t target_pte = root +
            (stage_sv48 ? (vs_stage ? 0x1000ULL : 0x4000ULL) : 0) +
            ((virtual_page >> 30) &
             (stage_sv48 || vs_stage ? 0x1ffULL : 0x7ffULL)) * 8;
        if (!outstanding.set_rob_head(canceled.rob, canceled.rob_flag) ||
            !outstanding.enqueue_load(canceled) ||
            !outstanding.issue_load(canceled, 512) ||
            !outstanding.run_until_ptw_request_covering(
                target_pte, ptw_before_walk, 4096, 12)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << outstanding.cycle() << " phase=outstanding-"
                      << stage << '-' << mode << '-' << scope
                      << "-old-pte-request reason="
                      << (outstanding.error().empty()
                              ? "PTW response was not held outstanding"
                              : outstanding.error()) << '\n';
            return false;
        }
        const std::uint64_t stale_ptw_snapshot = outstanding.ptw_requests();
        const bool remapped = vs_stage
            ? (stage_sv48
                ? outstanding.map_sv48_1g(
                      virtual_page, new_physical, root)
                : outstanding.map_sv39_1g(
                      virtual_page, new_physical, root))
            : (stage_sv48
                ? outstanding.map_sv48x4_1g(
                      virtual_page, new_physical, root)
                : outstanding.map_sv39x4_1g(
                      virtual_page, new_physical, root));
        if (!remapped ||
            !outstanding.issue_sfence_with_redirect(
                23,
                false,
                false,
                selective ? (vs_stage ? address : address >> 2) : 0,
                selective ? (vs_stage ? 73 : 79) : 0,
                !selective,
                !selective,
                vs_stage,
                !vs_stage) ||
            outstanding.writebacks() != 0) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << outstanding.cycle() << " phase=outstanding-"
                      << stage << '-' << mode << '-' << scope
                      << "-fence reason="
                      << (outstanding.error().empty()
                              ? "redirected stale walk produced a writeback"
                              : outstanding.error()) << '\n';
            return false;
        }
        if (outstanding.lq_dequeued() + outstanding.lq_canceled() <
                outstanding.lq_allocated() &&
            !outstanding.account_lq_cancellation(1)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << outstanding.cycle() << " phase=outstanding-"
                      << stage << '-' << mode << '-' << scope
                      << "-cancel-accounting reason="
                      << outstanding.error() << '\n';
            return false;
        }

        auto survivor = canceled;
        survivor.oracle_address = new_physical + 0x188;
        survivor.pdest = 121;
        survivor.lane = 2;
        outstanding.expect_load(survivor);
        if (!outstanding.enqueue_load(survivor) ||
            !outstanding.issue_load(survivor, 2048) ||
            !outstanding.run_until_complete(16384) ||
            !outstanding.run_until_lq_retired(4096) ||
            outstanding.ptw_requests() <= stale_ptw_snapshot ||
            outstanding.lq_dequeued() + outstanding.lq_canceled() !=
                outstanding.lq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << outstanding.cycle() << " phase=outstanding-"
                      << stage << '-' << mode << '-' << scope
                      << "-refill reason="
                      << (outstanding.error().empty()
                              ? "post-fence access reused the stale walk"
                              : outstanding.error()) << '\n';
            return false;
        }
        outstanding_cycles += outstanding.cycle();
        outstanding_ptw_requests += outstanding.ptw_requests();
        outstanding_writebacks += outstanding.writebacks();
        if (vs_stage) {
            ++outstanding_vs_walks;
        } else {
            ++outstanding_g_walks;
        }
        return true;
    };

    if (!run_outstanding_hfence(true, outstanding_selective) ||
        !run_outstanding_hfence(false, outstanding_selective)) {
        return 1;
    }

    unsigned fully_nested_vs_walks = 0;
    unsigned fully_nested_g_walks = 0;
    const auto run_fully_nested_hfence = [&](bool vs_stage, bool selective) {
        memblock::Environment outstanding(argc, argv);
        constexpr std::uint64_t virtual_page = 0x40000000ULL;
        constexpr std::uint64_t address = virtual_page + 0x188;
        constexpr std::uint64_t old_guest = 0x800000000ULL;
        constexpr std::uint64_t new_guest = 0x840000000ULL;
        constexpr std::uint64_t old_physical = 0x80000000ULL;
        constexpr std::uint64_t new_physical = 0xc0000000ULL;
        constexpr std::uint64_t vs_root = 0x100000000ULL;
        constexpr std::uint64_t g_root = 0x90000000ULL;
        const char *const stage = vs_stage ? "nested-vs" : "nested-g";
        const char *const scope = selective ? "selective" : "global";
        const char *const mode = outstanding_vs_sv48
            ? (outstanding_g_sv48 ? "sv48-sv48x4" : "sv48-sv39x4")
            : (outstanding_g_sv48 ? "sv39-sv48x4" : "sv39-sv39x4");
        outstanding.memory().fill_incrementing(old_physical, 0x1000, 0x3c);
        outstanding.memory().fill_incrementing(new_physical, 0x1000, 0xcc);
        outstanding.configure_backpressure(
            vs_stage ? 0x452821e638d01377ULL : 0xbe5466cf34e90c6cULL,
            true,
            memblock::ResponseLatencyProfiles{
                memblock::ResponseLatencyProfile::compact,
                memblock::ResponseLatencyProfile::spec,
                memblock::ResponseLatencyProfile::compact});
        const bool configured = outstanding.reset() &&
            (outstanding_vs_sv48
                ? outstanding.map_sv48_1g(
                      virtual_page, old_guest, vs_root)
                : outstanding.map_sv39_1g(
                      virtual_page, old_guest, vs_root)) &&
            (outstanding_g_sv48
                ? outstanding.map_sv48x4_1g(
                      vs_root, vs_root, g_root)
                : outstanding.map_sv39x4_1g(
                      vs_root, vs_root, g_root)) &&
            (outstanding_g_sv48
                ? outstanding.map_sv48x4_1g(
                      old_guest, old_physical, g_root)
                : outstanding.map_sv39x4_1g(
                      old_guest, old_physical, g_root)) &&
            (!vs_stage || (outstanding_g_sv48
                ? outstanding.map_sv48x4_1g(
                      new_guest, new_physical, g_root)
                : outstanding.map_sv39x4_1g(
                      new_guest, new_physical, g_root))) &&
            outstanding.activate_two_stage_modes(
                outstanding_vs_sv48
                    ? memblock::ReferencePageMode::sv48
                    : memblock::ReferencePageMode::sv39,
                outstanding_g_sv48
                    ? memblock::ReferencePageMode::sv48
                    : memblock::ReferencePageMode::sv39,
                vs_root, g_root, 83, 89);
        if (!configured) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << outstanding.cycle() << " phase=outstanding-"
                      << stage << '-' << mode << '-' << scope
                      << "-configuration reason="
                      << outstanding.error() << '\n';
            return false;
        }

        const memblock::LoadTransaction canceled{
            .address = address,
            .oracle_address = old_physical + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 28,
            .lq = 0,
            .pdest = 122,
            .lane = 1,
        };
        const std::uint64_t first_ptw_request = outstanding.ptw_requests();
        const std::uint64_t target_pte = vs_stage
            ? vs_root + (outstanding_vs_sv48 ? 0x1000ULL : 0) +
                  ((virtual_page >> 30) & 0x1ff) * 8
            : g_root + (outstanding_g_sv48 ? 0x4000ULL : 0) +
                  ((old_guest >> 30) &
                   (outstanding_g_sv48 ? 0x1ffULL : 0x7ffULL)) * 8;
        if (!outstanding.set_rob_head(canceled.rob, canceled.rob_flag) ||
            !outstanding.enqueue_load(canceled) ||
            !outstanding.issue_load(canceled, 512) ||
            !outstanding.run_until_ptw_request_covering(
                target_pte, first_ptw_request, 8192, 12)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << outstanding.cycle() << " phase=outstanding-"
                      << stage << '-' << mode << '-' << scope
                      << "-old-pte-request reason="
                      << outstanding.error() << '\n';
            return false;
        }
        const std::uint64_t stale_ptw_snapshot = outstanding.ptw_requests();
        const bool remapped = vs_stage
            ? (outstanding_vs_sv48
                ? outstanding.map_sv48_1g(
                      virtual_page, new_guest, vs_root)
                : outstanding.map_sv39_1g(
                      virtual_page, new_guest, vs_root))
            : (outstanding_g_sv48
                ? outstanding.map_sv48x4_1g(
                      old_guest, new_physical, g_root)
                : outstanding.map_sv39x4_1g(
                      old_guest, new_physical, g_root));
        if (!remapped ||
            !outstanding.issue_sfence_with_redirect(
                27,
                false,
                false,
                selective ? (vs_stage ? address : old_guest >> 2) : 0,
                selective ? (vs_stage ? 83 : 89) : 0,
                !selective,
                !selective,
                vs_stage,
                !vs_stage) ||
            outstanding.writebacks() != 0) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << outstanding.cycle() << " phase=outstanding-"
                      << stage << '-' << mode << '-' << scope
                      << "-fence reason="
                      << (outstanding.error().empty()
                              ? "redirected stale walk produced a writeback"
                              : outstanding.error()) << '\n';
            return false;
        }
        if (outstanding.lq_dequeued() + outstanding.lq_canceled() <
                outstanding.lq_allocated() &&
            !outstanding.account_lq_cancellation(1)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << outstanding.cycle() << " phase=outstanding-"
                      << stage << '-' << mode << '-' << scope
                      << "-cancel-accounting reason="
                      << outstanding.error() << '\n';
            return false;
        }

        auto survivor = canceled;
        survivor.oracle_address = new_physical + 0x188;
        survivor.pdest = 123;
        survivor.lane = 2;
        outstanding.expect_load(survivor);
        if (!outstanding.enqueue_load(survivor) ||
            !outstanding.issue_load(survivor, 2048) ||
            !outstanding.run_until_complete(32768) ||
            !outstanding.run_until_lq_retired(4096) ||
            outstanding.ptw_requests() <= stale_ptw_snapshot ||
            outstanding.lq_dequeued() + outstanding.lq_canceled() !=
                outstanding.lq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << outstanding.cycle() << " phase=outstanding-"
                      << stage << '-' << mode << '-' << scope
                      << "-refill reason="
                      << (outstanding.error().empty()
                              ? "post-fence access reused the stale walk"
                              : outstanding.error()) << '\n';
            return false;
        }
        outstanding_cycles += outstanding.cycle();
        outstanding_ptw_requests += outstanding.ptw_requests();
        outstanding_writebacks += outstanding.writebacks();
        if (vs_stage) {
            ++fully_nested_vs_walks;
        } else {
            ++fully_nested_g_walks;
        }
        return true;
    };

    if (!run_fully_nested_hfence(true, outstanding_selective) ||
        !run_fully_nested_hfence(false, outstanding_selective)) {
        return 1;
    }

    // Keep two unrelated root-leaf walks in flight at once.  The PTW manager
    // delays its first D response; both loads must still be pending after the
    // first A request is accepted.  The external PTW TileLink channel may
    // legally serialize the two requests, so request history independently
    // proves that both leaf PTEs were fetched.
    unsigned concurrent_walk_cases = 0;
    std::uint64_t concurrent_ptw_max_outstanding = 0;
    const auto run_concurrent_walks = [&](bool nested_translation) {
        memblock::Environment concurrent(argc, argv);
        constexpr std::array<std::uint64_t, 2> virtual_pages{{
            0x40000000ULL, 0x80000000ULL}};
        constexpr std::array<std::uint64_t, 2> guest_pages{{
            0x800000000ULL, 0x840000000ULL}};
        constexpr std::array<std::uint64_t, 2> physical_pages{{
            0x80000000ULL, 0xc0000000ULL}};
        constexpr std::uint64_t stage1_root = 0xbc000000ULL;
        constexpr std::uint64_t vs_root = 0x100000000ULL;
        constexpr std::uint64_t g_root = 0x90000000ULL;
        for (unsigned index = 0; index < physical_pages.size(); ++index) {
            concurrent.memory().fill_incrementing(
                physical_pages[index], 0x1000,
                static_cast<std::uint8_t>(0x29 + index * 0x70));
        }
        concurrent.configure_backpressure(
            nested_translation ? 0x6a09e667f3bcc909ULL
                               : 0xbb67ae8584caa73bULL,
            true,
            memblock::ResponseLatencyProfiles{
                memblock::ResponseLatencyProfile::compact,
                memblock::ResponseLatencyProfile::spec,
                memblock::ResponseLatencyProfile::compact});
        bool configured = concurrent.reset();
        for (unsigned index = 0;
             configured && index < virtual_pages.size(); ++index) {
            if (nested_translation) {
                configured = outstanding_vs_sv48
                    ? concurrent.map_sv48_1g(
                          virtual_pages[index], guest_pages[index], vs_root)
                    : concurrent.map_sv39_1g(
                          virtual_pages[index], guest_pages[index], vs_root);
            } else {
                configured = outstanding_vs_sv48
                    ? concurrent.map_sv48_1g(
                          virtual_pages[index], physical_pages[index], stage1_root)
                    : concurrent.map_sv39_1g(
                          virtual_pages[index], physical_pages[index], stage1_root);
            }
        }
        if (configured && nested_translation) {
            configured = outstanding_g_sv48
                ? concurrent.map_sv48x4_1g(vs_root, vs_root, g_root)
                : concurrent.map_sv39x4_1g(vs_root, vs_root, g_root);
        }
        for (unsigned index = 0;
             configured && nested_translation &&
                 index < guest_pages.size(); ++index) {
            configured = outstanding_g_sv48
                ? concurrent.map_sv48x4_1g(
                      guest_pages[index], physical_pages[index], g_root)
                : concurrent.map_sv39x4_1g(
                      guest_pages[index], physical_pages[index], g_root);
        }
        if (configured) {
            configured = nested_translation
                ? concurrent.activate_two_stage_modes(
                      outstanding_vs_sv48
                          ? memblock::ReferencePageMode::sv48
                          : memblock::ReferencePageMode::sv39,
                      outstanding_g_sv48
                          ? memblock::ReferencePageMode::sv48
                          : memblock::ReferencePageMode::sv39,
                      vs_root, g_root, 97, 99)
                : (outstanding_vs_sv48
                    ? concurrent.activate_sv48(stage1_root, 97)
                    : concurrent.activate_sv39(stage1_root, 97));
        }
        std::vector<memblock::LoadTransaction> loads;
        for (unsigned index = 0;
             configured && index < virtual_pages.size(); ++index) {
            loads.push_back(memblock::LoadTransaction{
                .address = virtual_pages[index] + 0x188,
                .oracle_address = physical_pages[index] + 0x188,
                .op = memblock::LoadOp::ld,
                .rob = static_cast<std::uint8_t>(40 + index),
                .lq = static_cast<std::uint8_t>(index),
                .pdest = static_cast<std::uint8_t>(124 + index),
                .lane = index,
            });
            concurrent.expect_load(loads.back());
            configured = concurrent.enqueue_load(loads.back());
        }
        const char *const stage = nested_translation ? "nested" : "stage1";
        const std::uint64_t first_ptw_request = concurrent.ptw_requests();
        const auto target_pte = [&](std::uint64_t virtual_page) {
            const std::uint64_t walk_root = nested_translation
                ? vs_root : stage1_root;
            return walk_root + (outstanding_vs_sv48 ? 0x1000ULL : 0) +
                ((virtual_page >> 30) & 0x1ffULL) * 8;
        };
        if (!configured ||
            !concurrent.set_rob_head(loads.back().rob, loads.back().rob_flag) ||
            !concurrent.issue_load_batch(loads, 2048) ||
            !concurrent.run_until_ptw_requests(first_ptw_request + 1, 4096) ||
            concurrent.pending_scalar_loads() != loads.size() ||
            !concurrent.run_until_all_complete(32768) ||
            !concurrent.run_until_lq_retired(4096) ||
            !concurrent.run_until_ptw_request_covering(
                target_pte(virtual_pages[0]), first_ptw_request, 1) ||
            !concurrent.run_until_ptw_request_covering(
                target_pte(virtual_pages[1]), first_ptw_request, 1) ||
            concurrent.writebacks() != loads.size()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << concurrent.cycle() << " phase=concurrent-" << stage
                      << "-walks max_ptw_outstanding="
                      << concurrent.ptw_max_outstanding_requests()
                      << " writebacks=" << concurrent.writebacks()
                      << " reason=" << concurrent.error() << '\n';
            return false;
        }
        outstanding_cycles += concurrent.cycle();
        outstanding_ptw_requests += concurrent.ptw_requests();
        outstanding_writebacks += concurrent.writebacks();
        concurrent_ptw_max_outstanding = std::max(
            concurrent_ptw_max_outstanding,
            concurrent.ptw_max_outstanding_requests());
        ++concurrent_walk_cases;
        return true;
    };

    if (!run_concurrent_walks(false) || !run_concurrent_walks(true)) {
        return 1;
    }

    std::cout << "MEMBLOCK_TRANSLATION_FENCE_PASS"
              << " cycle="
              << (environment.cycle() + nested.cycle() + vvma.cycle() +
                  same_id_cycles + outstanding_cycles)
              << " ptw_requests="
              << (environment.ptw_requests() + nested.ptw_requests() +
                  vvma.ptw_requests() + same_id_ptw_requests +
                  outstanding_ptw_requests)
              << " writebacks="
              << (environment.writebacks() + nested.writebacks() +
                  vvma.writebacks() + same_id_writebacks +
                  outstanding_writebacks)
              << " same_id_reuses=3"
              << " outstanding_vs_mode="
              << (outstanding_vs_sv48 ? "sv48" : "sv39")
              << " outstanding_g_mode="
              << (outstanding_g_sv48 ? "sv48x4" : "sv39x4")
              << " outstanding_scope="
              << (outstanding_selective ? "selective" : "global")
              << " outstanding_walks="
              << outstanding_stage1_walks + outstanding_vs_walks +
                  outstanding_g_walks + fully_nested_vs_walks +
                  fully_nested_g_walks
              << " outstanding_stage1=" << outstanding_stage1_walks
              << " outstanding_vs=" << outstanding_vs_walks
              << " outstanding_g=" << outstanding_g_walks
              << " outstanding_nested_vs=" << fully_nested_vs_walks
              << " outstanding_nested_g=" << fully_nested_g_walks
              << " concurrent_walk_cases=" << concurrent_walk_cases
              << " concurrent_ptw_max_outstanding="
              << concurrent_ptw_max_outstanding
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_translation_inflight_context(
    int argc,
    char **argv,
    bool outstanding_vs_sv48,
    bool outstanding_g_sv48)
{
    std::uint64_t outstanding_cycles = 0;
    std::uint64_t outstanding_ptw_requests = 0;
    std::uint64_t outstanding_writebacks = 0;

    // A context-changing instruction is older than the translated load and
    // redirects it. Hold the old PTW response beyond the CSR propagation and
    // redirect interval, then reuse the same ROB/LQ identity under a different
    // root and ID. Only the new mapping may produce an architectural writeback.
    unsigned inflight_context_cases = 0;
    unsigned inflight_mode_cases = 0;
    const auto run_inflight_context_switch = [&](
        bool nested_translation, bool switch_mode) {
        memblock::Environment context(argc, argv);
        constexpr std::uint64_t virtual_page = 0x40000000ULL;
        constexpr std::uint64_t address = virtual_page + 0x188;
        constexpr std::array<std::uint64_t, 2> guest_pages{{
            0x800000000ULL, 0x840000000ULL}};
        constexpr std::array<std::uint64_t, 2> physical_pages{{
            0x80000000ULL, 0xc0000000ULL}};
        constexpr std::array<std::uint64_t, 2> stage1_roots{{
            0xbc000000ULL, 0xbd000000ULL}};
        constexpr std::array<std::uint64_t, 2> vs_roots{{
            0x100000000ULL, 0x140000000ULL}};
        constexpr std::array<std::uint64_t, 2> g_roots{{
            0x90000000ULL, 0x94000000ULL}};
        for (unsigned index = 0; index < physical_pages.size(); ++index) {
            context.memory().fill_incrementing(
                physical_pages[index], 0x1000,
                static_cast<std::uint8_t>(0x3d + index * 0x71));
        }
        context.configure_backpressure(
            nested_translation ? 0x510e527fade682d1ULL
                               : 0x9b05688c2b3e6c1fULL,
            true,
            memblock::ResponseLatencyProfiles{
                memblock::ResponseLatencyProfile::compact,
                memblock::ResponseLatencyProfile::spec,
                memblock::ResponseLatencyProfile::compact});
        bool configured = context.reset();
        for (unsigned index = 0; configured && index < 2; ++index) {
            const bool map_vs_sv48 =
                index == 0 || !switch_mode
                    ? outstanding_vs_sv48 : !outstanding_vs_sv48;
            const bool map_g_sv48 =
                index == 0 || !switch_mode
                    ? outstanding_g_sv48 : !outstanding_g_sv48;
            configured = nested_translation
                ? (map_vs_sv48
                    ? context.map_sv48_1g(
                          virtual_page, guest_pages[index], vs_roots[index])
                    : context.map_sv39_1g(
                          virtual_page, guest_pages[index], vs_roots[index]))
                : (map_vs_sv48
                    ? context.map_sv48_1g(
                          virtual_page, physical_pages[index],
                          stage1_roots[index])
                    : context.map_sv39_1g(
                          virtual_page, physical_pages[index],
                          stage1_roots[index]));
            if (configured && nested_translation) {
                configured = map_g_sv48
                    ? context.map_sv48x4_1g(
                          vs_roots[index], vs_roots[index], g_roots[index])
                    : context.map_sv39x4_1g(
                          vs_roots[index], vs_roots[index], g_roots[index]);
            }
            if (configured && nested_translation) {
                configured = map_g_sv48
                    ? context.map_sv48x4_1g(
                          guest_pages[index], physical_pages[index],
                          g_roots[index])
                    : context.map_sv39x4_1g(
                          guest_pages[index], physical_pages[index],
                          g_roots[index]);
            }
        }
        if (configured) {
            configured = nested_translation
                ? context.activate_two_stage_modes(
                      outstanding_vs_sv48
                          ? memblock::ReferencePageMode::sv48
                          : memblock::ReferencePageMode::sv39,
                      outstanding_g_sv48
                          ? memblock::ReferencePageMode::sv48
                          : memblock::ReferencePageMode::sv39,
                      vs_roots[0], g_roots[0], 101, 103)
                : (outstanding_vs_sv48
                    ? context.activate_sv48(stage1_roots[0], 101)
                    : context.activate_sv39(stage1_roots[0], 101));
        }
        if (!configured) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << context.cycle() << " phase=inflight-context-"
                      << (nested_translation ? "nested" : "stage1")
                      << (switch_mode ? "-mode" : "-root-id")
                      << "-configuration reason=" << context.error() << '\n';
            return false;
        }

        memblock::LoadTransaction canceled{
            .address = address,
            .oracle_address = physical_pages[0] + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 50,
            .lq = 0,
            .pdest = 126,
            .lane = 0,
        };
        context.force_next_ptw_response_delay(256);
        const std::uint64_t first_ptw_request = context.ptw_requests();
        if (!context.set_rob_head(canceled.rob, canceled.rob_flag) ||
            !context.enqueue_load(canceled) ||
            !context.issue_load(canceled, 512) ||
            !context.run_until_ptw_requests(first_ptw_request + 1, 4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << context.cycle() << " phase=inflight-context-"
                      << (nested_translation ? "nested" : "stage1")
                      << (switch_mode ? "-mode" : "-root-id")
                      << "-old-request reason=" << context.error() << '\n';
            return false;
        }
        const std::uint64_t stale_ptw_snapshot = context.ptw_requests();
        const bool switched = nested_translation
            ? context.update_two_stage_context(
                  (switch_mode ? !outstanding_vs_sv48 : outstanding_vs_sv48)
                      ? memblock::ReferencePageMode::sv48
                      : memblock::ReferencePageMode::sv39,
                  (switch_mode ? !outstanding_g_sv48 : outstanding_g_sv48)
                      ? memblock::ReferencePageMode::sv48
                      : memblock::ReferencePageMode::sv39,
                  vs_roots[1], g_roots[1], 102, 104)
            : context.update_stage_one_context(
                  (switch_mode ? !outstanding_vs_sv48 : outstanding_vs_sv48)
                      ? memblock::ReferencePageMode::sv48
                      : memblock::ReferencePageMode::sv39,
                  stage1_roots[1], 102);
        if (!switched || !context.redirect_after(49, false, false) ||
            context.writebacks() != 0) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << context.cycle() << " phase=inflight-context-"
                      << (nested_translation ? "nested" : "stage1")
                      << (switch_mode ? "-mode" : "-root-id")
                      << "-switch reason="
                      << (context.error().empty()
                              ? "redirected old context produced a writeback"
                              : context.error()) << '\n';
            return false;
        }
        if (context.lq_dequeued() + context.lq_canceled() <
                context.lq_allocated() &&
            !context.account_lq_cancellation(1)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << context.cycle() << " phase=inflight-context-"
                      << (nested_translation ? "nested" : "stage1")
                      << (switch_mode ? "-mode" : "-root-id")
                      << "-cancel-accounting reason=" << context.error() << '\n';
            return false;
        }

        canceled.oracle_address = physical_pages[1] + 0x188;
        canceled.pdest = 127;
        canceled.lane = 1;
        context.expect_load(canceled);
        if (!context.enqueue_load(canceled) ||
            !context.issue_load(canceled, 2048) ||
            !context.run_until_complete(32768) ||
            !context.run_until_lq_retired(4096) ||
            context.ptw_requests() <= stale_ptw_snapshot ||
            context.writebacks() != 1 ||
            context.lq_dequeued() + context.lq_canceled() !=
                context.lq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << context.cycle() << " phase=inflight-context-"
                      << (nested_translation ? "nested" : "stage1")
                      << (switch_mode ? "-mode" : "-root-id")
                      << "-new-request reason="
                      << (context.error().empty()
                              ? "new context reused stale translation"
                              : context.error()) << '\n';
            return false;
        }
        outstanding_cycles += context.cycle();
        outstanding_ptw_requests += context.ptw_requests();
        outstanding_writebacks += context.writebacks();
        ++inflight_context_cases;
        inflight_mode_cases += switch_mode;
        return true;
    };

    if (!run_inflight_context_switch(false, false) ||
        !run_inflight_context_switch(true, false) ||
        !run_inflight_context_switch(false, true) ||
        !run_inflight_context_switch(true, true)) {
        return 1;
    }

    unsigned inflight_v_cases = 0;
    const auto run_inflight_virtualization_switch = [&](bool to_nested) {
        memblock::Environment context(argc, argv);
        constexpr std::uint64_t virtual_page = 0x40000000ULL;
        constexpr std::uint64_t address = virtual_page + 0x188;
        constexpr std::uint64_t guest_page = 0x880000000ULL;
        constexpr std::array<std::uint64_t, 2> physical_pages{{
            0x80000000ULL, 0xc0000000ULL}};
        constexpr std::uint64_t stage1_root = 0xbe000000ULL;
        constexpr std::uint64_t vs_root = 0x180000000ULL;
        constexpr std::uint64_t g_root = 0x98000000ULL;
        for (unsigned index = 0; index < physical_pages.size(); ++index) {
            context.memory().fill_incrementing(
                physical_pages[index], 0x1000,
                static_cast<std::uint8_t>(0x4f + index * 0x63));
        }
        context.configure_backpressure(
            to_nested ? 0x1f83d9abfb41bd6bULL
                      : 0x5be0cd19137e2179ULL,
            true,
            memblock::ResponseLatencyProfiles{
                memblock::ResponseLatencyProfile::compact,
                memblock::ResponseLatencyProfile::spec,
                memblock::ResponseLatencyProfile::compact});
        const std::uint64_t host_physical =
            to_nested ? physical_pages[0] : physical_pages[1];
        const std::uint64_t nested_physical =
            to_nested ? physical_pages[1] : physical_pages[0];
        bool configured = context.reset() &&
            (outstanding_vs_sv48
                ? context.map_sv48_1g(
                      virtual_page, host_physical, stage1_root)
                : context.map_sv39_1g(
                      virtual_page, host_physical, stage1_root)) &&
            (outstanding_vs_sv48
                ? context.map_sv48_1g(
                      virtual_page, guest_page, vs_root)
                : context.map_sv39_1g(
                      virtual_page, guest_page, vs_root)) &&
            (outstanding_g_sv48
                ? context.map_sv48x4_1g(vs_root, vs_root, g_root)
                : context.map_sv39x4_1g(vs_root, vs_root, g_root)) &&
            (outstanding_g_sv48
                ? context.map_sv48x4_1g(
                      guest_page, nested_physical, g_root)
                : context.map_sv39x4_1g(
                      guest_page, nested_physical, g_root));
        if (configured) {
            configured = to_nested
                ? (outstanding_vs_sv48
                    ? context.activate_sv48(stage1_root, 111)
                    : context.activate_sv39(stage1_root, 111))
                : context.activate_two_stage_modes(
                      outstanding_vs_sv48
                          ? memblock::ReferencePageMode::sv48
                          : memblock::ReferencePageMode::sv39,
                      outstanding_g_sv48
                          ? memblock::ReferencePageMode::sv48
                          : memblock::ReferencePageMode::sv39,
                      vs_root, g_root, 113, 115);
        }
        if (!configured) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << context.cycle() << " phase=inflight-v-"
                      << (to_nested ? "host-to-nested" : "nested-to-host")
                      << "-configuration reason=" << context.error() << '\n';
            return false;
        }

        memblock::LoadTransaction canceled{
            .address = address,
            .oracle_address =
                (to_nested ? host_physical : nested_physical) + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 52,
            .lq = 0,
            .pdest = 128,
            .lane = 0,
        };
        context.force_next_ptw_response_delay(256);
        const std::uint64_t first_ptw_request = context.ptw_requests();
        if (!context.set_rob_head(canceled.rob, canceled.rob_flag) ||
            !context.enqueue_load(canceled) ||
            !context.issue_load(canceled, 512) ||
            !context.run_until_ptw_requests(first_ptw_request + 1, 4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << context.cycle() << " phase=inflight-v-"
                      << (to_nested ? "host-to-nested" : "nested-to-host")
                      << "-old-request reason=" << context.error() << '\n';
            return false;
        }
        const std::uint64_t stale_ptw_snapshot = context.ptw_requests();
        const bool switched = to_nested
            ? context.activate_two_stage_modes(
                  outstanding_vs_sv48
                      ? memblock::ReferencePageMode::sv48
                      : memblock::ReferencePageMode::sv39,
                  outstanding_g_sv48
                      ? memblock::ReferencePageMode::sv48
                      : memblock::ReferencePageMode::sv39,
                  vs_root, g_root, 114, 116)
            : (outstanding_vs_sv48
                ? context.activate_sv48(stage1_root, 112)
                : context.activate_sv39(stage1_root, 112));
        if (!switched || !context.redirect_after(51, false, false) ||
            context.writebacks() != 0) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << context.cycle() << " phase=inflight-v-"
                      << (to_nested ? "host-to-nested" : "nested-to-host")
                      << "-switch reason="
                      << (context.error().empty()
                              ? "redirected old virtualization context wrote back"
                              : context.error()) << '\n';
            return false;
        }
        if (context.lq_dequeued() + context.lq_canceled() <
                context.lq_allocated() &&
            !context.account_lq_cancellation(1)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << context.cycle() << " phase=inflight-v-"
                      << (to_nested ? "host-to-nested" : "nested-to-host")
                      << "-cancel-accounting reason=" << context.error() << '\n';
            return false;
        }

        canceled.oracle_address =
            (to_nested ? nested_physical : host_physical) + 0x188;
        canceled.pdest = 129;
        canceled.lane = 1;
        context.expect_load(canceled);
        if (!context.enqueue_load(canceled) ||
            !context.issue_load(canceled, 2048) ||
            !context.run_until_complete(32768) ||
            !context.run_until_lq_retired(4096) ||
            context.ptw_requests() <= stale_ptw_snapshot ||
            context.writebacks() != 1 ||
            context.lq_dequeued() + context.lq_canceled() !=
                context.lq_allocated()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FENCE_FAIL cycle="
                      << context.cycle() << " phase=inflight-v-"
                      << (to_nested ? "host-to-nested" : "nested-to-host")
                      << "-new-request reason="
                      << (context.error().empty()
                              ? "new virtualization context reused stale translation"
                              : context.error()) << '\n';
            return false;
        }
        outstanding_cycles += context.cycle();
        outstanding_ptw_requests += context.ptw_requests();
        outstanding_writebacks += context.writebacks();
        ++inflight_context_cases;
        ++inflight_v_cases;
        return true;
    };

    if (!run_inflight_virtualization_switch(true) ||
        !run_inflight_virtualization_switch(false)) {
        return 1;
    }

    std::cout << "MEMBLOCK_TRANSLATION_INFLIGHT_CONTEXT_PASS"
              << " cycle=" << outstanding_cycles
              << " ptw_requests=" << outstanding_ptw_requests
              << " writebacks=" << outstanding_writebacks
              << " outstanding_vs_mode="
              << (outstanding_vs_sv48 ? "sv48" : "sv39")
              << " outstanding_g_mode="
              << (outstanding_g_sv48 ? "sv48x4" : "sv39x4")
              << " inflight_context_cases=" << inflight_context_cases
              << " inflight_mode_cases=" << inflight_mode_cases
              << " inflight_v_cases=" << inflight_v_cases
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_translation_context(int argc, char **argv)
{
    auto run_context_load = [](
        memblock::Environment &environment,
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint8_t rob,
        std::uint8_t lq,
        std::uint8_t pdest,
        unsigned lane) {
        const memblock::LoadTransaction transaction{
            .address = virtual_address,
            .oracle_address = physical_address,
            .op = memblock::LoadOp::ld,
            .rob = rob,
            .lq = lq,
            .pdest = pdest,
            .lane = lane,
        };
        environment.expect_load(transaction);
        return environment.set_rob_head(transaction.rob, transaction.rob_flag) &&
            environment.enqueue_load(transaction) &&
            environment.issue_load(transaction, 2048) &&
            environment.run_until_complete(16384) &&
            environment.run_until_lq_retired(4096);
    };

    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_address = 0x64000188ULL;
    constexpr std::uint64_t virtual_base = 0x64000000ULL;
    constexpr std::uint64_t sv39_physical = 0xc5000000ULL;
    constexpr std::uint64_t sv48_physical = 0xc6000000ULL;
    constexpr std::uint64_t sv39_root = 0x92000000ULL;
    constexpr std::uint64_t sv48_root = 0x93000000ULL;
    environment.memory().fill_incrementing(sv39_physical, 0x1000, 0x1d);
    environment.memory().fill_incrementing(sv48_physical, 0x1000, 0xe7);
    environment.configure_backpressure(0x9e3779b97f4a7c15ULL, true);
    if (!environment.reset() ||
        !environment.map_sv39_4k(virtual_base, sv39_physical, sv39_root) ||
        !environment.map_sv48_4k(virtual_base, sv48_physical, sv48_root) ||
        !environment.activate_sv39(sv39_root, 3)) {
        std::cerr << "MEMBLOCK_TRANSLATION_CONTEXT_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction old_context{
        .address = virtual_address,
        .oracle_address = sv39_physical + 0x188,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .pdest = 90,
        .lane = 0,
    };
    environment.expect_load(old_context);
    if (!environment.enqueue_load(old_context) ||
        !environment.issue_load(old_context, 512) ||
        !environment.run_until_complete(8192) ||
        !environment.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_TRANSLATION_CONTEXT_FAIL cycle="
                  << environment.cycle() << " phase=sv39 reason="
                  << environment.error() << '\n';
        return 1;
    }
    const std::uint64_t ptw_before_switch = environment.ptw_requests();

    if (!environment.activate_sv48(sv48_root, 4)) {
        std::cerr << "MEMBLOCK_TRANSLATION_CONTEXT_FAIL cycle="
                  << environment.cycle() << " phase=switch reason="
                  << environment.error() << '\n';
        return 1;
    }
    const memblock::LoadTransaction new_context{
        .address = virtual_address,
        .oracle_address = sv48_physical + 0x188,
        .op = memblock::LoadOp::ld,
        .rob = 1,
        .lq = 1,
        .pdest = 91,
        .lane = 1,
    };
    environment.expect_load(new_context);
    if (!environment.enqueue_load(new_context) ||
        !environment.issue_load(new_context, 512) ||
        !environment.run_until_complete(16384) ||
        !environment.run_until_lq_retired(4096) ||
        environment.ptw_requests() <= ptw_before_switch) {
        std::cerr << "MEMBLOCK_TRANSLATION_CONTEXT_FAIL cycle="
                  << environment.cycle() << " phase=sv48 reason="
                  << (environment.error().empty()
                          ? "MODE/root switch reused stale translation"
                          : environment.error())
                  << '\n';
        return 1;
    }

    std::uint64_t total_ptw_requests = environment.ptw_requests();
    std::uint64_t total_writebacks = environment.writebacks();
    unsigned accesses = 2;

    // Same-mode satp root and ASID changes must not reuse the old PA for the
    // same VA. Switching back also checks that identity tags are not confused.
    {
        memblock::Environment context(argc, argv);
        constexpr std::uint64_t address = 0x66000188ULL;
        constexpr std::uint64_t virtual_base = 0x66000000ULL;
        constexpr std::uint64_t physical_a = 0xc7000000ULL;
        constexpr std::uint64_t physical_b = 0xc7100000ULL;
        constexpr std::uint64_t root_a = 0xab000000ULL;
        constexpr std::uint64_t root_b = 0xad000000ULL;
        context.memory().fill_incrementing(physical_a, 0x1000, 0x31);
        context.memory().fill_incrementing(physical_b, 0x1000, 0xc1);
        if (!context.reset() ||
            !context.map_sv39_4k(virtual_base, physical_a, root_a) ||
            !context.map_sv39_4k(virtual_base, physical_b, root_b) ||
            !context.activate_sv39(root_a, 17) ||
            !run_context_load(
                context, address, physical_a + 0x188, 0, 0, 100, 0) ||
            !context.activate_sv39(root_b, 18) ||
            !run_context_load(
                context, address, physical_b + 0x188, 1, 1, 101, 1) ||
            !context.activate_sv39(root_a, 17) ||
            !run_context_load(
                context, address, physical_a + 0x188, 2, 2, 102, 2)) {
            std::cerr << "MEMBLOCK_TRANSLATION_CONTEXT_FAIL phase=satp-asid reason="
                      << context.error() << '\n';
            return 1;
        }
        total_ptw_requests += context.ptw_requests();
        total_writebacks += context.writebacks();
        accesses += 3;
    }

    // Change only the VS root/ASID while retaining the G-stage root/VMID.
    // Both VS page-table trees and both final GPAs are independently mapped.
    {
        memblock::Environment context(argc, argv);
        constexpr std::uint64_t address = 0x68000188ULL;
        constexpr std::uint64_t virtual_base = 0x68000000ULL;
        constexpr std::uint64_t guest_a = 0x8c000000ULL;
        constexpr std::uint64_t guest_b = 0x8d000000ULL;
        constexpr std::uint64_t physical_a = 0xc7200000ULL;
        constexpr std::uint64_t physical_b = 0xc7300000ULL;
        constexpr std::uint64_t vs_root_a = 0xae000000ULL;
        constexpr std::uint64_t vs_root_b = 0xaf000000ULL;
        constexpr std::uint64_t g_root = 0xb0000000ULL;
        context.memory().fill_incrementing(physical_a, 0x1000, 0x42);
        context.memory().fill_incrementing(physical_b, 0x1000, 0xd2);
        if (!context.reset() ||
            !context.map_sv39_4k(virtual_base, guest_a, vs_root_a) ||
            !context.map_sv39_4k(virtual_base, guest_b, vs_root_b) ||
            !context.map_sv39x4_4k(vs_root_a, vs_root_a, g_root) ||
            !context.map_sv39x4_4k(
                vs_root_a + 0x1000, vs_root_a + 0x1000, g_root) ||
            !context.map_sv39x4_4k(
                vs_root_a + 0x2000, vs_root_a + 0x2000, g_root) ||
            !context.map_sv39x4_4k(vs_root_b, vs_root_b, g_root) ||
            !context.map_sv39x4_4k(
                vs_root_b + 0x1000, vs_root_b + 0x1000, g_root) ||
            !context.map_sv39x4_4k(
                vs_root_b + 0x2000, vs_root_b + 0x2000, g_root) ||
            !context.map_sv39x4_4k(guest_a, physical_a, g_root) ||
            !context.map_sv39x4_4k(guest_b, physical_b, g_root) ||
            !context.activate_two_stage(vs_root_a, g_root, 21, 31) ||
            !run_context_load(
                context, address, physical_a + 0x188, 0, 0, 103, 0) ||
            !context.activate_two_stage(vs_root_b, g_root, 22, 31) ||
            !run_context_load(
                context, address, physical_b + 0x188, 1, 1, 104, 1) ||
            !context.activate_two_stage(vs_root_a, g_root, 21, 31) ||
            !run_context_load(
                context, address, physical_a + 0x188, 2, 2, 105, 2)) {
            std::cerr << "MEMBLOCK_TRANSLATION_CONTEXT_FAIL phase=vsatp-asid reason="
                      << context.error() << '\n';
            return 1;
        }
        total_ptw_requests += context.ptw_requests();
        total_writebacks += context.writebacks();
        accesses += 3;
    }

    // Retain the VS translation while changing hgatp VMID/root. The same GPA
    // deliberately resolves to different host pages under the two contexts.
    {
        memblock::Environment context(argc, argv);
        constexpr std::uint64_t address = 0x6a000188ULL;
        constexpr std::uint64_t virtual_base = 0x6a000000ULL;
        constexpr std::uint64_t guest = 0x8e000000ULL;
        constexpr std::uint64_t physical_a = 0xc7400000ULL;
        constexpr std::uint64_t physical_b = 0xc7500000ULL;
        constexpr std::uint64_t vs_root = 0xb1000000ULL;
        constexpr std::uint64_t g_root_a = 0xb2000000ULL;
        constexpr std::uint64_t g_root_b = 0xb3000000ULL;
        context.memory().fill_incrementing(physical_a, 0x1000, 0x53);
        context.memory().fill_incrementing(physical_b, 0x1000, 0xe3);
        if (!context.reset() ||
            !context.map_sv39_4k(virtual_base, guest, vs_root) ||
            !context.map_sv39x4_4k(vs_root, vs_root, g_root_a) ||
            !context.map_sv39x4_4k(
                vs_root + 0x1000, vs_root + 0x1000, g_root_a) ||
            !context.map_sv39x4_4k(
                vs_root + 0x2000, vs_root + 0x2000, g_root_a) ||
            !context.map_sv39x4_4k(guest, physical_a, g_root_a) ||
            !context.map_sv39x4_4k(vs_root, vs_root, g_root_b) ||
            !context.map_sv39x4_4k(
                vs_root + 0x1000, vs_root + 0x1000, g_root_b) ||
            !context.map_sv39x4_4k(
                vs_root + 0x2000, vs_root + 0x2000, g_root_b) ||
            !context.map_sv39x4_4k(guest, physical_b, g_root_b) ||
            !context.activate_two_stage(vs_root, g_root_a, 25, 35) ||
            !run_context_load(
                context, address, physical_a + 0x188, 0, 0, 106, 0) ||
            !context.activate_two_stage(vs_root, g_root_b, 25, 36) ||
            !run_context_load(
                context, address, physical_b + 0x188, 1, 1, 107, 1) ||
            !context.activate_two_stage(vs_root, g_root_a, 25, 35) ||
            !run_context_load(
                context, address, physical_a + 0x188, 2, 2, 108, 2)) {
            std::cerr << "MEMBLOCK_TRANSLATION_CONTEXT_FAIL phase=hgatp-vmid reason="
                      << context.error() << '\n';
            return 1;
        }
        total_ptw_requests += context.ptw_requests();
        total_writebacks += context.writebacks();
        accesses += 3;
    }

    // Exercise virt_changed in both directions with the same input address.
    {
        memblock::Environment context(argc, argv);
        constexpr std::uint64_t address = 0x6c000188ULL;
        constexpr std::uint64_t virtual_base = 0x6c000000ULL;
        constexpr std::uint64_t host_physical = 0xc7600000ULL;
        constexpr std::uint64_t guest = 0x8f000000ULL;
        constexpr std::uint64_t guest_host_physical = 0xc7700000ULL;
        constexpr std::uint64_t host_root = 0xb4000000ULL;
        constexpr std::uint64_t vs_root = 0xb5000000ULL;
        constexpr std::uint64_t g_root = 0xb6000000ULL;
        context.memory().fill_incrementing(host_physical, 0x1000, 0x64);
        context.memory().fill_incrementing(guest_host_physical, 0x1000, 0xf4);
        if (!context.reset() ||
            !context.map_sv39_4k(virtual_base, host_physical, host_root) ||
            !context.map_sv39_4k(virtual_base, guest, vs_root) ||
            !context.map_sv39x4_4k(vs_root, vs_root, g_root) ||
            !context.map_sv39x4_4k(
                vs_root + 0x1000, vs_root + 0x1000, g_root) ||
            !context.map_sv39x4_4k(
                vs_root + 0x2000, vs_root + 0x2000, g_root) ||
            !context.map_sv39x4_4k(guest, guest_host_physical, g_root) ||
            !context.activate_sv39(host_root, 27) ||
            !run_context_load(
                context, address, host_physical + 0x188, 0, 0, 109, 0) ||
            !context.activate_two_stage(vs_root, g_root, 28, 38) ||
            !run_context_load(
                context, address, guest_host_physical + 0x188, 1, 1, 110, 1) ||
            !context.activate_sv39(host_root, 27) ||
            !run_context_load(
                context, address, host_physical + 0x188, 2, 2, 111, 2)) {
            std::cerr << "MEMBLOCK_TRANSLATION_CONTEXT_FAIL phase=virt-transition reason="
                      << context.error() << '\n';
            return 1;
        }
        total_ptw_requests += context.ptw_requests();
        total_writebacks += context.writebacks();
        accesses += 3;
    }

    std::cout << "MEMBLOCK_TRANSLATION_CONTEXT_PASS"
              << " contexts=5"
              << " accesses=" << accesses
              << " ptw_requests=" << total_ptw_requests
              << " writebacks=" << total_writebacks
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_translation_bare(int argc, char **argv)
{
    unsigned completed = 0;
    std::array<std::uint64_t, 4> ptw_requests{};
    auto run_load = [&](memblock::Environment &environment,
                        const memblock::LoadTransaction &transaction) {
        environment.expect_load(transaction);
        return environment.set_rob_head(transaction.rob, transaction.rob_flag) &&
            environment.enqueue_load(transaction) &&
            environment.issue_load(transaction, 1024) &&
            environment.run_until_complete(16384) &&
            environment.run_until_lq_retired(4096);
    };

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t physical = 0x88000000ULL;
        environment.memory().fill_incrementing(physical, 0x1000, 0x2a);
        const memblock::LoadTransaction transaction{
            .address = physical + 0x188,
            .oracle_address = physical + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 130,
            .lane = 0,
        };
        if (!environment.reset() || !environment.activate_bare() ||
            !run_load(environment, transaction)) {
            std::cerr << "MEMBLOCK_TRANSLATION_BARE_FAIL phase=stage1 reason="
                      << environment.error() << '\n';
            return 1;
        }
        ptw_requests[completed] = environment.ptw_requests();
        ++completed;
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t guest = 0xb8000000ULL;
        constexpr std::uint64_t host = 0xc9000000ULL;
        constexpr std::uint64_t g_root = 0x99000000ULL;
        environment.memory().fill_incrementing(host, 0x1000, 0x4f);
        const memblock::LoadTransaction transaction{
            .address = guest + 0x188,
            .oracle_address = host + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 131,
            .lane = 1,
        };
        if (!environment.reset() ||
            !environment.map_sv39x4_4k(guest, host, g_root) ||
            !environment.activate_two_stage_modes(
                memblock::ReferencePageMode::bare,
                memblock::ReferencePageMode::sv39,
                0, g_root, 3, 5) ||
            !run_load(environment, transaction)) {
            std::cerr << "MEMBLOCK_TRANSLATION_BARE_FAIL phase=g-only reason="
                      << environment.error() << '\n';
            return 1;
        }
        const auto reference = memblock::reference_two_stage_walk(
            environment.memory(), 0, g_root, transaction.address,
            memblock::ReferencePageMode::bare,
            memblock::ReferencePageMode::sv39);
        if (!reference.translated || reference.physical_address != transaction.oracle_address) {
            std::cerr << "MEMBLOCK_TRANSLATION_BARE_FAIL phase=g-only-reference\n";
            return 1;
        }
        ptw_requests[completed] = environment.ptw_requests();
        ++completed;
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t virtual_address = 0x67000000ULL;
        constexpr std::uint64_t guest = 0xba000000ULL;
        constexpr std::uint64_t vs_root = 0x9a000000ULL;
        environment.memory().fill_incrementing(guest, 0x1000, 0x73);
        const memblock::LoadTransaction transaction{
            .address = virtual_address + 0x188,
            .oracle_address = guest + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 132,
            .lane = 2,
        };
        if (!environment.reset() ||
            !environment.map_sv39_4k(virtual_address, guest, vs_root) ||
            !environment.activate_two_stage_modes(
                memblock::ReferencePageMode::sv39,
                memblock::ReferencePageMode::bare,
                vs_root, 0, 7, 9) ||
            !run_load(environment, transaction)) {
            std::cerr << "MEMBLOCK_TRANSLATION_BARE_FAIL phase=vs-only reason="
                      << environment.error() << '\n';
            return 1;
        }
        ptw_requests[completed] = environment.ptw_requests();
        ++completed;
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t physical = 0x8a000000ULL;
        environment.memory().fill_incrementing(physical, 0x1000, 0x96);
        const memblock::LoadTransaction transaction{
            .address = physical + 0x188,
            .oracle_address = physical + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 133,
            .lane = 0,
        };
        if (!environment.reset() ||
            !environment.activate_two_stage_modes(
                memblock::ReferencePageMode::bare,
                memblock::ReferencePageMode::bare,
                0, 0, 11, 13) ||
            !run_load(environment, transaction)) {
            std::cerr << "MEMBLOCK_TRANSLATION_BARE_FAIL phase=both-bare reason="
                      << environment.error() << '\n';
            return 1;
        }
        ptw_requests[completed] = environment.ptw_requests();
        ++completed;
    }

    std::cout << "MEMBLOCK_TRANSLATION_BARE_PASS"
              << " cases=" << completed
              << " stage1_ptw=" << ptw_requests[0]
              << " g_only_ptw=" << ptw_requests[1]
              << " vs_only_ptw=" << ptw_requests[2]
              << " both_bare_ptw=" << ptw_requests[3]
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}
