#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

bool MixedScenario::execute_ptw_error(ActionWindow &window)
{
    auto &ptw_error_site = window.ptw_error_site;
    auto &ptw_error_level_class = window.ptw_error_level_class;
    auto &ptw_error_outcome = window.ptw_error_outcome;
    auto &ptw_error_stage1_mode = window.ptw_error_stage1_mode;
    auto &ptw_error_vs_mode = window.ptw_error_vs_mode;
    auto &ptw_error_g_mode = window.ptw_error_g_mode;
    auto &ptw_error_target_level = window.ptw_error_target_level;
    auto &ptw_error_store = window.ptw_error_store;
    auto &ptw_error_address = window.ptw_error_address;
    auto &ptw_error_stage1_root = window.ptw_error_stage1_root;
    auto &ptw_error_vs_root = window.ptw_error_vs_root;
    auto &ptw_error_g_root = window.ptw_error_g_root;
    auto &ptw_requests_before = window.ptw_requests_before;
    auto &requests_before = window.requests_before;
    auto &sample_dcache = window.sample_dcache;

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

    return true;
}

} // namespace memblock::scenarios::mixed
