#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

std::uint64_t MixedScenario::gstage_boundary_loads()
{
    std::uint64_t count = 0;
    for (unsigned cross = 0; cross < gstage_boundary_crosses.size(); ++cross) {
        if ((cross / 3U) % 2U == 0) {
            count += gstage_boundary_crosses[cross];
        }
    }
    return count;
}

std::uint64_t MixedScenario::canonical_boundary_loads()
{
    std::uint64_t count = 0;
    for (unsigned cross = 0; cross < canonical_boundary_crosses.size(); ++cross) {
        if ((cross / 3U) % 2U == 0) {
            count += canonical_boundary_crosses[cross];
        }
    }
    return count;
}

std::string MixedScenario::canonical_boundary_summary()
{
    std::ostringstream stream;
    stream << "actual_canonical_boundary=" << std::accumulate(
                                                  canonical_boundary_crosses.begin(), canonical_boundary_crosses.end(),
                                                  std::uint64_t{0});
    stream << " actual_canonical_boundary_cross=";
    for (unsigned cross = 0; cross < canonical_boundary_crosses.size(); ++cross) {
        stream << (cross == 0 ? "" : ",") << canonical_boundary_crosses[cross];
    }
    stream << " actual_canonical_boundary_controls=" << canonical_boundary_controls
           << " actual_canonical_boundary_recoveries=" << canonical_boundary_recoveries
           << " actual_canonical_boundary_store_checks=" << canonical_boundary_store_checks;
    stream << " actual_vector_canonical_boundary=" << std::accumulate(
                                                          vector_canonical_boundary_crosses.begin(),
                                                          vector_canonical_boundary_crosses.end(), std::uint64_t{0});
    stream << " actual_vector_canonical_boundary_cross=";
    for (unsigned cross = 0; cross < vector_canonical_boundary_crosses.size();
         ++cross) {
        stream << (cross == 0 ? "" : ",")
               << vector_canonical_boundary_crosses[cross];
    }
    stream << " actual_vector_canonical_boundary_controls="
           << vector_canonical_boundary_controls
           << " actual_vector_canonical_boundary_recoveries="
           << vector_canonical_boundary_recoveries
           << " actual_vector_canonical_boundary_store_checks="
           << vector_canonical_boundary_store_checks
           << " actual_vector_canonical_boundary_flows="
           << vector_canonical_boundary_flows;
    return stream.str();
}

std::string MixedScenario::gstage_boundary_summary()
{
    std::ostringstream stream;
    const auto append_crosses = [&](const char *name, const auto &crosses) {
        stream << name << '=' << std::accumulate(
                                     crosses.begin(), crosses.end(), std::uint64_t{0})
               << ' ' << name << "_cross=";
        for (unsigned cross = 0; cross < crosses.size(); ++cross) {
            stream << (cross == 0 ? "" : ",") << crosses[cross];
        }
    };
    append_crosses("actual_gstage_boundary", gstage_boundary_crosses);
    stream << " actual_gstage_boundary_controls=" << gstage_boundary_controls
           << " actual_gstage_boundary_recoveries=" << gstage_boundary_recoveries
           << " actual_gstage_boundary_store_checks=" << gstage_boundary_store_checks
           << ' ';
    append_crosses("actual_vector_gstage_boundary", vector_gstage_boundary_crosses);
    stream << " actual_vector_gstage_boundary_controls=" << vector_gstage_boundary_controls
           << " actual_vector_gstage_boundary_recoveries=" << vector_gstage_boundary_recoveries
           << " actual_vector_gstage_boundary_store_checks=" << vector_gstage_boundary_store_checks
           << " actual_vector_gstage_boundary_flows=" << vector_gstage_boundary_flows;
    return stream.str();
}

void MixedScenario::report_progress(bool force)
{
    if (progress_interval == 0 ||
        (!force && actions < next_progress_action)) {
        return;
    }
    std::cerr << "MEMBLOCK_CONSTRAINED_RANDOM_PROGRESS seed=" << options.seed
              << " actions=" << actions << " cycle=" << environment.cycle()
              << " phase=" << phase << std::endl;
    next_progress_action = progress_interval >
            std::numeric_limits<std::uint64_t>::max() - actions
        ? std::numeric_limits<std::uint64_t>::max()
        : actions + progress_interval;
}

std::string MixedScenario::load_feedback_summary(const memblock::Environment::ScalarLoadFeedbackStats &feedback)
{
    std::ostringstream stream;
    stream << "load_wakeups=" << feedback.wakeups[0] << ','
           << feedback.wakeups[1] << ',' << feedback.wakeups[2]
           << " load_cancels=" << feedback.ld2_cancels[0] << ','
           << feedback.ld2_cancels[1] << ',' << feedback.ld2_cancels[2];
    return stream.str();
}

std::string MixedScenario::raw_load_feedback_summary()
{
    const auto &feedback = environment.scalar_load_feedback_stats();
    std::ostringstream stream;
    stream << "raw_load_wakeups=" << feedback.wakeups[0] << ','
           << feedback.wakeups[1] << ',' << feedback.wakeups[2]
           << " raw_load_cancels=" << feedback.ld2_cancels[0] << ','
           << feedback.ld2_cancels[1] << ','
           << feedback.ld2_cancels[2];
    return stream.str();
}

unsigned long MixedScenario::total_ifetch_prefetches()
{
    const auto &stats = environment.ifetch_prefetch_stats();
    return std::accumulate(
        stats.requests.begin(), stats.requests.end(), std::uint64_t{0});
}

bool MixedScenario::observe_final_writes()
{
    const unsigned timeout = constrained_completion_timeout;
    phase = "final-write-observation-drain";
    if (!environment.run_until_sbuffer_empty(timeout) ||
        !environment.run_until_dcache_idle(timeout) ||
        !environment.checkpoint_dcache_alias_reference(
            permuted_physical_base, OrdinaryPageMappings::span) ||
        !environment.run_until_all_complete(timeout) ||
        !environment.run_until_queues_retired(timeout) ||
        !environment.run_until_uncache_drained(
            environment.uncache_requests(), timeout)) {
        return false;
    }

    // Every tracked cacheable store/atomic commit registered physical
    // 8-byte words.  Read each final word back through the DUT and compare it
    // with the independently maintained reference image.  This turns the
    // reference post-state into an externally observed architectural effect
    // instead of trusting the commit helper's bookkeeping.
    const std::vector<std::uint64_t> words =
        environment.final_observation_words();
    constexpr std::size_t kBatch = 3;
    for (std::size_t offset = 0; offset < words.size(); offset += kBatch) {
        const std::size_t count =
            std::min(kBatch, words.size() - offset);
        std::vector<memblock::LoadTransaction> readbacks;
        std::vector<unsigned> dispatch_lanes;
        readbacks.reserve(count);
        dispatch_lanes.reserve(count);
        for (std::size_t index = 0; index < count; ++index) {
            const std::uint64_t word = words[offset + index];
            const std::uint64_t rob = rob_offset++;
            const std::uint64_t lq = lq_offset++;
            memblock::LoadTransaction readback{
                .address = word,
                .op = memblock::LoadOp::ld,
                .rob = memblock::rob_pointer_value(rob),
                .rob_flag = memblock::rob_pointer_flag(rob),
                .lq = memblock::lq_pointer_value(lq),
                .lq_flag = memblock::lq_pointer_flag(lq),
                .sq = memblock::sq_pointer_value(sq_offset),
                .sq_flag = memblock::sq_pointer_flag(sq_offset),
                .pdest = static_cast<std::uint8_t>(
                    1U + static_cast<unsigned>(rob % 223U)),
                .lane = static_cast<unsigned>(index),
            };
            environment.expect_load_data(
                readback, environment.memory().read_u64(word));
            readbacks.push_back(readback);
            dispatch_lanes.push_back(static_cast<unsigned>(index));
        }
        phase = "final-write-observation-readback";
        if (!environment.set_rob_head(
                readbacks.front().rob, readbacks.front().rob_flag) ||
            !environment.enqueue_load_batch(
                readbacks, dispatch_lanes) ||
            !environment.issue_load_batch(readbacks, timeout) ||
            !environment.run_until_all_complete(timeout) ||
            !environment.run_until_lq_retired(timeout)) {
            return false;
        }
        environment.add_final_observation_observed_words(count);
    }

    phase = "final-write-observation-final-drain";
    if (!environment.run_until_all_complete(timeout) ||
        !environment.run_until_queues_retired(timeout) ||
        !environment.run_until_sbuffer_empty(timeout) ||
        !environment.run_until_dcache_idle(timeout) ||
        !environment.run_until_uncache_drained(
            environment.uncache_requests(), timeout)) {
        return false;
    }
    return environment.verify_final_nc_effects();
}

bool MixedScenario::finish()
{
    phase = "redirect-recovery-context";
    if (!environment.run_until_all_complete(4096) ||
        !environment.run_until_queues_retired(4096) ||
        !flush_pending_translation_fences() ||
        !environment.activate_bare()) {
        return false;
    }
    phase = "redirect-recovery";
    // A distinct older branch can resolve after the preceding memory work
    // retires. It owns this redirect and remains head while its younger load
    // is squashed; it allocates no memory queue entry.
    const std::uint64_t branch_anchor = rob_offset++;
    if (!environment.set_rob_head(memblock::rob_pointer_value(branch_anchor),
            memblock::rob_pointer_flag(branch_anchor))) return false;
    const auto lq_cancels_before = environment.lq_redirect_canceled_observed();
    const auto sq_cancels_before = environment.sq_redirect_canceled_observed();
    const auto canceled_op = static_cast<memblock::LoadOp>(random() % 7);
    const unsigned canceled_size =
        1U << (static_cast<unsigned>(canceled_op) & 3U);
    const std::uint64_t canceled_offset =
        (random() % ((0x10000U - canceled_size) / canceled_size + 1U)) *
        canceled_size;
    auto canceled = make_load(
        cache1_base + canceled_offset, canceled_op,
        random() % memblock::kScalarLoadLanes);
    if (!environment.enqueue_load(canceled) || !environment.issue_load(canceled) ||
        !environment.redirect_after(
            memblock::rob_pointer_value(branch_anchor),
            memblock::rob_pointer_flag(branch_anchor), false) ||
        !environment.run_cycles(32U + random() % 129U)) {
        return false;
    }
    coverage.sample(canceled);
    ++actions;
    const auto canceled_lq = environment.lq_redirect_canceled_observed() - lq_cancels_before;
    if (canceled_lq != 1 || environment.sq_redirect_canceled_observed() != sq_cancels_before) {
        phase = "redirect-recovery-cancellation";
        return false;
    }
    lq_offset -= canceled_lq;
    const std::uint64_t survivor_lq = lq_offset++;
    auto survivor = canceled;
    survivor.lq = memblock::lq_pointer_value(survivor_lq);
    survivor.lq_flag = memblock::lq_pointer_flag(survivor_lq);
    survivor.lane = static_cast<unsigned>(random() % memblock::kScalarLoadLanes);
    // Rename recovery restores this instruction's original free-list
    // candidate, so refetch reuses pdest with a fresh LQ/scoreboard epoch.
    if (!issue_load(survivor)) {
        return false;
    }
    ++coverage.redirect_recovery;
    ++coverage.cacheable;

    phase = "final-drain";
    if (!environment.run_until_all_complete(2048) ||
        !environment.run_until_queues_retired(4096) ||
        !environment.run_cycles(32) ||
        !observe_final_writes() ||
        environment.final_observation_observed_words() !=
            environment.final_observation_words().size()) {
        phase = "final-observation-accounting";
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
    if (actions != options.transactions ||
        !environment.dcache_grants_drained() ||
        environment.dcache_alias_recalls() != environment.dcache_alias_recall_responses() ||
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
        environment.dcache_probe_sources() != expected_probe_sources ||
        environment.dcache_probe_source_reuses() !=
            expected_probe_source_reuses ||
        environment.dcache_probe_source_wraps() !=
            expected_probe_source_wraps) {
        phase = "accounting-gates";
        return false;
    }
    return true;
}

int MixedScenario::run()
{
    const auto cadence_summary = [this]() {
        std::ostringstream stream;
        const auto &counts = environment.lsq_enqueue_cadence();
        for (std::size_t index = 0; index < counts.size(); ++index) {
            if (index != 0) stream << ',';
            stream << counts[index];
        }
        return stream.str();
    };
    report_progress();
    const auto ptw_order_summary = [&] {
        std::ostringstream out;
        out << " ptw_response_order=";
        const auto &counts = environment.ptw_response_order_stats();
        for (unsigned i = 0; i < counts.size(); ++i) out << (i ? "," : "") << counts[i];
        return out.str();
    };
    initialize_memory();
    const bool completed = execute();
    report_progress(true);
    if (!completed) {
        std::cerr << "MEMBLOCK_CONSTRAINED_RANDOM_FAIL seed=" << options.seed
                  << ptw_order_summary()
              << " transactions=" << actions
                  << " requested=" << options.transactions
                  << " cycle=" << environment.cycle()
                  << " phase=" << phase
                  << " accepted_a={" << environment.dcache_request_summary_since(
                         environment.tilelink_requests() > 16U
                             ? environment.tilelink_requests() - 16U : 0U, 16U)
                  << '}'
                  << " lq=" << environment.lq_dequeued() << '+'
                  << environment.lq_canceled() << '/'
                  << environment.lq_allocated()
                  << " sq=" << environment.sq_dequeued() << '+'
                  << environment.sq_canceled() << '/'
                  << environment.sq_allocated()
                  << " lsq_monitor_schema=3"
                  << " lsq_enqueue_cadence="
                  << cadence_summary()
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
                  << " final_observation_words="
                  << environment.final_observation_words().size()
                  << " final_observation_bytes="
                  << environment.final_observation_bytes()
                  << " final_observation_alias_skips="
                  << environment.final_observation_alias_skips()
                  << " final_observation_observed_words="
                  << environment.final_observation_observed_words()
                  << " final_nc_words="
                  << environment.final_observation_nc_words().size()
                  << " final_nc_bytes="
                  << environment.final_observation_nc_bytes()
                  << " ifetch_prefetches=" << total_ifetch_prefetches()
                  << " l2_stride_prefetches="
                  << environment.hardware_prefetch_stats().l2_source_counts[12]
                  << " reason="
                  << (environment.error().empty()
                             ? "mixed_accounting_gate_failed"
                             : environment.error())
                  << ' ' << load_feedback_summary(
                                environment.scalar_load_feedback_stats())
                  << ' ' << raw_load_feedback_summary()
                  << ' ' << coverage.summary() << ' '
                  << canonical_boundary_summary() << ' '
                  << gstage_boundary_summary() << ' '
                  << constraint_coverage.summary(
                         constraints,
                         environment.dcache_response_latency_stats(),
                         environment.ptw_response_latency_stats(),
                         environment.uncache_response_latency_stats())
                  << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_CONSTRAINED_RANDOM_PASS"
              << " seed=" << options.seed
              << ptw_order_summary()
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
              << " alias_recalls=" << environment.dcache_alias_recalls()
              << " alias_recall_responses=" << environment.dcache_alias_recall_responses()
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
              << " final_observation_words="
              << environment.final_observation_words().size()
              << " final_observation_bytes="
              << environment.final_observation_bytes()
              << " final_observation_alias_skips="
              << environment.final_observation_alias_skips()
              << " final_observation_observed_words="
              << environment.final_observation_observed_words()
              << " final_nc_words="
              << environment.final_observation_nc_words().size()
              << " final_nc_bytes="
              << environment.final_observation_nc_bytes()
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
              << " lsq_monitor_schema=3"
              << " lsq_enqueue_cadence="
              << cadence_summary()
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
              << canonical_boundary_summary() << ' '
              << gstage_boundary_summary() << ' '
              << constraint_coverage.summary(
                     constraints, environment.dcache_response_latency_stats(),
                     environment.ptw_response_latency_stats(),
                     environment.uncache_response_latency_stats())
              << '\n';
    return 0;
}

} // namespace memblock::scenarios::mixed
