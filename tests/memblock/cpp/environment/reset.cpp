#include "implementation.hpp"

namespace memblock {

auto Environment::Impl::check_pin_space()  -> bool
{
    pin_space_digest_ = 1469598103934665603ULL;
    for (unsigned pattern = 0;
         pattern < generated::kPinSpacePatternCount; ++pattern) {
        dut_.drive_pin_space_pattern(pattern);
        dut_.reset.external.set(std::uint64_t{1});
        dut_.refresh_comb();
        if (!dut_.verify_pin_space_pattern(pattern)) {
            error_.record("input-space pattern readback mismatch");
            return false;
        }
        pin_space_digest_ ^= dut_.sample_all_outputs();
        pin_space_digest_ *= 1099511628211ULL;
    }
    dut_.drive_idle_inputs();
    dut_.translation.csr.priv_dmode.set(std::uint64_t{3});
    dut_.translation.csr.priv_imode.set(std::uint64_t{3});
    dut_.refresh_comb();
    return true;
}

auto Environment::Impl::reset()  -> bool
{
    last_reset_completed_cycle_.reset();
    scalar_redirect_boundary_.reset();
    // Re-assert external reset for every invocation.  Relying on the
    // constructor's initial value made repeated-reset scenarios silently
    // run without resetting the DUT.
    // XSTileWrap applies the same child reset to the core and tile-local
    // managers. Model that reset domain by discarding every in-flight
    // protocol transaction before the DUT can reuse source identifiers.
    memory_agent_.reset_link_state();
    ptw_agent_.reset_link_state();
    uncache_agent_.reset_link_state();
    feedback_.reset_observations();
    bridge_.reset_observations();
    ifetch_ptw_pending_ = 0;
    dut_.translation.ifetch_request.valid.set(std::uint64_t{0});
    dut_.translation.ifetch_response.ready.set(std::uint64_t{1});
    dut_.reset.external.set(std::uint64_t{1});
    for (unsigned cycle = 0; cycle < 8; ++cycle) {
        tick(false);
    }
    dut_.reset.external.set(std::uint64_t{0});
    for (unsigned cycle = 0; cycle < 16; ++cycle) {
        tick(false);
        if (!dut_.reset.backend.boolean()) {
            last_reset_completed_cycle_ = this->cycle();
            return true;
        }
    }
    error_.record("internal reset did not deassert within 16 cycles");
    return false;
}

auto Environment::Impl::check_reset_backend_contract()  -> bool
{
    constexpr unsigned sync_stages = 3;
    constexpr unsigned functional_release_cycles = 2 * sync_stages;
    constexpr std::array<unsigned, 3> functional_pulse_widths{1, 2, 5};

    if (!reset()) {
        return false;
    }
    reset_functional_pulses_ = 0;
    reset_dft_pulses_ = 0;
    reset_scan_transitions_ = 0;
    reset_async_assertions_ = 0;

    const auto require_reset = [&](bool expected, const char *phase,
                                   unsigned elapsed) {
        dut_.refresh_comb();
        const bool observed = dut_.reset.backend.boolean();
        if (observed == expected) {
            return true;
        }
        std::ostringstream message;
        message << "backend reset mismatch phase=" << phase
                << " elapsed=" << elapsed
                << " expected=" << expected
                << " observed=" << observed;
        error_.record(message.str());
        return false;
    };
    const auto require_synchronous_release = [&](unsigned latency,
                                                 const char *phase) {
        if (!require_reset(true, phase, 0)) {
            return false;
        }
        for (unsigned elapsed = 1; elapsed <= latency; ++elapsed) {
            tick(false);
            if (!require_reset(elapsed < latency, phase, elapsed)) {
                return false;
            }
        }
        return true;
    };

    // Functional reset crosses the top-level and right MemBlock reset
    // generators. Both asynchronously assert and synchronously release.
    for (const unsigned pulse_width : functional_pulse_widths) {
        dut_.reset.external.set(std::uint64_t{1});
        if (!require_reset(true, "functional-assert", 0)) {
            return false;
        }
        ++reset_async_assertions_;
        for (unsigned elapsed = 0; elapsed < pulse_width; ++elapsed) {
            tick(false);
            if (!require_reset(true, "functional-hold", elapsed + 1)) {
                return false;
            }
        }
        dut_.reset.external.set(std::uint64_t{0});
        if (!require_synchronous_release(
                functional_release_cycles, "functional-release")) {
            return false;
        }
        ++reset_functional_pulses_;
    }

    // DFT functional mode selects lgc_rst_n independently in each
    // ResetGen. External functional reset must therefore be isolated, and
    // release reaches this output after one three-stage synchronizer.
    dut_.dft.reset_lgc_rst_n.set(std::uint64_t{1});
    dut_.dft.reset_mode.set(std::uint64_t{1});
    dut_.dft.reset_scan_mode.set(std::uint64_t{0});
    dut_.reset.external.set(std::uint64_t{1});
    if (!require_reset(false, "dft-functional-isolation", 0)) {
        return false;
    }
    tick(false);
    if (!require_reset(false, "dft-functional-isolation", 1)) {
        return false;
    }
    dut_.reset.external.set(std::uint64_t{0});
    dut_.dft.reset_lgc_rst_n.set(std::uint64_t{0});
    if (!require_reset(true, "dft-functional-assert", 0)) {
        return false;
    }
    ++reset_async_assertions_;
    for (unsigned elapsed = 0; elapsed < 2; ++elapsed) {
        tick(false);
        if (!require_reset(true, "dft-functional-hold", elapsed + 1)) {
            return false;
        }
    }
    dut_.dft.reset_lgc_rst_n.set(std::uint64_t{1});
    if (!require_synchronous_release(
            sync_stages, "dft-functional-release")) {
        return false;
    }
    ++reset_dft_pulses_;

    // Scan mode bypasses the synchronizer output and directly projects
    // the active-low logic reset pin.
    dut_.dft.reset_mode.set(std::uint64_t{0});
    dut_.dft.reset_scan_mode.set(std::uint64_t{1});
    if (!require_reset(false, "scan-idle", 0)) {
        return false;
    }
    dut_.dft.reset_lgc_rst_n.set(std::uint64_t{0});
    if (!require_reset(true, "scan-assert", 0)) {
        return false;
    }
    ++reset_async_assertions_;
    ++reset_scan_transitions_;
    tick(false);
    if (!require_reset(true, "scan-hold", 1)) {
        return false;
    }
    dut_.dft.reset_lgc_rst_n.set(std::uint64_t{1});
    if (!require_reset(false, "scan-release", 0)) {
        return false;
    }
    ++reset_scan_transitions_;

    dut_.dft.reset_scan_mode.set(std::uint64_t{0});
    dut_.dft.reset_mode.set(std::uint64_t{0});
    dut_.dft.reset_lgc_rst_n.set(std::uint64_t{1});
    return reset() && check_idle(2);
}

auto Environment::Impl::reset_functional_pulses() const -> std::uint64_t
{
    return reset_functional_pulses_;
}

auto Environment::Impl::reset_dft_pulses() const -> std::uint64_t
{ return reset_dft_pulses_; }

auto Environment::Impl::reset_scan_transitions() const -> std::uint64_t
{
    return reset_scan_transitions_;
}

auto Environment::Impl::reset_async_assertions() const -> std::uint64_t
{
    return reset_async_assertions_;
}

} // namespace memblock
