#include "implementation.hpp"

namespace memblock {

auto Environment::Impl::check_frontend_bridge_reset_recovery()  -> bool
{
    constexpr std::uint64_t old_icache_address = 0x84001000ULL;
    constexpr std::uint64_t old_instr_address = 0x88001000ULL;
    constexpr std::uint64_t old_ctrl_address = 0x00123464ULL;
    constexpr std::uint64_t old_ctrl_data = 0x1122334455667788ULL;
    constexpr std::uint64_t new_icache_address = 0x84002000ULL;
    constexpr std::uint64_t new_instr_address = 0x88002000ULL;
    constexpr std::uint64_t new_ctrl_address = 0x00124560ULL;
    constexpr std::uint64_t new_ctrl_data = 0x8877665544332211ULL;
    constexpr unsigned stalled_cycles = 4;

    if (!reset()) {
        return false;
    }
    frontend_reset_canceled_requests_ = 0;
    frontend_reset_canceled_responses_ = 0;
    frontend_reset_survivor_requests_ = 0;
    frontend_reset_stall_checks_ = 0;

    const auto drive_request_valid = [&](const std::array<bool, 3> &valid) {
        dut_.frontend.icache.ingress.a.valid.set(valid[0]);
        dut_.frontend.instruction_uncache.ingress.a.valid.set(
            valid[1]);
        dut_.frontend.icache_control.ingress.a.valid.set(
            valid[2]);
    };
    const auto drive_requests = [&](std::uint64_t icache_address,
                                    std::uint64_t instr_address,
                                    std::uint64_t ctrl_address,
                                    std::uint64_t ctrl_data) {
        dut_.frontend.icache.ingress.a.source.set(
            std::uint64_t{3});
        dut_.frontend.icache.ingress.a.address.set(
            icache_address);
        dut_.frontend.instruction_uncache.ingress.a.address.set(
            instr_address);
        dut_.frontend.icache_control.ingress.a.opcode.set(
            std::uint64_t{1});
        dut_.frontend.icache_control.ingress.a.param.set(
            std::uint64_t{0});
        dut_.frontend.icache_control.ingress.a.size.set(
            std::uint64_t{2});
        dut_.frontend.icache_control.ingress.a.source.set(
            std::uint64_t{27});
        dut_.frontend.icache_control.ingress.a.address.set(
            ctrl_address);
        dut_.frontend.icache_control.ingress.a.mask.set(
            std::uint64_t{0xf0});
        dut_.frontend.icache_control.ingress.a.data.set(
            ctrl_data);
        dut_.frontend.icache_control.ingress.a.corrupt.set(
            std::uint64_t{0});
    };
    const auto request_input_fires = [&]() {
        return std::array<bool, 3>{
            dut_.frontend.icache.ingress.a.ready.boolean(),
            dut_.frontend.instruction_uncache.ingress.a.ready.boolean(),
            dut_.frontend.icache_control.ingress.a.ready.boolean(),
        };
    };
    const auto request_output_valids = [&]() {
        return std::array<bool, 3>{
            dut_.frontend.icache.egress.a.valid.boolean(),
            dut_.frontend.instruction_uncache.egress.a.valid.boolean(),
            dut_.frontend.icache_control.egress.a.valid.boolean(),
        };
    };
    const auto check_request_payload = [&](std::uint64_t icache_address,
                                           std::uint64_t instr_address,
                                           std::uint64_t ctrl_address,
                                           std::uint64_t ctrl_data,
                                           const char *phase,
                                           bool require_all_valid) {
        const auto valid = request_output_valids();
        const bool icache_mismatch = valid[0] &&
            (dut_.frontend.icache.egress.a.opcode.value() != 4 ||
             dut_.frontend.icache.egress.a.size.value() != 6 ||
             dut_.frontend.icache.egress.a.source.value() != 3 ||
             dut_.frontend.icache.egress.a.address.value() !=
                 icache_address);
        const bool instr_mismatch = valid[1] &&
            dut_.frontend.instruction_uncache.egress.a.address.value() !=
                instr_address;
        const bool ctrl_mismatch = valid[2] &&
            (dut_.frontend.icache_control.egress.a.opcode.value() != 1 ||
             dut_.frontend.icache_control.egress.a.size.value() != 2 ||
             dut_.frontend.icache_control.egress.a.source.value() != 27 ||
             dut_.frontend.icache_control.egress.a.address.value() !=
                 ctrl_address ||
             dut_.frontend.icache_control.egress.a.mask.value() !=
                 0xf0 ||
             dut_.frontend.icache_control.egress.a.data.value() !=
                 ctrl_data);
        if ((require_all_valid &&
             (!valid[0] || !valid[1] || !valid[2])) ||
            icache_mismatch || instr_mismatch || ctrl_mismatch) {
            std::ostringstream message;
            message << "frontend request buffer mismatch phase=" << phase
                    << " valid=" << valid[0] << valid[1] << valid[2];
            error_.record(message.str());
            return false;
        }
        return true;
    };
    const auto accept_all_requests = [&](std::array<bool, 3> &accepted,
                                         const char *phase) {
        for (unsigned elapsed = 0; elapsed < 16; ++elapsed) {
            const std::array<bool, 3> valid{
                !accepted[0], !accepted[1], !accepted[2]};
            drive_request_valid(valid);
            dut_.refresh_comb();
            const auto ready = request_input_fires();
            const std::array<bool, 3> fire{
                valid[0] && ready[0], valid[1] && ready[1],
                valid[2] && ready[2]};
            tick(false);
            for (unsigned path = 0; path < accepted.size(); ++path) {
                accepted[path] = accepted[path] || fire[path];
            }
            if (accepted[0] && accepted[1] && accepted[2]) {
                drive_request_valid({false, false, false});
                return true;
            }
        }
        std::ostringstream message;
        message << "timed out accepting frontend requests phase=" << phase
                << " accepted=" << accepted[0] << accepted[1]
                << accepted[2];
        error_.record(message.str());
        return false;
    };
    const auto require_no_buffered_traffic = [&](const char *phase,
                                                 unsigned cycles) {
        for (unsigned elapsed = 0; elapsed < cycles; ++elapsed) {
            dut_.refresh_comb();
            const auto request_valid = request_output_valids();
            if (request_valid[0] || request_valid[1] || request_valid[2] ||
                dut_.frontend.icache.ingress.d.valid.boolean() ||
                dut_.frontend.instruction_uncache.ingress.d.valid.boolean() ||
                dut_.frontend.icache_control.ingress.d.valid.boolean()) {
                std::ostringstream message;
                message << "stale frontend bridge traffic phase=" << phase
                        << " elapsed=" << elapsed;
                error_.record(message.str());
                return false;
            }
            tick(false);
        }
        return true;
    };

    dut_.frontend.icache.egress.a.ready.set(
        std::uint64_t{0});
    dut_.frontend.instruction_uncache.egress.a.ready.set(
        std::uint64_t{0});
    dut_.frontend.icache_control.egress.a.ready.set(
        std::uint64_t{0});
    drive_requests(old_icache_address, old_instr_address,
                   old_ctrl_address, old_ctrl_data);
    std::array<bool, 3> accepted{};
    if (!accept_all_requests(accepted, "pre-reset-request")) {
        return false;
    }
    bool all_requests_buffered = false;
    for (unsigned elapsed = 0; elapsed < 8; ++elapsed) {
        dut_.refresh_comb();
        const auto valid = request_output_valids();
        if (valid[0] && valid[1] && valid[2]) {
            all_requests_buffered = true;
            break;
        }
        tick(false);
    }
    if (!all_requests_buffered) {
        error_.record("timed out advancing accepted frontend requests to stalled outputs");
        return false;
    }
    for (unsigned elapsed = 0; elapsed < stalled_cycles; ++elapsed) {
        dut_.refresh_comb();
        if (!check_request_payload(
                old_icache_address, old_instr_address, old_ctrl_address,
                old_ctrl_data, "pre-reset-stall", true)) {
            return false;
        }
        ++frontend_reset_stall_checks_;
        tick(false);
    }
    if (!reset() ||
        !require_no_buffered_traffic("request-reset", stalled_cycles)) {
        return false;
    }
    frontend_reset_canceled_requests_ = 3;

    dut_.frontend.icache_control.ingress.d.ready.set(
        std::uint64_t{0});
    dut_.frontend.icache_control.egress.d.opcode.set(
        std::uint64_t{1});
    dut_.frontend.icache_control.egress.d.size.set(
        std::uint64_t{2});
    dut_.frontend.icache_control.egress.d.source.set(
        std::uint64_t{27});
    dut_.frontend.icache_control.egress.d.data.set(
        std::uint64_t{0xa5a55a5adeadbeefULL});
    dut_.frontend.icache_control.egress.d.valid.set(
        std::uint64_t{1});
    bool response_accepted = false;
    for (unsigned elapsed = 0; elapsed < 16 && !response_accepted;
         ++elapsed) {
        dut_.refresh_comb();
        response_accepted =
            dut_.frontend.icache_control.egress.d.ready.boolean();
        tick(false);
    }
    dut_.frontend.icache_control.egress.d.valid.set(
        std::uint64_t{0});
    if (!response_accepted) {
        error_.record("timed out buffering ICache-control response");
        return false;
    }
    bool response_buffered = false;
    for (unsigned elapsed = 0; elapsed < 8; ++elapsed) {
        dut_.refresh_comb();
        if (dut_.frontend.icache_control.ingress.d.valid.boolean()) {
            response_buffered = true;
            break;
        }
        tick(false);
    }
    if (!response_buffered) {
        error_.record("timed out advancing ICache-control response to stalled output");
        return false;
    }
    for (unsigned elapsed = 0; elapsed < stalled_cycles; ++elapsed) {
        dut_.refresh_comb();
        if (!dut_.frontend.icache_control.ingress.d.valid.boolean() ||
            dut_.frontend.icache_control.ingress.d.opcode.value() != 1 ||
            dut_.frontend.icache_control.ingress.d.size.value() != 2 ||
            dut_.frontend.icache_control.ingress.d.source.value() != 27 ||
            dut_.frontend.icache_control.ingress.d.data.value() !=
                0xa5a55a5adeadbeefULL) {
            error_.record("ICache-control response changed while stalled before reset");
            return false;
        }
        ++frontend_reset_stall_checks_;
        tick(false);
    }
    if (!reset() ||
        !require_no_buffered_traffic("response-reset", stalled_cycles)) {
        return false;
    }
    frontend_reset_canceled_responses_ = 1;

    dut_.frontend.icache.egress.a.ready.set(
        std::uint64_t{1});
    dut_.frontend.instruction_uncache.egress.a.ready.set(
        std::uint64_t{1});
    dut_.frontend.icache_control.egress.a.ready.set(
        std::uint64_t{1});
    dut_.frontend.icache_control.ingress.d.ready.set(
        std::uint64_t{1});
    drive_requests(new_icache_address, new_instr_address,
                   new_ctrl_address, new_ctrl_data);
    std::array<bool, 3> survivor_accepted{};
    std::array<bool, 3> survivor_emerged{};
    for (unsigned elapsed = 0; elapsed < 32; ++elapsed) {
        const std::array<bool, 3> input_valid{
            !survivor_accepted[0], !survivor_accepted[1],
            !survivor_accepted[2]};
        drive_request_valid(input_valid);
        dut_.refresh_comb();
        const auto input_ready = request_input_fires();
        const auto output_valid = request_output_valids();
        if ((output_valid[0] || output_valid[1] || output_valid[2]) &&
            !check_request_payload(
                new_icache_address, new_instr_address, new_ctrl_address,
                new_ctrl_data, "post-reset-survivor", false)) {
            return false;
        }
        const std::array<bool, 3> input_fire{
            input_valid[0] && input_ready[0],
            input_valid[1] && input_ready[1],
            input_valid[2] && input_ready[2]};
        tick(false);
        for (unsigned path = 0; path < survivor_accepted.size(); ++path) {
            survivor_accepted[path] =
                survivor_accepted[path] || input_fire[path];
            if (output_valid[path]) {
                if (survivor_emerged[path]) {
                    error_.record("duplicate post-reset frontend request");
                    return false;
                }
                survivor_emerged[path] = true;
                ++frontend_reset_survivor_requests_;
            }
        }
        if (survivor_emerged[0] && survivor_emerged[1] &&
            survivor_emerged[2]) {
            drive_request_valid({false, false, false});
            break;
        }
    }
    if (frontend_reset_survivor_requests_ != 3) {
        error_.record("timed out draining post-reset frontend requests");
        return false;
    }
    return reset() && check_idle(2);
}

auto Environment::Impl::frontend_reset_canceled_requests() const -> std::uint64_t
{
    return frontend_reset_canceled_requests_;
}

auto Environment::Impl::frontend_reset_canceled_responses() const -> std::uint64_t
{
    return frontend_reset_canceled_responses_;
}

auto Environment::Impl::frontend_reset_survivor_requests() const -> std::uint64_t
{
    return frontend_reset_survivor_requests_;
}

auto Environment::Impl::frontend_reset_stall_checks() const -> std::uint64_t
{
    return frontend_reset_stall_checks_;
}

auto Environment::Impl::exercise_frontend_bridges(unsigned transaction_count,
    std::uint64_t seed)  -> bool
{
    if (transaction_count < 32) {
        error_.record("frontend bridge coverage requires at least 32 transactions");
        return false;
    }
    frontend_bridge_stats_ = {};

    auto mix64 = [](std::uint64_t value) -> std::uint64_t {
        value += 0x9e3779b97f4a7c15ULL;
        value = (value ^ (value >> 30)) * 0xbf58476d1ce4e5b9ULL;
        value = (value ^ (value >> 27)) * 0x94d049bb133111ebULL;
        return value ^ (value >> 31);
    };
    auto pattern_u64 = [&](std::uint64_t domain,
                           std::uint64_t index) -> std::uint64_t {
        return mix64(seed ^ domain ^
                     (index * 0xd6e8feb86659fd93ULL));
    };
    auto pattern_bytes = [&](std::uint64_t domain, std::uint64_t index,
                             std::size_t count) {
        std::vector<unsigned char> result(count);
        for (std::size_t byte = 0; byte < count; ++byte) {
            const std::uint64_t word = pattern_u64(
                domain + byte / sizeof(std::uint64_t), index);
            result[byte] = static_cast<unsigned char>(
                word >> (8 * (byte % sizeof(std::uint64_t))));
        }
        return result;
    };
    auto fail_value = [&](const char *path, std::uint64_t index,
                          const char *field, std::uint64_t expected,
                          std::uint64_t actual) {
        std::ostringstream message;
        message << "frontend bridge mismatch path=" << path
                << " index=" << index << " field=" << field
                << " expected=0x" << std::hex << expected
                << " actual=0x" << actual;
        error_.record(message.str());
        return false;
    };
    auto check_value = [&](const char *path, std::uint64_t index,
                           const char *field, std::uint64_t expected,
                           std::uint64_t actual) {
        if (expected != actual) {
            return fail_value(path, index, field, expected, actual);
        }
        ++frontend_bridge_stats_.field_checks;
        return true;
    };
    auto check_bytes = [&](const char *path, std::uint64_t index,
                           const char *field,
                           const std::vector<unsigned char> &expected,
                           const std::vector<unsigned char> &actual) {
        if (expected != actual) {
            std::ostringstream message;
            message << "frontend bridge mismatch path=" << path
                    << " index=" << index << " field=" << field;
            error_.record(message.str());
            return false;
        }
        ++frontend_bridge_stats_.field_checks;
        return true;
    };
    auto check_index = [&](const char *path, std::uint64_t index,
                           std::uint64_t limit) {
        if (index < limit) {
            return true;
        }
        std::ostringstream message;
        message << "unexpected extra frontend bridge transfer path=" << path
                << " index=" << index << " limit=" << limit;
        error_.record(message.str());
        return false;
    };
    auto icache_address = [](std::uint64_t index) -> std::uint64_t {
        return 0x84000000ULL + index * 64;
    };
    auto instr_address = [](std::uint64_t index) -> std::uint64_t {
        return 0x88000000ULL + index * 8;
    };
    auto ctrl_opcode = [](std::uint64_t index) {
        constexpr std::array<std::uint8_t, 3> opcodes{0, 1, 4};
        return opcodes[index % opcodes.size()];
    };
    auto ctrl_size = [](std::uint64_t index) {
        return static_cast<unsigned>(index % 4);
    };
    auto ctrl_address = [&](std::uint64_t index) -> std::uint64_t {
        const unsigned bytes = 1U << ctrl_size(index);
        const unsigned lane = static_cast<unsigned>(
            ((index * 3U) & 7U) / bytes * bytes);
        return 0x100000ULL + index * 16 + lane;
    };
    auto ctrl_mask = [&](std::uint64_t index) {
        const unsigned bytes = 1U << ctrl_size(index);
        const unsigned lane = static_cast<unsigned>(ctrl_address(index) & 7U);
        std::uint8_t mask = static_cast<std::uint8_t>(
            ((1U << bytes) - 1U) << lane);
        if (ctrl_opcode(index) == 1 && bytes > 1) {
            const std::uint8_t partial = static_cast<std::uint8_t>(
                pattern_u64(0x6374726c6d61736bULL, index));
            mask &= partial;
            if (mask == 0) {
                mask = static_cast<std::uint8_t>(1U << lane);
            }
        }
        return mask;
    };

    std::uint64_t readiness = seed ^ 0x6a09e667f3bcc909ULL;
    if (readiness == 0) {
        readiness = 1;
    }
    auto next_ready = [&]() {
        readiness ^= readiness << 13;
        readiness ^= readiness >> 7;
        readiness ^= readiness << 17;
        return (readiness & 3U) != 0;
    };

    std::uint64_t icache_input = 0;
    std::uint64_t icache_requests = 0;
    std::uint64_t icache_response_input = 0;
    std::uint64_t icache_responses = 0;
    std::uint64_t instr_input = 0;
    std::uint64_t instr_requests = 0;
    std::uint64_t instr_response_input = 0;
    std::uint64_t instr_responses = 0;
    std::uint64_t ctrl_input = 0;
    std::uint64_t ctrl_requests = 0;
    std::uint64_t ctrl_response_input = 0;
    std::uint64_t ctrl_responses = 0;
    const std::uint64_t icache_response_count =
        std::uint64_t{transaction_count} * 2;
    const std::uint64_t timeout =
        std::uint64_t{transaction_count} * 64 + 4096;

    for (std::uint64_t local_cycle = 0; local_cycle < timeout; ++local_cycle) {
        const bool initial_request_stall = local_cycle < 16;
        const bool icache_a_ready =
            !initial_request_stall && next_ready();
        const bool instr_a_ready =
            !initial_request_stall && next_ready();
        const bool ctrl_a_ready =
            !initial_request_stall && next_ready();
        const bool ctrl_d_ready = local_cycle >= 48 && next_ready();

        const std::uint64_t icache_completed_requests =
            icache_responses / 2;
        frontend_bridge_stats_.source_credit_stalls +=
            (icache_input < transaction_count &&
             icache_input - icache_completed_requests >= 16) +
            (instr_input < transaction_count &&
             instr_input != instr_responses) +
            (ctrl_input < transaction_count &&
             ctrl_input - ctrl_responses >= 32);
        const bool icache_input_valid = icache_input < transaction_count &&
            icache_input - icache_completed_requests < 16;
        dut_.frontend.icache.ingress.a.valid.set(
            icache_input_valid);
        if (icache_input_valid) {
            dut_.frontend.icache.ingress.a.source.set(
                icache_input & 0xfU);
            dut_.frontend.icache.ingress.a.address.set(
                icache_address(icache_input));
        }
        dut_.frontend.icache.egress.a.ready.set(
            icache_a_ready);

        const bool instr_input_valid = instr_input < transaction_count &&
            instr_input == instr_responses;
        dut_.frontend.instruction_uncache.ingress.a.valid.set(
            instr_input_valid);
        if (instr_input_valid) {
            dut_.frontend.instruction_uncache.ingress.a.address.set(
                instr_address(instr_input));
        }
        dut_.frontend.instruction_uncache.egress.a.ready.set(
            instr_a_ready);

        const bool ctrl_input_valid = ctrl_input < transaction_count &&
            ctrl_input - ctrl_responses < 32;
        dut_.frontend.icache_control.ingress.a.valid.set(
            ctrl_input_valid);
        if (ctrl_input_valid) {
            dut_.frontend.icache_control.ingress.a.opcode.set(
                ctrl_opcode(ctrl_input));
            dut_.frontend.icache_control.ingress.a.param.set(
                std::uint64_t{0});
            dut_.frontend.icache_control.ingress.a.size.set(
                ctrl_size(ctrl_input));
            dut_.frontend.icache_control.ingress.a.source.set(
                ctrl_input & 0x1fU);
            dut_.frontend.icache_control.ingress.a.address.set(
                ctrl_address(ctrl_input));
            dut_.frontend.icache_control.ingress.a.mask.set(
                ctrl_mask(ctrl_input));
            dut_.frontend.icache_control.ingress.a.data.set(
                pattern_u64(0x6374726c64617461ULL, ctrl_input));
            dut_.frontend.icache_control.ingress.a.corrupt.set(
                std::uint64_t{0});
        }
        dut_.frontend.icache_control.egress.a.ready.set(
            ctrl_a_ready);
        dut_.frontend.icache_control.ingress.d.ready.set(
            ctrl_d_ready);

        const bool icache_d_valid =
            icache_response_input < icache_requests * 2;
        dut_.frontend.icache.egress.d.valid.set(
            icache_d_valid);
        if (icache_d_valid) {
            const std::uint64_t request = icache_response_input / 2;
            dut_.frontend.icache.egress.d.opcode.set(
                std::uint64_t{1});
            dut_.frontend.icache.egress.d.param.set(
                std::uint64_t{0});
            dut_.frontend.icache.egress.d.size.set(
                std::uint64_t{6});
            dut_.frontend.icache.egress.d.source.set(
                request & 0xfU);
            dut_.frontend.icache.egress.d.sink.set(
                std::uint64_t{0});
            dut_.frontend.icache.egress.d.denied.set(
                std::uint64_t{0});
            auto data = pattern_bytes(
                0x6963616368656461ULL, icache_response_input, 32);
            dut_.frontend.icache.egress.d.data.set_bytes(
                data);
            dut_.frontend.icache.egress.d.corrupt.set(
                (icache_response_input % 11U) == 0);
        }

        const bool instr_d_valid = instr_response_input < instr_requests;
        dut_.frontend.instruction_uncache.egress.d.valid.set(
            instr_d_valid);
        if (instr_d_valid) {
            dut_.frontend.instruction_uncache.egress.d.opcode.set(
                std::uint64_t{1});
            dut_.frontend.instruction_uncache.egress.d.param.set(
                std::uint64_t{0});
            dut_.frontend.instruction_uncache.egress.d.size.set(
                std::uint64_t{3});
            dut_.frontend.instruction_uncache.egress.d.source.set(
                std::uint64_t{0});
            dut_.frontend.instruction_uncache.egress.d.sink.set(
                std::uint64_t{0});
            dut_.frontend.instruction_uncache.egress.d.denied.set(
                std::uint64_t{0});
            dut_.frontend.instruction_uncache.egress.d.data.set(
                pattern_u64(0x696e737472646174ULL, instr_response_input));
            dut_.frontend.instruction_uncache.egress.d.corrupt.set(
                (instr_response_input % 13U) == 0);
        }

        const bool ctrl_d_valid = ctrl_response_input < ctrl_requests;
        dut_.frontend.icache_control.egress.d.valid.set(
            ctrl_d_valid);
        if (ctrl_d_valid) {
            dut_.frontend.icache_control.egress.d.opcode.set(
                ctrl_opcode(ctrl_response_input) == 4 ? 1 : 0);
            dut_.frontend.icache_control.egress.d.size.set(
                ctrl_size(ctrl_response_input));
            dut_.frontend.icache_control.egress.d.source.set(
                ctrl_response_input & 0x1fU);
            dut_.frontend.icache_control.egress.d.data.set(
                pattern_u64(0x6374726c72657370ULL, ctrl_response_input));
        }

        dut_.refresh_comb();

        if (dut_.frontend.icache.egress.a.valid.boolean()) {
            if (!check_index("icache-a", icache_requests, transaction_count) ||
                !check_value("icache-a", icache_requests, "opcode", 4,
                             dut_.frontend.icache.egress.a.opcode.value()) ||
                !check_value("icache-a", icache_requests, "param", 0,
                             dut_.frontend.icache.egress.a.param.value()) ||
                !check_value("icache-a", icache_requests, "size", 6,
                             dut_.frontend.icache.egress.a.size.value()) ||
                !check_value("icache-a", icache_requests, "source",
                             icache_requests & 0xfU,
                             dut_.frontend.icache.egress.a.source.value()) ||
                !check_value("icache-a", icache_requests, "address",
                             icache_address(icache_requests),
                             dut_.frontend.icache.egress.a.address.value()) ||
                !check_value("icache-a", icache_requests, "alias", 0,
                             dut_.frontend.icache.egress.a.user_alias.value()) ||
                !check_value("icache-a", icache_requests, "reqSource", 1,
                             dut_.frontend.icache.egress.a.user_req_source.value()) ||
                !check_value("icache-a", icache_requests, "needHint", 0,
                             dut_.frontend.icache.egress.a.user_need_hint.value()) ||
                !check_value("icache-a", icache_requests, "mask", 0xffffffffU,
                             dut_.frontend.icache.egress.a.mask.value()) ||
                !check_bytes("icache-a", icache_requests, "data",
                             std::vector<unsigned char>(32, 0),
                             dut_.frontend.icache.egress.a.data.bytes()) ||
                !check_value("icache-a", icache_requests, "corrupt", 0,
                             dut_.frontend.icache.egress.a.corrupt.value())) {
                return false;
            }
        }
        if (dut_.frontend.instruction_uncache.egress.a.valid.boolean()) {
            if (!check_index("instr-a", instr_requests, transaction_count) ||
                !check_value("instr-a", instr_requests, "param", 0,
                             dut_.frontend.instruction_uncache.egress.a.param.value()) ||
                !check_value("instr-a", instr_requests, "address",
                             instr_address(instr_requests),
                             dut_.frontend.instruction_uncache.egress.a.address.value()) ||
                !check_value("instr-a", instr_requests, "corrupt", 0,
                             dut_.frontend.instruction_uncache.egress.a.corrupt.value())) {
                return false;
            }
        }
        if (dut_.frontend.icache_control.egress.a.valid.boolean()) {
            if (!check_index("ctrl-a", ctrl_requests, transaction_count) ||
                !check_value("ctrl-a", ctrl_requests, "opcode",
                             ctrl_opcode(ctrl_requests),
                             dut_.frontend.icache_control.egress.a.opcode.value()) ||
                !check_value("ctrl-a", ctrl_requests, "size",
                             ctrl_size(ctrl_requests),
                             dut_.frontend.icache_control.egress.a.size.value()) ||
                !check_value("ctrl-a", ctrl_requests, "source",
                             ctrl_requests & 0x1fU,
                             dut_.frontend.icache_control.egress.a.source.value()) ||
                !check_value("ctrl-a", ctrl_requests, "address",
                             ctrl_address(ctrl_requests),
                             dut_.frontend.icache_control.egress.a.address.value()) ||
                !check_value("ctrl-a", ctrl_requests, "mask",
                             ctrl_mask(ctrl_requests),
                             dut_.frontend.icache_control.egress.a.mask.value()) ||
                !check_value("ctrl-a", ctrl_requests, "data",
                             pattern_u64(0x6374726c64617461ULL, ctrl_requests),
                             dut_.frontend.icache_control.egress.a.data.value())) {
                return false;
            }
        }

        if (dut_.frontend.icache.ingress.d.valid.boolean()) {
            if (!check_index(
                    "icache-d", icache_responses, icache_response_count) ||
                !check_value("icache-d", icache_responses, "opcode", 1,
                             dut_.frontend.icache.ingress.d.opcode.value()) ||
                !check_value("icache-d", icache_responses, "source",
                             (icache_responses / 2) & 0xfU,
                             dut_.frontend.icache.ingress.d.source.value()) ||
                !check_bytes("icache-d", icache_responses, "data",
                             pattern_bytes(
                                 0x6963616368656461ULL, icache_responses, 32),
                             dut_.frontend.icache.ingress.d.data.bytes()) ||
                !check_value("icache-d", icache_responses, "corrupt",
                             (icache_responses % 11U) == 0,
                             dut_.frontend.icache.ingress.d.corrupt.value())) {
                return false;
            }
        }
        if (dut_.frontend.instruction_uncache.ingress.d.valid.boolean()) {
            if (!check_index("instr-d", instr_responses, transaction_count) ||
                !check_value("instr-d", instr_responses, "source", 0,
                             dut_.frontend.instruction_uncache.ingress.d.source.value()) ||
                !check_value("instr-d", instr_responses, "data",
                             pattern_u64(0x696e737472646174ULL, instr_responses),
                             dut_.frontend.instruction_uncache.ingress.d.data.value()) ||
                !check_value("instr-d", instr_responses, "corrupt",
                             (instr_responses % 13U) == 0,
                             dut_.frontend.instruction_uncache.ingress.d.corrupt.value())) {
                return false;
            }
        }
        if (dut_.frontend.icache_control.ingress.d.valid.boolean()) {
            if (!check_index("ctrl-d", ctrl_responses, transaction_count) ||
                !check_value("ctrl-d", ctrl_responses, "opcode",
                             ctrl_opcode(ctrl_responses) == 4 ? 1 : 0,
                             dut_.frontend.icache_control.ingress.d.opcode.value()) ||
                !check_value("ctrl-d", ctrl_responses, "param", 0,
                             dut_.frontend.icache_control.ingress.d.param.value()) ||
                !check_value("ctrl-d", ctrl_responses, "size",
                             ctrl_size(ctrl_responses),
                             dut_.frontend.icache_control.ingress.d.size.value()) ||
                !check_value("ctrl-d", ctrl_responses, "source",
                             ctrl_responses & 0x1fU,
                             dut_.frontend.icache_control.ingress.d.source.value()) ||
                !check_value("ctrl-d", ctrl_responses, "sink", 0,
                             dut_.frontend.icache_control.ingress.d.sink.value()) ||
                !check_value("ctrl-d", ctrl_responses, "denied", 0,
                             dut_.frontend.icache_control.ingress.d.denied.value()) ||
                !check_value("ctrl-d", ctrl_responses, "data",
                             pattern_u64(0x6374726c72657370ULL, ctrl_responses),
                             dut_.frontend.icache_control.ingress.d.data.value()) ||
                !check_value("ctrl-d", ctrl_responses, "corrupt", 0,
                             dut_.frontend.icache_control.ingress.d.corrupt.value())) {
                return false;
            }
        }

        const bool icache_input_fire = icache_input_valid &&
            dut_.frontend.icache.ingress.a.ready.boolean();
        const bool instr_input_fire = instr_input_valid &&
            dut_.frontend.instruction_uncache.ingress.a.ready.boolean();
        const bool ctrl_input_fire = ctrl_input_valid &&
            dut_.frontend.icache_control.ingress.a.ready.boolean();
        const bool icache_request_fire =
            dut_.frontend.icache.egress.a.valid.boolean() &&
            icache_a_ready;
        const bool instr_request_fire =
            dut_.frontend.instruction_uncache.egress.a.valid.boolean() &&
            instr_a_ready;
        const bool ctrl_request_fire =
            dut_.frontend.icache_control.egress.a.valid.boolean() &&
            ctrl_a_ready;
        const bool icache_response_input_fire = icache_d_valid &&
            dut_.frontend.icache.egress.d.ready.boolean();
        const bool instr_response_input_fire = instr_d_valid &&
            dut_.frontend.instruction_uncache.egress.d.ready.boolean();
        const bool ctrl_response_input_fire = ctrl_d_valid &&
            dut_.frontend.icache_control.egress.d.ready.boolean();
        const bool icache_response_fire =
            dut_.frontend.icache.ingress.d.valid.boolean();
        const bool instr_response_fire =
            dut_.frontend.instruction_uncache.ingress.d.valid.boolean();
        const bool ctrl_response_fire =
            dut_.frontend.icache_control.ingress.d.valid.boolean() &&
            ctrl_d_ready;

        frontend_bridge_stats_.request_stalls +=
            (icache_input_valid &&
             !dut_.frontend.icache.ingress.a.ready.boolean()) +
            (instr_input_valid &&
             !dut_.frontend.instruction_uncache.ingress.a.ready.boolean()) +
            (ctrl_input_valid &&
             !dut_.frontend.icache_control.ingress.a.ready.boolean()) +
            (dut_.frontend.icache.egress.a.valid.boolean() &&
             !icache_a_ready) +
            (dut_.frontend.instruction_uncache.egress.a.valid.boolean() &&
             !instr_a_ready) +
            (dut_.frontend.icache_control.egress.a.valid.boolean() &&
             !ctrl_a_ready);
        frontend_bridge_stats_.response_stalls +=
            (icache_d_valid &&
             !dut_.frontend.icache.egress.d.ready.boolean()) +
            (instr_d_valid &&
             !dut_.frontend.instruction_uncache.egress.d.ready.boolean()) +
            (ctrl_d_valid &&
             !dut_.frontend.icache_control.egress.d.ready.boolean()) +
            (dut_.frontend.icache_control.ingress.d.valid.boolean() &&
             !ctrl_d_ready);

        tick(false);
        icache_input += icache_input_fire;
        instr_input += instr_input_fire;
        ctrl_input += ctrl_input_fire;
        icache_requests += icache_request_fire;
        instr_requests += instr_request_fire;
        ctrl_requests += ctrl_request_fire;
        icache_response_input += icache_response_input_fire;
        instr_response_input += instr_response_input_fire;
        ctrl_response_input += ctrl_response_input_fire;
        icache_responses += icache_response_fire;
        instr_responses += instr_response_fire;
        ctrl_responses += ctrl_response_fire;

        if (icache_input == transaction_count &&
            icache_requests == transaction_count &&
            icache_response_input == icache_response_count &&
            icache_responses == icache_response_count &&
            instr_input == transaction_count &&
            instr_requests == transaction_count &&
            instr_response_input == transaction_count &&
            instr_responses == transaction_count &&
            ctrl_input == transaction_count &&
            ctrl_requests == transaction_count &&
            ctrl_response_input == transaction_count &&
            ctrl_responses == transaction_count) {
            frontend_bridge_stats_.requests =
                icache_requests + instr_requests + ctrl_requests;
            frontend_bridge_stats_.responses =
                icache_responses + instr_responses + ctrl_responses;
            if (frontend_bridge_stats_.request_stalls == 0 ||
                frontend_bridge_stats_.response_stalls == 0 ||
                frontend_bridge_stats_.source_credit_stalls == 0) {
                error_.record("frontend bridge run missed a required stall class");
                return false;
            }
            dut_.frontend.icache.ingress.a.valid.set(
                std::uint64_t{0});
            dut_.frontend.instruction_uncache.ingress.a.valid.set(
                std::uint64_t{0});
            dut_.frontend.icache_control.ingress.a.valid.set(
                std::uint64_t{0});
            dut_.frontend.icache.egress.d.valid.set(
                std::uint64_t{0});
            dut_.frontend.instruction_uncache.egress.d.valid.set(
                std::uint64_t{0});
            dut_.frontend.icache_control.egress.d.valid.set(
                std::uint64_t{0});
            dut_.frontend.icache.egress.a.ready.set(
                std::uint64_t{1});
            dut_.frontend.instruction_uncache.egress.a.ready.set(
                std::uint64_t{1});
            dut_.frontend.icache_control.egress.a.ready.set(
                std::uint64_t{1});
            dut_.frontend.icache_control.ingress.d.ready.set(
                std::uint64_t{1});
            return check_idle(8);
        }
    }

    std::ostringstream message;
    message << "timed out draining frontend bridges"
            << " icache=" << icache_input << '/' << icache_requests
            << '/' << icache_response_input << '/' << icache_responses
            << " instr=" << instr_input << '/' << instr_requests
            << '/' << instr_response_input << '/' << instr_responses
            << " ctrl=" << ctrl_input << '/' << ctrl_requests
            << '/' << ctrl_response_input << '/' << ctrl_responses;
    error_.record(message.str());
    return false;
}

} // namespace memblock
