#include "memblock_env.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <iostream>
#include <numeric>
#include <optional>
#include <random>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace {

struct Options {
    std::string_view test = "single-load";
    std::uint64_t seed = 1;
    unsigned transactions = 200;
    bool backpressure = true;
    bool hunt_boundaries = false;
};

std::uint64_t parse_u64(std::string_view text, const char *option)
{
    std::size_t consumed = 0;
    const std::string copy(text);
    const std::uint64_t value = std::stoull(copy, &consumed, 0);
    if (consumed != copy.size()) {
        throw std::invalid_argument(std::string("invalid value for ") + option);
    }
    return value;
}

Options parse_options(int argc, char **argv)
{
    Options options;
    for (int index = 1; index < argc; ++index) {
        const std::string_view argument(argv[index]);
        if (argument == "--test" && index + 1 < argc) {
            options.test = argv[++index];
        } else if (argument == "--seed" && index + 1 < argc) {
            options.seed = parse_u64(argv[++index], "--seed");
        } else if (argument == "--transactions" && index + 1 < argc) {
            options.transactions = static_cast<unsigned>(
                parse_u64(argv[++index], "--transactions"));
        } else if (argument == "--no-backpressure") {
            options.backpressure = false;
        } else if (argument == "--hunt-boundaries") {
            options.hunt_boundaries = true;
        }
    }
    return options;
}

struct LoadCoverage {
    std::array<std::uint64_t, 7> operations{};
    std::array<std::uint64_t, memblock::kScalarLoadLanes> lanes{};
    std::uint64_t cache_hits = 0;
    std::uint64_t cache_misses = 0;

    void sample(
        const memblock::LoadTransaction &transaction,
        std::uint64_t requests_before,
        std::uint64_t requests_after)
    {
        ++operations.at(static_cast<unsigned>(transaction.op));
        ++lanes.at(transaction.lane);
        if (requests_after == requests_before) {
            ++cache_hits;
        } else {
            ++cache_misses;
        }
    }

    bool complete() const
    {
        for (const auto count : operations) {
            if (count == 0) {
                return false;
            }
        }
        for (const auto count : lanes) {
            if (count == 0) {
                return false;
            }
        }
        return cache_hits != 0 && cache_misses != 0;
    }

    std::string summary() const
    {
        std::string result = "ops=";
        for (std::size_t index = 0; index < operations.size(); ++index) {
            result += (index == 0 ? "" : ",") + std::to_string(operations[index]);
        }
        result += " lanes=";
        for (std::size_t index = 0; index < lanes.size(); ++index) {
            result += (index == 0 ? "" : ",") + std::to_string(lanes[index]);
        }
        result += " hits=" + std::to_string(cache_hits);
        result += " misses=" + std::to_string(cache_misses);
        return result;
    }
};

struct VectorCoverage {
    std::array<std::uint64_t, 4> eews{};
    std::array<std::uint64_t, memblock::kVectorMemoryLanes> lanes{};
    std::uint64_t masked = 0;
    std::uint64_t unmasked = 0;
    std::uint64_t zero_vstart = 0;
    std::uint64_t nonzero_vstart = 0;
    std::uint64_t full_vl = 0;
    std::uint64_t partial_vl = 0;
    std::uint64_t aligned = 0;
    std::uint64_t split = 0;
    std::uint64_t active = 0;
    std::uint64_t inactive = 0;
    std::uint64_t cache_hits = 0;
    std::uint64_t cache_misses = 0;

    void sample(
        const memblock::VectorMemoryTransaction &transaction,
        std::uint64_t requests_before,
        std::uint64_t requests_after)
    {
        ++eews.at(transaction.eew);
        ++lanes.at(transaction.lane);
        ++(transaction.vm ? unmasked : masked);
        ++(transaction.vstart == 0 ? zero_vstart : nonzero_vstart);
        const unsigned element_count = 16U >> transaction.eew;
        ++(transaction.vl == element_count ? full_vl : partial_vl);
        ++((transaction.address & 15U) == 0 ? aligned : split);
        ++(memblock::active_vector_elements(transaction) == 0 ? inactive : active);
        ++(requests_after == requests_before ? cache_hits : cache_misses);
    }

    bool complete(bool require_cache_mix = true) const
    {
        return std::all_of(eews.begin(), eews.end(), [](auto count) { return count != 0; }) &&
               std::all_of(lanes.begin(), lanes.end(), [](auto count) { return count != 0; }) &&
               masked != 0 && unmasked != 0 && zero_vstart != 0 &&
               nonzero_vstart != 0 && full_vl != 0 && partial_vl != 0 &&
               aligned != 0 && split != 0 && active != 0 && inactive != 0 &&
               (!require_cache_mix || (cache_hits != 0 && cache_misses != 0));
    }

    std::string summary() const
    {
        return "eews=" + std::to_string(eews[0]) + ',' +
               std::to_string(eews[1]) + ',' + std::to_string(eews[2]) + ',' +
               std::to_string(eews[3]) + " lanes=" + std::to_string(lanes[0]) +
               ',' + std::to_string(lanes[1]) + " masked=" +
               std::to_string(masked) + " unmasked=" + std::to_string(unmasked) +
               " vstart0=" + std::to_string(zero_vstart) + " vstartnz=" +
               std::to_string(nonzero_vstart) + " full=" + std::to_string(full_vl) +
               " partial=" + std::to_string(partial_vl) + " aligned=" +
               std::to_string(aligned) + " split=" + std::to_string(split) +
               " active=" + std::to_string(active) + " inactive=" +
               std::to_string(inactive) + " hits=" + std::to_string(cache_hits) +
               " misses=" + std::to_string(cache_misses);
    }
};

struct MixedCoverage {
    static constexpr unsigned kConcurrentClasses = 5;
    std::array<std::uint64_t, 7> load_ops{};
    std::array<std::uint64_t, 4> store_ops{};
    std::array<std::uint64_t, memblock::kScalarLoadLanes> load_lanes{};
    std::array<std::uint64_t, memblock::kScalarStoreLanes> address_lanes{};
    std::array<std::uint64_t, memblock::kScalarStoreLanes> data_lanes{};
    std::array<std::uint64_t, 4> vector_load_eews{};
    std::array<std::uint64_t, 4> vector_store_eews{};
    std::array<std::uint64_t, 4> vector_load_address_modes{};
    std::array<std::uint64_t, 4> vector_store_address_modes{};
    std::array<std::uint64_t, 3> prefetch_ops{};
    std::array<std::uint64_t, memblock::kVectorMemoryLanes> vector_lanes{};
    std::uint64_t scalar_loads = 0;
    std::uint64_t scalar_stores = 0;
    std::uint64_t vector_loads = 0;
    std::uint64_t vector_stores = 0;
    std::uint64_t address_first = 0;
    std::uint64_t data_first = 0;
    std::uint64_t masked = 0;
    std::uint64_t unmasked = 0;
    std::uint64_t zero_vstart = 0;
    std::uint64_t nonzero_vstart = 0;
    std::uint64_t full_vl = 0;
    std::uint64_t partial_vl = 0;
    std::uint64_t aligned = 0;
    std::uint64_t split = 0;
    std::uint64_t scalar_misaligned = 0;
    std::uint64_t scalar_store_misaligned = 0;
    std::uint64_t vector_store_misaligned = 0;
    std::uint64_t vector_replays = 0;
    std::uint64_t two_stage = 0;
    std::uint64_t exceptions = 0;
    std::uint64_t heterogeneous_waves = 0;
    std::uint64_t simultaneous_scalar_vector = 0;
    std::uint64_t scalar_forwarding = 0;
    std::uint64_t vector_forwarding = 0;
    std::uint64_t scalar_to_vector = 0;
    std::uint64_t vector_to_scalar = 0;
    std::uint64_t cacheable = 0;
    std::uint64_t noncacheable = 0;
    std::uint64_t dcache_hits = 0;
    std::uint64_t dcache_misses = 0;
    std::uint64_t tlb_reuse = 0;
    std::uint64_t redirect_recovery = 0;
    std::uint64_t dirty_pressure = 0;
    std::uint64_t max_outstanding = 0;
    std::array<std::uint64_t, kConcurrentClasses> concurrent_ops{};
    std::uint64_t concurrent_windows = 0;
    std::uint64_t concurrent_actions = 0;
    std::uint64_t unresolved_overlap_samples = 0;
    std::uint64_t max_unresolved = 0;
    std::uint64_t max_unresolved_classes = 0;
    std::uint64_t dcache_request_stalls = 0;
    std::uint64_t dcache_response_delays = 0;
    std::uint64_t ptw_request_stalls = 0;
    std::uint64_t ptw_response_delays = 0;
    std::uint64_t uncache_request_stalls = 0;
    std::uint64_t uncache_response_delays = 0;

    void sample(const memblock::LoadTransaction &transaction)
    {
        ++scalar_loads;
        ++load_ops.at(static_cast<unsigned>(transaction.op));
        ++load_lanes.at(transaction.lane);
        const unsigned size = 1U << (static_cast<unsigned>(transaction.op) & 3U);
        scalar_misaligned += (transaction.address & (size - 1)) != 0;
    }

    void sample(const memblock::StoreTransaction &transaction, bool data_was_first)
    {
        ++scalar_stores;
        ++store_ops.at(static_cast<unsigned>(transaction.op));
        ++address_lanes.at(transaction.address_lane);
        ++data_lanes.at(transaction.data_lane);
        ++(data_was_first ? data_first : address_first);
        const unsigned size = 1U << static_cast<unsigned>(transaction.op);
        scalar_store_misaligned += (transaction.address & (size - 1)) != 0;
    }

    void sample(const memblock::VectorMemoryTransaction &transaction)
    {
        ++(transaction.store ? vector_stores : vector_loads);
        ++(transaction.store ? vector_store_eews : vector_load_eews)
              .at(transaction.eew);
        ++vector_lanes.at(transaction.lane);
        ++(transaction.store
               ? vector_store_address_modes
               : vector_load_address_modes)
              .at(static_cast<unsigned>(transaction.addressing));
        ++(transaction.vm ? unmasked : masked);
        ++(transaction.vstart == 0 ? zero_vstart : nonzero_vstart);
        const unsigned elements = 16U >> transaction.eew;
        ++(transaction.vl == elements ? full_vl : partial_vl);
        ++((transaction.address & 15U) == 0 ? aligned : split);
        if (transaction.store) {
            const unsigned bytes = 1U << transaction.eew;
            const std::uint16_t active = memblock::active_vector_elements(transaction);
            for (unsigned element = 0; element < elements; ++element) {
                if (((active >> element) & 1U) != 0 &&
                    (memblock::vector_element_address(transaction, element) &
                     (bytes - 1)) != 0) {
                    ++vector_store_misaligned;
                    break;
                }
            }
        }
    }

    void sample(const memblock::PrefetchTransaction &transaction)
    {
        const unsigned encoding = static_cast<unsigned>(transaction.op);
        ++prefetch_ops.at(encoding - 8);
    }

    bool complete() const
    {
        const auto all_nonzero = [](const auto &values) {
            return std::all_of(
                values.begin(), values.end(), [](auto value) { return value != 0; });
        };
        return all_nonzero(load_ops) && all_nonzero(store_ops) &&
               all_nonzero(load_lanes) && all_nonzero(address_lanes) &&
               all_nonzero(data_lanes) && all_nonzero(vector_load_eews) &&
               all_nonzero(vector_store_eews) && all_nonzero(prefetch_ops) &&
               all_nonzero(vector_lanes) &&
               all_nonzero(vector_load_address_modes) &&
               all_nonzero(vector_store_address_modes) &&
               scalar_loads != 0 && scalar_stores != 0 && vector_loads != 0 &&
               vector_stores != 0 && address_first != 0 && data_first != 0 &&
               masked != 0 && unmasked != 0 && zero_vstart != 0 &&
               nonzero_vstart != 0 && full_vl != 0 && partial_vl != 0 &&
               aligned != 0 && split != 0 && scalar_misaligned != 0 &&
               scalar_store_misaligned != 0 &&
               vector_store_misaligned != 0 && vector_replays != 0 &&
               two_stage != 0 && exceptions != 0 &&
               heterogeneous_waves >= 2 &&
               simultaneous_scalar_vector != 0 && scalar_forwarding != 0 &&
               vector_forwarding != 0 && scalar_to_vector != 0 &&
               vector_to_scalar != 0 && cacheable != 0 && noncacheable != 0 &&
               dcache_hits != 0 && dcache_misses != 0 && tlb_reuse != 0 &&
               redirect_recovery != 0 && dirty_pressure != 0 &&
               max_outstanding >= 5 && concurrent_windows >= 4 &&
               all_nonzero(concurrent_ops) && concurrent_actions != 0 &&
               unresolved_overlap_samples != 0 && max_unresolved >= 2 &&
               max_unresolved_classes >= 2;
    }

    bool backpressure_complete(bool required) const
    {
        return !required ||
               (dcache_request_stalls != 0 && dcache_response_delays != 0 &&
                ptw_request_stalls != 0 && ptw_response_delays != 0 &&
                uncache_request_stalls != 0 &&
                uncache_response_delays != 0);
    }

    std::string summary() const
    {
        return "load_ops=" + std::to_string(load_ops[0]) + ',' +
               std::to_string(load_ops[1]) + ',' + std::to_string(load_ops[2]) +
               ',' + std::to_string(load_ops[3]) + ',' +
               std::to_string(load_ops[4]) + ',' + std::to_string(load_ops[5]) +
               ',' + std::to_string(load_ops[6]) + " store_ops=" +
               std::to_string(store_ops[0]) + ',' + std::to_string(store_ops[1]) +
               ',' + std::to_string(store_ops[2]) + ',' +
               std::to_string(store_ops[3]) + " scalar=" +
               std::to_string(scalar_loads) + ',' +
               std::to_string(scalar_stores) + " vector=" +
               std::to_string(vector_loads) + ',' +
               std::to_string(vector_stores) + " eew_load=" +
               std::to_string(vector_load_eews[0]) + ',' +
               std::to_string(vector_load_eews[1]) + ',' +
               std::to_string(vector_load_eews[2]) + ',' +
               std::to_string(vector_load_eews[3]) + " eew_store=" +
               std::to_string(vector_store_eews[0]) + ',' +
               std::to_string(vector_store_eews[1]) + ',' +
               std::to_string(vector_store_eews[2]) + ',' +
               std::to_string(vector_store_eews[3]) + " vec_load_modes=" +
               std::to_string(vector_load_address_modes[0]) + ',' +
               std::to_string(vector_load_address_modes[1]) + ',' +
               std::to_string(vector_load_address_modes[2]) + ',' +
               std::to_string(vector_load_address_modes[3]) +
               " vec_store_modes=" +
               std::to_string(vector_store_address_modes[0]) + ',' +
               std::to_string(vector_store_address_modes[1]) + ',' +
               std::to_string(vector_store_address_modes[2]) + ',' +
               std::to_string(vector_store_address_modes[3]) + " prefetch=" +
               std::to_string(prefetch_ops[0]) + ',' +
               std::to_string(prefetch_ops[1]) + ',' +
               std::to_string(prefetch_ops[2]) + " masked=" +
               std::to_string(masked) + " unmasked=" +
               std::to_string(unmasked) + " vstart=" +
               std::to_string(zero_vstart) + ',' +
               std::to_string(nonzero_vstart) + " vl=" +
               std::to_string(full_vl) + ',' + std::to_string(partial_vl) +
               " align=" + std::to_string(aligned) + ',' +
               std::to_string(split) + " scalar_misaligned=" +
               std::to_string(scalar_misaligned) + " store_misaligned=" +
               std::to_string(scalar_store_misaligned) + ',' +
               std::to_string(vector_store_misaligned) + " store_order=" +
               std::to_string(address_first) + ',' +
               std::to_string(data_first) + " vector_replays=" +
               std::to_string(vector_replays) + " virtualization=" +
               std::to_string(two_stage) + " exceptions=" +
               std::to_string(exceptions) + " waves=" +
               std::to_string(heterogeneous_waves) + " coissue=" +
               std::to_string(simultaneous_scalar_vector) + " forwarding=" +
               std::to_string(scalar_forwarding) + ',' +
               std::to_string(vector_forwarding) + ',' +
               std::to_string(scalar_to_vector) + ',' +
               std::to_string(vector_to_scalar) + " memory_types=" +
               std::to_string(cacheable) + ',' + std::to_string(noncacheable) +
               " dcache=" + std::to_string(dcache_hits) + ',' +
               std::to_string(dcache_misses) + " tlb_reuse=" +
               std::to_string(tlb_reuse) + " redirects=" +
               std::to_string(redirect_recovery) + " dirty=" +
               std::to_string(dirty_pressure) + " max_outstanding=" +
               std::to_string(max_outstanding) + " concurrent_ops=" +
               std::to_string(concurrent_ops[0]) + ',' +
               std::to_string(concurrent_ops[1]) + ',' +
               std::to_string(concurrent_ops[2]) + ',' +
               std::to_string(concurrent_ops[3]) + ',' +
               std::to_string(concurrent_ops[4]) + " concurrent=" +
               std::to_string(concurrent_windows) + ',' +
               std::to_string(concurrent_actions) + ',' +
               std::to_string(unresolved_overlap_samples) + ',' +
               std::to_string(max_unresolved) + ',' +
               std::to_string(max_unresolved_classes) + " backpressure=" +
               std::to_string(dcache_request_stalls) + ',' +
               std::to_string(dcache_response_delays) + ',' +
               std::to_string(ptw_request_stalls) + ',' +
               std::to_string(ptw_response_delays) + ',' +
               std::to_string(uncache_request_stalls) + ',' +
               std::to_string(uncache_response_delays);
    }
};

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

int run_single_load(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t line = memblock::kDefaultMemoryBase;
    environment.memory().fill_incrementing(line, 64, 0x80);

    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_SINGLE_LOAD_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction transaction{
        .address = line + 24,
        .op = memblock::LoadOp::ld,
        .rob = 1,
        .lq = 0,
        .sq = 0,
        .pdest = 5,
        .lane = 0,
    };
    environment.expect_load(transaction);
    if (!environment.enqueue_load(transaction) ||
        !environment.issue_load(transaction) ||
        !environment.run_until_complete(512)) {
        std::cerr << "MEMBLOCK_SINGLE_LOAD_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_SINGLE_LOAD_PASS"
              << " cycle=" << environment.cycle()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " writebacks=" << environment.writebacks()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_vector_load(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x20000;
    environment.memory().fill_incrementing(base, 256, 0x41);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_VECTOR_LOAD_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    for (unsigned eew = 0; eew < 4; ++eew) {
        memblock::VectorMemoryTransaction transaction{
            .address = base + eew * 64,
            .eew = static_cast<std::uint8_t>(eew),
            .vl = static_cast<std::uint8_t>(16U >> eew),
            .rob = static_cast<std::uint8_t>(40 + eew),
            .lq = static_cast<std::uint8_t>(eew * 2),
            .pdest = static_cast<std::uint8_t>(80 + eew),
            .lane = eew % memblock::kVectorMemoryLanes,
        };
        for (unsigned byte = 0; byte < transaction.data.size(); ++byte) {
            transaction.data[byte] = static_cast<unsigned char>(0xa0 + byte);
        }
        environment.expect_vector(transaction);
        if (!environment.enqueue_vector(transaction) ||
            !environment.issue_vector(transaction) ||
            !environment.run_until_vector_complete(512)) {
            std::cerr << "MEMBLOCK_VECTOR_LOAD_FAIL"
                      << " width=" << (8U << eew)
                      << " lane=" << transaction.lane
                      << " cycle=" << environment.cycle()
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
    }
    if (!environment.run_until_lq_retired()) {
        std::cerr << "MEMBLOCK_VECTOR_LOAD_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_VECTOR_LOAD_PASS"
              << " cycle=" << environment.cycle()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " writebacks=" << environment.vector_load_writebacks()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_vector_split_load(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t address = memblock::kDefaultMemoryBase + 0x371dc;
    constexpr std::uint64_t inactive_address =
        memblock::kDefaultMemoryBase + 0x3db81;
    environment.memory().fill_incrementing(address & ~std::uint64_t{63}, 64, 0x53);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_VECTOR_SPLIT_LOAD_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    std::array<memblock::VectorMemoryTransaction, 3> transactions{{
        {
            .address = inactive_address,
            .eew = 0,
            .vl = 3,
            .vstart = 1,
            .vm = false,
            .mask_bits = 0,
            .rob = 38,
            .lq = 0,
            .pdest = 78,
            .lane = 1,
        },
        {
            .address = address - 0x14,
            .eew = 3,
            .vl = 0,
            .rob = 39,
            .lq = 2,
            .pdest = 79,
            .lane = 1,
        },
        {
        .address = address,
        .eew = 0,
        .vl = 12,
        .vstart = 1,
        .vm = true,
        .rob = 40,
        .lq = 4,
        .pdest = 80,
        .lane = 0,
        },
    }};
    for (unsigned index = 0; index < transactions.size(); ++index) {
        auto &transaction = transactions[index];
        for (unsigned byte = 0; byte < transaction.data.size(); ++byte) {
            transaction.data[byte] = static_cast<unsigned char>(
                0xa0 + index * 16 + byte);
        }
        environment.expect_vector(transaction);
        if (!environment.enqueue_vector(transaction) ||
            !environment.issue_vector(transaction) ||
            !environment.run_until_vector_complete(1024)) {
            std::cerr << "MEMBLOCK_VECTOR_SPLIT_LOAD_FAIL transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " requests=" << environment.tilelink_requests()
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
    }
    if (!environment.run_until_lq_retired()) {
        std::cerr << "MEMBLOCK_VECTOR_SPLIT_LOAD_FAIL cycle="
                  << environment.cycle() << " reason=" << environment.error()
                  << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_VECTOR_SPLIT_LOAD_PASS"
              << " cycle=" << environment.cycle()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " writebacks=" << environment.vector_load_writebacks()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_vector_store_forwarding(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x20400;
    environment.memory().fill_incrementing(base, 256, 0x19);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_VECTOR_STORE_FORWARD_FAIL cycle="
                  << environment.cycle() << " reason=" << environment.error() << '\n';
        return 1;
    }

    for (unsigned eew = 0; eew < 4; ++eew) {
        memblock::VectorMemoryTransaction store{
            .store = true,
            .address = base + eew * 64,
            .eew = static_cast<std::uint8_t>(eew),
            .vl = static_cast<std::uint8_t>(16U >> eew),
            .rob = static_cast<std::uint8_t>(60 + eew * 2),
            .lq = static_cast<std::uint8_t>(eew * 2),
            .sq = static_cast<std::uint8_t>(eew * 2),
            .lane = eew % memblock::kVectorMemoryLanes,
        };
        for (unsigned byte = 0; byte < store.data.size(); ++byte) {
            store.data[byte] = static_cast<unsigned char>(
                0xd0 + eew * 7 + byte * 3);
        }
        environment.expect_vector(store);
        if (!environment.enqueue_vector(store) || !environment.issue_vector(store) ||
            !environment.run_until_vector_complete(256)) {
            std::cerr << "MEMBLOCK_VECTOR_STORE_FORWARD_FAIL"
                      << " width=" << (8U << eew)
                      << " phase=store cycle=" << environment.cycle()
                      << " reason=" << environment.error() << '\n';
            return 1;
        }

        memblock::VectorMemoryTransaction load{
            .address = store.address,
            .data = {},
            .eew = store.eew,
            .vl = store.vl,
            .rob = static_cast<std::uint8_t>(store.rob + 1),
            .lq = static_cast<std::uint8_t>(eew * 2),
            .sq = static_cast<std::uint8_t>(eew * 2 + 2),
            .pdest = static_cast<std::uint8_t>(100 + eew),
            .lane = (eew + 1) % memblock::kVectorMemoryLanes,
        };
        environment.expect_vector_data(load, store.data);
        if (!environment.enqueue_vector(load) || !environment.issue_vector(load) ||
            !environment.run_until_vector_complete(512)) {
            std::cerr << "MEMBLOCK_VECTOR_STORE_FORWARD_FAIL"
                      << " width=" << (8U << eew)
                      << " phase=load cycle=" << environment.cycle()
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        if (!environment.commit_vector_store(store)) {
            std::cerr << "MEMBLOCK_VECTOR_STORE_FORWARD_FAIL"
                      << " width=" << (8U << eew)
                      << " phase=commit cycle=" << environment.cycle()
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
    }
    if (!environment.run_until_lq_retired()) {
        std::cerr << "MEMBLOCK_VECTOR_STORE_FORWARD_FAIL cycle="
                  << environment.cycle() << " reason=" << environment.error()
                  << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_VECTOR_STORE_FORWARD_PASS"
              << " cycle=" << environment.cycle()
              << " stores=" << environment.vector_store_writebacks()
              << " loads=" << environment.vector_load_writebacks()
              << " sq_dequeued=" << environment.sq_dequeued()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_store_forwarding(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x2000;
    environment.memory().fill_incrementing(base, 64, 0x10);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_STORE_FORWARD_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    constexpr std::array<memblock::StoreOp, 4> store_ops{
        memblock::StoreOp::sb,
        memblock::StoreOp::sh,
        memblock::StoreOp::sw,
        memblock::StoreOp::sd,
    };
    constexpr std::array<memblock::LoadOp, 4> load_ops{
        memblock::LoadOp::lbu,
        memblock::LoadOp::lhu,
        memblock::LoadOp::lwu,
        memblock::LoadOp::ld,
    };
    constexpr std::uint64_t store_data = 0xfedcba9876543281ULL;

    for (unsigned index = 0; index < store_ops.size(); ++index) {
        const unsigned size = 1U << index;
        const std::uint64_t address = base + index * 16 + (8 - size);
        const memblock::StoreTransaction store{
            .address = address,
            .data = store_data,
            .op = store_ops[index],
            .rob = static_cast<std::uint8_t>(index * 2),
            .sq = static_cast<std::uint8_t>(index),
            .address_lane = index % memblock::kScalarStoreLanes,
            .data_lane = (index + 1) % memblock::kScalarStoreLanes,
        };
        environment.expect_store(store);
        const bool issued = index % 2 == 0
            ? environment.enqueue_store(store, static_cast<std::uint8_t>(index)) &&
                  environment.issue_store_data(store) &&
                  environment.issue_store_address(store)
            : environment.enqueue_store(store, static_cast<std::uint8_t>(index)) &&
                  environment.issue_store_address(store) &&
                  environment.issue_store_data(store);
        if (!issued || !environment.run_until_store_complete(128)) {
            std::cerr << "MEMBLOCK_STORE_FORWARD_FAIL transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=store reason=" << environment.error() << '\n';
            return 1;
        }

        const memblock::LoadTransaction load{
            .address = address,
            .op = load_ops[index],
            .rob = static_cast<std::uint8_t>(index * 2 + 1),
            .lq = static_cast<std::uint8_t>(index),
            .sq = static_cast<std::uint8_t>(index + 1),
            .pdest = static_cast<std::uint8_t>(20 + index),
            .lane = index % memblock::kScalarLoadLanes,
        };
        const std::uint64_t expected = size == 8
            ? store_data
            : store_data & ((std::uint64_t{1} << (size * 8)) - 1);
        environment.expect_load_data(load, expected);
        if (!environment.enqueue_load(load) || !environment.issue_load(load) ||
            !environment.run_until_complete(256)) {
            std::cerr << "MEMBLOCK_STORE_FORWARD_FAIL transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=load reason=" << environment.error() << '\n';
            return 1;
        }
    }

    std::cout << "MEMBLOCK_STORE_FORWARD_PASS"
              << " cycle=" << environment.cycle()
              << " stores=" << environment.store_writebacks()
              << " loads=" << environment.writebacks()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_dcache_release(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr unsigned dcache_sets = 128;
    constexpr unsigned dcache_ways = 8;
    constexpr unsigned line_bytes = 64;
    constexpr unsigned line_count = dcache_ways + 2;
    constexpr std::uint64_t same_set_stride = dcache_sets * line_bytes;
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x60000;
    std::array<memblock::StoreTransaction, line_count> stores{};

    for (unsigned index = 0; index < stores.size(); ++index) {
        const std::uint64_t line = base + index * same_set_stride;
        environment.memory().fill_incrementing(
            line, line_bytes, static_cast<std::uint8_t>(0x20 + index * 7));
        stores[index] = memblock::StoreTransaction{
            .address = line + 24,
            .data = 0xc0decafe00000000ULL | index,
            .op = memblock::StoreOp::sd,
            .rob = memblock::rob_pointer_value(index),
            .rob_flag = memblock::rob_pointer_flag(index),
            .sq = memblock::sq_pointer_value(index),
            .sq_flag = memblock::sq_pointer_flag(index),
            .address_lane = index % memblock::kScalarStoreLanes,
            .data_lane = (index + 1) % memblock::kScalarStoreLanes,
        };
    }

    environment.configure_backpressure(0x243f6a8885a308d3ULL, true);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_DCACHE_RELEASE_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    for (unsigned index = 0; index < stores.size(); ++index) {
        const auto &store = stores[index];
        environment.expect_store(store);
        const bool data_first = (index & 1U) == 0;
        const bool issued = environment.enqueue_store(store, 0) &&
            (data_first
                 ? environment.issue_store_data(store) &&
                       environment.issue_store_address(store)
                 : environment.issue_store_address(store) &&
                       environment.issue_store_data(store));
        if (!issued || !environment.run_until_store_complete(256) ||
            !environment.commit_store(store)) {
            std::cerr << "MEMBLOCK_DCACHE_RELEASE_FAIL transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=store reason=" << environment.error() << '\n';
            return 1;
        }
    }

    if (!environment.run_until_release_data() || !environment.run_cycles(32)) {
        std::cerr << "MEMBLOCK_DCACHE_RELEASE_FAIL cycle=" << environment.cycle()
                  << " phase=release reason=" << environment.error() << '\n';
        return 1;
    }

    unsigned preserved_stores = 0;
    for (const auto &store : stores) {
        if (environment.memory().expected_load(store.address, memblock::LoadOp::ld) ==
            store.data) {
            ++preserved_stores;
        }
    }
    if (preserved_stores == 0) {
        std::cerr << "MEMBLOCK_DCACHE_RELEASE_FAIL cycle=" << environment.cycle()
                  << " phase=data reason=ReleaseData did not update sparse memory\n";
        return 1;
    }

    std::cout << "MEMBLOCK_DCACHE_RELEASE_PASS"
              << " cycle=" << environment.cycle()
              << " stores=" << stores.size()
              << " sq_dequeued=" << environment.sq_dequeued()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " releases=" << environment.tilelink_releases()
              << " release_data=" << environment.tilelink_release_data()
              << " preserved_stores=" << preserved_stores
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_store_rdata_order(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t older_base =
        memblock::kDefaultMemoryBase + 0x74000;
    constexpr std::uint64_t younger_base =
        memblock::kDefaultMemoryBase + (std::uint64_t{1} << 30) + 0x7000;
    const memblock::StoreTransaction older{
        .address = older_base + 8,
        .data = 0x0123456789abcdefULL,
        .op = memblock::StoreOp::sd,
        .rob = 0,
        .sq = 0,
        .address_lane = 0,
        .data_lane = 0,
    };
    const memblock::StoreTransaction younger{
        .address = younger_base + 65,
        .data = 0xfedcba9876543210ULL,
        .op = memblock::StoreOp::sd,
        .rob = 1,
        .sq = 1,
        .address_lane = 1,
        .data_lane = 1,
        .expected_exception_mask = memblock::kExceptionStoreAddressMisaligned,
    };

    if (!environment.reset() ||
        !environment.configure_sv39_nc(older.address, older.address) ||
        !environment.configure_sv39_nc(younger.address, younger.address) ||
        !environment.enqueue_store(older, 0) ||
        !environment.enqueue_store(younger, 0)) {
        std::cerr << "MEMBLOCK_STORE_RDATA_ORDER_FAIL cycle="
                  << environment.cycle() << " phase=allocate reason="
                  << environment.error() << '\n';
        return 1;
    }

    environment.expect_store(older);
    environment.expect_store(younger);
    if (!environment.warm_store_translation(younger) ||
        !environment.issue_store_data(younger) ||
        !environment.run_cycles(32) ||
        !environment.warm_store_translation(older) ||
        !environment.issue_store_data(older) ||
        !environment.run_until_store_complete(128) ||
        !environment.set_wfi(true) ||
        !environment.commit_stores_through(younger, 2) ||
        !environment.run_cycles(16)) {
        std::cerr << "MEMBLOCK_STORE_RDATA_ORDER_FAIL cycle="
                  << environment.cycle() << " phase=younger-first reason="
                  << environment.error() << '\n';
        return 1;
    }
    if (environment.sq_dequeued() != 0) {
        std::cerr << "MEMBLOCK_STORE_RDATA_ORDER_FAIL cycle="
                  << environment.cycle()
                  << " phase=ordering reason=younger store dequeued before older\n";
        return 1;
    }

    if (!environment.set_wfi(false) ||
        !environment.run_until_sq_dequeued(2, 512) ||
        !environment.run_cycles(32)) {
        std::cerr << "MEMBLOCK_STORE_RDATA_ORDER_FAIL cycle="
                  << environment.cycle() << " phase=older-complete reason="
                  << environment.error() << '\n';
        return 1;
    }

    const std::uint64_t observed_older =
        environment.memory().expected_load(older.address, memblock::LoadOp::ld);
    const std::uint64_t observed_younger =
        environment.memory().expected_load(younger.address, memblock::LoadOp::ld);
    if (observed_older != older.data || observed_younger != 0) {
        std::cerr << "MEMBLOCK_STORE_RDATA_ORDER_FAIL cycle="
                  << environment.cycle()
                  << " phase=data older=0x" << std::hex << observed_older
                  << " younger=0x" << observed_younger << std::dec
                  << " reason=NC store used out-of-order SQ read data\n";
        return 1;
    }

    std::cout << "MEMBLOCK_STORE_RDATA_ORDER_PASS"
              << " cycle=" << environment.cycle()
              << " stores=" << environment.store_writebacks()
              << " sq_dequeued=" << environment.sq_dequeued()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " ptw_requests=" << environment.ptw_requests()
              << " uncache_requests=" << environment.uncache_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_store_tlb_miss_preserve(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t address =
        memblock::kDefaultMemoryBase + 0x7c088;
    constexpr std::uint64_t miss_address = address + (std::uint64_t{1} << 30);
    const memblock::StoreTransaction store{
        .address = address,
        .data = 0x5aa5c33c96696996ULL,
        .op = memblock::StoreOp::sd,
        .rob = 0,
        .sq = 0,
        .address_lane = 0,
        .data_lane = 1,
    };

    environment.memory().fill_incrementing(address & ~std::uint64_t{63}, 64, 0x51);
    if (!environment.reset() ||
        !environment.configure_sv39(store.address, store.address) ||
        !environment.configure_sv39(miss_address, miss_address) ||
        !environment.enqueue_store(store, 0)) {
        std::cerr << "MEMBLOCK_STORE_TLB_MISS_PRESERVE_FAIL cycle="
                  << environment.cycle() << " phase=allocate reason="
                  << environment.error() << '\n';
        return 1;
    }
    environment.expect_store(store);
    if (!environment.warm_store_translation(store) ||
        !environment.run_cycles(8)) {
        std::cerr << "MEMBLOCK_STORE_TLB_MISS_PRESERVE_FAIL cycle="
                  << environment.cycle() << " phase=initial-hit reason="
                  << environment.error() << '\n';
        return 1;
    }

    const std::uint64_t initial_misses = environment.store_tlb_misses();
    const std::uint64_t initial_ptw = environment.ptw_requests();
    auto miss_store = store;
    miss_store.address = miss_address;
    const std::uint64_t miss_target = initial_misses + 1;
    if (!environment.issue_store_address(miss_store) ||
        !environment.run_until_store_tlb_misses(miss_target) ||
        !environment.run_until_ptw_requests(initial_ptw + 1)) {
        std::cerr << "MEMBLOCK_STORE_TLB_MISS_PRESERVE_FAIL cycle="
                  << environment.cycle() << " phase=tlb-miss"
                  << " feedbacks=" << environment.store_tlb_feedbacks()
                  << " misses=" << environment.store_tlb_misses()
                  << " ptw_requests=" << environment.ptw_requests()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    if (!environment.run_cycles(4)) {
        std::cerr << "MEMBLOCK_STORE_TLB_MISS_PRESERVE_FAIL cycle="
                  << environment.cycle() << " phase=assertion"
                  << " feedbacks=" << environment.store_tlb_feedbacks()
                  << " misses=" << environment.store_tlb_misses()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_STORE_TLB_MISS_PRESERVE_PASS"
              << " cycle=" << environment.cycle()
              << " stores=" << environment.store_writebacks()
              << " feedbacks=" << environment.store_tlb_feedbacks()
              << " misses=" << environment.store_tlb_misses()
              << " ptw_requests=" << environment.ptw_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_redirect(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t address = memblock::kDefaultMemoryBase + 0x4380;
    environment.memory().fill_incrementing(address & ~std::uint64_t{63}, 64, 0x40);
    environment.configure_backpressure(0x123456789abcdef0ULL, true);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_REDIRECT_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction canceled{
        .address = address,
        .op = memblock::LoadOp::ld,
        .rob = 20,
        .lq = 0,
        .sq = 0,
        .pdest = 30,
        .lane = 2,
    };
    if (!environment.enqueue_load(canceled) || !environment.issue_load(canceled) ||
        !environment.redirect_after(19, false, false) ||
        !environment.run_cycles(96)) {
        std::cerr << "MEMBLOCK_REDIRECT_FAIL cycle=" << environment.cycle()
                  << " phase=cancel reason=" << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction survivor{
        .address = address,
        .op = memblock::LoadOp::ld,
        .rob = 20,
        .lq = 0,
        .sq = 0,
        .pdest = 31,
        .lane = 1,
    };
    environment.expect_load(survivor);
    if (!environment.enqueue_load(survivor) || !environment.issue_load(survivor) ||
        !environment.run_until_complete(256)) {
        std::cerr << "MEMBLOCK_REDIRECT_FAIL cycle=" << environment.cycle()
                  << " phase=recovery reason=" << environment.error() << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_REDIRECT_PASS"
              << " cycle=" << environment.cycle()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " surviving_writebacks=" << environment.writebacks()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_queue_pressure(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr unsigned transaction_count = 120;
    constexpr unsigned wave_size = 60;
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x8000;
    environment.memory().fill_incrementing(base, 4096, 0x21);
    environment.configure_backpressure(0xa5a55a5af00dcafeULL, true);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_QUEUE_PRESSURE_FAIL cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    std::vector<memblock::LoadTransaction> transactions;
    transactions.reserve(transaction_count);
    for (unsigned index = 0; index < transaction_count; ++index) {
        transactions.push_back(memblock::LoadTransaction{
            .address = base + ((index * 37) % 512) * 8,
            .op = memblock::LoadOp::ld,
            .rob = memblock::rob_pointer_value(index),
            .rob_flag = memblock::rob_pointer_flag(index),
            .lq = memblock::lq_pointer_value(index),
            .lq_flag = memblock::lq_pointer_flag(index),
            .sq = 0,
            .pdest = static_cast<std::uint8_t>(1 + index),
            .lane = index % memblock::kScalarLoadLanes,
        });
    }

    for (unsigned wave = 0; wave < transaction_count; wave += wave_size) {
        const unsigned wave_end = std::min(wave + wave_size, transaction_count);
        for (unsigned index = wave; index < wave_end; ++index) {
            if (!environment.enqueue_load(transactions[index])) {
                std::cerr << "MEMBLOCK_QUEUE_PRESSURE_FAIL transaction=" << index
                          << " cycle=" << environment.cycle()
                          << " phase=enqueue reason=" << environment.error() << '\n';
                return 1;
            }
        }

        for (unsigned begin = wave; begin < wave_end; begin += 3) {
            std::vector<memblock::LoadTransaction> batch;
            for (unsigned offset = 0; offset < 3 && begin + offset < wave_end;
                 ++offset) {
                const auto &transaction = transactions[begin + offset];
                environment.expect_load(transaction);
                batch.push_back(transaction);
            }
            if (!environment.issue_load_batch(batch) ||
                !environment.run_until_complete(1024)) {
                std::cerr << "MEMBLOCK_QUEUE_PRESSURE_FAIL transaction=" << begin
                          << " cycle=" << environment.cycle()
                          << " phase=drain reason=" << environment.error() << '\n';
                return 1;
            }
        }
        if (!environment.run_until_lq_retired(256)) {
            std::cerr << "MEMBLOCK_QUEUE_PRESSURE_FAIL transaction=" << wave_end
                      << " cycle=" << environment.cycle()
                      << " phase=retire reason=" << environment.error() << '\n';
            return 1;
        }
    }
    std::cout << "MEMBLOCK_QUEUE_PRESSURE_PASS"
              << " cycle=" << environment.cycle()
              << " allocated=" << transaction_count
              << " writebacks=" << environment.writebacks()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_scalar_misaligned(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t bare_base = memblock::kDefaultMemoryBase + 0x50000;
    constexpr std::uint64_t virtual_base = 0x40000000ULL;
    constexpr std::uint64_t physical_page0 = 0xa1000000ULL;
    constexpr std::uint64_t physical_page1 = 0xa2000000ULL;
    constexpr std::uint64_t root = 0x91000000ULL;
    environment.memory().fill_incrementing(bare_base, 0x3000, 0x31);
    environment.memory().fill_incrementing(physical_page0, 0x1000, 0x57);
    environment.memory().fill_incrementing(physical_page1, 0x1000, 0xa3);
    environment.configure_backpressure(0x7f4a7c159e3779b9ULL, true);
    if (!environment.reset() || !environment.enable_misaligned_accesses()) {
        std::cerr << "MEMBLOCK_SCALAR_MISALIGNED_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    const std::array<memblock::LoadTransaction, 4> bare{{
        {
            .address = bare_base + 3,
            .op = memblock::LoadOp::lw,
            .rob = 0,
            .lq = 0,
            .pdest = 20,
            .lane = 0,
        },
        {
            .address = bare_base + 13,
            .op = memblock::LoadOp::ld,
            .rob = 1,
            .lq = 1,
            .pdest = 21,
            .lane = 1,
        },
        {
            .address = bare_base + 61,
            .op = memblock::LoadOp::ld,
            .rob = 2,
            .lq = 2,
            .pdest = 22,
            .lane = 2,
        },
        {
            .address = bare_base + 0xffd,
            .op = memblock::LoadOp::ld,
            .rob = 3,
            .lq = 3,
            .pdest = 23,
            .lane = 0,
        },
    }};
    for (std::size_t index = 0; index < bare.size(); ++index) {
        const auto &transaction = bare[index];
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 256) ||
            !environment.run_until_complete(4096) ||
            !environment.run_until_lq_retired(1024)) {
            std::cerr << "MEMBLOCK_SCALAR_MISALIGNED_FAIL transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=bare reason=" << environment.error() << '\n';
            return 1;
        }
    }

    if (!environment.map_sv39_4k(
            virtual_base, physical_page0, root) ||
        !environment.map_sv39_4k(
            virtual_base + 0x1000, physical_page1, root) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_SCALAR_MISALIGNED_FAIL cycle="
                  << environment.cycle() << " phase=sv39-config reason="
                  << environment.error() << '\n';
        return 1;
    }

    memblock::LoadTransaction cross_page{
        .address = virtual_base + 0xffd,
        .oracle_address = physical_page0 + 0xffd,
        .op = memblock::LoadOp::ld,
        .rob = 4,
        .lq = 4,
        .pdest = 24,
        .lane = 1,
    };
    std::uint64_t expected = 0;
    for (unsigned byte = 0; byte < 8; ++byte) {
        const std::uint64_t physical = byte < 3
            ? physical_page0 + 0xffd + byte
            : physical_page1 + byte - 3;
        expected |= std::uint64_t{environment.memory().read_byte(physical)}
                    << (8 * byte);
    }
    environment.expect_load_data(cross_page, expected);
    if (!environment.set_rob_head(cross_page.rob) ||
        !environment.enqueue_load(cross_page) ||
        !environment.issue_load(cross_page, 256) ||
        !environment.run_until_complete(8192) ||
        !environment.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_SCALAR_MISALIGNED_FAIL cycle="
                  << environment.cycle() << " phase=translated-cross-page reason="
                  << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_SCALAR_MISALIGNED_PASS"
              << " cycle=" << environment.cycle()
              << " writebacks=" << environment.writebacks()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " ptw_requests=" << environment.ptw_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_misaligned_stores(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t bare_base = memblock::kDefaultMemoryBase + 0x58000;
    constexpr std::uint64_t virtual_base = 0x48000000ULL;
    constexpr std::uint64_t physical_page0 = 0xa4000000ULL;
    constexpr std::uint64_t physical_page1 = 0xa5000000ULL;
    constexpr std::uint64_t root = 0x93000000ULL;
    environment.memory().fill_incrementing(bare_base, 0x4000, 0x2d);
    environment.memory().fill_incrementing(physical_page0, 0x1000, 0x61);
    environment.memory().fill_incrementing(physical_page1, 0x1000, 0xb7);
    environment.configure_backpressure(0xbb67ae8584caa73bULL, true);
    if (!environment.reset() || !environment.enable_misaligned_accesses()) {
        std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    const std::array<memblock::StoreTransaction, 3> scalar_stores{{
        {
            .address = bare_base + 13,
            .data = 0x0123456789abcdefULL,
            .op = memblock::StoreOp::sd,
            .rob = 0,
            .sq = 0,
            .address_lane = 0,
            .data_lane = 1,
        },
        {
            .address = bare_base + 61,
            .data = 0xfedcba9876543210ULL,
            .op = memblock::StoreOp::sd,
            .rob = 2,
            .sq = 1,
            .address_lane = 1,
            .data_lane = 0,
        },
        {
            .address = bare_base + 0xffd,
            .data = 0x55aa33cc0ff09669ULL,
            .op = memblock::StoreOp::sd,
            .rob = 4,
            .sq = 2,
            .address_lane = 0,
            .data_lane = 0,
        },
    }};
    for (std::size_t index = 0; index < scalar_stores.size(); ++index) {
        const auto &store = scalar_stores[index];
        environment.expect_store(store);
        const bool data_first = (index & 1U) != 0;
        if (!environment.set_rob_head(store.rob) ||
            !environment.enqueue_store(store, static_cast<std::uint8_t>(index)) ||
            !(data_first
                  ? environment.issue_store_data(store, 256) &&
                        environment.issue_store_address(store, 256)
                  : environment.issue_store_address(store, 256) &&
                        environment.issue_store_data(store, 256)) ||
            !environment.pulse_pending_store(store.rob, store.rob_flag) ||
            !environment.run_until_store_complete(4096) ||
            !environment.commit_store(store, 8192)) {
            std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=scalar-store reason=" << environment.error()
                      << '\n';
            return 1;
        }

        const memblock::LoadTransaction readback{
            .address = store.address,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(store.rob + 1),
            .lq = static_cast<std::uint8_t>(index),
            .sq = static_cast<std::uint8_t>(index + 1),
            .pdest = static_cast<std::uint8_t>(30 + index),
            .lane = static_cast<unsigned>(index % memblock::kScalarLoadLanes),
        };
        environment.expect_load_data(readback, store.data);
        if (!environment.set_rob_head(readback.rob) ||
            !environment.enqueue_load(readback) ||
            !environment.issue_load(readback, 256) ||
            !environment.run_until_complete(8192) ||
            !environment.run_until_lq_retired(2048)) {
            std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=scalar-readback reason=" << environment.error()
                      << '\n';
            return 1;
        }
    }

    memblock::VectorMemoryTransaction vector_store{
        .store = true,
        .address = bare_base + 0x2005,
        .stride = 8,
        .addressing = memblock::VectorAddressingMode::strided,
        .eew = 3,
        .vl = 2,
        .rob = 6,
        .lq = 3,
        .sq = 3,
        .lane = 1,
        .flow_num = 2,
    };
    for (unsigned byte = 0; byte < vector_store.data.size(); ++byte) {
        vector_store.data[byte] = static_cast<unsigned char>(0x91 + byte * 7);
    }
    environment.expect_vector(vector_store);
    if (!environment.set_rob_head(vector_store.rob) ||
        !environment.enqueue_vector(vector_store) ||
        !environment.issue_vector(vector_store, 512) ||
        !environment.run_cycles(32) ||
        !environment.pulse_pending_store(
            vector_store.rob, vector_store.rob_flag) ||
        !environment.run_until_vector_complete_with_replays(vector_store, 8192) ||
        !environment.commit_vector_store(vector_store, 8192)) {
        std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL cycle="
                  << environment.cycle()
                  << " phase=vector-store reason=" << environment.error() << '\n';
        return 1;
    }
    auto vector_readback = vector_store;
    vector_readback.store = false;
    vector_readback.rob = 7;
    vector_readback.lq = 3;
    vector_readback.sq = 5;
    vector_readback.pdest = 40;
    vector_readback.lane = 0;
    vector_readback.data.fill(0);
    environment.expect_vector_data(vector_readback, vector_store.data);
    if (!environment.set_rob_head(vector_readback.rob) ||
        !environment.enqueue_vector(vector_readback) ||
        !environment.issue_vector(vector_readback, 512) ||
        !environment.run_until_vector_complete(8192) ||
        !environment.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL cycle="
                  << environment.cycle()
                  << " phase=vector-readback reason=" << environment.error()
                  << '\n';
        return 1;
    }

    if (!environment.map_sv39_4k(virtual_base, physical_page0, root) ||
        !environment.map_sv39_4k(
            virtual_base + 0x1000, physical_page1, root) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL cycle="
                  << environment.cycle()
                  << " phase=sv39-config reason=" << environment.error() << '\n';
        return 1;
    }

    auto cross_page_store = vector_store;
    cross_page_store.address = virtual_base + 0xffd;
    cross_page_store.rob = 8;
    cross_page_store.lq = 5;
    cross_page_store.sq = 5;
    cross_page_store.lane = 0;
    for (unsigned byte = 0; byte < cross_page_store.data.size(); ++byte) {
        cross_page_store.data[byte] = static_cast<unsigned char>(0xe3 - byte * 5);
    }
    environment.expect_vector(cross_page_store);
    if (!environment.set_rob_head(cross_page_store.rob) ||
        !environment.enqueue_vector(cross_page_store) ||
        !environment.issue_vector(cross_page_store, 512) ||
        !environment.run_cycles(256) ||
        !environment.pulse_pending_store(
            cross_page_store.rob, cross_page_store.rob_flag) ||
        !environment.run_until_vector_complete_with_replays(
            cross_page_store, 16384, true) ||
        !environment.commit_vector_store(cross_page_store, 16384)) {
        std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL cycle="
                  << environment.cycle()
                  << " phase=vector-cross-page-store reason="
                  << environment.error() << '\n';
        return 1;
    }
    auto cross_page_readback = cross_page_store;
    cross_page_readback.store = false;
    cross_page_readback.rob = 9;
    cross_page_readback.lq = 5;
    cross_page_readback.sq = 7;
    cross_page_readback.pdest = 41;
    cross_page_readback.lane = 1;
    cross_page_readback.data.fill(0);
    environment.expect_vector_data(cross_page_readback, cross_page_store.data);
    if (!environment.set_rob_head(cross_page_readback.rob) ||
        !environment.enqueue_vector(cross_page_readback) ||
        !environment.issue_vector(cross_page_readback, 512) ||
        !environment.run_until_vector_complete(16384) ||
        !environment.run_until_queues_retired(8192)) {
        std::cerr << "MEMBLOCK_MISALIGNED_STORES_FAIL cycle="
                  << environment.cycle()
                  << " phase=vector-cross-page-readback reason="
                  << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_MISALIGNED_STORES_PASS"
              << " cycle=" << environment.cycle()
              << " scalar_store_writebacks=" << environment.store_writebacks()
              << " scalar_load_writebacks=" << environment.writebacks()
              << " vector_store_writebacks="
              << environment.vector_store_writebacks()
              << " vector_load_writebacks="
              << environment.vector_load_writebacks()
              << " vector_replays=" << environment.vector_replay_feedbacks()
              << " ptw_requests=" << environment.ptw_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_vector_addressing(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x60000;
    environment.memory().fill_incrementing(base, 0x4000, 0x19);
    environment.configure_backpressure(0x243f6a8885a308d3ULL, true);
    if (!environment.reset() || !environment.enable_misaligned_accesses()) {
        std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    std::array<memblock::VectorMemoryTransaction, 3> loads{};
    loads[0] = memblock::VectorMemoryTransaction{
        .address = base + 0x100,
        .stride = 6,
        .addressing = memblock::VectorAddressingMode::strided,
        .eew = 1,
        .vl = 8,
        .rob = 0,
        .lq = 0,
        .pdest = 70,
        .lane = 0,
        .flow_num = 8,
    };
    loads[1] = memblock::VectorMemoryTransaction{
        .address = base + 0x400,
        .addressing = memblock::VectorAddressingMode::indexed_unordered,
        .eew = 0,
        .vl = 16,
        .rob = 1,
        .lq = 8,
        .pdest = 71,
        .lane = 1,
        .flow_num = 16,
    };
    loads[2] = memblock::VectorMemoryTransaction{
        .address = base + 0x800,
        .addressing = memblock::VectorAddressingMode::indexed_ordered,
        .eew = 2,
        .vl = 4,
        .rob = 2,
        .lq = 24,
        .pdest = 72,
        .lane = 0,
        .flow_num = 4,
    };
    for (unsigned element = 0; element < 16; ++element) {
        loads[1].index[element] = static_cast<unsigned char>((element * 13) & 63);
    }
    const std::array<std::uint32_t, 4> word_indices{{52, 4, 36, 20}};
    for (unsigned element = 0; element < word_indices.size(); ++element) {
        for (unsigned byte = 0; byte < 4; ++byte) {
            loads[2].index[element * 4 + byte] = static_cast<unsigned char>(
                word_indices[element] >> (8 * byte));
        }
    }
    for (std::size_t index = 0; index < loads.size(); ++index) {
        auto &transaction = loads[index];
        for (unsigned byte = 0; byte < transaction.data.size(); ++byte) {
            transaction.data[byte] = static_cast<unsigned char>(0xd0 + byte);
        }
        environment.expect_vector(transaction);
        if (!environment.enqueue_vector(transaction) ||
            !environment.issue_vector(transaction, 512) ||
            !environment.run_until_vector_complete(8192) ||
            !environment.run_until_lq_retired(2048)) {
            std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=load reason=" << environment.error() << '\n';
            return 1;
        }
    }

    std::array<memblock::VectorMemoryTransaction, 3> stores{};
    stores[0] = memblock::VectorMemoryTransaction{
        .store = true, .address = base + 0xc00, .stride = 5,
        .addressing = memblock::VectorAddressingMode::strided, .eew = 0,
        .vl = 16, .rob = 3, .lq = 28, .sq = 0, .lane = 1, .flow_num = 16,
    };
    stores[1] = memblock::VectorMemoryTransaction{
        .store = true, .address = base + 0x1000,
        .addressing = memblock::VectorAddressingMode::indexed_unordered, .eew = 1,
        .vl = 8, .rob = 5, .lq = 44, .sq = 16, .lane = 0, .flow_num = 8,
    };
    stores[2] = memblock::VectorMemoryTransaction{
        .store = true, .address = base + 0x1400,
        .addressing = memblock::VectorAddressingMode::indexed_ordered, .eew = 2,
        .vl = 4, .rob = 7, .lq = 52, .sq = 24, .lane = 1, .flow_num = 4,
    };
    for (std::size_t store_index = 0; store_index < stores.size(); ++store_index) {
        auto &store = stores[store_index];
        const unsigned element_bytes = 1U << store.eew;
        const unsigned elements = 16U >> store.eew;
        for (unsigned byte = 0; byte < store.data.size(); ++byte) {
            store.data[byte] = static_cast<unsigned char>(
                0x40 + store_index * 0x30 + byte * 3);
        }
        if (store.addressing != memblock::VectorAddressingMode::strided) {
            for (unsigned element = 0; element < elements; ++element) {
                const std::uint64_t offset =
                    ((element * 3 + store_index) % elements) * element_bytes;
                for (unsigned byte = 0; byte < element_bytes; ++byte) {
                    store.index[element * element_bytes + byte] =
                        static_cast<unsigned char>(offset >> (8 * byte));
                }
            }
        }
        environment.expect_vector(store);
        if (!environment.enqueue_vector(store) ||
            !environment.issue_vector(store, 512) ||
            !environment.run_until_vector_complete(8192) ||
            !environment.commit_vector_store(store, 8192)) {
            std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL transaction="
                      << store_index << " cycle=" << environment.cycle()
                      << " phase=store reason=" << environment.error() << '\n';
            return 1;
        }
        auto verify = store;
        verify.store = false;
        verify.rob = static_cast<std::uint8_t>(store.rob + 1);
        verify.lq = store.lq;
        verify.sq = static_cast<std::uint8_t>(store.sq + store.flow_num);
        verify.pdest = static_cast<std::uint8_t>(73 + store_index);
        verify.lane = (store.lane + 1) % memblock::kVectorMemoryLanes;
        verify.data.fill(0);
        environment.expect_vector_data(verify, store.data);
        if (!environment.enqueue_vector(verify) ||
            !environment.issue_vector(verify, 512) ||
            !environment.run_until_vector_complete(8192) ||
            !environment.run_until_queues_retired(4096)) {
            std::cerr << "MEMBLOCK_VECTOR_ADDRESSING_FAIL transaction="
                      << store_index << " cycle=" << environment.cycle()
                      << " phase=store-readback reason=" << environment.error() << '\n';
            return 1;
        }
    }

    std::cout << "MEMBLOCK_VECTOR_ADDRESSING_PASS"
              << " cycle=" << environment.cycle()
              << " load_writebacks=" << environment.vector_load_writebacks()
              << " store_writebacks=" << environment.vector_store_writebacks()
              << " store_modes=3"
              << " tilelink_requests=" << environment.tilelink_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_exception_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_base = 0x50000000ULL;
    constexpr std::uint64_t physical_base = 0xa3000000ULL;
    constexpr std::uint64_t root = 0x92000000ULL;
    environment.memory().fill_incrementing(physical_base, 0x2000, 0x43);
    if (!environment.reset() || !environment.enable_misaligned_accesses() ||
        !environment.map_sv39_4k(
            virtual_base, physical_base, root) ||
        !environment.map_sv39_4k(
            virtual_base + 0x1000,
            physical_base + 0x1000,
            root,
            true,
            true,
            false,
            false,
            true) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction page_fault{
        .address = virtual_base + 0x3000,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .pdest = 40,
        .lane = 0,
        .expected_exception_mask = memblock::kExceptionLoadPageFault,
    };
    environment.expect_load(page_fault);
    if (!environment.enqueue_load(page_fault) ||
        !environment.issue_load(page_fault, 256) ||
        !environment.run_until_complete(4096) ||
        !environment.run_until_lq_retired(1024)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=load-page-fault reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::PrefetchTransaction prefetch_fault{
        .address = virtual_base + 0x4000,
        .op = memblock::PrefetchOp::read,
        .rob = 1,
        .lq = 1,
        .lane = 1,
    };
    environment.expect_prefetch(prefetch_fault);
    if (!environment.enqueue_prefetch(prefetch_fault) ||
        !environment.issue_prefetch(prefetch_fault, 256) ||
        !environment.run_until_complete(4096) ||
        !environment.run_until_lq_retired(1024)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=prefetch-page-fault reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction nc_misaligned_load{
        .address = virtual_base + 0x1001,
        .oracle_address = physical_base + 0x1001,
        .op = memblock::LoadOp::lw,
        .rob = 2,
        .lq = 2,
        .pdest = 41,
        .lane = 2,
        .expected_exception_mask = memblock::kExceptionLoadAddressMisaligned,
    };
    environment.expect_load(nc_misaligned_load);
    if (!environment.set_rob_head(nc_misaligned_load.rob) ||
        !environment.enqueue_load(nc_misaligned_load) ||
        !environment.issue_load(nc_misaligned_load, 256) ||
        !environment.run_until_complete(4096) ||
        !environment.run_until_lq_retired(1024)) {
        std::cerr << "MEMBLOCK_EXCEPTION_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=pbmt-load-misaligned reason="
                  << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_EXCEPTION_CONTRACTS_PASS"
              << " cycle=" << environment.cycle()
              << " load_writebacks=" << environment.writebacks()
              << " prefetch_writebacks=" << environment.prefetch_writebacks()
              << " ptw_requests=" << environment.ptw_requests()
              << " uncache_requests=" << environment.uncache_requests()
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
    if (!environment.enqueue_load(cold) ||
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
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
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
    std::cout << "MEMBLOCK_SCALAR_GUEST_FAULT_PASS"
              << " cycle=" << environment.cycle()
              << " writebacks=" << environment.writebacks()
              << " ptw_requests=" << environment.ptw_requests()
              << " vaddr=0x" << std::hex << environment.exception_vaddr()
              << " gpaddr=0x" << environment.exception_gpaddr() << std::dec
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_random_loads(int argc, char **argv, const Options &options)
{
    memblock::Environment environment(argc, argv);
    std::mt19937_64 random(options.seed);
    constexpr std::uint64_t memory_size = 64 * 1024;
    for (std::uint64_t offset = 0; offset < memory_size; ++offset) {
        const std::uint8_t value = static_cast<std::uint8_t>(
            (offset * 73 + (offset >> 5) * 19 + options.seed) & 0xff);
        environment.memory().write_byte(memblock::kDefaultMemoryBase + offset, value);
    }
    environment.configure_backpressure(
        options.seed ^ 0xd1b54a32d192ed03ULL, options.backpressure);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_RANDOM_FAIL seed=" << options.seed
                  << " transaction=0 cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    LoadCoverage coverage;
    std::uint64_t recent_line = 0;
    for (unsigned index = 0; index < options.transactions; ++index) {
        const auto op = static_cast<memblock::LoadOp>(random() % 7);
        const unsigned size = 1U << (static_cast<unsigned>(op) & 3U);
        std::uint64_t line = random() % (memory_size / 64);
        if (index != 0 && (random() & 3U) != 0) {
            line = recent_line;
        }
        recent_line = line;
        const unsigned slots = 64 / size;
        const std::uint64_t address = memblock::kDefaultMemoryBase + line * 64 +
                                      (random() % slots) * size;
        const memblock::LoadTransaction transaction{
            .address = address,
            .op = op,
            .rob = memblock::rob_pointer_value(index),
            .rob_flag = memblock::rob_pointer_flag(index),
            .lq = memblock::lq_pointer_value(index),
            .lq_flag = memblock::lq_pointer_flag(index),
            .sq = 0,
            .sq_flag = false,
            .pdest = static_cast<std::uint8_t>(1 + random() % 255),
            .lane = static_cast<unsigned>(random() % memblock::kScalarLoadLanes),
        };
        const std::uint64_t requests_before = environment.tilelink_requests();
        environment.expect_load(transaction);
        if (!environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction) ||
            !environment.run_until_complete(1024)) {
            std::cerr << "MEMBLOCK_RANDOM_FAIL seed=" << options.seed
                      << " transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " address=0x" << std::hex << address << std::dec
                      << " op=" << static_cast<unsigned>(op)
                      << " lane=" << transaction.lane
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        coverage.sample(
            transaction, requests_before, environment.tilelink_requests());
    }

    if (options.transactions >= 21 && !coverage.complete()) {
        std::cerr << "MEMBLOCK_RANDOM_FAIL seed=" << options.seed
                  << " transaction=" << options.transactions
                  << " cycle=" << environment.cycle()
                  << " reason=incomplete_coverage " << coverage.summary() << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_RANDOM_PASS"
              << " seed=" << options.seed
              << " transactions=" << options.transactions
              << " cycle=" << environment.cycle()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " writebacks=" << environment.writebacks()
              << ' ' << coverage.summary()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_random_vector_loads(int argc, char **argv, const Options &options)
{
    memblock::Environment environment(argc, argv);
    std::mt19937_64 random(options.seed ^ 0x6a09e667f3bcc909ULL);
    constexpr std::uint64_t memory_size = 64 * 1024;
    for (std::uint64_t offset = 0; offset < memory_size; ++offset) {
        environment.memory().write_byte(
            memblock::kDefaultMemoryBase + 0x30000 + offset,
            static_cast<std::uint8_t>(
                (offset * 29 + (offset >> 4) * 113 + options.seed) & 0xff));
    }
    environment.configure_backpressure(
        options.seed ^ 0xbb67ae8584caa73bULL, options.backpressure);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_RANDOM_VECTOR_FAIL seed=" << options.seed
                  << " transaction=0 cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    VectorCoverage coverage;
    std::uint64_t recent_line = 0;
    for (unsigned index = 0; index < options.transactions; ++index) {
        const unsigned eew = random() % 4;
        const unsigned element_bytes = 1U << eew;
        const unsigned element_count = 16U / element_bytes;
        std::uint64_t line = random() % (memory_size / 64);
        if (index != 0 && (random() & 3U) != 0) {
            line = recent_line;
        }
        recent_line = line;
        const unsigned offset_slots = (64 - 16) / element_bytes + 1;
        const unsigned line_offset = (random() % offset_slots) * element_bytes;
        memblock::VectorMemoryTransaction transaction{
            .address = memblock::kDefaultMemoryBase + 0x30000 + line * 64 +
                       line_offset,
            .eew = static_cast<std::uint8_t>(eew),
            .vl = static_cast<std::uint8_t>(random() % (element_count + 1)),
            .vm = (random() & 1U) != 0,
            .mask_bits = static_cast<std::uint16_t>(random()),
            .vma = (random() & 1U) != 0,
            .vta = (random() & 1U) != 0,
            .rob = memblock::rob_pointer_value(index),
            .rob_flag = memblock::rob_pointer_flag(index),
            .lq = memblock::lq_pointer_value(index * 2),
            .lq_flag = memblock::lq_pointer_flag(index * 2),
            .pdest = static_cast<std::uint8_t>(1 + random() % 255),
            .lane = static_cast<unsigned>(random() % memblock::kVectorMemoryLanes),
        };
        transaction.vstart = transaction.vl == 0
            ? 0
            : static_cast<std::uint8_t>(random() % (transaction.vl + 1));
        for (auto &byte : transaction.data) {
            byte = static_cast<unsigned char>(random());
        }
        const std::uint64_t requests_before = environment.tilelink_requests();
        environment.expect_vector(transaction);
        if (!environment.enqueue_vector(transaction) ||
            !environment.issue_vector(transaction) ||
            !environment.run_until_vector_complete(1024)) {
            std::cerr << "MEMBLOCK_RANDOM_VECTOR_FAIL seed=" << options.seed
                      << " transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " address=0x" << std::hex << transaction.address << std::dec
                      << " eew=" << eew << " vl="
                      << static_cast<unsigned>(transaction.vl)
                      << " vstart=" << static_cast<unsigned>(transaction.vstart)
                      << " vm=" << transaction.vm << " lane=" << transaction.lane
                      << " active=0x" << std::hex
                      << memblock::active_vector_elements(transaction) << std::dec
                      << " rob=" << static_cast<unsigned>(transaction.rob)
                      << ':' << transaction.rob_flag
                      << " lq=" << static_cast<unsigned>(transaction.lq)
                      << ':' << transaction.lq_flag
                      << " requests_before=" << requests_before
                      << " requests_after=" << environment.tilelink_requests()
                      << " releases=" << environment.tilelink_releases()
                      << " lq_allocated=" << environment.lq_allocated()
                      << " lq_dequeued=" << environment.lq_dequeued()
                      << " writebacks=" << environment.vector_load_writebacks()
                      << " reason=" << environment.error() << '\n';
            return 1;
        }
        coverage.sample(
            transaction, requests_before, environment.tilelink_requests());
    }

    if (options.transactions >= 64 && !coverage.complete()) {
        std::cerr << "MEMBLOCK_RANDOM_VECTOR_FAIL seed=" << options.seed
                  << " transaction=" << options.transactions
                  << " cycle=" << environment.cycle()
                  << " reason=incomplete_coverage " << coverage.summary() << '\n';
        return 1;
    }
    if (!environment.run_until_lq_retired()) {
        std::cerr << "MEMBLOCK_RANDOM_VECTOR_FAIL seed=" << options.seed
                  << " transaction=" << options.transactions
                  << " cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_RANDOM_VECTOR_PASS"
              << " seed=" << options.seed
              << " transactions=" << options.transactions
              << " cycle=" << environment.cycle()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " tilelink_releases=" << environment.tilelink_releases()
              << " lq_allocated=" << environment.lq_allocated()
              << " lq_dequeued=" << environment.lq_dequeued()
              << " writebacks=" << environment.vector_load_writebacks()
              << ' ' << coverage.summary()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_random_vector_forwarding(int argc, char **argv, const Options &options)
{
    memblock::Environment environment(argc, argv);
    std::mt19937_64 random(options.seed ^ 0x3c6ef372fe94f82bULL);
    constexpr unsigned max_transactions = 24;
    const unsigned transaction_count = std::min(options.transactions, max_transactions);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x50000;
    environment.memory().fill_incrementing(base, 4096, 0x27);
    environment.configure_backpressure(
        options.seed ^ 0xa54ff53a5f1d36f1ULL, options.backpressure);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_RANDOM_VECTOR_FORWARD_FAIL seed=" << options.seed
                  << " transaction=0 cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    VectorCoverage coverage;
    for (unsigned index = 0; index < transaction_count; ++index) {
        const unsigned eew = index < 4 ? index : random() % 4;
        const unsigned element_bytes = 1U << eew;
        const unsigned element_count = 16U / element_bytes;
        const unsigned line_offset = (index & 1U) == 0
            ? 0
            : element_bytes;
        memblock::VectorMemoryTransaction store{
            .store = true,
            .address = base + index * 64 + line_offset,
            .eew = static_cast<std::uint8_t>(eew),
            .vl = static_cast<std::uint8_t>(
                index % 6 == 0 ? 0
                : index % 6 == 1 ? element_count
                                 : random() % (element_count + 1)),
            .vm = (index & 1U) != 0,
            .mask_bits = static_cast<std::uint16_t>(random()),
            .vma = (random() & 1U) != 0,
            .vta = (random() & 1U) != 0,
            .rob = memblock::rob_pointer_value(index * 2),
            .rob_flag = memblock::rob_pointer_flag(index * 2),
            .lq = memblock::lq_pointer_value(index * 2),
            .lq_flag = memblock::lq_pointer_flag(index * 2),
            .sq = memblock::sq_pointer_value(index * 2),
            .sq_flag = memblock::sq_pointer_flag(index * 2),
            .lane = index % memblock::kVectorMemoryLanes,
        };
        store.vstart = store.vl > 1 && index % 5 == 0
            ? 1
            : store.vl == 0 ? 0
                            : static_cast<std::uint8_t>(random() % (store.vl + 1));
        for (auto &byte : store.data) {
            byte = static_cast<unsigned char>(random());
        }
        environment.expect_vector(store);
        if (!environment.enqueue_vector(store) || !environment.issue_vector(store) ||
            !environment.run_until_vector_complete(512)) {
            std::cerr << "MEMBLOCK_RANDOM_VECTOR_FORWARD_FAIL seed=" << options.seed
                      << " transaction=" << index
                      << " phase=store cycle=" << environment.cycle()
                      << " address=0x" << std::hex << store.address << std::dec
                      << " eew=" << eew << " reason=" << environment.error() << '\n';
            return 1;
        }

        memblock::VectorMemoryTransaction load{
            .address = store.address,
            .eew = store.eew,
            .vl = static_cast<std::uint8_t>(element_count),
            .rob = memblock::rob_pointer_value(index * 2 + 1),
            .rob_flag = memblock::rob_pointer_flag(index * 2 + 1),
            .lq = memblock::lq_pointer_value(index * 2),
            .lq_flag = memblock::lq_pointer_flag(index * 2),
            .sq = memblock::sq_pointer_value(index * 2 + 2),
            .sq_flag = memblock::sq_pointer_flag(index * 2 + 2),
            .pdest = static_cast<std::uint8_t>(1 + random() % 255),
            .lane = (index + 1) % memblock::kVectorMemoryLanes,
        };
        for (auto &byte : load.data) {
            byte = static_cast<unsigned char>(random());
        }
        auto expected = environment.memory().expected_vector_load(load);
        const std::uint16_t active = memblock::active_vector_elements(store);
        for (unsigned element = 0; element < element_count; ++element) {
            if (((active >> element) & 1U) == 0) {
                continue;
            }
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                const unsigned offset = element * element_bytes + byte;
                expected[offset] = store.data[offset];
            }
        }
        const std::uint64_t requests_before = environment.tilelink_requests();
        environment.expect_vector_data(load, expected);
        if (!environment.enqueue_vector(load) || !environment.issue_vector(load) ||
            !environment.run_until_vector_complete(1024)) {
            std::cerr << "MEMBLOCK_RANDOM_VECTOR_FORWARD_FAIL seed=" << options.seed
                      << " transaction=" << index
                      << " phase=load cycle=" << environment.cycle()
                      << " address=0x" << std::hex << load.address << std::dec
                      << " eew=" << eew << " reason=" << environment.error() << '\n';
            return 1;
        }
        coverage.sample(store, requests_before, environment.tilelink_requests());
    }

    if (transaction_count >= 16 && !coverage.complete(false)) {
        std::cerr << "MEMBLOCK_RANDOM_VECTOR_FORWARD_FAIL seed=" << options.seed
                  << " transaction=" << transaction_count
                  << " cycle=" << environment.cycle()
                  << " reason=incomplete_coverage " << coverage.summary() << '\n';
        return 1;
    }
    if (!environment.run_until_lq_retired()) {
        std::cerr << "MEMBLOCK_RANDOM_VECTOR_FORWARD_FAIL seed=" << options.seed
                  << " transaction=" << transaction_count
                  << " cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_RANDOM_VECTOR_FORWARD_PASS"
              << " seed=" << options.seed
              << " transactions=" << transaction_count
              << " cycle=" << environment.cycle()
              << " stores=" << environment.vector_store_writebacks()
              << " loads=" << environment.vector_load_writebacks()
              << ' ' << coverage.summary()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_random_forwarding(int argc, char **argv, const Options &options)
{
    memblock::Environment environment(argc, argv);
    std::mt19937_64 random(options.seed ^ 0x9e3779b97f4a7c15ULL);
    constexpr unsigned max_transactions = 48;
    const unsigned transaction_count = std::min(options.transactions, max_transactions);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x12000;
    environment.memory().fill_incrementing(base, 4096, 0x31);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_RANDOM_FORWARD_FAIL seed=" << options.seed
                  << " transaction=0 cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    std::array<std::uint64_t, 4> operations{};
    std::array<std::uint64_t, memblock::kScalarStoreLanes> address_lanes{};
    std::array<std::uint64_t, memblock::kScalarStoreLanes> data_lanes{};
    std::uint64_t signed_loads = 0;
    std::uint64_t unsigned_loads = 0;
    for (unsigned index = 0; index < transaction_count; ++index) {
        const unsigned op_value = static_cast<unsigned>(random() % 4);
        const auto store_op = static_cast<memblock::StoreOp>(op_value);
        const unsigned size = 1U << op_value;
        const bool is_unsigned = size != 8 && (random() & 1U) != 0;
        const auto load_op = static_cast<memblock::LoadOp>(
            op_value + (is_unsigned ? 4U : 0U));
        const std::uint64_t address = base + index * 64 +
                                      ((random() % (64 / size)) * size);
        const std::uint64_t data = random();
        const memblock::StoreTransaction store{
            .address = address,
            .data = data,
            .op = store_op,
            .rob = memblock::rob_pointer_value(index * 2),
            .rob_flag = memblock::rob_pointer_flag(index * 2),
            .sq = memblock::sq_pointer_value(index),
            .sq_flag = memblock::sq_pointer_flag(index),
            .address_lane = static_cast<unsigned>(random() % memblock::kScalarStoreLanes),
            .data_lane = static_cast<unsigned>(random() % memblock::kScalarStoreLanes),
        };
        environment.expect_store(store);
        const bool data_first = (random() & 1U) != 0;
        const bool store_ok = environment.enqueue_store(
                                  store, static_cast<std::uint8_t>(index)) &&
                              (data_first
                                   ? environment.issue_store_data(store) &&
                                         environment.issue_store_address(store)
                                   : environment.issue_store_address(store) &&
                                         environment.issue_store_data(store)) &&
                              environment.run_until_store_complete(128);
        if (!store_ok) {
            std::cerr << "MEMBLOCK_RANDOM_FORWARD_FAIL seed=" << options.seed
                      << " transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=store reason=" << environment.error() << '\n';
            return 1;
        }

        const memblock::LoadTransaction load{
            .address = address,
            .op = load_op,
            .rob = memblock::rob_pointer_value(index * 2 + 1),
            .rob_flag = memblock::rob_pointer_flag(index * 2 + 1),
            .lq = memblock::lq_pointer_value(index),
            .lq_flag = memblock::lq_pointer_flag(index),
            .sq = memblock::sq_pointer_value(index + 1),
            .sq_flag = memblock::sq_pointer_flag(index + 1),
            .pdest = static_cast<std::uint8_t>(1 + random() % 255),
            .lane = static_cast<unsigned>(random() % memblock::kScalarLoadLanes),
        };
        const unsigned bits = size * 8;
        const std::uint64_t mask = bits == 64
            ? ~std::uint64_t{0}
            : (std::uint64_t{1} << bits) - 1;
        const std::uint64_t raw = data & mask;
        const std::uint64_t expected = is_unsigned
            ? raw
            : memblock::sign_extend(raw, bits);
        environment.expect_load_data(load, expected);
        if (!environment.enqueue_load(load) || !environment.issue_load(load) ||
            !environment.run_until_complete(256)) {
            std::cerr << "MEMBLOCK_RANDOM_FORWARD_FAIL seed=" << options.seed
                      << " transaction=" << index
                      << " cycle=" << environment.cycle()
                      << " phase=load reason=" << environment.error() << '\n';
            return 1;
        }
        ++operations[op_value];
        ++address_lanes[store.address_lane];
        ++data_lanes[store.data_lane];
        if (is_unsigned) {
            ++unsigned_loads;
        } else {
            ++signed_loads;
        }
    }

    const bool coverage_complete = transaction_count < 16 ||
        (std::all_of(operations.begin(), operations.end(), [](auto count) { return count != 0; }) &&
         std::all_of(address_lanes.begin(), address_lanes.end(), [](auto count) { return count != 0; }) &&
         std::all_of(data_lanes.begin(), data_lanes.end(), [](auto count) { return count != 0; }) &&
         signed_loads != 0 && unsigned_loads != 0);
    if (!coverage_complete) {
        std::cerr << "MEMBLOCK_RANDOM_FORWARD_FAIL seed=" << options.seed
                  << " transaction=" << transaction_count
                  << " cycle=" << environment.cycle()
                  << " reason=incomplete_coverage\n";
        return 1;
    }
    std::cout << "MEMBLOCK_RANDOM_FORWARD_PASS"
              << " seed=" << options.seed
              << " transactions=" << transaction_count
              << " cycle=" << environment.cycle()
              << " stores=" << environment.store_writebacks()
              << " loads=" << environment.writebacks()
              << " store_ops=" << operations[0] << ',' << operations[1] << ','
              << operations[2] << ',' << operations[3]
              << " signed=" << signed_loads
              << " unsigned=" << unsigned_loads
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_random_mixed(int argc, char **argv, const Options &options)
{
    // The constrained coverage prefix consumes most of a short run. Require
    // enough randomized windows afterwards to exercise producer overlap.
    constexpr unsigned minimum_actions = 128;
    if (options.transactions < minimum_actions) {
        std::cerr << "MEMBLOCK_RANDOM_MIXED_FAIL seed=" << options.seed
                  << " transactions=0 cycle=0"
                  << " reason=random-mixed_requires_at_least_"
                  << minimum_actions << "_actions\n";
        return 1;
    }

    memblock::Environment environment(argc, argv);
    std::mt19937_64 random(options.seed ^ 0x510e527fade682d1ULL);
    MixedCoverage coverage;
    std::string phase = "reset";
    unsigned actions = 0;
    std::uint64_t rob_offset = 0;
    std::uint64_t lq_offset = 0;
    std::uint64_t sq_offset = 0;
    constexpr std::uint64_t bare_base = memblock::kDefaultMemoryBase + 0x100000;
    constexpr std::uint64_t cache0_base = memblock::kDefaultMemoryBase + 0x200000;
    constexpr std::uint64_t cache1_base =
        memblock::kDefaultMemoryBase + (std::uint64_t{1} << 30) + 0x200000;
    constexpr std::uint64_t nc_base =
        memblock::kDefaultMemoryBase + (std::uint64_t{2} << 30) + 0x200000;
    constexpr std::uint64_t guest_virtual = 0x60000000ULL;
    constexpr std::uint64_t guest_fault_virtual = 0xa0000000ULL;
    constexpr std::uint64_t guest_physical = 0xb0000000ULL;
    constexpr std::uint64_t host_physical = 0xd0000000ULL;
    constexpr std::uint64_t vs_root = 0x96000000ULL;
    constexpr std::uint64_t g_root = 0x97000000ULL;

    environment.memory().fill_incrementing(bare_base, 0x20000, 0x19);
    environment.memory().fill_incrementing(cache0_base, 0x20000, 0x43);
    environment.memory().fill_incrementing(cache1_base, 0x10000, 0x71);
    environment.memory().fill_incrementing(nc_base, 0x1000, 0xa3);
    environment.memory().fill_incrementing(host_physical, 0x1000, 0xc5);
    environment.configure_backpressure(
        options.seed ^ 0x1f83d9abfb41bd6bULL, options.backpressure);

    auto make_load = [&](std::uint64_t address, memblock::LoadOp op,
                         unsigned lane) {
        const std::uint64_t rob = rob_offset++;
        const std::uint64_t lq = lq_offset++;
        return memblock::LoadTransaction{
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
    };
    auto make_store = [&](std::uint64_t address, std::uint64_t data,
                          memblock::StoreOp op, unsigned address_lane,
                          unsigned data_lane) {
        const std::uint64_t rob = rob_offset++;
        const std::uint64_t sq = sq_offset++;
        return memblock::StoreTransaction{
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
        return transaction;
    };
    auto random_delay = [&](unsigned minimum, unsigned maximum) {
        return minimum + static_cast<unsigned>(random() % (maximum - minimum + 1));
    };
    auto random_bare_address = [&](std::uint64_t region, unsigned alignment,
                                   unsigned span) {
        const std::uint64_t offset = random() % span;
        return region + ((offset / alignment) * alignment);
    };
    auto issue_load = [&](const memblock::LoadTransaction &transaction,
                          std::optional<std::uint64_t> expected = std::nullopt) {
        if (expected) {
            environment.expect_load_data(transaction, *expected);
        } else {
            environment.expect_load(transaction);
        }
        if (!environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction) ||
            !environment.run_until_all_complete(2048)) {
            return false;
        }
        coverage.sample(transaction);
        ++actions;
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
        if (!environment.enqueue_vector(transaction) ||
            !environment.issue_vector(transaction) ||
            !environment.run_until_all_complete(2048)) {
            return false;
        }
        coverage.sample(transaction);
        ++actions;
        return true;
    };
    auto issue_store = [&](const memblock::StoreTransaction &transaction,
                           bool data_first) {
        environment.expect_store(transaction);
        const bool issued = environment.enqueue_store(
                                transaction,
                                memblock::lq_pointer_value(lq_offset)) &&
                            (data_first
                                 ? environment.issue_store_data(transaction) &&
                                       environment.issue_store_address(transaction)
                                 : environment.issue_store_address(transaction) &&
                                       environment.issue_store_data(transaction));
        if (!issued || !environment.run_until_all_complete(1024)) {
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
    auto overlay_vector_store = [&](const memblock::VectorMemoryTransaction &store,
                                    const memblock::VectorMemoryTransaction &load) {
        auto expected = environment.memory().expected_vector_load(load);
        const unsigned bytes = 1U << store.eew;
        const unsigned elements = 16U / bytes;
        const std::uint16_t active = memblock::active_vector_elements(store);
        for (unsigned element = 0; element < elements; ++element) {
            if (((active >> element) & 1U) == 0) {
                continue;
            }
            for (unsigned byte = 0; byte < bytes; ++byte) {
                const unsigned offset = element * bytes + byte;
                expected[offset] = store.data[offset];
            }
        }
        return expected;
    };
    auto randomize_vector_addressing = [&](memblock::VectorMemoryTransaction &transaction) {
        const unsigned element_bytes = 1U << transaction.eew;
        const unsigned elements = 16U >> transaction.eew;
        if (transaction.addressing == memblock::VectorAddressingMode::strided) {
            transaction.stride = static_cast<std::int64_t>(
                element_bytes * (1U + random() % 4));
            return;
        }
        if (transaction.addressing != memblock::VectorAddressingMode::indexed_unordered &&
            transaction.addressing != memblock::VectorAddressingMode::indexed_ordered) {
            return;
        }
        for (unsigned element = 0; element < elements; ++element) {
            const std::uint64_t offset =
                (random() % (64U / element_bytes)) * element_bytes;
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                transaction.index[element * element_bytes + byte] =
                    static_cast<unsigned char>(offset >> (8 * byte));
            }
        }
    };

    const bool completed = [&]() {
        if (!environment.reset() || !environment.enable_misaligned_accesses()) {
            return false;
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
                shape == 1 ? elements : 1U + random() % elements);
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
            !environment.configure_sv39_nc(nc_base, nc_base)) {
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
        const auto nc_store = make_store(
            nc_base + 0x180, 0x13579bdf2468ace0ULL,
            memblock::StoreOp::sd, 1, 0);
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
        constexpr unsigned line_bytes = 64;
        constexpr unsigned dirty_lines = 10;
        constexpr std::uint64_t same_set_stride = dcache_sets * line_bytes;
        constexpr std::uint64_t dirty_base = cache0_base + 0x30000;
        const std::uint64_t release_before = environment.tilelink_release_data();
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
        coverage.dirty_pressure = environment.tilelink_release_data_verified();

        phase = "seeded-mixed-tail";
        const unsigned target_before_redirect = options.transactions - 2;
        // The tail is a constrained-random issue window. Each window contains
        // all five producer classes before any completion drain, so cache,
        // TLB, forwarding, and queue timing can interact in one simulation.
        while (actions + 6 <= target_before_redirect) {
            const std::uint64_t window_base =
                cache0_base + 0x10000 + (random() % 256) * 128;
            const auto required_mode = static_cast<memblock::VectorAddressingMode>(
                coverage.concurrent_windows < 4
                    ? coverage.concurrent_windows
                    : random() % 4);
            auto scalar = make_load(
                window_base + (random() % 8U) * 8U,
                static_cast<memblock::LoadOp>(random() % 7), random() % 3);
            auto vector_load = make_vector(
                false, window_base + 32, random() % 4, random() % 2,
                required_mode);
            auto scalar_store = make_store(
                window_base + (random() % 8U) * 8U, random(),
                static_cast<memblock::StoreOp>(random() % 4), random() % 2,
                random() % 2);
            auto vector_store = make_vector(
                true, window_base + 0x100 + (random() % 8U) * 8U,
                random() % 4, random() % 2,
                required_mode);
            auto prefetch = make_prefetch(
                window_base + 0x180 + (random() % 8U) * 8U,
                static_cast<memblock::PrefetchOp>(8 + random() % 3), random() % 3);
            vector_load.vm = (random() & 1U) != 0;
            vector_load.mask_bits = static_cast<std::uint16_t>(random());
            vector_store.vm = (random() & 1U) != 0;
            vector_store.mask_bits = static_cast<std::uint16_t>(random());
            vector_load.vl = static_cast<std::uint8_t>(
                random() % ((16U >> vector_load.eew) + 1));
            vector_store.vl = static_cast<std::uint8_t>(
                random() % ((16U >> vector_store.eew) + 1));
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

            environment.expect_load(scalar);
            environment.expect_vector(vector_load);
            environment.expect_store(scalar_store);
            environment.expect_vector(vector_store);
            environment.expect_prefetch(prefetch);
            if (!environment.enqueue_load(scalar) ||
                !environment.enqueue_vector(vector_load) ||
                !environment.enqueue_store(
                    scalar_store, memblock::lq_pointer_value(lq_offset)) ||
                !environment.enqueue_vector(vector_store) ||
                !environment.enqueue_prefetch(prefetch)) {
                return false;
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
            if (!environment.run_cycles(random_delay(0, 3))) {
                return false;
            }
            bool scalar_store_data_first = false;
            for (const unsigned issue_class : issue_order) {
                switch (issue_class) {
                case 0:
                    if (!environment.issue_load(scalar, 2048)) {
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
            if (!environment.run_until_all_complete(4096) ||
                !environment.run_cycles(8) ||
                !environment.commit_store(scalar_store, 4096) ||
                !environment.commit_vector_store(vector_store, 4096)) {
                return false;
            }
            const unsigned scalar_bytes =
                1U << static_cast<unsigned>(scalar_store.op);
            const auto scalar_readback = make_load(
                scalar_store.address,
                static_cast<memblock::LoadOp>(scalar_store.op), random() % 3);
            if (!issue_load(
                    scalar_readback,
                    scalar_forward_value(scalar_store.data, scalar_bytes, false))) {
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
            coverage.concurrent_actions += 6;
            for (auto &count : coverage.concurrent_ops) {
                ++count;
            }
            actions += 5;
            coverage.cacheable += 6;
        }
        while (actions < target_before_redirect) {
            const unsigned remaining = target_before_redirect - actions;
            unsigned kind = static_cast<unsigned>(random() % 4);
            if (remaining == 1) {
                kind &= 1U;
            }
            const std::uint64_t address =
                cache0_base + 0x10000 + (random() % 256) * 64;
            if (kind == 0) {
                const auto transaction = make_load(
                    address, static_cast<memblock::LoadOp>(random() % 7),
                    random() % 3);
                if (!issue_load(transaction)) {
                    return false;
                }
                ++coverage.cacheable;
            } else if (kind == 1) {
                const unsigned eew = random() % 4;
                const auto addressing = static_cast<memblock::VectorAddressingMode>(
                    random() % 4);
                auto transaction = make_vector(
                    false, address + ((random() & 1U) ? (1U << eew) : 0),
                    eew, random() % 2, addressing);
                randomize_vector_addressing(transaction);
                const unsigned elements = 16U >> eew;
                transaction.vm = (random() & 1U) != 0;
                transaction.mask_bits = static_cast<std::uint16_t>(random());
                transaction.vl = static_cast<std::uint8_t>(random() % (elements + 1));
                transaction.vstart = transaction.vl == 0
                    ? 0
                    : static_cast<std::uint8_t>(random() % (transaction.vl + 1));
                if (!issue_vector(transaction)) {
                    return false;
                }
                ++coverage.cacheable;
            } else if (kind == 2) {
                const unsigned op = random() % 4;
                const unsigned size = 1U << op;
                const bool is_unsigned = op != 3 && (random() & 1U) != 0;
                const auto store = make_store(
                    address + (8 - size), random(),
                    static_cast<memblock::StoreOp>(op), random() % 2,
                    random() % 2);
                if (!issue_store(store, (random() & 1U) != 0)) {
                    return false;
                }
                const auto load = make_load(
                    store.address,
                    static_cast<memblock::LoadOp>(op + (is_unsigned ? 4U : 0U)),
                    random() % 3);
                if (!issue_load(
                        load, scalar_forward_value(store.data, size, is_unsigned)) ||
                    !environment.commit_store(store)) {
                    return false;
                }
                ++coverage.scalar_forwarding;
                coverage.cacheable += 2;
            } else {
                const unsigned eew = random() % 4;
                const auto addressing = static_cast<memblock::VectorAddressingMode>(
                    random() % 4);
                auto store = make_vector(
                    true, address, eew, random() % 2, addressing);
                randomize_vector_addressing(store);
                const unsigned elements = 16U >> eew;
                store.vm = (random() & 1U) != 0;
                store.mask_bits = static_cast<std::uint16_t>(random());
                store.vl = static_cast<std::uint8_t>(random() % (elements + 1));
                store.vstart = store.vl == 0
                    ? 0
                    : static_cast<std::uint8_t>(random() % (store.vl + 1));
                if (!issue_vector(store)) {
                    return false;
                }
                auto load = make_vector(
                    false, store.address, eew, random() % 2, store.addressing);
                load.vl = store.vl;
                load.vstart = store.vstart;
                load.vm = store.vm;
                load.mask_bits = store.mask_bits;
                load.vma = store.vma;
                load.vta = store.vta;
                load.stride = store.stride;
                load.index = store.index;
                if (!issue_vector(load, overlay_vector_store(store, load)) ||
                    !environment.commit_vector_store(store)) {
                    return false;
                }
                ++coverage.vector_forwarding;
                coverage.cacheable += 2;
            }
        }

        phase = "redirect-recovery";
        const std::uint64_t canceled_rob_offset = rob_offset;
        auto canceled = make_load(
            cache1_base + 0xf800, memblock::LoadOp::ld, 2);
        if (!environment.enqueue_load(canceled) || !environment.issue_load(canceled) ||
            !environment.redirect_after(
                memblock::rob_pointer_value(canceled_rob_offset - 1),
                memblock::rob_pointer_flag(canceled_rob_offset - 1), false) ||
            !environment.run_cycles(96) ||
            !environment.account_lq_cancellation(1)) {
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
            !environment.run_until_queues_retired(4096)) {
            return false;
        }
        coverage.dcache_request_stalls = environment.dcache_request_stalls();
        coverage.dcache_response_delays = environment.dcache_response_delays();
        coverage.ptw_request_stalls = environment.ptw_request_stalls();
        coverage.ptw_response_delays = environment.ptw_response_delays();
        coverage.uncache_request_stalls = environment.uncache_request_stalls();
        coverage.uncache_response_delays = environment.uncache_response_delays();
        if (actions != options.transactions || environment.ptw_requests() < 3 ||
            environment.uncache_requests() < 2 ||
            environment.tilelink_release_data() <= release_before ||
            environment.lq_dequeued() + environment.lq_canceled() !=
                environment.lq_allocated() ||
            environment.sq_dequeued() + environment.sq_canceled() !=
                environment.sq_allocated() ||
            !coverage.complete() ||
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
                  << " ptw=" << environment.ptw_requests()
                  << " uncache=" << environment.uncache_requests()
                  << " release_data=" << environment.tilelink_release_data()
                  << " reason="
                  << (environment.error().empty()
                          ? "mixed_coverage_or_accounting_gate_failed"
                          : environment.error())
                  << ' ' << coverage.summary() << '\n';
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
              << " release_data=" << environment.tilelink_release_data()
              << " ptw_requests=" << environment.ptw_requests()
              << " uncache_requests=" << environment.uncache_requests()
              << " lq=" << environment.lq_dequeued() << '+'
              << environment.lq_canceled() << '/'
              << environment.lq_allocated()
              << " sq=" << environment.sq_dequeued() << '+'
              << environment.sq_canceled() << '/'
              << environment.sq_allocated()
              << ' ' << coverage.summary()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_random_boundary_hunt(int argc, char **argv, const Options &options)
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
            std::cerr << "MEMBLOCK_RANDOM_BOUNDARY_HUNT_FAIL seed="
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
            std::cerr << "MEMBLOCK_RANDOM_BOUNDARY_HUNT_FAIL seed="
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
            std::cerr << "MEMBLOCK_RANDOM_BOUNDARY_HUNT_SAMPLE_FAIL seed="
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
    std::cout << "MEMBLOCK_RANDOM_BOUNDARY_HUNT_"
              << (failures == 0 ? "PASS" : "FAIL")
              << " seed=" << options.seed << " transactions=" << samples
              << " failures=" << failures
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return failures == 0 ? 0 : 1;
}

} // namespace

int main(int argc, char **argv)
{
    try {
        const Options options = parse_options(argc, argv);
        if (options.test == "smoke") {
            return run_smoke(argc, argv);
        }
        if (options.test == "pin-space") {
            return run_pin_space(argc, argv);
        }
        if (options.test == "single-load") {
            return run_single_load(argc, argv);
        }
        if (options.test == "vector-load") {
            return run_vector_load(argc, argv);
        }
        if (options.test == "vector-split-load") {
            return run_vector_split_load(argc, argv);
        }
        if (options.test == "vector-store-forwarding") {
            return run_vector_store_forwarding(argc, argv);
        }
        if (options.test == "random-loads") {
            return run_random_loads(argc, argv, options);
        }
        if (options.test == "random-vector-loads") {
            return run_random_vector_loads(argc, argv, options);
        }
        if (options.test == "random-vector-forwarding") {
            return run_random_vector_forwarding(argc, argv, options);
        }
        if (options.test == "store-forwarding") {
            return run_store_forwarding(argc, argv);
        }
        if (options.test == "dcache-release") {
            return run_dcache_release(argc, argv);
        }
        if (options.test == "store-rdata-order") {
            return run_store_rdata_order(argc, argv);
        }
        if (options.test == "store-tlb-miss-preserve") {
            return run_store_tlb_miss_preserve(argc, argv);
        }
        if (options.test == "redirect") {
            return run_redirect(argc, argv);
        }
        if (options.test == "queue-pressure") {
            return run_queue_pressure(argc, argv);
        }
        if (options.test == "scalar-misaligned") {
            return run_scalar_misaligned(argc, argv);
        }
        if (options.test == "misaligned-stores") {
            return run_misaligned_stores(argc, argv);
        }
        if (options.test == "vector-addressing") {
            return run_vector_addressing(argc, argv);
        }
        if (options.test == "exception-contracts") {
            return run_exception_contracts(argc, argv);
        }
        if (options.test == "two-stage-translation") {
            return run_two_stage_translation(argc, argv);
        }
        if (options.test == "vector-guest-fault") {
            return run_vector_guest_fault(argc, argv);
        }
        if (options.test == "vector-guest-fault-split") {
            return run_vector_guest_fault(argc, argv, true);
        }
        if (options.test == "scalar-guest-fault") {
            return run_scalar_guest_fault(argc, argv);
        }
        if (options.test == "random-forwarding") {
            return run_random_forwarding(argc, argv, options);
        }
        if (options.test == "random-mixed") {
            return run_random_mixed(argc, argv, options);
        }
        if (options.test == "random-boundary-hunt") {
            return run_random_boundary_hunt(argc, argv, options);
        }
        std::cerr << "unknown test: " << options.test << '\n';
        return 2;
    } catch (const std::exception &error) {
        std::cerr << "MEMBLOCK_ARGUMENT_ERROR reason=" << error.what() << '\n';
        return 2;
    }
}
