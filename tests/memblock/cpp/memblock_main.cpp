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
#include <unordered_map>
#include <vector>

namespace {

struct Options {
    std::string_view test = "single-load";
    std::uint64_t seed = 1;
    unsigned transactions = 200;
    bool backpressure = true;
    bool hunt_boundaries = false;
};

// Keep long stress runs reproducible while avoiding accidental correlation
// between transaction shape, payload bytes, and issue scheduling.  The
// derived seeds are part of the scenario contract: changing one stream does
// not silently perturb all other dimensions.
struct StressRandom {
    static std::uint64_t splitmix64(std::uint64_t value)
    {
        value += 0x9e3779b97f4a7c15ULL;
        value = (value ^ (value >> 30)) * 0xbf58476d1ce4e5b9ULL;
        value = (value ^ (value >> 27)) * 0x94d049bb133111ebULL;
        return value ^ (value >> 31);
    }

    explicit StressRandom(std::uint64_t seed)
        : traffic(splitmix64(seed ^ 0x243f6a8885a308d3ULL)),
          shape(splitmix64(seed ^ 0x13198a2e03707344ULL)),
          payload(splitmix64(seed ^ 0xa4093822299f31d0ULL)),
          scheduler(splitmix64(seed ^ 0x082efa98ec4e6c89ULL))
    {}

    std::uint64_t operator()() { return traffic(); }
    std::uint64_t next_shape() { return shape(); }
    std::uint64_t next_payload() { return payload(); }
    std::uint64_t next_schedule() { return scheduler(); }

    std::mt19937_64 traffic;
    std::mt19937_64 shape;
    std::mt19937_64 payload;
    std::mt19937_64 scheduler;
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
    std::array<std::uint64_t, memblock::generated::kLsqEnqueueLanes>
        dispatch_widths{};
    std::array<std::uint64_t, memblock::generated::kLsqEnqueueLanes>
        dispatch_lanes{};
    std::array<std::uint64_t, 7> load_ops{};
    std::array<std::uint64_t, 4> store_ops{};
    std::array<std::uint64_t, memblock::kScalarLoadLanes> load_lanes{};
    std::array<std::uint64_t, memblock::kScalarStoreLanes> address_lanes{};
    std::array<std::uint64_t, memblock::kScalarStoreLanes> data_lanes{};
    std::array<std::uint64_t, 4> vector_load_eews{};
    std::array<std::uint64_t, 4> vector_store_eews{};
    std::array<std::uint64_t, 4> vector_load_address_modes{};
    std::array<std::uint64_t, 4> vector_store_address_modes{};
    std::array<std::uint64_t, 3> vector_load_stride_signs{};
    std::array<std::uint64_t, 2> vector_store_stride_signs{};
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
    std::uint64_t rvc = 0;
    std::uint64_t non_rvc = 0;
    std::uint64_t ftq_nonzero = 0;
    std::uint64_t store_set_hit = 0;
    std::uint64_t store_set_miss = 0;
    std::uint64_t load_wait = 0;
    std::uint64_t strict_load_wait = 0;

    void sample(const memblock::LoadTransaction &transaction)
    {
        ++scalar_loads;
        ++load_ops.at(static_cast<unsigned>(transaction.op));
        ++load_lanes.at(transaction.lane);
        ++(transaction.predecode_rvc ? rvc : non_rvc);
        ftq_nonzero += transaction.ftq_ptr != 0 || transaction.ftq_offset != 0;
        ++(transaction.store_set_hit ? store_set_hit : store_set_miss);
        load_wait += transaction.load_wait_bit;
        strict_load_wait += transaction.load_wait_strict;
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
        if (transaction.addressing == memblock::VectorAddressingMode::strided) {
            if (transaction.store) {
                ++vector_store_stride_signs.at(transaction.stride < 0 ? 0 : 1);
            } else {
                ++vector_load_stride_signs.at(
                    transaction.stride < 0 ? 0 : (transaction.stride == 0 ? 1 : 2));
            }
        }
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
        return all_nonzero(dispatch_widths) && all_nonzero(dispatch_lanes) &&
               all_nonzero(load_ops) && all_nonzero(store_ops) &&
               all_nonzero(load_lanes) && all_nonzero(address_lanes) &&
               all_nonzero(data_lanes) && all_nonzero(vector_load_eews) &&
               all_nonzero(vector_store_eews) && all_nonzero(prefetch_ops) &&
               all_nonzero(vector_lanes) &&
               all_nonzero(vector_load_address_modes) &&
               all_nonzero(vector_store_address_modes) &&
               all_nonzero(vector_load_stride_signs) &&
               all_nonzero(vector_store_stride_signs) &&
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
               max_unresolved_classes >= 2 && rvc != 0 && non_rvc != 0 &&
               ftq_nonzero != 0 && store_set_hit != 0 && store_set_miss != 0 &&
               load_wait != 0;
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
        return "dispatch_widths=" + std::to_string(dispatch_widths[0]) + ',' +
               std::to_string(dispatch_widths[1]) + ',' +
               std::to_string(dispatch_widths[2]) + ',' +
               std::to_string(dispatch_widths[3]) + ',' +
               std::to_string(dispatch_widths[4]) + ',' +
               std::to_string(dispatch_widths[5]) + " dispatch_lanes=" +
               std::to_string(dispatch_lanes[0]) + ',' +
               std::to_string(dispatch_lanes[1]) + ',' +
               std::to_string(dispatch_lanes[2]) + ',' +
               std::to_string(dispatch_lanes[3]) + ',' +
               std::to_string(dispatch_lanes[4]) + ',' +
               std::to_string(dispatch_lanes[5]) + " load_ops=" +
               std::to_string(load_ops[0]) + ',' +
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
               std::to_string(vector_store_address_modes[3]) +
               " vec_load_stride=" +
               std::to_string(vector_load_stride_signs[0]) + ',' +
               std::to_string(vector_load_stride_signs[1]) + ',' +
               std::to_string(vector_load_stride_signs[2]) +
               " vec_store_stride=" +
               std::to_string(vector_store_stride_signs[0]) + ',' +
               std::to_string(vector_store_stride_signs[1]) + " metadata=" +
               std::to_string(rvc) + ',' + std::to_string(non_rvc) + ',' +
               std::to_string(ftq_nonzero) + ',' + std::to_string(store_set_hit) +
               ',' + std::to_string(store_set_miss) + ',' +
               std::to_string(load_wait) + ',' + std::to_string(strict_load_wait) +
               " prefetch=" +
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

struct StressCoverage {
    std::array<std::uint64_t, 7> load_ops{};
    std::array<std::uint64_t, 4> store_ops{};
    std::array<std::uint64_t, 3> load_lanes{};
    std::array<std::uint64_t, 2> address_lanes{};
    std::array<std::uint64_t, 2> data_lanes{};
    std::array<std::uint64_t, 2> store_order{};
    std::array<std::uint64_t, 4> vector_load_eews{};
    std::array<std::uint64_t, 4> vector_store_eews{};
    std::array<std::uint64_t, 4> vector_load_modes{};
    std::array<std::uint64_t, 4> vector_store_modes{};
    std::array<std::uint64_t, 2> vector_lanes{};
    std::array<std::uint64_t, 3> prefetch_ops{};
    std::uint64_t masked = 0;
    std::uint64_t unmasked = 0;
    std::uint64_t zero_vstart = 0;
    std::uint64_t nonzero_vstart = 0;
    std::uint64_t full_vl = 0;
    std::uint64_t partial_vl = 0;
    std::uint64_t aligned = 0;
    std::uint64_t split = 0;
    std::uint64_t scalar_misaligned = 0;
    std::uint64_t scalar_forwarding = 0;
    std::uint64_t vector_forwarding = 0;
    std::uint64_t waves = 0;
    std::uint64_t actions = 0;
    std::uint64_t max_outstanding = 0;
    std::uint64_t dcache_hits = 0;
    std::uint64_t dcache_misses = 0;
    std::uint64_t memory_regions = 0;
    std::array<std::uint64_t, 4> combinations{};
    std::uint64_t dcache_request_stalls = 0;
    std::uint64_t dcache_response_delays = 0;
    std::uint64_t ptw_request_stalls = 0;
    std::uint64_t ptw_response_delays = 0;
    std::uint64_t uncache_request_stalls = 0;
    std::uint64_t uncache_response_delays = 0;
    std::uint64_t rvc = 0;
    std::uint64_t non_rvc = 0;
    std::uint64_t ftq_nonzero = 0;
    std::uint64_t store_set_hit = 0;
    std::uint64_t store_set_miss = 0;
    std::uint64_t load_wait = 0;
    std::uint64_t strict_load_wait = 0;

    void sample(
        const memblock::LoadTransaction &transaction,
        std::uint64_t requests_before,
        std::uint64_t requests_after)
    {
        ++load_ops.at(static_cast<unsigned>(transaction.op));
        ++load_lanes.at(transaction.lane);
        ++(transaction.predecode_rvc ? rvc : non_rvc);
        ftq_nonzero += transaction.ftq_ptr != 0 || transaction.ftq_offset != 0;
        ++(transaction.store_set_hit ? store_set_hit : store_set_miss);
        load_wait += transaction.load_wait_bit;
        strict_load_wait += transaction.load_wait_strict;
        const unsigned size = 1U << (static_cast<unsigned>(transaction.op) & 3U);
        scalar_misaligned += (transaction.address & (size - 1)) != 0;
        ++(requests_after == requests_before ? dcache_hits : dcache_misses);
    }

    void sample(const memblock::StoreTransaction &transaction, bool data_first)
    {
        ++store_ops.at(static_cast<unsigned>(transaction.op));
        ++address_lanes.at(transaction.address_lane);
        ++data_lanes.at(transaction.data_lane);
        ++store_order.at(data_first ? 1 : 0);
        const unsigned size = 1U << static_cast<unsigned>(transaction.op);
        scalar_misaligned += (transaction.address & (size - 1)) != 0;
    }

    void sample(
        const memblock::VectorMemoryTransaction &transaction,
        std::uint64_t requests_before,
        std::uint64_t requests_after)
    {
        auto &eews = transaction.store ? vector_store_eews : vector_load_eews;
        auto &modes = transaction.store ? vector_store_modes : vector_load_modes;
        ++eews.at(transaction.eew);
        ++modes.at(static_cast<unsigned>(transaction.addressing));
        ++vector_lanes.at(transaction.lane);
        ++(transaction.vm ? unmasked : masked);
        ++(transaction.vstart == 0 ? zero_vstart : nonzero_vstart);
        const unsigned elements = 16U >> transaction.eew;
        ++(transaction.vl == elements ? full_vl : partial_vl);
        ++((transaction.address & 15U) == 0 ? aligned : split);
        ++(requests_after == requests_before ? dcache_hits : dcache_misses);
    }

    void sample(const memblock::PrefetchTransaction &transaction)
    {
        ++prefetch_ops.at(static_cast<unsigned>(transaction.op) - 8);
    }

    bool complete() const
    {
        const auto all_nonzero = [](const auto &values) {
            return std::all_of(
                values.begin(), values.end(), [](auto value) { return value != 0; });
        };
        const bool load_modes_without_ordered = vector_load_modes[0] != 0 &&
            vector_load_modes[1] != 0 && vector_load_modes[2] != 0;
        const bool store_modes_without_ordered = vector_store_modes[0] != 0 &&
            vector_store_modes[1] != 0 && vector_store_modes[2] != 0;
        return all_nonzero(load_ops) && all_nonzero(store_ops) &&
               all_nonzero(load_lanes) && all_nonzero(address_lanes) &&
               all_nonzero(data_lanes) && all_nonzero(store_order) &&
               all_nonzero(vector_load_eews) &&
               all_nonzero(vector_store_eews) && load_modes_without_ordered &&
               store_modes_without_ordered &&
               all_nonzero(vector_lanes) &&
               all_nonzero(prefetch_ops) && masked != 0 && unmasked != 0 &&
               zero_vstart != 0 && nonzero_vstart != 0 && full_vl != 0 &&
               partial_vl != 0 && aligned != 0 && split != 0 &&
               scalar_misaligned != 0 && scalar_forwarding != 0 &&
               vector_forwarding != 0 && waves >= 4 && max_outstanding >= 10 &&
               dcache_hits != 0 && dcache_misses != 0 && memory_regions >= 2 &&
               all_nonzero(combinations) && rvc != 0 && non_rvc != 0 &&
               ftq_nonzero != 0 && store_set_hit != 0 && store_set_miss != 0 &&
               load_wait != 0;
    }

    bool backpressure_complete(bool required) const
    {
        return !required ||
               (dcache_request_stalls != 0 && dcache_response_delays != 0);
    }

    std::string summary() const
    {
        auto csv = [](const auto &values) {
            std::string result;
            for (std::size_t index = 0; index < values.size(); ++index) {
                result += (index == 0 ? "" : ",") + std::to_string(values[index]);
            }
            return result;
        };
        return "stress_load_ops=" + csv(load_ops) +
               " stress_rng_streams=4" +
               " stress_store_ops=" + csv(store_ops) +
               " stress_load_lanes=" + csv(load_lanes) +
               " stress_address_lanes=" + csv(address_lanes) +
               " stress_data_lanes=" + csv(data_lanes) +
               " stress_store_order=" + csv(store_order) +
               " stress_eew_load=" + csv(vector_load_eews) +
               " stress_eew_store=" + csv(vector_store_eews) +
               " stress_vec_load_modes=" + csv(vector_load_modes) +
               " stress_vec_store_modes=" + csv(vector_store_modes) +
               " stress_vec_lanes=" + csv(vector_lanes) +
               " stress_prefetch=" + csv(prefetch_ops) +
               " stress_masked=" + std::to_string(masked) +
               " stress_unmasked=" + std::to_string(unmasked) +
               " stress_vstart=" + std::to_string(zero_vstart) + "," +
               std::to_string(nonzero_vstart) +
               " stress_vl=" + std::to_string(full_vl) + "," +
               std::to_string(partial_vl) +
               " stress_alignment=" + std::to_string(aligned) + "," +
               std::to_string(split) +
               " stress_metadata=" + std::to_string(rvc) + "," +
               std::to_string(non_rvc) + "," + std::to_string(ftq_nonzero) +
               "," + std::to_string(store_set_hit) + "," +
               std::to_string(store_set_miss) + "," +
               std::to_string(load_wait) + "," +
               std::to_string(strict_load_wait) +
               " stress_misaligned=" + std::to_string(scalar_misaligned) +
               " stress_forwarding=" + std::to_string(scalar_forwarding) + "," +
               std::to_string(vector_forwarding) +
               " stress_waves=" + std::to_string(waves) +
               " stress_actions=" + std::to_string(actions) +
               " stress_max_outstanding=" + std::to_string(max_outstanding) +
               " stress_dcache=" + std::to_string(dcache_hits) + "," +
               std::to_string(dcache_misses) +
               " stress_regions=" + std::to_string(memory_regions) +
               " stress_combinations=" + csv(combinations) +
               " stress_backpressure=" + std::to_string(dcache_request_stalls) + "," +
               std::to_string(dcache_response_delays) + "," +
               std::to_string(ptw_request_stalls) + "," +
               std::to_string(ptw_response_delays) + "," +
               std::to_string(uncache_request_stalls) + "," +
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

int run_fp_loads(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x18000;
    environment.memory().fill_incrementing(base, 64, 0x5b);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_FP_LOADS_FAIL cycle=" << environment.cycle()
                  << " phase=reset reason=" << environment.error() << '\n';
        return 1;
    }

    // The MemBlock boundary carries destination-class enables separately from
    // the width opcode.  Exercise both narrow and wide FP writebacks and
    // explicitly require that the integer register file remains untouched.
    const std::array<memblock::LoadTransaction, 2> transactions{{
        {
            .address = base + 12,
            .op = memblock::LoadOp::lw,
            .rob = 12,
            .lq = 0,
            .pdest = 17,
            .lane = 0,
            .rf_wen = false,
            .fp_wen = true,
        },
        {
            .address = base + 24,
            .op = memblock::LoadOp::ld,
            .rob = 13,
            .lq = 1,
            .pdest = 19,
            .lane = 1,
            .rf_wen = false,
            .fp_wen = true,
        },
    }};
    for (const auto &transaction : transactions) {
        const auto raw = environment.memory().expected_load(
            transaction.address, transaction.op);
        const auto expected = transaction.op == memblock::LoadOp::lw
            ? (std::uint64_t{0xffffffff00000000ULL} | (raw & 0xffffffffULL))
            : raw;
        environment.expect_load_data(transaction, expected);
        if (!environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 512) ||
            !environment.run_until_complete(2048)) {
            std::cerr << "MEMBLOCK_FP_LOADS_FAIL cycle=" << environment.cycle()
                      << " phase=completion reason=" << environment.error() << '\n';
            return 1;
        }
    }
    if (environment.writebacks() != transactions.size()) {
        std::cerr << "MEMBLOCK_FP_LOADS_FAIL cycle=" << environment.cycle()
                  << " phase=writeback-count expected=" << transactions.size()
                  << " actual=" << environment.writebacks() << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_FP_LOADS_PASS"
              << " cycle=" << environment.cycle()
              << " writebacks=" << environment.writebacks()
              << " fp_destinations=" << transactions.size()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_trigger_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x2a000;
    constexpr std::uint64_t address = base + 0x28;
    environment.memory().fill_incrementing(base, 64, 0x62);
    if (!environment.reset() ||
        !environment.configure_memory_trigger(
            0, address, 0, true, false)) {
        std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    // TriggerAction.BreakpointExp is encoded as zero.  It must appear in the
    // load writeback exception vector and suppress the destination write.
    const memblock::LoadTransaction transaction{
        .address = address,
        .op = memblock::LoadOp::ld,
        .rob = 33,
        .lq = 0,
        .sq = 0,
        .pdest = 23,
        .lane = 0,
        .expected_exception_mask = memblock::kExceptionBreakpoint,
        .expected_trigger = 0,
        .predecode_rvc = true,
        .ftq_ptr = 7,
        .ftq_offset = 2,
    };
    environment.expect_load(transaction);
    if (!environment.enqueue_load(transaction) ||
        !environment.issue_load(transaction) ||
        !environment.run_until_complete(512)) {
        std::cerr << "MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=breakpoint-load reason="
                  << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_TRIGGER_CONTRACTS_PASS"
              << " cycle=" << environment.cycle()
              << " breakpoint_loads=" << environment.writebacks()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_metadata_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x2b000;
    const std::uint64_t address = base + 0x10;
    environment.memory().fill_incrementing(base, 64, 0x37);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_METADATA_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction transaction{
        .address = address,
        .op = memblock::LoadOp::ld,
        .rob = 34,
        .lq = 1,
        .sq = 0,
        .pdest = 24,
        .lane = 1,
        // The top-level issueLda contract does not expose exceptionVec: LSQ
        // retains the enqueue copy for its own exception machinery, while
        // LoadUnit receives a separately shaped issue uop.  Keep this runtime
        // contract focused on the observable RVC/FTQ issue metadata.
        .expected_exception_mask = 0,
        .input_exception_mask = 0,
        .predecode_rvc = true,
        .ftq_ptr = 9,
        .ftq_offset = 3,
        .store_set_hit = true,
        .wait_for_rob_flag = false,
        .wait_for_rob_value = 0,
        .load_wait_bit = true,
        .load_wait_strict = true,
    };
    environment.expect_load(transaction);
    if (!environment.enqueue_load(transaction) ||
        !environment.issue_load(transaction) ||
        !environment.run_until_complete(512)) {
        std::cerr << "MEMBLOCK_METADATA_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=exception-vector reason="
                  << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_METADATA_CONTRACTS_PASS"
              << " cycle=" << environment.cycle()
              << " issue_metadata=rvc-ftq-store-set-wait"
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_dcache_errors(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x2c000;
    environment.memory().fill_incrementing(base, 0x1000, 0x49);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle=" << environment.cycle()
                  << " phase=reset reason=" << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction denied{
        .address = base + 0x100,
        .op = memblock::LoadOp::ld,
        .rob = 35,
        .lq = 2,
        .pdest = 25,
        .lane = 0,
        .expected_exception_mask = memblock::kExceptionLoadAccessFault,
    };
    environment.expect_load(denied);
    environment.inject_next_dcache_response_error(true, false);
    if (!environment.set_rob_head(denied.rob, denied.rob_flag) ||
        !environment.enqueue_load(denied) ||
        !environment.issue_load(denied) ||
        !environment.run_until_complete(1024) ||
        !environment.run_cycles(8) ||
        !environment.redirect_after(denied.rob, denied.rob_flag, true) ||
        !environment.run_cycles(96) ||
        !environment.account_lq_cancellation(1)) {
        std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle=" << environment.cycle()
                  << " phase=denied reason=" << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction corrupt{
        .address = base + 0x200,
        .op = memblock::LoadOp::ld,
        .rob = 36,
        .lq = 3,
        .pdest = 26,
        .lane = 1,
        .expected_exception_mask = memblock::kExceptionHardwareError,
    };
    environment.expect_load(corrupt);
    environment.inject_next_dcache_response_error(false, true);
    if (!environment.set_rob_head(corrupt.rob, corrupt.rob_flag) ||
        !environment.enqueue_load(corrupt) ||
        !environment.issue_load(corrupt) ||
        !environment.run_until_complete(1024) ||
        !environment.run_cycles(8) ||
        !environment.redirect_after(corrupt.rob, corrupt.rob_flag, true) ||
        !environment.run_cycles(96) ||
        !environment.account_lq_cancellation(1)) {
        std::cerr << "MEMBLOCK_DCACHE_ERRORS_FAIL cycle=" << environment.cycle()
                  << " phase=corrupt reason=" << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_DCACHE_ERRORS_PASS"
              << " cycle=" << environment.cycle()
              << " denied=1 corrupt=1"
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_atomic_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t address = memblock::kDefaultMemoryBase + 0x2e000;
    environment.memory().fill_incrementing(address & ~std::uint64_t{63}, 64, 0x2d);
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }

    std::uint64_t model_value = environment.memory().expected_load(
        address, memblock::LoadOp::ld);
    const std::array<std::pair<memblock::AtomicOp, std::uint64_t>, 9> operations{{
        {memblock::AtomicOp::amoadd_d, 0x1020304050607080ULL},
        {memblock::AtomicOp::amoxor_d, 0x00ff00ff00ff00ffULL},
        {memblock::AtomicOp::amoand_d, 0xf0ffffffffffffffULL},
        {memblock::AtomicOp::amoor_d, 0x0000000000001200ULL},
        {memblock::AtomicOp::amoswap_d, 0x8899aabbccddeeffULL},
        {memblock::AtomicOp::amomin_d, 0x7fffffffffffffffULL},
        {memblock::AtomicOp::amomax_d, 0x8000000000000000ULL},
        {memblock::AtomicOp::amominu_d, 0x0000000000000011ULL},
        {memblock::AtomicOp::amomaxu_d, 0xfffffffffffffff0ULL},
    }};
    for (std::size_t index = 0; index < operations.size(); ++index) {
        const auto [op, operand] = operations[index];
        const std::uint8_t atomic_rob = static_cast<std::uint8_t>(index * 2);
        const memblock::AtomicTransaction atomic{
            .address = address,
            .op = op,
            .data = operand,
            .rob = atomic_rob,
            .pdest = static_cast<std::uint8_t>(62 + index),
            .address_lane = static_cast<unsigned>(index & 1U),
            .data_lane = static_cast<unsigned>(index & 1U),
        };
        const memblock::LoadTransaction old_value_wb{
            .address = address,
            .op = memblock::LoadOp::ld,
            .rob = atomic.rob,
            .pdest = atomic.pdest,
            .lane = 0,
            .rf_wen = true,
        };
        environment.expect_load_data(old_value_wb, model_value);
        if (!environment.issue_atomic(atomic, 1024) ||
            !environment.run_until_complete(8192)) {
            std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=amo-writeback index="
                      << index << " reason=" << environment.error() << '\n';
            return 1;
        }

        switch (op) {
        case memblock::AtomicOp::amoadd_d:
            model_value += operand;
            break;
        case memblock::AtomicOp::amoxor_d:
            model_value ^= operand;
            break;
        case memblock::AtomicOp::amoand_d:
            model_value &= operand;
            break;
        case memblock::AtomicOp::amoor_d:
            model_value |= operand;
            break;
        case memblock::AtomicOp::amoswap_d:
            model_value = operand;
            break;
        case memblock::AtomicOp::amomin_d:
            model_value = static_cast<std::int64_t>(model_value) <
                    static_cast<std::int64_t>(operand)
                ? model_value
                : operand;
            break;
        case memblock::AtomicOp::amomax_d:
            model_value = static_cast<std::int64_t>(model_value) >
                    static_cast<std::int64_t>(operand)
                ? model_value
                : operand;
            break;
        case memblock::AtomicOp::amominu_d:
            model_value = std::min(model_value, operand);
            break;
        case memblock::AtomicOp::amomaxu_d:
            model_value = std::max(model_value, operand);
            break;
        default:
            break;
        }

        const memblock::LoadTransaction readback{
            .address = address,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(atomic.rob + 1),
            .lq = static_cast<std::uint8_t>(index),
            .pdest = static_cast<std::uint8_t>(80 + index),
            .lane = 1,
        };
        environment.expect_load_data(readback, model_value);
        if (!environment.set_rob_head(readback.rob) ||
            !environment.enqueue_load(readback) ||
            !environment.issue_load(readback, 1024) ||
            !environment.run_until_complete(8192) ||
            !environment.run_until_lq_retired(1024)) {
            std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=amo-readback index="
                      << index << " reason=" << environment.error() << '\n';
            return 1;
        }
    }

    const std::uint64_t word_address = address + 8;

    // Atomics have no split/replay path: natural alignment is checked when
    // the DTLB response arrives.  Exercise every byte offset forbidden by
    // each operand width and require an exceptional writeback without any
    // DCache transaction or architectural register write.
    const std::uint64_t atomic_requests_before_misaligned =
        environment.tilelink_requests();
    auto check_misaligned_atomic = [&](std::uint64_t misaligned_address,
                                       memblock::AtomicOp op,
                                       std::uint64_t data,
                                       std::uint8_t rob,
                                       std::uint8_t pdest,
                                       unsigned lane,
                                       const char *phase) {
        const memblock::AtomicTransaction atomic{
            .address = misaligned_address,
            .op = op,
            .data = data,
            .rob = rob,
            .pdest = pdest,
            .address_lane = lane,
            .data_lane = lane,
        };
        const memblock::LoadTransaction writeback{
            .address = atomic.address,
            .op = memblock::LoadOp::ld,
            .rob = atomic.rob,
            .pdest = atomic.pdest,
            .lane = lane,
            .expected_exception_mask = memblock::kExceptionStoreAddressMisaligned,
        };
        environment.expect_load_data(writeback, 0);
        return environment.issue_atomic(atomic, 1024) &&
            environment.run_until_complete(8192) &&
            environment.tilelink_requests() == atomic_requests_before_misaligned;
    };
    for (unsigned offset = 1; offset < 8; ++offset) {
        if (!check_misaligned_atomic(
                address + offset, memblock::AtomicOp::amoadd_d,
                0x1122334455667788ULL ^ offset,
                static_cast<std::uint8_t>(18 + offset),
                static_cast<std::uint8_t>(118 + offset), offset & 1U,
                "misaligned-d")) {
            std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=misaligned-d offset="
                      << offset << " reason=" << environment.error()
                      << " dcache_requests=" << environment.tilelink_requests()
                      << " expected=" << atomic_requests_before_misaligned << '\n';
            return 1;
        }
    }
    for (unsigned offset = 1; offset < 4; ++offset) {
        if (!check_misaligned_atomic(
                word_address + offset, memblock::AtomicOp::amoor_w,
                0xa5a55a5aU ^ offset,
                static_cast<std::uint8_t>(26 + offset),
                static_cast<std::uint8_t>(126 + offset), offset & 1U,
                "misaligned-w")) {
            std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=misaligned-w offset="
                      << offset << " reason=" << environment.error()
                      << " dcache_requests=" << environment.tilelink_requests()
                      << " expected=" << atomic_requests_before_misaligned << '\n';
            return 1;
        }
    }

    // Exercise the complete W-width ALU family independently from the
    // D-width sequence.  Atomic W results are sign-extended to XLEN, while
    // only the addressed 32-bit word is updated in the cache line.
    std::uint64_t word_line_value = environment.memory().expected_load(
        word_address, memblock::LoadOp::ld);
    std::uint32_t word_value = static_cast<std::uint32_t>(
        word_line_value);
    const std::array<std::pair<memblock::AtomicOp, std::uint32_t>, 9>
        word_operations{{
            {memblock::AtomicOp::amoadd_w, 0x10203040U},
            {memblock::AtomicOp::amoxor_w, 0x00ff00ffU},
            {memblock::AtomicOp::amoand_w, 0xf0ffffffU},
            {memblock::AtomicOp::amoor_w, 0x00001200U},
            {memblock::AtomicOp::amoswap_w, 0x8899aabBU},
            {memblock::AtomicOp::amomin_w, 0x7fffffffU},
            {memblock::AtomicOp::amomax_w, 0x80000000U},
            {memblock::AtomicOp::amominu_w, 0x00000011U},
            {memblock::AtomicOp::amomaxu_w, 0xfffffff0U},
        }};
    for (std::size_t index = 0; index < word_operations.size(); ++index) {
        const auto [op, operand] = word_operations[index];
        const std::uint8_t atomic_rob = static_cast<std::uint8_t>(32 + index * 2);
        const memblock::AtomicTransaction atomic{
            .address = word_address,
            .op = op,
            .data = operand,
            .rob = atomic_rob,
            .pdest = static_cast<std::uint8_t>(102 + index),
            .address_lane = static_cast<unsigned>(index & 1U),
            .data_lane = static_cast<unsigned>(index & 1U),
        };
        const std::uint64_t old_value = memblock::sign_extend(word_value, 32);
        const memblock::LoadTransaction old_value_wb{
            .address = word_address,
            .op = memblock::LoadOp::ld,
            .rob = atomic.rob,
            .pdest = atomic.pdest,
            .lane = 0,
            .rf_wen = true,
        };
        environment.expect_load_data(old_value_wb, old_value);
        if (!environment.issue_atomic(atomic, 1024) ||
            !environment.run_until_complete(8192)) {
            std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=amo-w-writeback index="
                      << index << " reason=" << environment.error() << '\n';
            return 1;
        }

        const std::int32_t signed_old = static_cast<std::int32_t>(word_value);
        const std::int32_t signed_operand = static_cast<std::int32_t>(operand);
        switch (op) {
        case memblock::AtomicOp::amoadd_w:
            word_value = static_cast<std::uint32_t>(word_value + operand);
            break;
        case memblock::AtomicOp::amoxor_w:
            word_value ^= operand;
            break;
        case memblock::AtomicOp::amoand_w:
            word_value &= operand;
            break;
        case memblock::AtomicOp::amoor_w:
            word_value |= operand;
            break;
        case memblock::AtomicOp::amoswap_w:
            word_value = operand;
            break;
        case memblock::AtomicOp::amomin_w:
            word_value = signed_old < signed_operand ? word_value : operand;
            break;
        case memblock::AtomicOp::amomax_w:
            word_value = signed_old > signed_operand ? word_value : operand;
            break;
        case memblock::AtomicOp::amominu_w:
            word_value = std::min(word_value, operand);
            break;
        case memblock::AtomicOp::amomaxu_w:
            word_value = std::max(word_value, operand);
            break;
        default:
            break;
        }
        const memblock::LoadTransaction readback{
            .address = word_address,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(atomic.rob + 1),
            .lq = static_cast<std::uint8_t>(operations.size() + index),
            .pdest = static_cast<std::uint8_t>(120 + index),
            .lane = 1,
        };
        word_line_value = (word_line_value & ~std::uint64_t{0xffffffff}) |
            static_cast<std::uint64_t>(word_value);
        environment.expect_load_data(readback, word_line_value);
        if (!environment.set_rob_head(readback.rob) ||
            !environment.enqueue_load(readback) ||
            !environment.issue_load(readback, 1024) ||
            !environment.run_until_complete(8192) ||
            !environment.run_until_lq_retired(1024)) {
            std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=amo-w-readback index="
                      << index << " reason=" << environment.error() << '\n';
            return 1;
        }
    }

    auto run_compare_swap = [&](std::uint64_t cas_address,
                                memblock::AtomicOp cas_op,
                                std::uint64_t &line_value,
                                std::uint64_t compare,
                                std::uint64_t swap,
                                std::uint8_t atomic_rob,
                                std::uint8_t pdest,
                                std::uint8_t lq,
                                bool word) {
        const std::uint64_t old_value = word
            ? memblock::sign_extend(static_cast<std::uint32_t>(line_value), 32)
            : line_value;
        const memblock::AtomicTransaction atomic{
            .address = cas_address,
            .op = cas_op,
            .data = swap,
            .compare = compare,
            .rob = atomic_rob,
            .pdest = pdest,
            .address_lane = 0,
            .data_lane = 0,
        };
        const memblock::LoadTransaction old_value_wb{
            .address = cas_address,
            .op = memblock::LoadOp::ld,
            .rob = atomic.rob,
            .pdest = atomic.pdest,
            .lane = 0,
            .rf_wen = true,
        };
        environment.expect_load_data(old_value_wb, old_value);
        if (!environment.issue_atomic(atomic, 1024) ||
            !environment.run_until_complete(8192)) {
            return false;
        }
        const bool match = word
            ? static_cast<std::uint32_t>(line_value) ==
                  static_cast<std::uint32_t>(compare)
            : line_value == compare;
        if (match) {
            line_value = word
                ? ((line_value & ~std::uint64_t{0xffffffff}) |
                   static_cast<std::uint32_t>(swap))
                : swap;
        }
        const memblock::LoadTransaction readback{
            .address = cas_address,
            .op = memblock::LoadOp::ld,
            .rob = static_cast<std::uint8_t>(atomic.rob + 1),
            .lq = lq,
            .pdest = static_cast<std::uint8_t>(pdest + 1),
            .lane = 1,
        };
        environment.expect_load_data(readback, line_value);
        return environment.set_rob_head(readback.rob) &&
            environment.enqueue_load(readback) &&
            environment.issue_load(readback, 1024) &&
            environment.run_until_complete(8192) &&
            environment.run_until_lq_retired(1024);
    };

    const std::uint64_t word_compare = word_value;
    if (!run_compare_swap(
            word_address, memblock::AtomicOp::amocas_w, word_line_value,
            word_compare, 0x2468ace0U, 50, 140, 18, true) ||
        !run_compare_swap(
            word_address, memblock::AtomicOp::amocas_w, word_line_value,
            static_cast<std::uint32_t>(word_compare + 1), 0x13579bdfU,
            52, 142, 19, true)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=amocas-w reason="
                  << environment.error() << '\n';
        return 1;
    }
    word_value = static_cast<std::uint32_t>(word_line_value);
    if (!run_compare_swap(
            address, memblock::AtomicOp::amocas_d, model_value,
            model_value, 0x0123456789abcdefULL, 54, 144, 20, false) ||
        !run_compare_swap(
            address, memblock::AtomicOp::amocas_d, model_value,
            model_value ^ 1U, 0xfedcba9876543210ULL, 56, 146, 21, false)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=amocas-d reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::AtomicTransaction lr_word{
        .address = word_address,
        .op = memblock::AtomicOp::lr_w,
        .rob = 58,
        .pdest = 148,
        .address_lane = 0,
        .data_lane = 0,
    };
    const memblock::LoadTransaction lr_word_wb{
        .address = word_address, .op = memblock::LoadOp::ld,
        .rob = lr_word.rob, .pdest = lr_word.pdest, .lane = 0,
        .rf_wen = true,
    };
    environment.expect_load_data(
        lr_word_wb, memblock::sign_extend(word_value, 32));
    if (!environment.issue_atomic(lr_word, 1024) ||
        !environment.run_until_complete(8192)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=lr-w reason="
                  << environment.error() << '\n';
        return 1;
    }
    constexpr std::uint32_t word_reservation_value = 0x13579bdfU;
    const memblock::AtomicTransaction sc_word{
        .address = word_address,
        .op = memblock::AtomicOp::sc_w,
        .data = word_reservation_value,
        .rob = 59,
        .pdest = 149,
        .address_lane = 1,
        .data_lane = 1,
    };
    const memblock::LoadTransaction sc_word_wb{
        .address = word_address, .op = memblock::LoadOp::ld,
        .rob = sc_word.rob, .pdest = sc_word.pdest, .lane = 0,
        .rf_wen = true,
    };
    environment.expect_load_data(sc_word_wb, 0);
    if (!environment.issue_atomic(sc_word, 1024) ||
        !environment.run_until_complete(8192)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=sc-w-success reason="
                  << environment.error() << '\n';
        return 1;
    }
    word_value = word_reservation_value;
    word_line_value = (word_line_value & ~std::uint64_t{0xffffffff}) |
        word_value;
    const memblock::AtomicTransaction sc_word_failed{
        .address = word_address,
        .op = memblock::AtomicOp::sc_w,
        .data = 0x2468ace0U,
        .rob = 60,
        .pdest = 150,
        .address_lane = 0,
        .data_lane = 0,
    };
    const memblock::LoadTransaction sc_word_failed_wb{
        .address = word_address, .op = memblock::LoadOp::ld,
        .rob = sc_word_failed.rob, .pdest = sc_word_failed.pdest,
        .lane = 0, .rf_wen = true,
    };
    environment.expect_load_data(sc_word_failed_wb, 1);
    if (!environment.issue_atomic(sc_word_failed, 1024) ||
        !environment.run_until_complete(8192)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=sc-w-failure reason="
                  << environment.error() << '\n';
        return 1;
    }
    const memblock::LoadTransaction word_reservation_readback{
        .address = word_address, .op = memblock::LoadOp::ld, .rob = 61,
        .lq = 22, .pdest = 151, .lane = 1,
    };
    environment.expect_load_data(word_reservation_readback, word_line_value);
    if (!environment.set_rob_head(word_reservation_readback.rob) ||
        !environment.enqueue_load(word_reservation_readback) ||
        !environment.issue_load(word_reservation_readback, 1024) ||
        !environment.run_until_complete(8192) ||
        !environment.run_until_lq_retired(1024)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=sc-w-readback reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::AtomicTransaction lr{
        .address = address,
        .op = memblock::AtomicOp::lr_d,
        .rob = 62,
        .pdest = 92,
        .address_lane = 0,
        .data_lane = 0,
    };
    const memblock::LoadTransaction lr_wb{
        .address = address, .op = memblock::LoadOp::ld, .rob = lr.rob,
        .pdest = lr.pdest, .lane = 0, .rf_wen = true,
    };
    environment.expect_load_data(lr_wb, model_value);
    if (!environment.issue_atomic(lr, 1024) ||
        !environment.run_until_complete(8192)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=lr reason="
                  << environment.error() << '\n';
        return 1;
    }

    constexpr std::uint64_t reservation_value = 0xfeedfacecafebeefULL;
    const memblock::AtomicTransaction sc{
        .address = address,
        .op = memblock::AtomicOp::sc_d,
        .data = reservation_value,
        .rob = 63,
        .pdest = 93,
        .address_lane = 1,
        .data_lane = 1,
    };
    const memblock::LoadTransaction sc_wb{
        .address = address, .op = memblock::LoadOp::ld, .rob = sc.rob,
        .pdest = sc.pdest, .lane = 0, .rf_wen = true,
    };
    environment.expect_load_data(sc_wb, 0);
    if (!environment.issue_atomic(sc, 1024) ||
        !environment.run_until_complete(8192)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=sc-success reason="
                  << environment.error() << '\n';
        return 1;
    }
    model_value = reservation_value;

    const memblock::AtomicTransaction sc_failed{
        .address = address,
        .op = memblock::AtomicOp::sc_d,
        .data = 0x1111222233334444ULL,
        .rob = 64,
        .pdest = 94,
        .address_lane = 0,
        .data_lane = 0,
    };
    const memblock::LoadTransaction sc_failed_wb{
        .address = address, .op = memblock::LoadOp::ld,
        .rob = sc_failed.rob, .pdest = sc_failed.pdest,
        .lane = 0, .rf_wen = true,
    };
    environment.expect_load_data(sc_failed_wb, 1);
    if (!environment.issue_atomic(sc_failed, 1024) ||
        !environment.run_until_complete(8192)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=sc-failure reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction reservation_readback{
        .address = address, .op = memblock::LoadOp::ld, .rob = 65,
        .lq = static_cast<std::uint8_t>(operations.size() +
            word_operations.size() + 5),
        .pdest = 95, .lane = 1,
    };
    environment.expect_load_data(reservation_readback, model_value);
    if (!environment.set_rob_head(reservation_readback.rob) ||
        !environment.enqueue_load(reservation_readback) ||
        !environment.issue_load(reservation_readback, 1024) ||
        !environment.run_until_complete(8192) ||
        !environment.run_until_lq_retired(1024)) {
        std::cerr << "MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=sc-readback reason="
                  << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_ATOMIC_CONTRACTS_PASS"
              << " cycle=" << environment.cycle()
              << " amo_d_variants=" << operations.size()
              << " amo_w_variants=" << word_operations.size()
              << " amocas_variants=4"
              << " lr_sc=1 misaligned_d_offsets=7 misaligned_w_offsets=3"
              << " misaligned=10"
              << " final=0x" << std::hex << model_value << std::dec
              << " tilelink_requests=" << environment.tilelink_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_uncache_errors(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_base = 0x50032000ULL;
    constexpr std::uint64_t physical_base = 0x90032000ULL;
    constexpr std::uint64_t root = 0x97000000ULL;
    environment.memory().fill_incrementing(physical_base & ~std::uint64_t{63}, 64, 0x5c);
    environment.configure_backpressure(0x4d595df4d0f33173ULL, true);
    if (!environment.reset() ||
        !environment.map_sv39_4k(
            virtual_base, physical_base, root, true, true, false, false, true) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_UNCACHE_ERRORS_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction denied{
        .address = virtual_base + 8,
        .oracle_address = physical_base + 8,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .pdest = 104,
        .lane = 0,
        .expected_exception_mask = memblock::kExceptionLoadAccessFault,
    };
    environment.expect_load(denied);
    environment.inject_next_uncache_response_error(true, false);
    const auto retire_denied = [&]() {
        return environment.lq_dequeued() + environment.lq_canceled() >=
                environment.lq_allocated() ||
            environment.account_lq_cancellation(1);
    };
    if (!environment.enqueue_load(denied) ||
        !environment.issue_load(denied, 1024) ||
        !environment.run_until_complete(8192) ||
        !environment.run_cycles(8) ||
        !environment.redirect_after(denied.rob, denied.rob_flag, true) ||
        !environment.run_cycles(96) ||
        !retire_denied()) {
        std::cerr << "MEMBLOCK_UNCACHE_ERRORS_FAIL cycle="
                  << environment.cycle() << " phase=denied reason="
                  << environment.error()
                  << " uncache_requests=" << environment.uncache_requests()
                  << " dcache_requests=" << environment.tilelink_requests() << '\n';
        return 1;
    }

    const memblock::LoadTransaction corrupt{
        .address = virtual_base + 16,
        .oracle_address = physical_base + 16,
        .op = memblock::LoadOp::ld,
        .rob = 1,
        .lq = 1,
        .pdest = 105,
        .lane = 1,
        .expected_exception_mask = memblock::kExceptionHardwareError,
    };
    environment.expect_load(corrupt);
    environment.inject_next_uncache_response_error(false, true);
    const auto retire_corrupt = [&]() {
        return environment.lq_dequeued() + environment.lq_canceled() >=
                environment.lq_allocated() ||
            environment.account_lq_cancellation(1);
    };
    if (!environment.set_rob_head(corrupt.rob) ||
        !environment.enqueue_load(corrupt) ||
        !environment.issue_load(corrupt, 1024) ||
        !environment.run_until_complete(8192) ||
        !environment.run_cycles(8) ||
        !environment.redirect_after(corrupt.rob, corrupt.rob_flag, true) ||
        !environment.run_cycles(96) ||
        !retire_corrupt()) {
        std::cerr << "MEMBLOCK_UNCACHE_ERRORS_FAIL cycle="
                  << environment.cycle() << " phase=corrupt reason="
                  << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_UNCACHE_ERRORS_PASS"
              << " cycle=" << environment.cycle()
              << " denied=1 corrupt=1"
              << " uncache_requests=" << environment.uncache_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_atomic_dchannel_errors(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t base = memblock::kDefaultMemoryBase + 0x32000;
    struct AtomicErrorCase {
        memblock::AtomicOp op;
        const char *name;
        bool word;
        bool load_reserved;
    };
    const std::array<AtomicErrorCase, 22> cases{{
        {memblock::AtomicOp::lr_w, "lr_w", true, true},
        {memblock::AtomicOp::amoswap_w, "amoswap_w", true, false},
        {memblock::AtomicOp::amoadd_w, "amoadd_w", true, false},
        {memblock::AtomicOp::amoxor_w, "amoxor_w", true, false},
        {memblock::AtomicOp::amoand_w, "amoand_w", true, false},
        {memblock::AtomicOp::amoor_w, "amoor_w", true, false},
        {memblock::AtomicOp::amomin_w, "amomin_w", true, false},
        {memblock::AtomicOp::amomax_w, "amomax_w", true, false},
        {memblock::AtomicOp::amominu_w, "amominu_w", true, false},
        {memblock::AtomicOp::amomaxu_w, "amomaxu_w", true, false},
        {memblock::AtomicOp::amocas_w, "amocas_w", true, false},
        {memblock::AtomicOp::lr_d, "lr_d", false, true},
        {memblock::AtomicOp::amoswap_d, "amoswap_d", false, false},
        {memblock::AtomicOp::amoadd_d, "amoadd_d", false, false},
        {memblock::AtomicOp::amoxor_d, "amoxor_d", false, false},
        {memblock::AtomicOp::amoand_d, "amoand_d", false, false},
        {memblock::AtomicOp::amoor_d, "amoor_d", false, false},
        {memblock::AtomicOp::amomin_d, "amomin_d", false, false},
        {memblock::AtomicOp::amomax_d, "amomax_d", false, false},
        {memblock::AtomicOp::amominu_d, "amominu_d", false, false},
        {memblock::AtomicOp::amomaxu_d, "amomaxu_d", false, false},
        {memblock::AtomicOp::amocas_d, "amocas_d", false, false},
    }};
    if (!environment.reset()) {
        std::cerr << "MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_FAIL cycle="
                  << environment.cycle() << " phase=reset reason="
                  << environment.error() << '\n';
        return 1;
    }
    environment.configure_backpressure(0x243f6a8885a308d3ULL, true);
    environment.configure_cache_error_enable(true);
    std::size_t denied_cases = 0;
    std::size_t corrupt_cases = 0;
    std::size_t readbacks = 0;
    std::size_t lr_reservation_checks = 0;
    std::size_t sc_corrupt_hit_checks = 0;

    for (unsigned error_kind = 0; error_kind < 2; ++error_kind) {
        const bool denied = error_kind == 0;
        const bool corrupt = !denied;
        for (std::size_t index = 0; index < cases.size(); ++index) {
            const AtomicErrorCase &test_case = cases[index];
            const std::size_t global_index = error_kind * cases.size() + index;
            const std::uint64_t line_address = base + global_index * 64;
            const std::uint64_t atomic_address = line_address +
                (test_case.word ? 4 : 0);
            const memblock::LoadOp read_op = test_case.word
                ? memblock::LoadOp::lwu
                : memblock::LoadOp::ld;
            environment.memory().fill_incrementing(
                line_address, 64,
                static_cast<std::uint8_t>(0x31U + global_index));
            const std::uint64_t original =
                environment.memory().expected_load(atomic_address, read_op);
            const std::uint8_t atomic_rob =
                static_cast<std::uint8_t>(global_index * 2);
            const memblock::AtomicTransaction atomic{
                .address = atomic_address,
                .op = test_case.op,
                .data = 0xfedcba9876543210ULL ^
                    (0x0101010101010101ULL * global_index),
                .compare = original,
                .rob = atomic_rob,
                .pdest = static_cast<std::uint8_t>(32 + global_index),
                .address_lane = static_cast<unsigned>(global_index & 1U),
                .data_lane = static_cast<unsigned>((global_index >> 1) & 1U),
            };
            const std::uint32_t expected_exception = corrupt
                ? memblock::kExceptionHardwareError
                : (test_case.load_reserved
                    ? memblock::kExceptionLoadAccessFault
                    : memblock::kExceptionStoreAccessFault);
            const memblock::LoadTransaction atomic_wb{
                .address = atomic.address,
                .op = read_op,
                .rob = atomic.rob,
                .pdest = atomic.pdest,
                .lane = static_cast<unsigned>(global_index % 3),
                .expected_exception_mask = expected_exception,
            };
            environment.expect_load_data(atomic_wb, 0);
            environment.inject_next_dcache_response_error(denied, corrupt);
            const std::uint64_t requests_before_atomic =
                environment.tilelink_requests();
            if (!environment.set_rob_head(atomic.rob) ||
                !environment.issue_atomic(atomic, 1024) ||
                !environment.run_until_complete(8192) ||
                environment.tilelink_requests() != requests_before_atomic + 1) {
                std::cerr << "MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_FAIL cycle="
                          << environment.cycle() << " phase="
                          << (denied ? "denied" : "corrupt")
                          << "-atomic op=" << test_case.name
                          << " index=" << index
                          << " reason=" << environment.error()
                          << " dcache_requests="
                          << environment.tilelink_requests()
                          << " expected_requests=" << requests_before_atomic + 1
                          << '\n';
                return 1;
            }
            denied_cases += denied;
            corrupt_cases += corrupt;

            // A denied refill must not have installed the line, whereas a
            // corrupt refill remains cached with its error metadata.  In both
            // cases the diagnostic data must be the untouched manager value.
            environment.configure_cache_error_enable(false);
            const std::uint64_t requests_before_readback =
                environment.tilelink_requests();
            const memblock::LoadTransaction readback{
                .address = atomic.address,
                .op = read_op,
                .rob = static_cast<std::uint8_t>(atomic.rob + 1),
                .lq = static_cast<std::uint8_t>(global_index),
                .pdest = static_cast<std::uint8_t>(96 + global_index),
                .lane = static_cast<unsigned>(global_index % 3),
                .expected_exception_mask = corrupt
                    ? memblock::kExceptionHardwareError
                    : 0,
                .check_data_on_exception = corrupt,
            };
            environment.expect_load_data(readback, original);
            const bool readback_completed =
                environment.set_rob_head(readback.rob) &&
                environment.enqueue_load(readback) &&
                environment.issue_load(readback, 1024) &&
                environment.run_until_complete(8192);
            const std::uint64_t expected_requests = requests_before_readback +
                (denied ? 1 : 0);
            const bool readback_retired = corrupt
                ? (environment.run_cycles(8) &&
                   environment.redirect_after(
                       readback.rob, readback.rob_flag, true) &&
                   environment.run_cycles(96) &&
                   (environment.lq_dequeued() + environment.lq_canceled() >=
                        environment.lq_allocated() ||
                    environment.account_lq_cancellation(1)))
                : environment.run_until_lq_retired(1024);
            if (!readback_completed ||
                environment.tilelink_requests() != expected_requests ||
                !readback_retired) {
                std::cerr << "MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_FAIL cycle="
                          << environment.cycle() << " phase="
                          << (denied ? "denied" : "corrupt")
                          << "-readback op=" << test_case.name
                          << " index=" << index
                          << " reason=" << environment.error()
                          << " dcache_requests="
                          << environment.tilelink_requests()
                          << " expected_requests=" << expected_requests << '\n';
                return 1;
            }
            ++readbacks;
            environment.configure_cache_error_enable(true);

            if (denied && test_case.load_reserved) {
                // A faulting LR must not establish a reservation.  The clean
                // readback above installs the line without creating one, so a
                // following SC must return failure without TileLink traffic.
                const std::uint8_t sc_rob = static_cast<std::uint8_t>(
                    readback.rob + 1);
                const memblock::AtomicTransaction sc{
                    .address = atomic.address,
                    .op = test_case.word
                        ? memblock::AtomicOp::sc_w
                        : memblock::AtomicOp::sc_d,
                    .data = 0x0123456789abcdefULL,
                    .rob = sc_rob,
                    .pdest = static_cast<std::uint8_t>(150 + lr_reservation_checks),
                    .address_lane = static_cast<unsigned>(lr_reservation_checks & 1U),
                    .data_lane = static_cast<unsigned>(lr_reservation_checks & 1U),
                };
                const memblock::LoadTransaction sc_wb{
                    .address = sc.address,
                    .op = read_op,
                    .rob = sc.rob,
                    .pdest = sc.pdest,
                    .lane = 0,
                    .rf_wen = true,
                };
                environment.expect_load_data(sc_wb, 1);
                const std::uint64_t requests_before_sc =
                    environment.tilelink_requests();
                if (!environment.set_rob_head(sc.rob) ||
                    !environment.issue_atomic(sc, 1024) ||
                    !environment.run_until_complete(8192) ||
                    environment.tilelink_requests() != requests_before_sc) {
                    std::cerr << "MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_FAIL cycle="
                              << environment.cycle()
                              << " phase=denied-lr-reservation op="
                              << test_case.name << " reason="
                              << environment.error() << " dcache_requests="
                              << environment.tilelink_requests()
                              << " expected_requests=" << requests_before_sc
                              << '\n';
                    return 1;
                }
                ++lr_reservation_checks;
            } else if (corrupt && test_case.load_reserved) {
                // SC cannot issue a D-channel request on a miss: it fails as
                // soon as either the reservation or cache hit is absent.  A
                // corrupt LR refill is retained without a reservation, making
                // this the reachable SC error case: a hit on cached corrupt
                // metadata must report hardwareError and perform no write.
                const memblock::AtomicTransaction sc{
                    .address = atomic.address,
                    .op = test_case.word
                        ? memblock::AtomicOp::sc_w
                        : memblock::AtomicOp::sc_d,
                    .data = 0x89abcdef01234567ULL,
                    .rob = static_cast<std::uint8_t>(readback.rob + 1),
                    .pdest = static_cast<std::uint8_t>(170 + sc_corrupt_hit_checks),
                    .address_lane = static_cast<unsigned>(sc_corrupt_hit_checks & 1U),
                    .data_lane = static_cast<unsigned>(sc_corrupt_hit_checks & 1U),
                };
                const memblock::LoadTransaction sc_wb{
                    .address = sc.address,
                    .op = read_op,
                    .rob = sc.rob,
                    .pdest = sc.pdest,
                    .lane = 0,
                    .expected_exception_mask = memblock::kExceptionHardwareError,
                };
                environment.expect_load_data(sc_wb, 0);
                const std::uint64_t requests_before_sc =
                    environment.tilelink_requests();
                if (!environment.set_rob_head(sc.rob) ||
                    !environment.issue_atomic(sc, 1024) ||
                    !environment.run_until_complete(8192) ||
                    environment.tilelink_requests() != requests_before_sc) {
                    std::cerr << "MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_FAIL cycle="
                              << environment.cycle()
                              << " phase=corrupt-sc-hit op="
                              << test_case.name << " reason="
                              << environment.error() << " dcache_requests="
                              << environment.tilelink_requests()
                              << " expected_requests=" << requests_before_sc
                              << '\n';
                    return 1;
                }
                ++sc_corrupt_hit_checks;
            }
        }
    }

    std::cout << "MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_PASS cycle="
              << environment.cycle()
              << " denied_cases=" << denied_cases
              << " corrupt_cases=" << corrupt_cases
              << " readbacks=" << readbacks
              << " lr_reservation_checks=" << lr_reservation_checks
              << " sc_corrupt_hit_checks=" << sc_corrupt_hit_checks
              << " tilelink_requests=" << environment.tilelink_requests()
              << " rtl_sha256="
              << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_uncache_widths(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_base = 0x50034000ULL;
    constexpr std::uint64_t physical_base = 0x90034000ULL;
    constexpr std::uint64_t root = 0x97004000ULL;
    environment.memory().fill_incrementing(physical_base, 64, 0x80);
    environment.configure_backpressure(0x6a09e667f3bcc909ULL, true);
    if (!environment.reset() ||
        !environment.map_sv39_4k(
            virtual_base, physical_base, root, true, true, false, false, true) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_UNCACHE_WIDTHS_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    // Exercise every scalar width and sign mode at every legal byte lane of
    // an 8-byte Uncache beat. The response agent returns the complete beat,
    // so this catches address-lane, size, mask, and sign-extension mistakes.
    const std::array<memblock::LoadOp, 2> byte_ops{
        memblock::LoadOp::lb, memblock::LoadOp::lbu};
    const std::array<memblock::LoadOp, 2> half_ops{
        memblock::LoadOp::lh, memblock::LoadOp::lhu};
    const std::array<memblock::LoadOp, 2> word_ops{
        memblock::LoadOp::lw, memblock::LoadOp::lwu};
    unsigned case_count = 0;
    std::uint8_t rob = 0;
    std::uint8_t lq = 0;
    auto run_case = [&](memblock::LoadOp op, unsigned offset) {
        const memblock::LoadTransaction transaction{
            .address = virtual_base + offset,
            .oracle_address = physical_base + offset,
            .op = op,
            .rob = rob,
            .lq = lq,
            .pdest = static_cast<std::uint8_t>(120 + case_count),
            .lane = case_count % memblock::kScalarLoadLanes,
        };
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 2048) ||
            !environment.run_until_complete(8192) ||
            !environment.run_until_lq_retired(2048)) {
            std::cerr << "MEMBLOCK_UNCACHE_WIDTHS_FAIL cycle="
                      << environment.cycle() << " case=" << case_count
                      << " op=" << static_cast<unsigned>(op)
                      << " offset=" << offset << " reason="
                      << environment.error() << '\n';
            return false;
        }
        ++case_count;
        ++rob;
        ++lq;
        return true;
    };

    for (const auto op : byte_ops) {
        for (unsigned offset = 0; offset < 8; ++offset) {
            if (!run_case(op, offset)) {
                return 1;
            }
        }
    }
    for (const auto op : half_ops) {
        for (unsigned offset = 0; offset < 8; offset += 2) {
            if (!run_case(op, offset)) {
                return 1;
            }
        }
    }
    for (const auto op : word_ops) {
        for (unsigned offset = 0; offset < 8; offset += 4) {
            if (!run_case(op, offset)) {
                return 1;
            }
        }
    }
    if (!run_case(memblock::LoadOp::ld, 0)) {
        return 1;
    }

    std::cout << "MEMBLOCK_UNCACHE_WIDTHS_PASS"
              << " cycle=" << environment.cycle()
              << " cases=" << case_count
              << " uncache_requests=" << environment.uncache_requests()
              << " request_stalls=" << environment.uncache_request_stalls()
              << " response_delays=" << environment.uncache_response_delays()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_mmio_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_base = 0x50038000ULL;
    constexpr std::uint64_t physical_base = 0x90038000ULL;
    constexpr std::uint64_t root = 0x9700c000ULL;
    environment.memory().fill_incrementing(physical_base, 64, 0x2d);
    environment.configure_backpressure(0x1f83d9abfb41bd6bULL, true);
    if (!environment.reset() ||
        !environment.map_sv39_4k(
            virtual_base, physical_base, root, true, true, false, false,
            false, true) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    unsigned normal_count = 0;
    unsigned fault_count = 0;
    auto run_case = [&](std::uint8_t rob, std::uint8_t lq,
                        std::uint8_t pdest, std::uint64_t offset,
                        std::uint32_t exception, bool denied, bool corrupt) {
        const memblock::LoadTransaction transaction{
            .address = virtual_base + offset,
            .oracle_address = physical_base + offset,
            .op = memblock::LoadOp::ld,
            .rob = rob,
            .lq = lq,
            .pdest = pdest,
            .lane = static_cast<unsigned>(rob % memblock::kScalarLoadLanes),
            .expected_exception_mask = exception,
            .expected_debug_is_mmio = true,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        environment.expect_load(transaction);
        if (denied || corrupt) {
            environment.inject_next_uncache_response_error(denied, corrupt);
        }
        const std::uint64_t dcache_before = environment.tilelink_requests();
        if (!environment.set_rob_head(transaction.rob) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 2048) ||
            !environment.wait_for_mmio_request(
                transaction.rob, transaction.rob_flag, 4096) ||
            !environment.run_until_complete(8192)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=writeback rob="
                      << static_cast<unsigned>(rob) << " reason="
                      << environment.error() << '\n';
            return false;
        }
        if (environment.tilelink_requests() != dcache_before) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << environment.cycle() << " phase=dcache-bypass rob="
                      << static_cast<unsigned>(rob) << " reason=MMIO issued a DCache request\n";
            return false;
        }
        if (exception == 0) {
            ++normal_count;
            return environment.run_until_lq_retired(2048);
        }
        ++fault_count;
        if (!environment.run_cycles(8) ||
            !environment.redirect_after(transaction.rob, transaction.rob_flag, true) ||
            !environment.run_cycles(96)) {
            return false;
        }
        if (environment.lq_dequeued() + environment.lq_canceled() <
            environment.lq_allocated()) {
            return environment.account_lq_cancellation(1);
        }
        return true;
    };

    if (!run_case(0, 0, 150, 8, 0, false, false) ||
        !run_case(1, 1, 151, 16, memblock::kExceptionLoadAccessFault,
                  true, false) ||
        !run_case(2, 2, 152, 24, memblock::kExceptionHardwareError,
                  false, true)) {
        std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=final reason="
                  << environment.error() << '\n';
        return 1;
    }

    // MMIO stores are committed through StoreQueue's uncache state machine;
    // unlike scalar MMIO loads they do not produce an address writeback until
    // the request/response sequence has completed.  Check both the absence
    // of DCache traffic and the final MMIO metadata on that delayed pulse.
    const memblock::StoreTransaction mmio_store{
        .address = virtual_base + 0x28,
        .oracle_address = physical_base + 0x28,
        .data = 0x0123456789abcdefULL,
        .op = memblock::StoreOp::sd,
        .rob = 3,
        .sq = 0,
        .address_lane = 0,
        .data_lane = 1,
        // The PBMT=IO page is backed by the configured DDR PMA region.  The
        // StoreQueue writeback therefore reports memBackTypeMM=1 and
        // debug.isMMIO=0; PMA-IO would be a separate device-map test.
        .expected_debug_is_mmio = false,
        .expected_debug_is_ncio = false,
    };
    const std::uint64_t dcache_before_store = environment.tilelink_requests();
    const std::uint64_t uncache_before_store = environment.uncache_requests();
    const std::uint64_t store_misses_before = environment.store_tlb_misses();
    environment.expect_store(mmio_store);
    if (!environment.set_rob_head(mmio_store.rob, mmio_store.rob_flag) ||
        !environment.enqueue_store(mmio_store, 0) ||
        !environment.issue_store_address(mmio_store, 2048) ||
        !environment.run_until_store_tlb_misses(store_misses_before + 1, 4096) ||
        !environment.run_cycles(256) ||
        !environment.issue_store_address(mmio_store, 2048) ||
        !environment.issue_store_data(mmio_store, 2048) ||
        !environment.run_cycles(64) ||
        !environment.wait_for_mmio_store_request(
            mmio_store.rob, mmio_store.rob_flag, 8192) ||
        !environment.run_until_store_complete(8192) ||
        !environment.commit_stores_through(mmio_store, 1) ||
        !environment.run_cycles(16)) {
        std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=store-writeback reason="
                  << environment.error()
                  << " store_mmio_valid=" << environment.store_mmio_valid()
                  << " store_mmio_rob="
                  << static_cast<unsigned>(environment.store_mmio_rob())
                  << " store_tlb_feedbacks="
                  << environment.store_tlb_feedbacks()
                  << " store_tlb_misses=" << environment.store_tlb_misses()
                  << " uncache_requests=" << environment.uncache_requests()
                  << '\n';
        return 1;
    }
    environment.record_committed_store(mmio_store);
    if (environment.sq_dequeued() != environment.sq_allocated()) {
        std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=store-retirement reason="
                  << "MMIO store did not leave SQ"
                  << " sq_dequeued=" << environment.sq_dequeued()
                  << " sq_allocated=" << environment.sq_allocated() << '\n';
        return 1;
    }
    if (environment.tilelink_requests() != dcache_before_store ||
        environment.uncache_requests() != uncache_before_store + 1) {
        std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=store-bypass reason="
                  << "MMIO store used the wrong manager path"
                  << " dcache_requests=" << environment.tilelink_requests()
                  << " uncache_requests=" << environment.uncache_requests()
                  << '\n';
        return 1;
    }

    // Exercise the SoC's physical PMA device window independently of PBMT=IO.
    // The 0x30050000..0x3801ffff PMA interval has c=0 and is outside the
    // DebugModule-only access guard, so normal CPU accesses are classified as
    // MMIO from the physical PMA response.
    unsigned pma_load_count = 0;
    unsigned pma_store_count = 0;
    unsigned pma_denied_count = 0;
    {
        memblock::Environment pma_environment(argc, argv);
        constexpr std::uint64_t pma_physical_base = 0x35000000ULL;
        pma_environment.memory().fill_incrementing(
            pma_physical_base, 64, 0x63);
        pma_environment.configure_backpressure(
            0x5a17c3e9d2b84f61ULL, true);
        if (!pma_environment.reset() ||
            !pma_environment.activate_bare(43)) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << pma_environment.cycle()
                      << " phase=pma-configuration reason="
                      << pma_environment.error() << '\n';
            return 1;
        }

        const memblock::LoadTransaction pma_load{
            .address = pma_physical_base + 0x10,
            .oracle_address = pma_physical_base + 0x10,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 158,
            .lane = 0,
            .expected_debug_is_mmio = true,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
        };
        pma_environment.expect_load(pma_load);
        const std::uint64_t pma_dcache_before =
            pma_environment.tilelink_requests();
        if (!pma_environment.set_rob_head(pma_load.rob) ||
            !pma_environment.enqueue_load(pma_load) ||
            !pma_environment.issue_load(pma_load, 2048) ||
            !pma_environment.wait_for_mmio_request(
                pma_load.rob, pma_load.rob_flag, 4096) ||
            !pma_environment.run_until_complete(8192) ||
            !pma_environment.run_until_lq_retired(2048) ||
            pma_environment.tilelink_requests() != pma_dcache_before) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << pma_environment.cycle()
                      << " phase=pma-load reason="
                      << pma_environment.error() << '\n';
            return 1;
        }
        ++pma_load_count;

        const memblock::StoreTransaction pma_store{
            .address = pma_physical_base + 0x28,
            .oracle_address = pma_physical_base + 0x28,
            .data = 0x8877665544332211ULL,
            .op = memblock::StoreOp::sd,
            .rob = 1,
            .sq = 0,
            .address_lane = 0,
            .data_lane = 1,
            .expected_debug_is_mmio = true,
            .expected_debug_is_ncio = false,
        };
        const std::uint64_t pma_uncache_before =
            pma_environment.uncache_requests();
        pma_environment.expect_store(pma_store);
        if (!pma_environment.set_rob_head(pma_store.rob) ||
            !pma_environment.enqueue_store(pma_store, 0) ||
            !pma_environment.issue_store_address(pma_store, 2048) ||
            !pma_environment.issue_store_data(pma_store, 2048) ||
            !pma_environment.run_cycles(64) ||
            !pma_environment.wait_for_mmio_store_request(
                pma_store.rob, pma_store.rob_flag, 8192) ||
            !pma_environment.run_until_store_complete(8192) ||
            !pma_environment.commit_stores_through(pma_store, 1) ||
            !pma_environment.run_cycles(16) ||
            pma_environment.uncache_requests() != pma_uncache_before + 1) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << pma_environment.cycle()
                      << " phase=pma-store reason="
                      << pma_environment.error() << '\n';
            return 1;
        }
        pma_environment.record_committed_store(pma_store);
        if (pma_environment.sq_dequeued() != pma_environment.sq_allocated()) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << pma_environment.cycle()
                      << " phase=pma-store-retirement reason="
                      << "PMA MMIO store did not leave SQ\n";
            return 1;
        }
        ++pma_store_count;

        // The DebugModule PMA interval is c=0 but is additionally guarded by
        // the CSR debug-mode bit.  A normal CPU access must report an access
        // fault and must not leak a request onto either memory manager.
        const memblock::LoadTransaction pma_denied{
            .address = 0x38020010ULL,
            .oracle_address = 0x38020010ULL,
            .op = memblock::LoadOp::ld,
            .rob = 2,
            .lq = 1,
            .pdest = 159,
            .lane = 0,
            .expected_exception_mask = memblock::kExceptionLoadAccessFault,
        };
        const std::uint64_t denied_dcache_before =
            pma_environment.tilelink_requests();
        const std::uint64_t denied_uncache_before =
            pma_environment.uncache_requests();
        pma_environment.expect_load(pma_denied);
        if (!pma_environment.set_rob_head(pma_denied.rob) ||
            !pma_environment.enqueue_load(pma_denied) ||
            !pma_environment.issue_load(pma_denied, 2048) ||
            !pma_environment.run_until_complete(8192) ||
            !pma_environment.run_until_lq_retired(2048) ||
            pma_environment.tilelink_requests() != denied_dcache_before ||
            pma_environment.uncache_requests() != denied_uncache_before) {
            std::cerr << "MEMBLOCK_MMIO_CONTRACTS_FAIL cycle="
                      << pma_environment.cycle()
                      << " phase=pma-debug-denied reason="
                      << pma_environment.error() << '\n';
            return 1;
        }
        ++pma_denied_count;
    }

    std::cout << "MEMBLOCK_MMIO_CONTRACTS_PASS"
              << " cycle=" << environment.cycle()
              << " normal=" << normal_count
              << " denied=1 corrupt=1"
              << " stores=1"
              << " pma_loads=" << pma_load_count
              << " pma_stores=" << pma_store_count
              << " pma_denied=" << pma_denied_count
              << " dcache_requests=" << environment.tilelink_requests()
              << " uncache_requests=" << environment.uncache_requests()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_cbo_zero_contracts(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_base = 0x5003c000ULL;
    constexpr std::uint64_t physical_base = 0x9003c000ULL;
    constexpr std::uint64_t root = 0x97010000ULL;
    environment.memory().fill_incrementing(physical_base, 64, 0xa7);
    environment.configure_backpressure(0x4d595df4d0f33173ULL, true);
    if (!environment.reset() ||
        !environment.map_sv39_4k(
            virtual_base, physical_base, root, true, true, false, false,
            false, false) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_CBO_ZERO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    // CBO.ZERO uses the 0x7 encoding and writes an entire 64-byte line through
    // the cacheable StoreQueue/SBuffer wline path.  Use a nonzero source
    // operand so the test also proves the StoreQueue's CBO.ZERO data override.
    const memblock::StoreTransaction cbo_zero{
        .address = virtual_base + 0x20,
        .oracle_address = physical_base,
        .data = 0xdeadbeefcafef00dULL,
        .op = memblock::StoreOp::cbo_zero,
        .rob = 0,
        .sq = 0,
        .address_lane = 0,
        .data_lane = 1,
        .expected_debug_is_mmio = false,
        .expected_debug_is_ncio = false,
    };
    environment.expect_store(cbo_zero);
    const std::uint64_t dcache_before = environment.tilelink_requests();
    const std::uint64_t store_misses_before = environment.store_tlb_misses();
    const std::uint64_t sq_target = environment.sq_dequeued() + 1;
    if (!environment.set_rob_head(cbo_zero.rob, cbo_zero.rob_flag) ||
        !environment.enqueue_store(cbo_zero, 0) ||
        !environment.issue_store_address(cbo_zero, 2048) ||
        !environment.run_until_store_tlb_misses(store_misses_before + 1, 4096) ||
        !environment.run_cycles(256) ||
        !environment.issue_store_address(cbo_zero, 2048) ||
        !environment.issue_store_data(cbo_zero, 2048) ||
        !environment.commit_stores_through(cbo_zero, 1) ||
        !environment.run_until_store_complete(16384) ||
        !environment.run_until_sq_dequeued(sq_target, 16384)) {
        std::cerr << "MEMBLOCK_CBO_ZERO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=zero-store reason="
                  << environment.error() << " dcache_requests="
                  << environment.tilelink_requests() << " sq="
                  << environment.sq_dequeued() << '/' << environment.sq_allocated()
                  << " store_mmio_valid=" << environment.store_mmio_valid()
                  << '\n';
        return 1;
    }
    if (environment.tilelink_requests() <= dcache_before ||
        environment.uncache_requests() != 0 ||
        environment.dcache_request_stalls() == 0) {
        std::cerr << "MEMBLOCK_CBO_ZERO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=manager-path reason="
                  << "unexpected manager request count dcache="
                  << environment.tilelink_requests() << " uncache="
                  << environment.uncache_requests() << '\n';
        return 1;
    }

    // Read the line through the cache before updating the reference mirror.
    // This makes the result sensitive to the full-line zero emitted by RTL.
    const memblock::LoadTransaction readback{
        .address = virtual_base,
        .oracle_address = physical_base,
        .op = memblock::LoadOp::ld,
        .rob = 1,
        .lq = 0,
        .pdest = 191,
        .lane = 0,
        .expected_debug_is_mmio = false,
        .expected_debug_is_ncio = false,
        .expected_debug_is_perf_cnt = false,
    };
    environment.expect_load_data(readback, 0);
    if (!environment.set_rob_head(readback.rob) ||
        !environment.enqueue_load(readback) ||
        !environment.issue_load(readback, 2048) ||
        !environment.run_until_complete(16384) ||
        !environment.run_until_lq_retired(2048) ||
        environment.uncache_requests() != 0) {
        std::cerr << "MEMBLOCK_CBO_ZERO_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=zero-readback reason="
                  << environment.error() << '\n';
        return 1;
    }
    environment.record_committed_store(cbo_zero);
    std::cout << "MEMBLOCK_CBO_ZERO_CONTRACTS_PASS"
              << " cycle=" << environment.cycle()
              << " cbo_zero_line=1"
              << " readback=1"
              << " dcache_requests=" << environment.tilelink_requests()
              << " uncache_requests=" << environment.uncache_requests()
              << " dcache_request_stalls=" << environment.dcache_request_stalls()
              << " dcache_response_delays=" << environment.dcache_response_delays()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_reset_recovery(int argc, char **argv)
{
    memblock::Environment environment(argc, argv);
    constexpr std::uint64_t virtual_base = 0x50036000ULL;
    constexpr std::uint64_t physical_base = 0x90036000ULL;
    constexpr std::uint64_t root = 0x97008000ULL;
    environment.memory().fill_incrementing(physical_base, 64, 0x36);
    environment.configure_backpressure(0x510e527fade682d1ULL, true);
    if (!environment.reset() ||
        !environment.map_sv39_4k(
            virtual_base, physical_base, root) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_RESET_RECOVERY_FAIL cycle="
                  << environment.cycle() << " phase=configuration reason="
                  << environment.error() << '\n';
        return 1;
    }

    // Do not register an expectation for the first load. It is deliberately
    // reset while translation/manager traffic is outstanding; any stale
    // writeback after reset is therefore an unexpected architectural event.
    const memblock::LoadTransaction canceled{
        .address = virtual_base + 24,
        .oracle_address = physical_base + 24,
        .op = memblock::LoadOp::ld,
        .rob = 0,
        .lq = 0,
        .pdest = 140,
        .lane = 0,
    };
    if (!environment.enqueue_load(canceled) ||
        !environment.issue_load(canceled, 2048) ||
        !environment.reset()) {
        std::cerr << "MEMBLOCK_RESET_RECOVERY_FAIL cycle="
                  << environment.cycle() << " phase=reset-with-outstanding reason="
                  << environment.error() << '\n';
        return 1;
    }
    const std::uint64_t retired =
        environment.lq_dequeued() + environment.lq_canceled();
    if (retired < environment.lq_allocated() &&
        !environment.account_lq_cancellation(
            static_cast<unsigned>(environment.lq_allocated() - retired))) {
        std::cerr << "MEMBLOCK_RESET_RECOVERY_FAIL cycle="
                  << environment.cycle() << " phase=cancel-accounting reason="
                  << environment.error() << '\n';
        return 1;
    }

    const memblock::LoadTransaction survivor{
        .address = virtual_base + 8,
        .oracle_address = physical_base + 8,
        .op = memblock::LoadOp::ld,
        .rob = 1,
        // The DUT's reset starts a fresh internal LQ epoch.  Its first
        // post-reset allocation therefore reuses LQ slot zero even though
        // the software conservation counter still includes the canceled
        // pre-reset entry.
        .lq = 0,
        .pdest = 141,
        .lane = 1,
    };
    if (!environment.map_sv39_4k(virtual_base, physical_base, root) ||
        !environment.activate_sv39(root)) {
        std::cerr << "MEMBLOCK_RESET_RECOVERY_FAIL cycle="
                  << environment.cycle() << " phase=reconfigure reason="
                  << environment.error() << '\n';
        return 1;
    }
    environment.expect_load(survivor);
    if (!environment.set_rob_head(survivor.rob) ||
        !environment.enqueue_load(survivor) ||
        !environment.issue_load(survivor, 2048) ||
        !environment.run_until_complete(8192) ||
        !environment.run_until_lq_retired(2048)) {
        std::cerr << "MEMBLOCK_RESET_RECOVERY_FAIL cycle="
                  << environment.cycle() << " phase=survivor reason="
                  << environment.error() << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_RESET_RECOVERY_PASS"
              << " cycle=" << environment.cycle()
              << " resets=2"
              << " canceled=1"
              << " survivor_writebacks=1"
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
        if (transaction.lane == 0) {
            transaction.expected_trigger = memblock::kVectorWritebackTriggerNone;
            transaction.expected_debug_is_mmio = false;
            transaction.expected_debug_is_ncio = false;
            transaction.expected_debug_is_perf_cnt = false;
        }
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
        auto expected_line = environment.memory().read_beat(line, line_bytes);
        for (unsigned byte = 0; byte < sizeof(std::uint64_t); ++byte) {
            expected_line[24 + byte] = static_cast<unsigned char>(
                stores[index].data >> (8 * byte));
        }
        environment.expect_release_line(line, expected_line);
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
        if (environment.bus_expected_load(store.address, memblock::LoadOp::ld) ==
            store.data) {
            ++preserved_stores;
        }
    }
    const std::uint64_t verified_releases =
        environment.tilelink_release_data_verified();
    if (environment.tilelink_release_data() < line_count - dcache_ways ||
        verified_releases != environment.tilelink_release_data() ||
        preserved_stores != verified_releases) {
        std::cerr << "MEMBLOCK_DCACHE_RELEASE_FAIL cycle=" << environment.cycle()
                  << " phase=data reason=not every observed ReleaseData preserved bytes"
                  << " release_data=" << environment.tilelink_release_data()
                  << " verified=" << verified_releases
                  << " preserved=" << preserved_stores << '\n';
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
        // StoreUnit propagates TriggerAction.None on the exceptional address
        // writeback path; the standalone store-data adapter uses zero instead.
        .expected_trigger = memblock::kTriggerNone,
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

    // Uncache stores update the bus-side memory agent directly.  The
    // architectural reference image is intentionally updated only after an
    // observed commit, so this ordering oracle must inspect the bus image.
    const std::uint64_t observed_older =
        environment.bus_expected_load(older.address, memblock::LoadOp::ld);
    const std::uint64_t observed_younger =
        environment.bus_expected_load(younger.address, memblock::LoadOp::ld);
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
            .expected_trigger = memblock::kStoreWritebackTriggerNone,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
        },
        {
            .address = bare_base + 61,
            .data = 0xfedcba9876543210ULL,
            .op = memblock::StoreOp::sd,
            .rob = 2,
            .sq = 1,
            .address_lane = 1,
            .data_lane = 0,
            .expected_trigger = memblock::kStoreWritebackTriggerNone,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
        },
        {
            .address = bare_base + 0xffd,
            .data = 0x55aa33cc0ff09669ULL,
            .op = memblock::StoreOp::sd,
            .rob = 4,
            .sq = 2,
            .address_lane = 0,
            .data_lane = 0,
            .expected_trigger = memblock::kStoreWritebackTriggerNone,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
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
    vector_readback.expected_trigger = memblock::kVectorWritebackTriggerNone;
    vector_readback.expected_debug_is_mmio = false;
    vector_readback.expected_debug_is_ncio = false;
    vector_readback.expected_debug_is_perf_cnt = false;
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
    cross_page_store.expected_trigger = memblock::kVectorWritebackTriggerNone;
    cross_page_store.expected_debug_is_mmio = false;
    cross_page_store.expected_debug_is_ncio = false;
    cross_page_store.expected_debug_is_perf_cnt = false;
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

int run_l2_tlb_contracts(int argc, char **argv)
{
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
    // a separate non-blocking requestor: its response tells the external L2
    // whether the L1 lookup hit.  A miss is intentionally returned to that
    // external L2; MemBlock does not refill this port from its own PTW.
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

    memblock::Environment::L2TlbResponse hit;
    if (!environment.issue_l2_tlb_request(
            warm.address, 0, false, false, false, hit) || !hit.miss ||
        hit.page_fault || hit.guest_page_fault || hit.access_fault ||
        hit.pbmt != 0) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=l1-miss-response reason="
                  << environment.error() << " miss=" << hit.miss
                  << " paddr=0x" << std::hex << hit.paddr
                  << std::dec << " pf=" << hit.page_fault
                  << " gpf=" << hit.guest_page_fault
                  << " af=" << hit.access_fault << '\n';
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
        no_translate.paddr != 0 || no_translate.pbmt != 0) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=no-translate reason="
                  << environment.error() << " miss=" << no_translate.miss
                  << " paddr=0x" << std::hex << no_translate.paddr
                  << std::dec << " pbmt=" << static_cast<unsigned>(no_translate.pbmt)
                  << " pf=" << no_translate.page_fault
                  << " gpf=" << no_translate.guest_page_fault
                  << " af=" << no_translate.access_fault << '\n';
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
            unmapped, 0, false, true, false, miss) || !miss.miss ||
        miss.pbmt != 0 || miss.page_fault ||
        miss.guest_page_fault || miss.access_fault) {
        std::cerr << "MEMBLOCK_L2_TLB_CONTRACTS_FAIL cycle="
                  << environment.cycle() << " phase=cold-miss reason="
                  << environment.error() << " miss=" << miss.miss
                  << " paddr=0x" << std::hex << miss.paddr << std::dec
                  << " pf=" << miss.page_fault
                  << " gpf=" << miss.guest_page_fault
                  << " af=" << miss.access_fault << '\n';
        return 1;
    }

    std::cout << "MEMBLOCK_L2_TLB_CONTRACTS_PASS"
              << " cycle=" << environment.cycle()
              << " l1_miss_response=1 no_translate=1 killed=1 prefetch_miss=1"
              << " hints=32"
              << " hit_paddr=0x" << std::hex << hit.paddr << std::dec
              << " l2_pmp_ld=" << hit.pmp_load_denied
              << " l2_pmp_mmio=" << hit.pmp_mmio
              << " ptw_requests=" << environment.ptw_requests()
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

int run_translation_fence(int argc, char **argv)
{
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

    std::cout << "MEMBLOCK_TRANSLATION_FENCE_PASS"
              << " cycle=" << (environment.cycle() + nested.cycle() + vvma.cycle())
              << " ptw_requests="
              << (environment.ptw_requests() + nested.ptw_requests() + vvma.ptw_requests())
              << " writebacks="
              << (environment.writebacks() + nested.writebacks() + vvma.writebacks())
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_translation_context(int argc, char **argv)
{
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

    std::cout << "MEMBLOCK_TRANSLATION_CONTEXT_PASS"
              << " cycle=" << environment.cycle()
              << " ptw_requests=" << environment.ptw_requests()
              << " writebacks=" << environment.writebacks()
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_translation_bare(int argc, char **argv)
{
    unsigned completed = 0;
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
        ++completed;
    }

    std::cout << "MEMBLOCK_TRANSLATION_BARE_PASS"
              << " cases=" << completed
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_translation_faults(int argc, char **argv)
{
    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t canonical = 0x0000400012340000ULL;
        constexpr std::uint64_t physical = 0xca000000ULL;
        constexpr std::uint64_t root = 0x9b000000ULL;
        // Sv48 VA bit 47 is one while bits 63:48 are zero: intentionally
        // noncanonical and therefore a stage-1 load page fault.
        constexpr std::uint64_t noncanonical = 0x0000800012340188ULL;
        environment.memory().fill_incrementing(physical, 0x1000, 0x21);
        const memblock::LoadTransaction transaction{
            .address = noncanonical,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 140,
            .lane = 0,
            .expected_exception_mask = memblock::kExceptionLoadPageFault,
        };
        if (!environment.reset() ||
            !environment.map_sv48_4k(canonical, physical, root) ||
            !environment.activate_sv48(root, 23)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=noncanonical-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 1024) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=noncanonical-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t guest_virtual = 0x68000000ULL;
        constexpr std::uint64_t high_guest_physical = 0x20000000000ULL;
        constexpr std::uint64_t vs_root = 0x9c000000ULL;
        constexpr std::uint64_t g_root = 0x9d000000ULL;
        // Sv39x4 has a 41-bit GPA.  The VS leaf is valid, but its GPA is one
        // bit above that range, so the fault must be a G-stage guest fault.
        if (!environment.reset() ||
            !environment.map_sv39_4k(
                guest_virtual, high_guest_physical, vs_root) ||
            !environment.map_sv39x4_4k(vs_root, vs_root, g_root) ||
            !environment.map_sv39x4_4k(
                vs_root + 0x1000, vs_root + 0x1000, g_root) ||
            !environment.map_sv39x4_4k(
                vs_root + 0x2000, vs_root + 0x2000, g_root) ||
            !environment.activate_two_stage(
                vs_root, g_root, 25, 27)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=high-gpa-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        const auto reference = memblock::reference_two_stage_walk(
            environment.memory(), vs_root, g_root, guest_virtual + 0x188);
        if (reference.translated || !reference.guest_page_fault ||
            reference.faulting_guest_physical_address != high_guest_physical + 0x188) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=high-gpa-reference\n";
            return 1;
        }
        const memblock::LoadTransaction transaction{
            .address = guest_virtual + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 141,
            .lane = 1,
            .expected_exception_mask = memblock::kExceptionLoadGuestPageFault,
        };
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 2048) ||
            !environment.run_until_complete(32768) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=high-gpa-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t virtual_address = 0x20000000ULL;
        constexpr std::uint64_t misaligned_physical = 0xc4001000ULL;
        constexpr std::uint64_t root = 0x9f000000ULL;
        constexpr std::uint64_t l1 = root + 0x1000ULL;
        // A level-1 Sv39 leaf must describe a 2 MiB-aligned physical base.
        // Keep the low PPN bits set to make the DUT reject this malformed
        // superpage instead of silently masking them.
        constexpr std::uint64_t pte_valid = 1ULL << 0;
        constexpr std::uint64_t pte_read = 1ULL << 1;
        constexpr std::uint64_t pte_write = 1ULL << 2;
        constexpr std::uint64_t pte_accessed = 1ULL << 6;
        constexpr std::uint64_t pte_dirty = 1ULL << 7;
        constexpr std::uint64_t l2_index = 0;
        constexpr std::uint64_t l1_index = (virtual_address >> 21) & 0x1ffULL;
        if (!environment.reset()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=misaligned-superpage-reset reason="
                      << environment.error() << '\n';
            return 1;
        }
        environment.memory().write_u64(
            root + l2_index * 8,
            ((l1 >> 12) << 10) | pte_valid);
        environment.memory().write_u64(
            l1 + l1_index * 8,
            ((misaligned_physical >> 12) << 10) |
                pte_valid | pte_read | pte_write | pte_accessed | pte_dirty);
        const auto reference = memblock::reference_page_walk(
            environment.memory(), root, virtual_address + 0x188,
            memblock::ReferencePageMode::sv39);
        if (reference.translated || reference.faulting_pte_address == 0 ||
            reference.fault_level != 1) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=misaligned-superpage-reference\n";
            return 1;
        }
        const memblock::LoadTransaction transaction{
            .address = virtual_address + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 142,
            .lane = 2,
            .expected_exception_mask = memblock::kExceptionLoadPageFault,
        };
        if (!environment.activate_sv39(root, 29)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=misaligned-superpage-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 2048) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=misaligned-superpage-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t virtual_address = 0x72000000ULL;
        constexpr std::uint64_t root = 0xa3000000ULL;
        if (!environment.reset()) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=invalid-pte-reset reason="
                      << environment.error() << '\n';
            return 1;
        }
        const auto reference = memblock::reference_page_walk(
            environment.memory(), root, virtual_address + 0x188,
            memblock::ReferencePageMode::sv39);
        if (reference.translated || reference.faulting_pte_address == 0 ||
            reference.fault_level != 2) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=invalid-pte-reference\n";
            return 1;
        }
        const memblock::LoadTransaction transaction{
            .address = virtual_address + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 143,
            .lane = 0,
            .expected_exception_mask = memblock::kExceptionLoadPageFault,
        };
        if (!environment.activate_sv39(root, 37)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=invalid-pte-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 2048) ||
            !environment.run_until_complete(16384) ||
            !environment.run_until_lq_retired(4096)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=invalid-pte-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
    }

    {
        memblock::Environment environment(argc, argv);
        constexpr std::uint64_t guest_virtual = 0x6a000000ULL;
        constexpr std::uint64_t high_guest_physical = 0x4000000000000ULL;
        constexpr std::uint64_t vs_root = 0xa4000000ULL;
        constexpr std::uint64_t g_root = 0xa5000000ULL;
        if (!environment.reset() ||
            !environment.map_sv48_4k(
                guest_virtual, high_guest_physical, vs_root) ||
            // A four-level VS walk creates root plus three child tables.
            !environment.map_sv48x4_4k(vs_root, vs_root, g_root) ||
            !environment.map_sv48x4_4k(vs_root + 0x1000, vs_root + 0x1000, g_root) ||
            !environment.map_sv48x4_4k(vs_root + 0x2000, vs_root + 0x2000, g_root) ||
            !environment.map_sv48x4_4k(vs_root + 0x3000, vs_root + 0x3000, g_root) ||
            !environment.activate_two_stage_modes(
                memblock::ReferencePageMode::sv48,
                memblock::ReferencePageMode::sv48,
                vs_root, g_root, 39, 41)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=high-gpa-sv48-configuration reason="
                      << environment.error() << '\n';
            return 1;
        }
        const auto reference = memblock::reference_two_stage_walk(
            environment.memory(), vs_root, g_root, guest_virtual + 0x188,
            memblock::ReferencePageMode::sv48,
            memblock::ReferencePageMode::sv48);
        if (reference.translated || !reference.guest_page_fault ||
            reference.faulting_guest_physical_address != high_guest_physical + 0x188) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=high-gpa-sv48-reference\n";
            return 1;
        }
        const memblock::LoadTransaction transaction{
            .address = guest_virtual + 0x188,
            .op = memblock::LoadOp::ld,
            .rob = 0,
            .lq = 0,
            .pdest = 144,
            .lane = 1,
            .expected_exception_mask = memblock::kExceptionLoadGuestPageFault,
        };
        environment.expect_load(transaction);
        if (!environment.set_rob_head(transaction.rob, transaction.rob_flag) ||
            !environment.enqueue_load(transaction) ||
            !environment.issue_load(transaction, 4096) ||
            !environment.run_until_complete(32768) ||
            !environment.run_until_lq_retired(8192)) {
            std::cerr << "MEMBLOCK_TRANSLATION_FAULTS_FAIL phase=high-gpa-sv48-execution reason="
                      << environment.error() << '\n';
            return 1;
        }
    }

    std::cout << "MEMBLOCK_TRANSLATION_FAULTS_PASS cases=5"
              << " rtl_sha256=" << memblock::generated::kRtlSha256 << '\n';
    return 0;
}

int run_translation_permissions(int argc, char **argv)
{
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
    }

    std::cout << "MEMBLOCK_TRANSLATION_PERMISSIONS_PASS cases=4"
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

    std::cout << "MEMBLOCK_TRANSLATION_SUPERPAGES_PASS"
              << " cases=" << completed
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
    environment.configure_backpressure(
        options.seed ^ 0xd1b54a32d192ed03ULL, options.backpressure);
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
            ++coverage.dispatch_widths[width - 1];
            for (const unsigned lane : dispatch_lanes) {
                ++coverage.dispatch_lanes[lane];
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

        phase = "seeded-mixed-tail";
        const unsigned target_before_redirect = options.transactions - 2;
        // The tail is a constrained-random issue window. Each window contains
        // all five producer classes before any completion drain, so cache,
        // TLB, forwarding, and queue timing can interact in one simulation.
        while (actions + 7 <= target_before_redirect) {
            const std::uint64_t window_base =
                cache0_base + 0x10000 + (random() % 256) * 128;
            const auto required_load_mode = static_cast<memblock::VectorAddressingMode>(
                coverage.concurrent_windows < 4
                    ? coverage.concurrent_windows
                    : random() % 4);
            const auto required_store_mode = static_cast<memblock::VectorAddressingMode>(
                coverage.concurrent_windows < 4
                    ? (coverage.concurrent_windows + 1) % 4
                    : random() % 4);
            auto scalar = make_load(
                window_base + random() % 64U,
                static_cast<memblock::LoadOp>(random() % 7), random() % 3);
            auto vector_load = make_vector(
                false, window_base + 32, random() % 4, random() % 2,
                required_load_mode);
            auto scalar_store = make_store(
                window_base + random() % 64U, random(),
                static_cast<memblock::StoreOp>(random() % 4), random() % 2,
                random() % 2);
            auto vector_store = make_vector(
                true, window_base + 0x100 + (random() % 8U) * 8U,
                random() % 4, random() % 2,
                required_store_mode);
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
            if (!environment.run_until_all_complete(4096) ||
                !environment.run_cycles(8) ||
                !environment.commit_store(scalar_store, 4096) ||
                !environment.commit_vector_store(vector_store, 4096) ||
                !environment.set_rob_head(prefetch.rob, prefetch.rob_flag) ||
                !environment.run_until_queues_retired(4096)) {
                return false;
            }
            const unsigned scalar_bytes =
                1U << static_cast<unsigned>(scalar_store.op);
            const auto scalar_readback = make_load(
                scalar_store.address,
                static_cast<memblock::LoadOp>(scalar_store.op), random() % 3);
            if (!environment.set_rob_head(
                    scalar_readback.rob, scalar_readback.rob_flag) ||
                !issue_load(
                    scalar_readback,
                    scalar_forward_value(scalar_store.data, scalar_bytes, false)) ||
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
            coverage.concurrent_actions += 7;
            for (auto &count : coverage.concurrent_ops) {
                ++count;
            }
            actions += 5;
            coverage.cacheable += 7;
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

int run_random_stress(int argc, char **argv, const Options &options)
{
    constexpr unsigned minimum_actions = 96;
    if (options.transactions < minimum_actions) {
        std::cerr << "MEMBLOCK_RANDOM_STRESS_FAIL seed=" << options.seed
                  << " transactions=0 cycle=0 reason=random-stress_requires_at_least_"
                  << minimum_actions << "_actions\n";
        return 1;
    }

    enum class Kind { scalar_store, scalar_load, vector_store, vector_load,
                      prefetch, extra_load };
    struct Action {
        Kind kind;
        unsigned group;
    };
    struct Group {
        bool has_scalar_store = false;
        bool has_scalar_load = false;
        bool has_vector_store = false;
        bool has_vector_load = false;
        bool has_prefetch = false;
        bool has_extra_load = false;
        memblock::StoreTransaction scalar_store{};
        memblock::LoadTransaction scalar_load{};
        memblock::VectorMemoryTransaction vector_store{};
        memblock::VectorMemoryTransaction vector_load{};
        memblock::PrefetchTransaction prefetch{};
        memblock::LoadTransaction extra_load{};
    };

    memblock::Environment environment(argc, argv);
    StressRandom random(options.seed);
    StressCoverage coverage;
    // The generated DUT resets pendingPtr to ROB zero. Start at one so the
    // first store cannot be interpreted as already committed before the
    // stress driver's explicit commit point.
    std::uint64_t rob_offset = 1;
    std::uint64_t lq_offset = 0;
    std::uint64_t sq_offset = 0;
    unsigned completed_actions = 0;
    unsigned burst_index = 0;
    std::string phase = "reset";
    constexpr std::uint64_t region0 = memblock::kDefaultMemoryBase + 0x500000;
    constexpr std::uint64_t region1 = memblock::kDefaultMemoryBase + 0x900000;
    constexpr std::uint64_t region_span = 0x400000;

    environment.memory().fill_incrementing(region0, region_span, 0x13);
    environment.memory().fill_incrementing(region1, region_span, 0x97);
    environment.configure_backpressure(
        StressRandom::splitmix64(options.seed ^ 0x13198a2e03707344ULL),
        options.backpressure);

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
            .pdest = static_cast<std::uint8_t>(1 + random.next_payload() % 255),
            .lane = lane,
        };
        transaction.predecode_rvc = (rob % 4U) == 0 ||
            (random.next_shape() & 3U) == 0;
        transaction.ftq_ptr = random.next_shape() & 0x3fU;
        transaction.ftq_offset = static_cast<std::uint8_t>(random.next_shape() & 7U);
        transaction.store_set_hit = (rob % 5U) == 0 ||
            (random.next_shape() % 5U) == 0;
        transaction.load_wait_bit = rob != 0 &&
            ((rob % 7U) == 0 || (random.next_shape() % 7U) == 0);
        // Keep strict wait out of overlapping stress bursts; its independent
        // sideband drive is covered by metadata-contracts.
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
                           unsigned lane, memblock::VectorAddressingMode mode) {
        const std::uint64_t rob = rob_offset++;
        const std::uint64_t lq = lq_offset;
        const std::uint64_t sq = sq_offset;
        const std::uint8_t flow_num = mode ==
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
            .addressing = mode,
            .eew = static_cast<std::uint8_t>(eew),
            .vl = static_cast<std::uint8_t>(16U >> eew),
            .rob = memblock::rob_pointer_value(rob),
            .rob_flag = memblock::rob_pointer_flag(rob),
            .lq = memblock::lq_pointer_value(lq),
            .lq_flag = memblock::lq_pointer_flag(lq),
            .sq = memblock::sq_pointer_value(sq),
            .sq_flag = memblock::sq_pointer_flag(sq),
            .pdest = static_cast<std::uint8_t>(1 + random.next_payload() % 255),
            .lane = lane,
            .flow_num = flow_num,
        };
        for (auto &byte : transaction.data) {
            byte = static_cast<unsigned char>(random.next_payload());
        }
        transaction.ftq_ptr = random.next_shape() & 0x3fU;
        transaction.ftq_offset = static_cast<std::uint8_t>(random.next_shape() & 7U);
        // Keep LMUL coupled to the explicitly modeled flow count; the
        // standalone issue boundary does not carry vset/vl context.
        transaction.vlmul = 0;
        return transaction;
    };
    auto randomize_vector_addressing = [&](memblock::VectorMemoryTransaction &transaction) {
        const unsigned element_bytes = 1U << transaction.eew;
        const unsigned elements = 16U >> transaction.eew;
        if (transaction.addressing == memblock::VectorAddressingMode::strided) {
            // Repeated-address stores have no deterministic forwarding order.
            // Keep forwarding bursts non-overlapping while retaining zero-
            // stride coverage for independent loads in random-mixed.
            if (transaction.store) {
                const std::array<std::int64_t, 5> store_strides{{
                    -static_cast<std::int64_t>(2U * element_bytes),
                    -static_cast<std::int64_t>(element_bytes),
                    static_cast<std::int64_t>(element_bytes),
                    static_cast<std::int64_t>(2U * element_bytes),
                    static_cast<std::int64_t>(4U * element_bytes)}};
                transaction.stride = store_strides[
                    random.next_shape() % store_strides.size()];
            } else {
                const std::array<std::int64_t, 6> load_strides{{
                    -static_cast<std::int64_t>(2U * element_bytes),
                    -static_cast<std::int64_t>(element_bytes), 0,
                    static_cast<std::int64_t>(element_bytes),
                    static_cast<std::int64_t>(2U * element_bytes),
                    static_cast<std::int64_t>(4U * element_bytes)}};
                transaction.stride = load_strides[
                    random.next_shape() % load_strides.size()];
            }
            return;
        }
        if (transaction.addressing != memblock::VectorAddressingMode::indexed_unordered &&
            transaction.addressing != memblock::VectorAddressingMode::indexed_ordered) {
            return;
        }
        std::vector<unsigned> slots(256U / element_bytes);
        std::iota(slots.begin(), slots.end(), 0U);
        if (transaction.store) {
            std::shuffle(slots.begin(), slots.end(), random.payload);
        }
        for (unsigned element = 0; element < elements; ++element) {
            const std::uint64_t slot = transaction.store
                ? slots[element]
                : random.next_shape() % slots.size();
            const std::uint64_t offset = slot * element_bytes;
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                transaction.index[element * element_bytes + byte] =
                    static_cast<unsigned char>(offset >> (8 * byte));
            }
        }
    };
    auto scalar_forward_value = [&](const memblock::StoreTransaction &store,
                                    const memblock::LoadTransaction &load) {
        const unsigned load_bytes = 1U <<
            (static_cast<unsigned>(load.op) & 3U);
        const unsigned store_bytes = 1U << static_cast<unsigned>(store.op);
        std::uint64_t value = 0;
        const std::uint64_t old = load.oracle_address.value_or(load.address);
        for (unsigned byte = 0; byte < load_bytes; ++byte) {
            std::uint8_t result = environment.memory().read_byte(old + byte);
            for (unsigned store_byte = 0; store_byte < store_bytes; ++store_byte) {
                if (old + byte == store.address + store_byte) {
                    result = static_cast<std::uint8_t>(
                        store.data >> (8 * store_byte));
                }
            }
            value |= std::uint64_t{result} << (8 * byte);
        }
        const bool is_unsigned = (static_cast<unsigned>(load.op) & 4U) != 0;
        return is_unsigned ? value : memblock::sign_extend(value, load_bytes * 8);
    };
    auto vector_forward_value = [&](const memblock::VectorMemoryTransaction &store,
                                    const memblock::VectorMemoryTransaction &load) {
        auto expected = environment.memory().expected_vector_load(load);
        std::unordered_map<std::uint64_t, unsigned char> forwarded;
        const unsigned store_bytes = 1U << store.eew;
        const unsigned store_elements = 16U >> store.eew;
        const std::uint16_t store_active = memblock::active_vector_elements(store);
        for (unsigned element = 0; element < store_elements; ++element) {
            if (((store_active >> element) & 1U) == 0) {
                continue;
            }
            const auto address = memblock::vector_element_address(store, element);
            for (unsigned byte = 0; byte < store_bytes; ++byte) {
                forwarded[address + byte] = store.data[element * store_bytes + byte];
            }
        }
        const unsigned load_bytes = 1U << load.eew;
        const unsigned load_elements = 16U >> load.eew;
        const std::uint16_t load_active = memblock::active_vector_elements(load);
        for (unsigned element = 0; element < load_elements; ++element) {
            if (((load_active >> element) & 1U) == 0) {
                continue;
            }
            const auto address = memblock::vector_element_address(load, element);
            for (unsigned byte = 0; byte < load_bytes; ++byte) {
                const auto it = forwarded.find(address + byte);
                if (it != forwarded.end()) {
                    expected[element * load_bytes + byte] = it->second;
                }
            }
        }
        return expected;
    };
    auto observe_outstanding = [&]() {
        const std::size_t scalar = environment.pending_scalar_loads();
        const std::size_t prefetch = environment.pending_prefetches();
        const std::size_t store = environment.pending_scalar_stores();
        const std::size_t vector_load = environment.pending_vector_loads();
        const std::size_t vector_store = environment.pending_vector_stores();
        const std::size_t total = scalar + prefetch + store + vector_load + vector_store;
        coverage.max_outstanding = std::max<std::uint64_t>(coverage.max_outstanding, total);
    };

    if (!environment.reset() || !environment.enable_misaligned_accesses()) {
        std::cerr << "MEMBLOCK_RANDOM_STRESS_FAIL seed=" << options.seed
                  << " transactions=0 cycle=" << environment.cycle()
                  << " reason=" << environment.error() << '\n';
        return 1;
    }

    while (completed_actions < options.transactions) {
        phase = "burst-" + std::to_string(burst_index);
        const unsigned remaining = options.transactions - completed_actions;
        const unsigned burst_actions = remaining >= 12 && burst_index >= 4 &&
                burst_index % 5 != 0
            ? 12
            : std::min(6U, remaining);
        const unsigned group_count = burst_actions > 6 ? 2 : 1;
        std::vector<Group> groups(group_count);
        std::vector<Action> issue_actions;
        issue_actions.reserve(burst_actions);
        const bool second_region = (burst_index & 1U) != 0;
        const std::uint64_t region = second_region ? region1 : region0;
        coverage.memory_regions = second_region ? 2 : std::max<std::uint64_t>(1, coverage.memory_regions);

        for (unsigned group_index = 0; group_index < group_count; ++group_index) {
            const unsigned slots = std::min(6U, burst_actions - group_index * 6);
            auto &group = groups[group_index];
            const unsigned eew = burst_index < 4
                ? burst_index
                : (group_count == 2 ? 1U + random.next_shape() % 3U
                                     : random.next_shape() % 4U);
            auto vector_mode = static_cast<memblock::VectorAddressingMode>(
                burst_index < 3 ? burst_index : random.next_shape() % 3);
            // Ordered indexed operations must see all older LSQ elements
            // retired. Keep that mode in the single-group prefix; later
            // two-group bursts still cover indexed unordered traffic while
            // preserving genuine outstanding pressure.
            if (group_count == 2 &&
                vector_mode == memblock::VectorAddressingMode::indexed_ordered) {
                vector_mode = memblock::VectorAddressingMode::indexed_unordered;
            }
            const auto store_mode = vector_mode ==
                    memblock::VectorAddressingMode::indexed_ordered
                ? memblock::VectorAddressingMode::indexed_unordered
                : vector_mode;
            const std::uint64_t base = region + (burst_index % 256) * 0x1000 +
                group_index * 0x400;
            const unsigned store_op_value = burst_index < 4
                ? burst_index
                : random.next_shape() % 4;
            // Keep the store/forwarding pair naturally aligned.  The scalar
            // load in front of it still alternates aligned and split-line
            // addresses, so the stress campaign retains misaligned DCache
            // traffic without depending on StoreQueue's known unsupported
            // unaligned-forwarding replay path.
            const auto store_address = base + 0x80;
            // ROB order intentionally puts an independent load before the
            // store, then creates dependent scalar/vector loads after it.
            // This gives pendingPtr a legal head while retaining real
            // forwarding checks for the younger loads.
            group.has_scalar_load = slots >= 1;
            group.has_scalar_store = slots >= 2;
            group.has_extra_load = slots >= 3;
            group.has_vector_store = slots >= 4;
            group.has_vector_load = slots >= 5;
            group.has_prefetch = slots >= 6;
            if (group.has_scalar_load) {
                const bool is_unsigned = store_op_value != 3 &&
                    (random.next_shape() & 1U) != 0;
                group.scalar_load = make_load(
                    base + 0x40 + ((burst_index & 1U) ? 1U : 0U),
                    static_cast<memblock::LoadOp>(store_op_value +
                        (is_unsigned ? 4U : 0U)),
                    (burst_index + group_index) % memblock::kScalarLoadLanes);
            }
            if (group.has_scalar_store) {
                group.scalar_store = make_store(
                    store_address, random.next_payload(),
                    static_cast<memblock::StoreOp>(store_op_value),
                    group_index % memblock::kScalarStoreLanes,
                    (group_index + 1) % memblock::kScalarStoreLanes);
            }
            if (group.has_extra_load) {
                group.extra_load = make_load(
                    store_address, static_cast<memblock::LoadOp>(store_op_value),
                    (burst_index + group_index + 2) % memblock::kScalarLoadLanes);
            }
            if (group.has_vector_store) {
                group.vector_store = make_vector(
                    true, base + 0x200 +
                        ((burst_index & 1U) ? (1U << eew) : 0U),
                    eew, (burst_index + group_index) % memblock::kVectorMemoryLanes,
                    store_mode);
                group.vector_store.vm = (burst_index % 2) == 0;
                group.vector_store.mask_bits = static_cast<std::uint16_t>(
                    random.next_shape());
                const unsigned elements = 16U >> eew;
                group.vector_store.vl = burst_index % 2 == 0
                    ? static_cast<std::uint8_t>(elements)
                    : static_cast<std::uint8_t>(
                        1U + random.next_shape() % elements);
                group.vector_store.vstart = burst_index % 3 == 1 &&
                        group.vector_store.vl != 0
                    ? static_cast<std::uint8_t>(std::min<unsigned>(1, group.vector_store.vl))
                    : 0;
                randomize_vector_addressing(group.vector_store);
            }
            if (group.has_vector_load) {
                group.vector_load = make_vector(
                    false, group.vector_store.address, eew,
                    group_index % memblock::kVectorMemoryLanes,
                    vector_mode);
                group.vector_load.vl = group.vector_store.vl;
                group.vector_load.vstart = group.vector_store.vstart;
                group.vector_load.vm = group.vector_store.vm;
                group.vector_load.mask_bits = group.vector_store.mask_bits;
                group.vector_load.vma = group.vector_store.vma;
                group.vector_load.vta = group.vector_store.vta;
                group.vector_load.stride = group.vector_store.stride;
                group.vector_load.index = group.vector_store.index;
                group.vector_load.flow_num = group.vector_store.flow_num;
            }
            if (group.has_prefetch) {
                group.prefetch = make_prefetch(
                    base + 0x380, static_cast<memblock::PrefetchOp>(8 + burst_index % 3),
                    (burst_index + group_index) % memblock::kScalarLoadLanes);
            }
            if (group.has_scalar_load) {
                environment.expect_load(group.scalar_load);
                if (!environment.enqueue_load(group.scalar_load)) {
                    break;
                }
                issue_actions.push_back({Kind::scalar_load, group_index});
            }
            if (group.has_scalar_store) {
                environment.expect_store(group.scalar_store);
                if (!environment.enqueue_store(
                        group.scalar_store, memblock::lq_pointer_value(lq_offset))) {
                    break;
                }
                issue_actions.push_back({Kind::scalar_store, group_index});
            }
            if (group.has_extra_load) {
                environment.expect_load_data(
                    group.extra_load,
                    scalar_forward_value(group.scalar_store, group.extra_load));
                if (!environment.enqueue_load(group.extra_load)) {
                    break;
                }
                issue_actions.push_back({Kind::extra_load, group_index});
            }
            if (group.has_vector_store) {
                environment.expect_vector(group.vector_store);
                if (!environment.enqueue_vector(group.vector_store)) {
                    break;
                }
                issue_actions.push_back({Kind::vector_store, group_index});
            }
            if (group.has_vector_load) {
                environment.expect_vector_data(
                    group.vector_load,
                    vector_forward_value(group.vector_store, group.vector_load));
                if (!environment.enqueue_vector(group.vector_load)) {
                    break;
                }
                issue_actions.push_back({Kind::vector_load, group_index});
            }
            if (group.has_prefetch) {
                environment.expect_prefetch(group.prefetch);
                if (!environment.enqueue_prefetch(group.prefetch)) {
                    break;
                }
                issue_actions.push_back({Kind::prefetch, group_index});
            }
        }
        if (!environment.ok() || issue_actions.size() != burst_actions) {
            break;
        }
        const Group &first = groups.front();
        const auto issue_head = first.has_scalar_load
            ? memblock::rob_identity(first.scalar_load.rob, first.scalar_load.rob_flag)
            : first.has_scalar_store
                ? memblock::rob_identity(first.scalar_store.rob, first.scalar_store.rob_flag)
                : first.has_extra_load
                    ? memblock::rob_identity(first.extra_load.rob, first.extra_load.rob_flag)
                    : first.has_vector_store
                        ? memblock::rob_identity(first.vector_store.rob, first.vector_store.rob_flag)
                        : first.has_vector_load
                            ? memblock::rob_identity(first.vector_load.rob, first.vector_load.rob_flag)
                            : memblock::rob_identity(first.prefetch.rob, first.prefetch.rob_flag);
        if (!environment.set_rob_head(issue_head.value, issue_head.flag)) {
            break;
        }
        std::vector<bool> issued(issue_actions.size(), false);
        unsigned issued_count = 0;
        while (issued_count < issue_actions.size()) {
            std::vector<unsigned> candidates;
            for (unsigned index = 0; index < issue_actions.size(); ++index) {
                if (issued[index]) {
                    continue;
                }
                const auto action = issue_actions[index];
                bool dependency_ready = true;
                if (action.kind == Kind::extra_load) {
                    for (unsigned prior = 0; prior < issue_actions.size(); ++prior) {
                        if (issue_actions[prior].group == action.group &&
                            issue_actions[prior].kind == Kind::scalar_store) {
                            dependency_ready = issued[prior];
                        }
                    }
                }
                if (action.kind == Kind::vector_load) {
                    for (unsigned prior = 0; prior < issue_actions.size(); ++prior) {
                        if (issue_actions[prior].group == action.group &&
                            issue_actions[prior].kind == Kind::vector_store) {
                            dependency_ready = issued[prior];
                        }
                    }
                }
                if (dependency_ready) {
                    candidates.push_back(index);
                }
            }
            if (candidates.empty()) {
                phase = "issue-dependency-deadlock";
                break;
            }
            const unsigned selected = candidates[
                random.next_schedule() % candidates.size()];
            const auto action = issue_actions[selected];
            auto &group = groups[action.group];
            bool ok = false;
            std::uint64_t requests_before = environment.tilelink_requests();
            switch (action.kind) {
            case Kind::scalar_store: {
                const bool data_first = (random.next_schedule() & 1U) != 0;
                ok = data_first
                    ? environment.issue_store_data(group.scalar_store, 2048) &&
                          environment.issue_store_address(group.scalar_store, 2048)
                    : environment.issue_store_address(group.scalar_store, 2048) &&
                          environment.issue_store_data(group.scalar_store, 2048);
                coverage.sample(group.scalar_store, data_first);
                break;
            }
            case Kind::scalar_load:
                ok = environment.issue_load(group.scalar_load, 2048);
                coverage.sample(group.scalar_load, requests_before,
                                environment.tilelink_requests());
                break;
            case Kind::vector_store:
                ok = environment.issue_vector(group.vector_store, 2048);
                coverage.sample(group.vector_store, requests_before,
                                environment.tilelink_requests());
                break;
            case Kind::vector_load:
                ok = environment.issue_vector(group.vector_load, 2048);
                coverage.sample(group.vector_load, requests_before,
                                environment.tilelink_requests());
                ++coverage.vector_forwarding;
                break;
            case Kind::prefetch:
                ok = environment.issue_prefetch(group.prefetch, 2048);
                coverage.sample(group.prefetch);
                break;
            case Kind::extra_load:
                ok = environment.issue_load(group.extra_load, 2048);
                coverage.sample(group.extra_load, requests_before,
                                environment.tilelink_requests());
                ++coverage.scalar_forwarding;
                break;
            }
            if (!ok || !environment.run_cycles(static_cast<unsigned>(
                    random.next_schedule() % 4))) {
                break;
            }
            issued[selected] = true;
            ++issued_count;
            observe_outstanding();
        }
        if (issued_count != issue_actions.size()) {
            break;
        }
        const Group &last = groups.back();
        const memblock::RobIdentity last_rob = last.has_prefetch
            ? memblock::rob_identity(last.prefetch.rob, last.prefetch.rob_flag)
            : last.has_vector_load
                ? memblock::rob_identity(last.vector_load.rob, last.vector_load.rob_flag)
                : last.has_vector_store
                    ? memblock::rob_identity(last.vector_store.rob, last.vector_store.rob_flag)
                    : last.has_extra_load
                        ? memblock::rob_identity(last.extra_load.rob, last.extra_load.rob_flag)
                        : last.has_scalar_store
                            ? memblock::rob_identity(last.scalar_store.rob, last.scalar_store.rob_flag)
                            : memblock::rob_identity(last.scalar_load.rob, last.scalar_load.rob_flag);
        if (!environment.set_rob_head(last_rob.value, last_rob.flag) ||
            !environment.run_until_all_complete(8192) ||
            !environment.run_until_queues_retired(8192) ||
            !environment.run_cycles(4)) {
            break;
        }
        for (const auto &group : groups) {
            if (group.has_scalar_store) {
                environment.record_committed_store(group.scalar_store);
            }
            if (group.has_vector_store) {
                environment.record_committed_vector_store(group.vector_store);
            }
        }
        bool masked_vector = false;
        bool unmasked_vector = false;
        bool strided_vector = false;
        bool indexed_vector = false;
        bool scalar_misaligned = false;
        bool scalar_forwarding = false;
        for (const auto &group : groups) {
            if (group.has_vector_store) {
                masked_vector |= !group.vector_store.vm;
                unmasked_vector |= group.vector_store.vm;
                strided_vector |= group.vector_store.addressing ==
                    memblock::VectorAddressingMode::strided;
                indexed_vector |= group.vector_store.addressing ==
                    memblock::VectorAddressingMode::indexed_unordered;
            }
            if (group.has_vector_load) {
                strided_vector |= group.vector_load.addressing ==
                    memblock::VectorAddressingMode::strided;
                indexed_vector |= group.vector_load.addressing ==
                    memblock::VectorAddressingMode::indexed_unordered;
            }
            if (group.has_scalar_load) {
                const unsigned bytes = 1U <<
                    (static_cast<unsigned>(group.scalar_load.op) & 3U);
                scalar_misaligned |= (group.scalar_load.address & (bytes - 1)) != 0;
            }
            if (group.has_scalar_store) {
                const unsigned bytes = 1U <<
                    static_cast<unsigned>(group.scalar_store.op);
                scalar_misaligned |= (group.scalar_store.address & (bytes - 1)) != 0;
            }
            scalar_forwarding |= group.has_extra_load;
        }
        // These counters represent actual generated crosses, rather than
        // phase indices, so a passing gate proves feature overlap occurred.
        if (group_count == 2 && masked_vector) {
            ++coverage.combinations[0];
        }
        if (group_count == 2 && unmasked_vector) {
            ++coverage.combinations[1];
        }
        if (scalar_misaligned && strided_vector) {
            ++coverage.combinations[2];
        }
        if (indexed_vector && scalar_forwarding) {
            ++coverage.combinations[3];
        }
        ++coverage.waves;
        coverage.actions += burst_actions;
        completed_actions += burst_actions;
        ++burst_index;
    }

    coverage.dcache_request_stalls = environment.dcache_request_stalls();
    coverage.dcache_response_delays = environment.dcache_response_delays();
    coverage.ptw_request_stalls = environment.ptw_request_stalls();
    coverage.ptw_response_delays = environment.ptw_response_delays();
    coverage.uncache_request_stalls = environment.uncache_request_stalls();
    coverage.uncache_response_delays = environment.uncache_response_delays();
    const bool passed = completed_actions == options.transactions &&
        environment.ok() && coverage.complete() &&
        coverage.backpressure_complete(options.backpressure) &&
        environment.lq_dequeued() + environment.lq_canceled() == environment.lq_allocated() &&
        environment.sq_dequeued() + environment.sq_canceled() == environment.sq_allocated();
    if (!passed) {
        std::cerr << "MEMBLOCK_RANDOM_STRESS_FAIL seed=" << options.seed
                  << " transactions=" << completed_actions
                  << " requested=" << options.transactions
                  << " cycle=" << environment.cycle()
                  << " phase=" << phase
                  << " lq=" << environment.lq_dequeued() << '+'
                  << environment.lq_canceled() << '/' << environment.lq_allocated()
                  << " sq=" << environment.sq_dequeued() << '+'
                  << environment.sq_canceled() << '/' << environment.sq_allocated()
                  << " reason=" << (environment.error().empty()
                                         ? "stress_coverage_or_accounting_gate_failed"
                                         : environment.error())
                  << ' ' << coverage.summary() << '\n';
        return 1;
    }
    std::cout << "MEMBLOCK_RANDOM_STRESS_PASS"
              << " seed=" << options.seed
              << " transactions=" << completed_actions
              << " cycle=" << environment.cycle()
              << " tilelink_requests=" << environment.tilelink_requests()
              << " ptw_requests=" << environment.ptw_requests()
              << " uncache_requests=" << environment.uncache_requests()
              << " lq=" << environment.lq_dequeued() << '+'
              << environment.lq_canceled() << '/' << environment.lq_allocated()
              << " sq=" << environment.sq_dequeued() << '+'
              << environment.sq_canceled() << '/' << environment.sq_allocated()
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
        if (options.test == "fp-loads") {
            return run_fp_loads(argc, argv);
        }
        if (options.test == "trigger-contracts") {
            return run_trigger_contracts(argc, argv);
        }
        if (options.test == "metadata-contracts") {
            return run_metadata_contracts(argc, argv);
        }
        if (options.test == "dcache-errors") {
            return run_dcache_errors(argc, argv);
        }
        if (options.test == "atomic-contracts") {
            return run_atomic_contracts(argc, argv);
        }
        if (options.test == "uncache-errors") {
            return run_uncache_errors(argc, argv);
        }
        if (options.test == "atomic-dchannel-errors") {
            return run_atomic_dchannel_errors(argc, argv);
        }
        if (options.test == "uncache-widths") {
            return run_uncache_widths(argc, argv);
        }
        if (options.test == "mmio-contracts") {
            return run_mmio_contracts(argc, argv);
        }
        if (options.test == "cbo-zero-contracts") {
            return run_cbo_zero_contracts(argc, argv);
        }
        if (options.test == "reset-recovery") {
            return run_reset_recovery(argc, argv);
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
        if (options.test == "l2-tlb-contracts") {
            return run_l2_tlb_contracts(argc, argv);
        }
        if (options.test == "two-stage-translation") {
            return run_two_stage_translation(argc, argv);
        }
        if (options.test == "translation-matrix") {
            return run_translation_matrix(argc, argv);
        }
        if (options.test == "translation-fence") {
            return run_translation_fence(argc, argv);
        }
        if (options.test == "translation-context") {
            return run_translation_context(argc, argv);
        }
        if (options.test == "translation-bare") {
            return run_translation_bare(argc, argv);
        }
        if (options.test == "translation-faults") {
            return run_translation_faults(argc, argv);
        }
        if (options.test == "translation-permissions") {
            return run_translation_permissions(argc, argv);
        }
        if (options.test == "translation-superpages") {
            return run_translation_superpages(argc, argv);
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
        if (options.test == "random-stress") {
            return run_random_stress(argc, argv, options);
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
