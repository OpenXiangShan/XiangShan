#pragma once
#include "memblock_env.hpp"
#include <random>

namespace memblock::scenarios::mixed {

// A bounded independent allocation witness, not a complete rename/free-list
// simulation. Every writing uop gets its own physical destination. Stores and
// operand-only segment uops do not allocate a destination.
inline std::uint8_t random_destination_block(std::mt19937_64 &random,
    unsigned registers, unsigned count)
{
    if (count == 0 || count >= registers) {
        throw std::invalid_argument("physical destination block outside register file");
    }
    return static_cast<std::uint8_t>(1U + random() % (registers - count));
}

// The backend VldMergeUnit is outside MemBlock. Its producer policy fills
// agnostic elements with ones, except a no-op (vstart >= vl). Model this source
// from independent bytes so a later same-logical-Vd uop receives a CPU-compatible
// old destination. This never consumes a DUT result or relaxes its comparator.
inline std::array<unsigned char, 16> vector_load_backend_source(
    const memblock::SparseMemory &memory,
    const memblock::VectorMemoryTransaction &transaction)
{
    auto result = memory.expected_vector_load(transaction);
    if (transaction.vstart >= memblock::vector_effective_vl(transaction)) {
        return result;
    }
    const unsigned width = 1U << memblock::vector_data_eew(transaction);
    const auto agnostic = (transaction.vma
        ? memblock::vector_mask_inactive_elements(transaction) : 0U) |
        (transaction.vta ? memblock::vector_tail_elements(transaction) : 0U);
    for (unsigned element = 0; element < 16U / width; ++element) {
        if ((agnostic >> element) & 1U) {
            std::fill_n(result.begin() + element * width, width, 0xff);
        }
    }
    return result;
}

inline bool vector_load_depends_on_previous(
    const memblock::VectorMemoryTransaction &previous,
    const memblock::VectorMemoryTransaction &current)
{
    return !current.store && memblock::vector_is_special_indexed(current) &&
        previous.rob == current.rob && previous.rob_flag == current.rob_flag &&
        previous.vuop_idx + 1U == current.vuop_idx &&
        memblock::vector_indexed_vd_index(previous) ==
            memblock::vector_indexed_vd_index(current);
}

// Unordered indexed instructions may issue independent destination chains in
// any order, while a repeated-Vd load must follow its predecessor. This samples
// a ready vertex at every step rather than choosing just forward/reverse order.
inline std::vector<unsigned> vector_uop_issue_order(
    const std::vector<memblock::VectorMemoryTransaction> &uops,
    std::mt19937_64 &random)
{
    std::vector<unsigned> order;
    std::vector<bool> chosen(uops.size(), false);
    for (unsigned step = 0; step < uops.size(); ++step) {
        std::vector<unsigned> ready;
        for (unsigned index = 0; index < uops.size(); ++index) {
            if (chosen[index]) continue;
            const bool unordered = uops[index].addressing ==
                memblock::VectorAddressingMode::indexed_unordered;
            if (!unordered && index != step) continue;
            if (index != 0 && vector_load_depends_on_previous(
                    uops[index - 1U], uops[index]) && !chosen[index - 1U]) continue;
            ready.push_back(index);
        }
        if (ready.empty()) throw std::logic_error("vector uop dependency cycle");
        const unsigned index = ready.size() == 1U
            ? ready.front() : ready[random() % ready.size()];
        chosen[index] = true;
        order.push_back(index);
    }
    return order;
}

// Call before registering expectations. EMUL>LMUL repeats a logical Vd, not a
// physical destination. Loads consume the preceding rename's independently
// modeled value; stores reread the same unchanged architectural source.
inline unsigned chain_indexed_vector_sources(
    std::vector<memblock::VectorMemoryTransaction> &uops,
    const memblock::SparseMemory &memory)
{
    unsigned chains = 0;
    for (std::size_t index = 1; index < uops.size(); ++index) {
        auto &current = uops[index];
        const auto &previous = uops[index - 1U];
        if (!memblock::vector_is_special_indexed(current) ||
            memblock::vector_indexed_vd_index(previous) !=
                memblock::vector_indexed_vd_index(current)) {
            continue;
        }
        current.data = current.store ? previous.data
            : vector_load_backend_source(memory, previous);
        ++chains;
    }
    return chains;
}

} // namespace memblock::scenarios::mixed
