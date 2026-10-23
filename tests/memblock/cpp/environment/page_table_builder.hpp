#pragma once
#include "memblock_env.hpp"

namespace memblock {
// Builds the independent page-table image. Allocation state belongs to the
// builder; activation and clocked CSR/fence driving remain with the driver.
class PageTableBuilder {
public:
    PageTableBuilder(SparseMemory &memory, FailureState &error)
        : memory_(memory), error_(error) {}
    bool map_reference_leaf(
        std::uint64_t input_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table,
        ReferencePageMode mode,
        bool x4,
        unsigned leaf_level,
        bool readable,
        bool writable,
        bool executable,
        bool user,
        bool noncacheable,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool io = false,
        bool global = false);
    bool map_reference_napot64k(
        std::uint64_t input_base,
        std::uint64_t physical_base,
        std::uint64_t root_page_table,
        ReferencePageMode mode,
        bool x4,
        bool readable,
        bool writable,
        bool executable,
        bool user,
        bool noncacheable,
        bool accessed,
        std::optional<bool> dirty,
        bool io);
private:
    SparseMemory &memory_;
    FailureState &error_;
    std::unordered_map<std::uint64_t, std::uint64_t> next_page_table_;
    std::unordered_map<std::uint64_t, std::uint64_t> next_gstage_page_table_;
};
} // namespace memblock
