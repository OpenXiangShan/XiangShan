#include "page_table_builder.hpp"

namespace memblock {

auto PageTableBuilder::map_reference_leaf(std::uint64_t input_address,
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
    bool accessed,
    std::optional<bool> dirty,
    bool io,
    bool global)  -> bool
{
    constexpr std::uint64_t page_mask = 0xfff;
    constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
    constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
    constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
    constexpr std::uint64_t pte_execute = std::uint64_t{1} << 3;
    constexpr std::uint64_t pte_user = std::uint64_t{1} << 4;
    constexpr std::uint64_t pte_global = std::uint64_t{1} << 5;
    constexpr std::uint64_t pte_accessed = std::uint64_t{1} << 6;
    constexpr std::uint64_t pte_dirty = std::uint64_t{1} << 7;
    constexpr std::uint64_t pte_pbmt_nc = std::uint64_t{1} << 61;
    constexpr std::uint64_t pte_pbmt_io = std::uint64_t{1} << 62;
    if (leaf_level >= reference_page_levels(mode)) {
        error_.record("page mapping leaf level exceeds selected mode");
        return false;
    }
    const unsigned offset_bits = 12 + 9 * leaf_level;
    const std::uint64_t offset_mask =
        (std::uint64_t{1} << offset_bits) - 1;
    const std::uint64_t root_mask = x4 ? 0x3fffULL : page_mask;
    if ((root_page_table & root_mask) != 0 ||
        (input_address & offset_mask) != (physical_address & offset_mask)) {
        error_.record(x4
            ? "x4 mapping requires a 16-KiB root and equal leaf offsets"
            : "mapping requires an aligned root and equal leaf offsets");
        return false;
    }
    if (x4 ? !reference_gpa_in_range(input_address, mode)
           : !reference_canonical_virtual_address(input_address, mode)) {
        error_.record(x4 ? "x4 mapping exceeds the guest address space"
                    : "mapping requires a canonical virtual address");
        return false;
    }
    if (writable && !readable) {
        error_.record("page mapping does not permit W=1,R=0 leaf mappings");
        return false;
    }
    if (noncacheable && io) {
        error_.record("page mapping cannot select NC and IO simultaneously");
        return false;
    }

    auto &next_tables = x4 ? next_gstage_page_table_ : next_page_table_;
    const std::uint64_t initial = root_page_table + (x4 ? 0x4000 : 0x1000);
    auto allocate_table = [&]() {
        auto [it, inserted] = next_tables.emplace(root_page_table, initial);
        const std::uint64_t result = it->second;
        it->second += 0x1000;
        return result;
    };

    const unsigned top_level = reference_page_levels(mode) - 1;
    std::uint64_t table = root_page_table;
    for (int level = static_cast<int>(top_level);
         level > static_cast<int>(leaf_level); --level) {
        const unsigned shift = 12 + 9 * static_cast<unsigned>(level);
        const std::uint64_t index_mask =
            x4 && static_cast<unsigned>(level) == top_level ? 0x7ff : 0x1ff;
        const std::uint64_t index = (input_address >> shift) & index_mask;
        const std::uint64_t pte_address = table + index * 8;
        const std::uint64_t pte = memory_.read_u64(pte_address);
        std::uint64_t child = 0;
        if ((pte & 1U) != 0 && !reference_pte_is_leaf(pte)) {
            child = reference_pte_ppn(pte) << 12;
        } else {
            child = allocate_table();
            memory_.write_u64(
                pte_address, ((child >> 12) << 10) | pte_valid);
        }
        table = child;
    }

    const unsigned leaf_shift = 12 + 9 * leaf_level;
    const std::uint64_t leaf_mask =
        x4 && leaf_level == top_level ? 0x7ff : 0x1ff;
    const std::uint64_t leaf_index =
        (input_address >> leaf_shift) & leaf_mask;
    const std::uint64_t flags = pte_valid |
        (readable ? pte_read : 0) |
        (writable ? pte_write : 0) |
        (executable ? pte_execute : 0) |
        (user ? pte_user : 0) |
        (global ? pte_global : 0) |
        (accessed ? pte_accessed : 0) |
        (dirty.value_or(writable) ? pte_dirty : 0) |
        (noncacheable ? pte_pbmt_nc : 0) |
        (io ? pte_pbmt_io : 0);
    memory_.write_u64(
        table + leaf_index * 8,
        (((physical_address & ~page_mask) >> 12) << 10) | flags);
    return true;
}

auto PageTableBuilder::map_reference_napot64k(std::uint64_t input_base,
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
    bool io)  -> bool
{
    constexpr std::uint64_t napot_size = 0x10000;
    constexpr std::uint64_t napot_mask = napot_size - 1;
    constexpr std::uint64_t page_size = 0x1000;
    constexpr std::uint64_t pte_ppn_mask =
        ((std::uint64_t{1} << 44) - 1) << 10;
    constexpr std::uint64_t pte_napot = std::uint64_t{1} << 63;
    if ((input_base & napot_mask) != 0 ||
        (physical_base & napot_mask) != 0) {
        error_.record("Svnapot 64-KiB mapping requires aligned virtual and physical bases");
        return false;
    }

    for (unsigned page = 0; page < 16; ++page) {
        const std::uint64_t offset = page * page_size;
        if (!map_reference_leaf(
                input_base + offset, physical_base + offset,
                root_page_table, mode, x4, 0, readable, writable,
                executable, user, noncacheable, accessed, dirty, io)) {
            return false;
        }
    }

    const std::uint64_t napot_ppn = (physical_base >> 12) | 8U;
    for (unsigned page = 0; page < 16; ++page) {
        const std::uint64_t address = input_base + page * page_size;
        const auto pte_address = reference_pte_address_at_level(
            memory_, root_page_table, address, mode, 0, x4);
        if (!pte_address) {
            error_.record("failed to locate Svnapot leaf PTE");
            return false;
        }
        const std::uint64_t ordinary_pte = memory_.read_u64(*pte_address);
        memory_.write_u64(
            *pte_address,
            (ordinary_pte & ~pte_ppn_mask) | (napot_ppn << 10) |
                pte_napot);
    }
    return true;
}

} // namespace memblock
