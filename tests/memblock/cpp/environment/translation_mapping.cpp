#include "implementation.hpp"

namespace memblock {

auto Environment::Impl::configure_sv39(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool writable,
    bool noncacheable)  -> bool
{
    constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
    constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
    constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
    constexpr std::uint64_t pte_accessed = std::uint64_t{1} << 6;
    constexpr std::uint64_t pte_dirty = std::uint64_t{1} << 7;
    constexpr std::uint64_t pte_pbmt_nc = std::uint64_t{1} << 61;
    constexpr std::uint64_t gigabyte_mask = (std::uint64_t{1} << 30) - 1;
    if ((physical_address & gigabyte_mask) !=
        (virtual_address & gigabyte_mask)) {
        error_.record("Sv39 NC helper requires equal 1-GiB page offsets");
        return false;
    }
    const std::uint64_t physical_base = physical_address & ~gigabyte_mask;
    const std::uint64_t vpn2 = (virtual_address >> 30) & 0x1ff;
    const std::uint64_t permissions = pte_read | (writable ? pte_write : 0);
    const std::uint64_t pte = ((physical_base >> 12) << 10) |
                              (noncacheable ? pte_pbmt_nc : 0) |
                              pte_dirty | pte_accessed |
                              permissions | pte_valid;
    memory_.write_u64(root_page_table + vpn2 * 8, pte);

    // Permit S-mode page-table walks and data accesses over the full PA space.
    if (!write_distributed_csr(0x3b0, ~std::uint64_t{0}) ||
        !write_distributed_csr(0x3a0, 0x1f)) {
        return false;
    }
    dut_.translation.csr.priv_dmode.set(std::uint64_t{1});
    dut_.translation.csr.satp_mode.set(std::uint64_t{8});
    dut_.translation.csr.satp_asid.set(std::uint64_t{0});
    dut_.translation.csr.satp_ppn.set(root_page_table >> 12);
    dut_.translation.csr.m_pbmte.set(std::uint64_t{1});
    dut_.translation.csr.priv_virt.set(std::uint64_t{0});
    dut_.translation.csr.satp_changed.set(std::uint64_t{1});
    dut_.translation.csr.priv_virt_changed.set(std::uint64_t{1});
    tick(false);
    dut_.translation.csr.satp_changed.set(std::uint64_t{0});
    dut_.translation.csr.priv_virt_changed.set(std::uint64_t{0});
    // MemBlock pipelines the TLB CSR and fence indication before the DTLBs.
    // Do not issue the cold miss while that delayed flush is still active.
    return run_cycles(16) && check_components();
}

auto Environment::Impl::configure_sv39_nc(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool writable)  -> bool
{
    return configure_sv39(
        virtual_address, physical_address, root_page_table, writable, true);
}

auto Environment::Impl::map_sv39_4k(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable,
    bool io,
    bool global)  -> bool
{
    return map_reference_leaf(
        virtual_address, physical_address, root_page_table,
        ReferencePageMode::sv39, false, 0, readable, writable,
        executable, user, noncacheable, true, std::nullopt, io, global);
}

auto Environment::Impl::map_sv48_4k(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable,
    bool io,
    bool global)  -> bool
{
    return map_reference_leaf(
        virtual_address, physical_address, root_page_table,
        ReferencePageMode::sv48, false, 0, readable, writable,
        executable, user, noncacheable, true, std::nullopt, io, global);
}

auto Environment::Impl::activate_sv39(std::uint64_t root_page_table,
    std::uint16_t asid)  -> bool
{
    return activate_stage_one(
        ReferencePageMode::sv39, root_page_table, asid);
}

auto Environment::Impl::activate_sv48(std::uint64_t root_page_table,
    std::uint16_t asid)  -> bool
{
    return activate_stage_one(
        ReferencePageMode::sv48, root_page_table, asid);
}

auto Environment::Impl::activate_bare(std::uint16_t asid)  -> bool
{
    return activate_stage_one(ReferencePageMode::bare, 0, asid);
}

auto Environment::Impl::map_sv39_leaf(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    unsigned leaf_level,
    std::uint64_t root_page_table,
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
    return map_reference_leaf(
        virtual_address, physical_address, root_page_table,
        ReferencePageMode::sv39, false, leaf_level, readable, writable,
        executable, user, noncacheable, accessed, dirty, io, global);
}

auto Environment::Impl::map_sv39_2m(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable)  -> bool
{
    return map_sv39_leaf(
        virtual_address, physical_address, 1, root_page_table, readable,
        writable, executable, user, noncacheable);
}

auto Environment::Impl::map_sv39_1g(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable)  -> bool
{
    return map_sv39_leaf(
        virtual_address, physical_address, 2, root_page_table, readable,
        writable, executable, user, noncacheable);
}

auto Environment::Impl::map_sv48_leaf(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    unsigned leaf_level,
    std::uint64_t root_page_table,
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
    return map_reference_leaf(
        virtual_address, physical_address, root_page_table,
        ReferencePageMode::sv48, false, leaf_level, readable, writable,
        executable, user, noncacheable, accessed, dirty, io, global);
}

auto Environment::Impl::map_sv48_2m(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable)  -> bool
{
    return map_sv48_leaf(
        virtual_address, physical_address, 1, root_page_table, readable,
        writable, executable, user, noncacheable);
}

auto Environment::Impl::map_sv48_1g(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable)  -> bool
{
    return map_sv48_leaf(
        virtual_address, physical_address, 2, root_page_table, readable,
        writable, executable, user, noncacheable);
}

auto Environment::Impl::map_sv48_512g(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable)  -> bool
{
    return map_sv48_leaf(
        virtual_address, physical_address, 3, root_page_table, readable,
        writable, executable, user, noncacheable);
}

auto Environment::Impl::map_sv48x4_4k(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool global)  -> bool
{
    return map_reference_leaf(
        guest_physical_address, host_physical_address, root_page_table,
        ReferencePageMode::sv48, true, 0, readable, writable,
        executable, true, false, true, std::nullopt, false, global);
}

auto Environment::Impl::map_sv48x4_leaf(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    unsigned leaf_level,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool accessed,
    std::optional<bool> dirty,
    bool user,
    bool noncacheable,
    bool io,
    bool global)  -> bool
{
    return map_reference_leaf(
        guest_physical_address, host_physical_address, root_page_table,
        ReferencePageMode::sv48, true, leaf_level, readable, writable,
        executable, user, noncacheable, accessed, dirty, io, global);
}

auto Environment::Impl::map_sv48x4_2m(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable)  -> bool
{
    return map_sv48x4_leaf(
        guest_physical_address, host_physical_address, 1,
        root_page_table, readable, writable, executable);
}

auto Environment::Impl::map_sv48x4_1g(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable)  -> bool
{
    return map_sv48x4_leaf(
        guest_physical_address, host_physical_address, 2,
        root_page_table, readable, writable, executable);
}

auto Environment::Impl::map_sv48x4_512g(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable)  -> bool
{
    return map_sv48x4_leaf(
        guest_physical_address, host_physical_address, 3,
        root_page_table, readable, writable, executable);
}

auto Environment::Impl::map_sv39_napot64k(std::uint64_t virtual_base,
    std::uint64_t physical_base,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable,
    bool accessed,
    std::optional<bool> dirty,
    bool io)  -> bool
{
    return map_reference_napot64k(
        virtual_base, physical_base, root_page_table,
        ReferencePageMode::sv39, false, readable, writable, executable,
        user, noncacheable, accessed, dirty, io);
}

auto Environment::Impl::map_sv48_napot64k(std::uint64_t virtual_base,
    std::uint64_t physical_base,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable,
    bool accessed,
    std::optional<bool> dirty,
    bool io)  -> bool
{
    return map_reference_napot64k(
        virtual_base, physical_base, root_page_table,
        ReferencePageMode::sv48, false, readable, writable, executable,
        user, noncacheable, accessed, dirty, io);
}

auto Environment::Impl::map_sv39x4_napot64k(std::uint64_t guest_physical_base,
    std::uint64_t host_physical_base,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool accessed,
    std::optional<bool> dirty,
    bool user,
    bool noncacheable,
    bool io)  -> bool
{
    return map_reference_napot64k(
        guest_physical_base, host_physical_base, root_page_table,
        ReferencePageMode::sv39, true, readable, writable, executable,
        user, noncacheable, accessed, dirty, io);
}

auto Environment::Impl::map_sv48x4_napot64k(std::uint64_t guest_physical_base,
    std::uint64_t host_physical_base,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool accessed,
    std::optional<bool> dirty,
    bool user,
    bool noncacheable,
    bool io)  -> bool
{
    return map_reference_napot64k(
        guest_physical_base, host_physical_base, root_page_table,
        ReferencePageMode::sv48, true, readable, writable, executable,
        user, noncacheable, accessed, dirty, io);
}

auto Environment::Impl::map_reference_leaf(std::uint64_t input_address,
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
    return page_tables_.map_reference_leaf(
        input_address, physical_address, root_page_table, mode, x4, leaf_level, readable, writable, executable, user, noncacheable, accessed, dirty, io, global);
}

auto Environment::Impl::map_reference_napot64k(std::uint64_t input_base,
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
    return page_tables_.map_reference_napot64k(
        input_base, physical_base, root_page_table, mode, x4, readable, writable, executable, user, noncacheable, accessed, dirty, io);
}

auto Environment::Impl::activate_stage_one(ReferencePageMode mode,
    std::uint64_t root_page_table,
    std::uint16_t asid)  -> bool
{
    if (!write_distributed_csr(0x3b0, ~std::uint64_t{0}) ||
        !write_distributed_csr(0x3a0, 0x1f)) {
        return false;
    }
    dut_.translation.csr.priv_dmode.set(std::uint64_t{1});
    dut_.translation.csr.satp_mode.set(
        static_cast<std::uint64_t>(mode));
    dut_.translation.csr.satp_asid.set(asid);
    dut_.translation.csr.satp_ppn.set(root_page_table >> 12);
    dut_.translation.csr.m_pbmte.set(std::uint64_t{1});
    dut_.translation.csr.priv_virt.set(std::uint64_t{0});
    dut_.translation.csr.satp_changed.set(std::uint64_t{1});
    dut_.translation.csr.priv_virt_changed.set(std::uint64_t{1});
    tick(false);
    dut_.translation.csr.satp_changed.set(std::uint64_t{0});
    dut_.translation.csr.priv_virt_changed.set(std::uint64_t{0});
    return run_cycles(16) && check_components();
}

} // namespace memblock
