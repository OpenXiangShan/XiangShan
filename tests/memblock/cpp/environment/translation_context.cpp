#include "implementation.hpp"

namespace memblock {

auto Environment::Impl::map_sv39x4_4k(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool global)  -> bool
{
    return map_reference_leaf(
        guest_physical_address, host_physical_address, root_page_table,
        ReferencePageMode::sv39, true, 0, readable, writable,
        executable, true, false, true, std::nullopt, false, global);
}

auto Environment::Impl::map_sv39x4_leaf(std::uint64_t guest_physical_address,
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
        ReferencePageMode::sv39, true, leaf_level, readable, writable,
        executable, user, noncacheable, accessed, dirty, io, global);
}

auto Environment::Impl::map_sv39x4_2m(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable)  -> bool
{
    return map_sv39x4_leaf(
        guest_physical_address, host_physical_address, 1,
        root_page_table, readable, writable, executable);
}

auto Environment::Impl::map_sv39x4_1g(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable)  -> bool
{
    return map_sv39x4_leaf(
        guest_physical_address, host_physical_address, 2,
        root_page_table, readable, writable, executable);
}

auto Environment::Impl::activate_two_stage(std::uint64_t vs_root_page_table,
    std::uint64_t g_root_page_table,
    std::uint16_t asid,
    std::uint16_t vmid)  -> bool
{
    return activate_two_stage_modes(
        ReferencePageMode::sv39,
        ReferencePageMode::sv39,
        vs_root_page_table,
        g_root_page_table,
        asid,
        vmid);
}

auto Environment::Impl::activate_two_stage_modes(ReferencePageMode vs_mode,
    ReferencePageMode g_mode,
    std::uint64_t vs_root_page_table,
    std::uint64_t g_root_page_table,
    std::uint16_t asid,
    std::uint16_t vmid)  -> bool
{
    if (!write_distributed_csr(0x3b0, ~std::uint64_t{0}) ||
        !write_distributed_csr(0x3a0, 0x1f)) {
        return false;
    }
    dut_.translation.csr.satp_mode.set(std::uint64_t{0});
    dut_.translation.csr.vsatp_mode.set(
        static_cast<std::uint64_t>(vs_mode));
    dut_.translation.csr.vsatp_asid.set(asid);
    dut_.translation.csr.vsatp_ppn.set(vs_root_page_table >> 12);
    dut_.translation.csr.hgatp_mode.set(
        static_cast<std::uint64_t>(g_mode));
    dut_.translation.csr.hgatp_vmid.set(vmid);
    dut_.translation.csr.hgatp_ppn.set(g_root_page_table >> 12);
    dut_.translation.csr.priv_dmode.set(std::uint64_t{1});
    dut_.translation.csr.priv_virt.set(std::uint64_t{1});
    dut_.translation.csr.vsatp_changed.set(std::uint64_t{1});
    dut_.translation.csr.hgatp_changed.set(std::uint64_t{1});
    dut_.translation.csr.priv_virt_changed.set(std::uint64_t{1});
    tick(false);
    dut_.translation.csr.vsatp_changed.set(std::uint64_t{0});
    dut_.translation.csr.hgatp_changed.set(std::uint64_t{0});
    dut_.translation.csr.priv_virt_changed.set(std::uint64_t{0});
    return run_cycles(16) && check_components();
}

auto Environment::Impl::update_stage_one_context(ReferencePageMode mode,
    std::uint64_t root_page_table,
    std::uint16_t asid)  -> bool
{
    dut_.translation.csr.satp_mode.set(
        static_cast<std::uint64_t>(mode));
    dut_.translation.csr.satp_asid.set(asid);
    dut_.translation.csr.satp_ppn.set(root_page_table >> 12);
    dut_.translation.csr.satp_changed.set(std::uint64_t{1});
    tick(false);
    dut_.translation.csr.satp_changed.set(std::uint64_t{0});
    return run_cycles(16) && check_components();
}

auto Environment::Impl::update_vs_context(ReferencePageMode mode,
    std::uint64_t root_page_table,
    std::uint16_t asid)  -> bool
{
    dut_.translation.csr.vsatp_mode.set(
        static_cast<std::uint64_t>(mode));
    dut_.translation.csr.vsatp_asid.set(asid);
    dut_.translation.csr.vsatp_ppn.set(root_page_table >> 12);
    dut_.translation.csr.vsatp_changed.set(std::uint64_t{1});
    tick(false);
    dut_.translation.csr.vsatp_changed.set(std::uint64_t{0});
    return run_cycles(16) && check_components();
}

auto Environment::Impl::update_two_stage_context(ReferencePageMode vs_mode,
    ReferencePageMode g_mode,
    std::uint64_t vs_root_page_table,
    std::uint64_t g_root_page_table,
    std::uint16_t asid,
    std::uint16_t vmid)  -> bool
{
    dut_.translation.csr.vsatp_mode.set(
        static_cast<std::uint64_t>(vs_mode));
    dut_.translation.csr.vsatp_asid.set(asid);
    dut_.translation.csr.vsatp_ppn.set(vs_root_page_table >> 12);
    dut_.translation.csr.hgatp_mode.set(
        static_cast<std::uint64_t>(g_mode));
    dut_.translation.csr.hgatp_vmid.set(vmid);
    dut_.translation.csr.hgatp_ppn.set(g_root_page_table >> 12);
    dut_.translation.csr.vsatp_changed.set(std::uint64_t{1});
    dut_.translation.csr.hgatp_changed.set(std::uint64_t{1});
    tick(false);
    dut_.translation.csr.vsatp_changed.set(std::uint64_t{0});
    dut_.translation.csr.hgatp_changed.set(std::uint64_t{0});
    return run_cycles(16) && check_components();
}

auto Environment::Impl::update_g_context(ReferencePageMode mode,
    std::uint64_t root_page_table,
    std::uint16_t vmid)  -> bool
{
    dut_.translation.csr.hgatp_mode.set(
        static_cast<std::uint64_t>(mode));
    dut_.translation.csr.hgatp_vmid.set(vmid);
    dut_.translation.csr.hgatp_ppn.set(root_page_table >> 12);
    dut_.translation.csr.hgatp_changed.set(std::uint64_t{1});
    tick(false);
    dut_.translation.csr.hgatp_changed.set(std::uint64_t{0});
    return run_cycles(16) && check_components();
}

auto Environment::Impl::set_translation_permissions(ReferencePrivilegeMode data_privilege,
    bool mxr,
    bool sum,
    bool vmxr,
    bool vsum)  -> bool
{
    dut_.translation.csr.priv_dmode.set(
        static_cast<std::uint64_t>(data_privilege));
    dut_.translation.csr.priv_mxr.set(mxr);
    dut_.translation.csr.priv_sum.set(sum);
    dut_.translation.csr.priv_vmxr.set(vmxr);
    dut_.translation.csr.priv_vsum.set(vsum);
    // TlbCsrBundle is registered and duplicated before it reaches every
    // DTLB port. Let all copies settle before issuing the first request.
    return run_cycles(16) && check_components();
}

auto Environment::Impl::set_instruction_privilege(ReferencePrivilegeMode privilege)  -> bool
{
    dut_.translation.csr.priv_imode.set(
        static_cast<std::uint64_t>(privilege));
    return run_cycles(16) && check_components();
}

auto Environment::Impl::set_debug_mode(bool enabled)  -> bool
{
    dut_.translation.csr.priv_debug.set(enabled);
    // TlbCsrBundle is registered and duplicated before its PMA/PMP use.
    return run_cycles(16) && check_components();
}

auto Environment::Impl::set_hypervisor_access_permissions(ReferencePrivilegeMode spvp,
    bool mxr,
    bool vmxr,
    bool vsum,
    ReferencePrivilegeMode current_privilege)  -> bool
{
    dut_.translation.csr.priv_dmode.set(
        static_cast<std::uint64_t>(current_privilege));
    dut_.translation.csr.priv_virt.set(std::uint64_t{0});
    dut_.translation.csr.priv_spvp.set(
        static_cast<std::uint64_t>(spvp));
    dut_.translation.csr.priv_mxr.set(mxr);
    dut_.translation.csr.priv_vmxr.set(vmxr);
    dut_.translation.csr.priv_vsum.set(vsum);
    dut_.translation.csr.priv_virt_changed.set(std::uint64_t{1});
    tick(false);
    dut_.translation.csr.priv_virt_changed.set(std::uint64_t{0});
    return run_cycles(16) && check_components();
}

auto Environment::Impl::configure_pmp(const std::vector<std::uint64_t> &encoded_addresses,
    const std::vector<std::uint8_t> &config_bytes)  -> bool
{
    constexpr unsigned pmp_entries = 32;
    constexpr unsigned entries_per_config_csr = 8;
    constexpr std::uint16_t pmpcfg0 = 0x3a0;
    constexpr std::uint16_t pmpaddr0 = 0x3b0;
    if (encoded_addresses.size() != config_bytes.size() ||
        encoded_addresses.size() > pmp_entries) {
        error_.record("PMP address/config vectors must have equal size at most 32");
        return false;
    }
    // Address registers must be established before enabling TOR/NAPOT.
    // This ordering also lets tests observe that a locked entry rejects a
    // later address rewrite.
    for (unsigned index = 0; index < encoded_addresses.size(); ++index) {
        if (!write_distributed_csr(
                static_cast<std::uint16_t>(pmpaddr0 + index),
                encoded_addresses[index])) {
            return false;
        }
    }
    for (unsigned bank = 0; bank < pmp_entries / entries_per_config_csr;
         ++bank) {
        std::uint64_t packed = 0;
        for (unsigned byte = 0; byte < entries_per_config_csr; ++byte) {
            const unsigned index = bank * entries_per_config_csr + byte;
            if (index < config_bytes.size()) {
                packed |= std::uint64_t{config_bytes[index]} << (byte * 8);
            }
        }
        if (!write_distributed_csr(
                static_cast<std::uint16_t>(pmpcfg0 + bank * 2), packed)) {
            return false;
        }
    }
    return run_cycles(16) && check_components();
}

auto Environment::Impl::set_page_based_memory_types(bool machine_enabled,
    bool hypervisor_enabled)  -> bool
{
    dut_.translation.csr.m_pbmte.set(machine_enabled);
    dut_.translation.csr.h_pbmte.set(hypervisor_enabled);
    return run_cycles(16) && check_components();
}

auto Environment::Impl::set_uncache_write_outstanding(bool enabled)  -> bool
{
    dut_.control.csr.uncache_write_outstanding_enable.set(
        enabled);
    return run_cycles(16) && check_components();
}

} // namespace memblock
