#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

unsigned long long MixedScenario::hypervisor_virtual_base(unsigned spvp, unsigned pbmt,
    unsigned topology)
{
    return 0x70000000ULL +
        (spvp * RandomConstraints::hypervisor_pbmt_pair_count + pbmt) *
        0x400000ULL +
        topology * 0x100000ULL;
}

unsigned long long MixedScenario::hypervisor_guest_base(unsigned spvp, unsigned pbmt,
    unsigned topology)
{
    return 0x1000000000ULL +
        (spvp * RandomConstraints::hypervisor_pbmt_pair_count + pbmt) *
        0x400000ULL +
        topology * 0x100000ULL;
}

unsigned long long MixedScenario::hypervisor_physical_base(unsigned pbmt,
    unsigned topology)
{
    return 0x200000000ULL + pbmt * 0x400000ULL +
        topology * 0x100000ULL;
}

unsigned long long MixedScenario::hypervisor_pma_device_virtual_base(unsigned spvp,
    unsigned topology)
{
    return 0x74000000ULL + spvp * 0x400000ULL + topology * 0x100000ULL;
}

unsigned long long MixedScenario::hypervisor_pma_device_guest_base(unsigned spvp,
    unsigned topology)
{
    return 0x1040000000ULL + spvp * 0x400000ULL + topology * 0x100000ULL;
}

unsigned long long MixedScenario::hypervisor_pma_device_physical_base(unsigned spvp,
    unsigned topology)
{
    return 0x35000000ULL + spvp * 0x400000ULL + topology * 0x100000ULL;
}

unsigned long long MixedScenario::hypervisor_pmp_virtual_base(unsigned spvp,
    unsigned topology)
{
    return 0x75000000ULL + spvp * 0x400000ULL + topology * 0x100000ULL;
}

unsigned long long MixedScenario::hypervisor_pmp_guest_base(unsigned spvp,
    unsigned topology)
{
    return 0x1050000000ULL + spvp * 0x400000ULL + topology * 0x100000ULL;
}

unsigned long long MixedScenario::hypervisor_pmp_physical_base(unsigned spvp,
    unsigned topology)
{
    return 0x240000000ULL + spvp * 0x400000ULL + topology * 0x100000ULL;
}

std::uint64_t MixedScenario::action_cacheable_address(ActionWindow &window,
    unsigned alignment,
    std::optional<bool> force_misaligned, bool allow_permuted)
{
    auto &translation = window.translation;
    auto &leaf = window.leaf;
    auto &leaf_addressed = window.leaf_addressed;
    leaf_addressed = true;
    return constrained_cacheable_address(
        alignment, translation, leaf, force_misaligned, allow_permuted);
}

} // namespace memblock::scenarios::mixed
