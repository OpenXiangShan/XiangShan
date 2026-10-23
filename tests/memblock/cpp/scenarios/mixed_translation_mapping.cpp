#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

memblock::ReferencePageMode MixedScenario::page_mode(unsigned index)
{
    return index == 0
        ? memblock::ReferencePageMode::sv39
        : memblock::ReferencePageMode::sv48;
}

bool MixedScenario::map_stage_page(unsigned mode, std::uint64_t root,
    std::uint64_t virtual_address,
    std::uint64_t physical_address,
    bool noncacheable,
    bool io,
    bool executable,
    bool user)
{
    return mode == 0
        ? environment.map_sv39_4k(
              virtual_address, physical_address, root, true, true,
              executable, user, noncacheable, io)
        : environment.map_sv48_4k(
              virtual_address, physical_address, root, true, true,
              executable, user, noncacheable, io);
}

bool MixedScenario::map_g_page(unsigned mode, std::uint64_t root,
    std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    bool executable,
    bool noncacheable,
    bool io)
{
    return mode == 0
        ? environment.map_sv39x4_leaf(
              guest_physical_address, host_physical_address, 0, root,
              true, true, executable, true, std::nullopt, true,
              noncacheable, io)
        : environment.map_sv48x4_leaf(
              guest_physical_address, host_physical_address, 0, root,
              true, true, executable, true, std::nullopt, true,
              noncacheable, io);
}

bool MixedScenario::map_stage_2m(unsigned mode, std::uint64_t root,
    std::uint64_t address)
{
    return mode == 0
        ? environment.map_sv39_2m(
              address, address, root, true, true, true)
        : environment.map_sv48_2m(
              address, address, root, true, true, true);
}

bool MixedScenario::map_g_2m(unsigned mode, std::uint64_t root,
    std::uint64_t address)
{
    return mode == 0
        ? environment.map_sv39x4_2m(
              address, address, root, true, true, true)
        : environment.map_sv48x4_2m(
              address, address, root, true, true, true);
}

bool MixedScenario::map_stage_napot(
    unsigned mode, std::uint64_t root,
    std::uint64_t virtual_base,
    std::optional<std::uint64_t> physical_base,
    bool user,
    bool noncacheable,
    bool io)
{
    const std::uint64_t physical = physical_base.value_or(virtual_base);
    return mode == 0
        ? environment.map_sv39_napot64k(
              virtual_base, physical, root, true, true, true, user,
              noncacheable, true, std::nullopt, io)
        : environment.map_sv48_napot64k(
              virtual_base, physical, root, true, true, true, user,
              noncacheable, true, std::nullopt, io);
}

bool MixedScenario::map_g_napot(
    unsigned mode, std::uint64_t root,
    std::uint64_t guest_physical_base,
    std::optional<std::uint64_t> host_physical_base,
    bool noncacheable,
    bool io)
{
    const std::uint64_t physical =
        host_physical_base.value_or(guest_physical_base);
    return mode == 0
        ? environment.map_sv39x4_napot64k(
              guest_physical_base, physical, root,
              true, true, true, true, std::nullopt, true,
              noncacheable, io)
        : environment.map_sv48x4_napot64k(
              guest_physical_base, physical, root,
              true, true, true, true, std::nullopt, true,
              noncacheable, io);
}

bool MixedScenario::prepare_random_translation_contexts()
{
    if (constraints.gstage_boundary_per_mille != 0) {
        for (unsigned mode = 0; mode < 2; ++mode) {
            const unsigned bits = mode == 0 ? 41U : 50U;
            const std::uint64_t limit = std::uint64_t{1} << bits;
            // Separate roots keep GPA-zero aliases clear of ordinary
            // nested mappings. Both sides are permissive truncation controls.
            for (unsigned side = 0; side < 2; ++side) {
                if (!map_g_page(
                        mode, gstage_boundary_roots[mode],
                        side == 0 ? limit - 0x1000 : 0,
                        gstage_boundary_physical +
                            (mode * 2U + side) * 0x1000, true)) {
                    return false;
                }
            }
        }
    }
    if (constraints.canonical_boundary_per_mille != 0) {
        for (unsigned mode = 0; mode < 2; ++mode) {
            const unsigned bits = mode == 0 ? 39U : 48U;
            const std::uint64_t low_limit = std::uint64_t{1} << (bits - 1);
            const std::uint64_t high_base = 0U - low_limit;
            // Map both valid sign-extension aliases permissively. A DUT
            // truncation must succeed here, rather than falsely producing
            // the expected page fault through an unrelated missing PTE.
            for (unsigned side = 0; side < 2; ++side) {
                if (!map_stage_page(
                        mode, random_stage1_roots[mode],
                        side == 0 ? low_limit - 0x1000 : high_base,
                        canonical_boundary_physical +
                            (mode * 2U + side) * 0x1000,
                        false, false, true)) {
                    return false;
                }
            }
        }
    }
    const auto map_all_contexts = [&](std::uint64_t virtual_address,
                                      std::uint64_t physical_address,
                                      bool noncacheable = false,
                                      bool io = false,
                                      bool executable = false) {
        for (unsigned mode = 0; mode < 2; ++mode) {
            if (!map_stage_page(
                    mode, random_stage1_roots[mode], virtual_address,
                    physical_address, noncacheable, io, executable) ||
                !map_stage_page(
                    mode, random_vs_roots[mode], virtual_address,
                    physical_address, noncacheable, io, executable) ||
                !map_g_page(
                    mode, random_g_roots[mode], physical_address,
                    physical_address, executable)) {
                return false;
            }
        }
        return true;
    };

    if (constraints.vector_segment_high_va_per_mille != 0) {
        // A distinct canonical alias retains every XLEN address bit while
        // using the same independent physical memory image as ordinary
        // segment traffic. Do not map a truncated noncanonical address.
        for (unsigned mode = 0; mode < 2; ++mode) {
            const std::uint64_t high_base = 0U -
                (std::uint64_t{1} << (mode == 0 ? 38U : 47U));
            for (std::uint64_t page = cache0_base + 0xf000;
                 page <= cache0_base + 0x92000; page += 0x1000) {
                if (!map_stage_page(
                        mode, random_stage1_roots[mode], high_base + page,
                        page, false, false, true) ||
                    !map_stage_page(
                        mode, random_vs_roots[mode], high_base + page,
                        page, false, false, true)) {
                    return false;
                }
            }
        }
    }
    // Include a page of padding for negative vector strides and a page
    // beyond the nominal cold set for indexed/split vector accesses.
    for (std::uint64_t page = cache0_base + 0xf000;
         page <= cache0_base + 0x92000; page += 0x1000) {
        if (!map_all_contexts(page, page, false, false, true)) {
            return false;
        }
    }
    for (std::uint64_t page = stage1_napot_base;
         page < stage1_napot_base + 0x40000; page += 0x1000) {
        if (!map_all_contexts(page, page, false, false, true)) {
            return false;
        }
    }
    for (std::uint64_t page = ptw_error_base;
         page < ptw_error_base + ptw_error_span; page += 0x1000) {
        const std::uint64_t guest_page =
            ptw_error_guest_base + (page - ptw_error_base);
        for (unsigned mode = 0; mode < 2; ++mode) {
            if (!map_stage_page(
                    mode, random_stage1_roots[mode], page, page,
                    false, false, true) ||
                !map_stage_page(
                    mode, random_vs_roots[mode], page, guest_page,
                    false, false, true) ||
                !map_g_page(
                    mode, random_g_roots[mode], page, page, true) ||
                !map_g_page(
                    mode, random_g_roots[mode], guest_page, page, true)) {
                return false;
            }
        }
    }
    for (unsigned mode = 0; mode < 2; ++mode) {
        if (!map_stage_napot(
                mode, random_stage1_roots[mode], stage1_napot_base) ||
            !map_stage_napot(
                mode, random_vs_roots[mode], nested_vs_napot_base) ||
            !map_stage_napot(
                mode, random_vs_roots[mode], nested_both_napot_base) ||
            !map_g_napot(
                mode, random_g_roots[mode], nested_g_napot_base) ||
            !map_g_napot(
                mode, random_g_roots[mode], nested_both_napot_base)) {
            return false;
        }
    }
    // The following page is deliberately left unmapped.  VFOF actions
    // derive first/later-fault expectations from this independent page
    // table image rather than any DUT TLB or replay state.
    for (unsigned mode = 0; mode < 2; ++mode) {
        if (!map_stage_page(
                mode, random_stage1_roots[mode], vector_fof_virtual_base,
                vector_fof_physical_base, false, false, true)) {
            return false;
        }
    }
    for (std::uint64_t block = cmo_base;
         block < cmo_base + cmo_span; block += 0x200000) {
        for (unsigned mode = 0; mode < 2; ++mode) {
            if (!map_stage_2m(mode, random_stage1_roots[mode], block) ||
                !map_stage_2m(mode, random_vs_roots[mode], block) ||
                !map_g_2m(mode, random_g_roots[mode], block)) {
                return false;
            }
        }
    }
    for (std::uint64_t block = probe_overlap_base;
         block < probe_overlap_base + probe_overlap_span;
         block += 0x200000) {
        for (unsigned mode = 0; mode < 2; ++mode) {
            if (!map_stage_2m(mode, random_stage1_roots[mode], block) ||
                !map_stage_2m(mode, random_vs_roots[mode], block) ||
                !map_g_2m(mode, random_g_roots[mode], block)) {
                return false;
            }
        }
    }
    for (std::uint64_t block = dcache_error_base;
         block < dcache_error_base + dcache_error_span;
         block += 0x200000) {
        for (unsigned mode = 0; mode < 2; ++mode) {
            if (!map_stage_2m(mode, random_stage1_roots[mode], block) ||
                !map_stage_2m(mode, random_vs_roots[mode], block) ||
                !map_g_2m(mode, random_g_roots[mode], block)) {
                return false;
            }
        }
    }
    for (std::uint64_t block = atomic_error_base;
         block < atomic_error_base + atomic_error_span;
         block += 0x200000) {
        for (unsigned mode = 0; mode < 2; ++mode) {
            if (!map_stage_2m(mode, random_stage1_roots[mode], block) ||
                !map_stage_2m(mode, random_vs_roots[mode], block) ||
                !map_g_2m(mode, random_g_roots[mode], block)) {
                return false;
            }
        }
    }
    for (std::uint64_t block = load_merge_base;
         block < load_merge_base + load_merge_span;
         block += 0x200000) {
        for (unsigned mode = 0; mode < 2; ++mode) {
            if (!map_stage_2m(mode, random_stage1_roots[mode], block) ||
                !map_stage_2m(mode, random_vs_roots[mode], block) ||
                !map_g_2m(mode, random_g_roots[mode], block)) {
                return false;
            }
        }
    }
    for (std::uint64_t block = set_pressure_base;
         block < set_pressure_base + set_pressure_span;
         block += 0x200000) {
        for (unsigned mode = 0; mode < 2; ++mode) {
            if (!map_stage_2m(mode, random_stage1_roots[mode], block) ||
                !map_stage_2m(mode, random_vs_roots[mode], block) ||
                !map_g_2m(mode, random_g_roots[mode], block)) {
                return false;
            }
        }
    }
    for (std::uint64_t block = miss_burst_base;
         block < miss_burst_base + miss_burst_span;
         block += 0x200000) {
        for (unsigned mode = 0; mode < 2; ++mode) {
            if (!map_stage_2m(mode, random_stage1_roots[mode], block) ||
                !map_stage_2m(mode, random_vs_roots[mode], block) ||
                !map_g_2m(mode, random_g_roots[mode], block)) {
                return false;
            }
        }
    }
    for (std::uint64_t block = bank_conflict_base;
         block < bank_conflict_base + bank_conflict_span;
         block += 0x200000) {
        for (unsigned mode = 0; mode < 2; ++mode) {
            if (!map_stage_2m(mode, random_stage1_roots[mode], block) ||
                !map_stage_2m(mode, random_vs_roots[mode], block) ||
                !map_g_2m(mode, random_g_roots[mode], block)) {
                return false;
            }
        }
    }
    if (!map_all_contexts(
            cache1_base + 0xf000, cache1_base + 0xf000,
            false, false, true) ||
        !map_all_contexts(atomic_base, atomic_base, false, false, true) ||
        !map_all_contexts(nc_base, nc_base, true, false)) {
        return false;
    }
    for (unsigned spvp = 0; spvp < 2; ++spvp) {
        for (unsigned pbmt = 0; pbmt < hypervisor_pbmt_mappings.size();
             ++pbmt) {
            const auto mapping = hypervisor_pbmt_mappings[pbmt];
            for (unsigned topology = 0; topology < 4; ++topology) {
                const bool vs_napot = (topology & 2U) != 0;
                const bool g_napot = (topology & 1U) != 0;
                for (unsigned mode = 0; mode < 2; ++mode) {
                    for (std::uint64_t offset = 0;
                         offset < hypervisor_span; offset += 0x10000) {
                        const std::uint64_t virtual_base =
                            hypervisor_virtual_base(spvp, pbmt, topology) +
                            offset;
                        const std::uint64_t guest_base =
                            hypervisor_guest_base(spvp, pbmt, topology) +
                            offset;
                        const std::uint64_t physical_base =
                            hypervisor_physical_base(pbmt, topology) + offset;
                        if (vs_napot) {
                            if (!map_stage_napot(
                                    mode, random_vs_roots[mode],
                                    virtual_base, guest_base, spvp != 0,
                                    mapping.vs_nc, mapping.vs_io)) {
                                return false;
                            }
                        } else {
                            for (std::uint64_t page = 0; page < 0x10000;
                                 page += 0x1000) {
                                if (!map_stage_page(
                                        mode, random_vs_roots[mode],
                                        virtual_base + page,
                                        guest_base + page, mapping.vs_nc,
                                        mapping.vs_io, true, spvp != 0)) {
                                    return false;
                                }
                            }
                        }
                        if (g_napot) {
                            if (!map_g_napot(
                                    mode, random_g_roots[mode], guest_base,
                                    physical_base, mapping.g_nc,
                                    mapping.g_io)) {
                                return false;
                            }
                        } else {
                            for (std::uint64_t page = 0; page < 0x10000;
                                 page += 0x1000) {
                                if (!map_g_page(
                                        mode, random_g_roots[mode],
                                        guest_base + page,
                                        physical_base + page, true,
                                        mapping.g_nc, mapping.g_io)) {
                                    return false;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    for (unsigned spvp = 0; spvp < 2; ++spvp) {
        for (unsigned topology = 0; topology < 4; ++topology) {
            const bool vs_napot = (topology & 2U) != 0;
            const bool g_napot = (topology & 1U) != 0;
            for (unsigned mode = 0; mode < 2; ++mode) {
                for (std::uint64_t offset = 0;
                     offset < hypervisor_span; offset += 0x10000) {
                    const std::uint64_t virtual_base =
                        hypervisor_pma_device_virtual_base(spvp, topology) +
                        offset;
                    const std::uint64_t guest_base =
                        hypervisor_pma_device_guest_base(spvp, topology) +
                        offset;
                    const std::uint64_t physical_base =
                        hypervisor_pma_device_physical_base(spvp, topology) +
                        offset;
                    if (vs_napot) {
                        if (!map_stage_napot(
                                mode, random_vs_roots[mode], virtual_base,
                                guest_base, spvp != 0)) {
                            return false;
                        }
                    } else {
                        for (std::uint64_t page = 0; page < 0x10000;
                             page += 0x1000) {
                            if (!map_stage_page(
                                    mode, random_vs_roots[mode],
                                    virtual_base + page, guest_base + page,
                                    false, false, true, spvp != 0)) {
                                return false;
                            }
                        }
                    }
                    if (g_napot) {
                        if (!map_g_napot(
                                mode, random_g_roots[mode], guest_base,
                                physical_base)) {
                            return false;
                        }
                    } else {
                        for (std::uint64_t page = 0; page < 0x10000;
                             page += 0x1000) {
                            if (!map_g_page(
                                    mode, random_g_roots[mode],
                                    guest_base + page, physical_base + page,
                                    true)) {
                                return false;
                            }
                        }
                    }
                }
            }
        }
    }
    for (unsigned spvp = 0; spvp < 2; ++spvp) {
        for (unsigned topology = 0; topology < 4; ++topology) {
            const bool vs_napot = (topology & 2U) != 0;
            const bool g_napot = (topology & 1U) != 0;
            for (unsigned mode = 0; mode < 2; ++mode) {
                for (std::uint64_t offset = 0;
                     offset < hypervisor_span; offset += 0x10000) {
                    const std::uint64_t virtual_base =
                        hypervisor_pmp_virtual_base(spvp, topology) +
                        offset;
                    const std::uint64_t guest_base =
                        hypervisor_pmp_guest_base(spvp, topology) + offset;
                    const std::uint64_t physical_base =
                        hypervisor_pmp_physical_base(spvp, topology) +
                        offset;
                    if (vs_napot) {
                        if (!map_stage_napot(
                                mode, random_vs_roots[mode], virtual_base,
                                guest_base, spvp != 0)) {
                            return false;
                        }
                    } else {
                        for (std::uint64_t page = 0; page < 0x10000;
                             page += 0x1000) {
                            if (!map_stage_page(
                                    mode, random_vs_roots[mode],
                                    virtual_base + page, guest_base + page,
                                    false, false, true, spvp != 0)) {
                                return false;
                            }
                        }
                    }
                    if (g_napot) {
                        if (!map_g_napot(
                                mode, random_g_roots[mode], guest_base,
                                physical_base)) {
                            return false;
                        }
                    } else {
                        for (std::uint64_t page = 0; page < 0x10000;
                             page += 0x1000) {
                            if (!map_g_page(
                                    mode, random_g_roots[mode],
                                    guest_base + page,
                                    physical_base + page, true)) {
                                return false;
                            }
                        }
                    }
                }
            }
        }
    }
    for (unsigned mode = 0; mode < 2; ++mode) {
        if (!map_stage_page(
                mode, random_stage1_roots[mode], mmio_virtual,
                mmio_physical, false, true) ||
            !map_stage_page(
                mode, random_vs_roots[mode], mmio_virtual,
                mmio_physical, false, true) ||
            !map_g_page(
                mode, random_g_roots[mode], mmio_physical,
                mmio_physical)) {
            return false;
        }
    }

    // G-stage must translate implicit accesses to every VS page-table
    // page. Keep these mappings at 4 KiB: this both matches ordinary
    // page-table memory and avoids making an implicit PTE access depend
    // on a separate G-stage superpage behavior.
    for (unsigned g_mode = 0; g_mode < 2; ++g_mode) {
        for (const std::uint64_t vs_page_table : random_vs_roots) {
            for (std::uint64_t page = vs_page_table;
                 page < vs_page_table + 0x200000; page += 0x1000) {
                if (!map_g_page(
                        g_mode, random_g_roots[g_mode], page, page)) {
                    return false;
                }
            }
        }
    }
    std::vector<std::pair<std::uint64_t, std::uint64_t>>
        reference_addresses{{
            {cache0_base + 0x106a8, cache0_base + 0x106a8},
            {cache1_base + 0xf800, cache1_base + 0xf800},
            {nc_base + 0x188, nc_base + 0x188},
            {mmio_virtual + 0x188, mmio_physical + 0x188},
            {ptw_error_base + 0x188, ptw_error_base + 0x188},
            {load_merge_base + 0x188, load_merge_base + 0x188},
            {set_pressure_base + 0x188, set_pressure_base + 0x188},
        }};
    for (unsigned spvp = 0; spvp < 2; ++spvp) {
        for (unsigned pbmt = 0; pbmt < hypervisor_pbmt_mappings.size();
             ++pbmt) {
            for (unsigned topology = 0; topology < 4; ++topology) {
                const std::uint64_t offset =
                    ((spvp * hypervisor_pbmt_mappings.size() + pbmt +
                         topology) % 8) *
                        0x10000ULL +
                    0x188;
                reference_addresses.emplace_back(
                    hypervisor_virtual_base(spvp, pbmt, topology) + offset,
                    hypervisor_physical_base(pbmt, topology) + offset);
            }
        }
        for (unsigned topology = 0; topology < 4; ++topology) {
            const std::uint64_t offset =
                ((spvp + topology) % 8) * 0x10000ULL + 0x188;
            reference_addresses.emplace_back(
                hypervisor_pma_device_virtual_base(spvp, topology) + offset,
                hypervisor_pma_device_physical_base(spvp, topology) + offset);
            reference_addresses.emplace_back(
                hypervisor_pmp_virtual_base(spvp, topology) +
                    hypervisor_pmp_region_offset + 0x188,
                hypervisor_pmp_physical_base(spvp, topology) +
                    hypervisor_pmp_region_offset + 0x188);
        }
    }
    for (unsigned vs_mode = 0; vs_mode < 2; ++vs_mode) {
        for (unsigned g_mode = 0; g_mode < 2; ++g_mode) {
            for (const auto &[virtual_address, physical_address] :
                reference_addresses) {
                const auto walk = memblock::reference_two_stage_walk(
                    environment.memory(), random_vs_roots[vs_mode],
                    random_g_roots[g_mode], virtual_address,
                    page_mode(vs_mode), page_mode(g_mode));
                if (!walk.translated ||
                    walk.physical_address != physical_address) {
                    std::ostringstream detail;
                    detail << "random-translation-reference"
                           << ":vs=" << vs_mode << ":g=" << g_mode
                           << ":va=0x" << std::hex << virtual_address
                           << ":expected=0x" << physical_address
                           << ":actual=0x" << walk.physical_address;
                    phase = detail.str();
                    return false;
                }
            }
        }
    }
    for (unsigned mode = 0; mode < 2; ++mode) {
        const auto walk = memblock::reference_page_walk(
            environment.memory(), random_stage1_roots[mode],
            stage1_napot_base + 0xa188, page_mode(mode));
        if (!walk.translated ||
            walk.physical_address != stage1_napot_base + 0xa188) {
            phase = "random-stage1-napot-reference";
            return false;
        }
    }
    constexpr std::array<std::uint64_t, 4> nested_leaf_regions{{
        cache0_base + 0x10000,
        nested_g_napot_base,
        nested_vs_napot_base,
        nested_both_napot_base,
    }};
    for (unsigned vs_mode = 0; vs_mode < 2; ++vs_mode) {
        for (unsigned g_mode = 0; g_mode < 2; ++g_mode) {
            for (unsigned topology = 0;
                 topology < nested_leaf_regions.size(); ++topology) {
                const std::uint64_t address =
                    nested_leaf_regions[topology] + 0xa188;
                const auto walk = memblock::reference_two_stage_walk(
                    environment.memory(), random_vs_roots[vs_mode],
                    random_g_roots[g_mode], address,
                    page_mode(vs_mode), page_mode(g_mode));
                if (!walk.translated || walk.physical_address != address) {
                    phase = "random-nested-napot-reference";
                    return false;
                }
            }
        }
    }
    return true;
}

bool MixedScenario::activate_translation_context(const TranslationContext &context)
{
    if (context.regime == RandomConstraints::translation_bare) {
        return environment.activate_bare();
    }
    if (context.regime == RandomConstraints::translation_stage1) {
        return context.stage1_mode == 0
            ? environment.activate_sv39(
                  random_stage1_roots[context.stage1_mode], 23)
            : environment.activate_sv48(
                  random_stage1_roots[context.stage1_mode], 23);
    }
    return environment.activate_two_stage_modes(
               page_mode(context.vs_mode), page_mode(context.g_mode),
               random_vs_roots[context.vs_mode],
               random_g_roots[context.g_mode], 29, 31) &&
        environment.set_page_based_memory_types(true, true);
}

} // namespace memblock::scenarios::mixed
