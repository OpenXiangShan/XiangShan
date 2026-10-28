#pragma once
#include "../shared.hpp"

namespace memblock::scenarios::mixed {

// A separate seed stream freezes mappings independently of action, payload,
// scheduling and reference-memory read order. VA and GPA windows are distinct
// and below the Sv39 canonical / Sv39x4 unsigned limits.
struct OrdinaryPageMappings {
    static constexpr std::uint64_t virtual_base = 0x2000000000ULL;
    static constexpr std::uint64_t guest_base = 0x2400000000ULL;
    static constexpr unsigned page_count = 132;
    static constexpr std::uint64_t span = page_count * 0x1000ULL;
    std::array<std::vector<unsigned>, 2> host;
    std::array<std::vector<unsigned>, 2> vs;
    std::array<std::vector<unsigned>, 2> g;
    std::array<std::shared_ptr<const memblock::ReferencePageMap>, 2> host_maps;
    std::array<std::array<std::shared_ptr<const memblock::ReferencePageMap>, 2>, 2>
        nested_maps;

    OrdinaryPageMappings(std::uint64_t seed, std::uint64_t physical_base)
    {
        std::mt19937_64 mapping_random(seed ^ 0x8f6c2d9137a4b5e0ULL);
        // The current manager has no directory for cross-colour synonyms.
        // Preserve VA[13:12] == PA[13:12] so it can legally serve these pages
        // without granting stale backing bytes to a second cache alias.
        // Shuffle all 33 pages within each of the four colours independently.
        static_assert(page_count % 4U == 0);
        const auto shuffle = [&](auto &permutation,
                                 std::uint64_t source_base,
                                 std::uint64_t target_base) {
            permutation.resize(page_count);
            for (unsigned colour = 0; colour < 4; ++colour) {
                std::vector<unsigned> targets;
                for (unsigned page = 0; page < page_count; ++page) {
                    if (((target_base >> 12U) + page) % 4U == colour) {
                        targets.push_back(page);
                    }
                }
                std::shuffle(targets.begin(), targets.end(), mapping_random);
                unsigned next = 0;
                for (unsigned page = 0; page < page_count; ++page) {
                    if (((source_base >> 12U) + page) % 4U == colour) {
                        permutation[page] = targets.at(next++);
                    }
                }
            }
        };
        for (unsigned mode = 0; mode < 2; ++mode) {
            shuffle(host[mode], virtual_base, physical_base);
            shuffle(vs[mode], virtual_base, guest_base);
            shuffle(g[mode], guest_base, physical_base);
            std::vector<std::uint64_t> pages;
            for (const auto page : host[mode]) {
                pages.push_back(physical_base + page * 0x1000ULL);
            }
            host_maps[mode] = std::make_shared<const memblock::ReferencePageMap>(
                virtual_base, std::move(pages), mode);
        }
        for (unsigned vs_mode = 0; vs_mode < 2; ++vs_mode) {
            for (unsigned g_mode = 0; g_mode < 2; ++g_mode) {
                std::vector<std::uint64_t> pages;
                for (const auto guest_page : vs[vs_mode]) {
                    pages.push_back(physical_base + g[g_mode][guest_page] * 0x1000ULL);
                }
                nested_maps[vs_mode][g_mode] =
                    std::make_shared<const memblock::ReferencePageMap>(virtual_base,
                        std::move(pages), 2U + vs_mode * 2U + g_mode);
            }
        }
    }
};

} // namespace memblock::scenarios::mixed
