// Host-only fixture. Python inserts the named production methods below; the
// environment records CSR operations and delegates PTE writes to the real builder.
#include "scenarios/mixed/state.hpp"
#include "scenarios/mixed/translation_permutation.hpp"
#include "environment/page_table_builder.hpp"
#include <map>
#include <functional>
#include <set>

using namespace memblock;
using namespace memblock::scenarios;
using memblock::scenarios::mixed::AddressConstants;
using memblock::scenarios::mixed::OrdinaryPageMappings;

void require(bool condition, const std::string &message)
{
    if (!condition) throw std::runtime_error(message);
}

struct Event {
    char kind;
    OrdinaryPrivilegeContext privilege;
    std::uint64_t first = 0, second = 0;
    unsigned mode = 0, guest_mode = 0;
};

struct HostEnvironment {
    SparseMemory image;
    FailureState failure{"host-page-table", "mapping"};
    PageTableBuilder builder{image, failure};
    std::size_t mapping_calls = 0;
    std::vector<Event> events;
    OrdinaryPrivilegeContext privilege{};

    SparseMemory &memory() { return image; }

    bool map_stage(ReferencePageMode mode, std::uint64_t va, std::uint64_t pa,
        std::uint64_t root, bool r, bool w, bool x, bool u, bool nc, bool io)
    {
        ++mapping_calls;
        return builder.map_reference_leaf(va, pa, root, mode, false, 0,
            r, w, x, u, nc, true, std::nullopt, io);
    }
    bool map_sv39_4k(std::uint64_t va, std::uint64_t pa, std::uint64_t root,
        bool r, bool w, bool x, bool u, bool nc = false, bool io = false)
    { return map_stage(ReferencePageMode::sv39, va, pa, root, r, w, x, u, nc, io); }
    bool map_sv48_4k(std::uint64_t va, std::uint64_t pa, std::uint64_t root,
        bool r, bool w, bool x, bool u, bool nc = false, bool io = false)
    { return map_stage(ReferencePageMode::sv48, va, pa, root, r, w, x, u, nc, io); }
    bool map_guest(ReferencePageMode mode, std::uint64_t va, std::uint64_t pa,
        unsigned level, std::uint64_t root, bool r, bool w, bool x, bool a,
        std::optional<bool> d, bool u, bool nc, bool io)
    {
        ++mapping_calls;
        return builder.map_reference_leaf(va, pa, root, mode, true, level,
            r, w, x, u, nc, a, d, io);
    }
    bool map_sv39x4_leaf(std::uint64_t va, std::uint64_t pa, unsigned level,
        std::uint64_t root, bool r, bool w, bool x, bool a,
        std::optional<bool> d, bool u, bool nc, bool io)
    { return map_guest(ReferencePageMode::sv39, va, pa, level, root, r, w, x, a, d, u, nc, io); }
    bool map_sv48x4_leaf(std::uint64_t va, std::uint64_t pa, unsigned level,
        std::uint64_t root, bool r, bool w, bool x, bool a,
        std::optional<bool> d, bool u, bool nc, bool io)
    { return map_guest(ReferencePageMode::sv48, va, pa, level, root, r, w, x, a, d, u, nc, io); }
    bool map_napot(ReferencePageMode mode, std::uint64_t va, std::uint64_t pa,
        std::uint64_t root, bool r, bool w, bool x, bool u, bool nc, bool a,
        std::optional<bool> d, bool io)
    {
        ++mapping_calls;
        return builder.map_reference_napot64k(va, pa, root, mode, false,
            r, w, x, u, nc, a, d, io);
    }
    bool map_sv39_napot64k(std::uint64_t va, std::uint64_t pa, std::uint64_t root,
        bool r, bool w, bool x, bool u, bool nc, bool a,
        std::optional<bool> d, bool io)
    { return map_napot(ReferencePageMode::sv39, va, pa, root, r, w, x, u, nc, a, d, io); }
    bool map_sv48_napot64k(std::uint64_t va, std::uint64_t pa, std::uint64_t root,
        bool r, bool w, bool x, bool u, bool nc, bool a,
        std::optional<bool> d, bool io)
    { return map_napot(ReferencePageMode::sv48, va, pa, root, r, w, x, u, nc, a, d, io); }

    bool set_instruction_privilege(ReferencePrivilegeMode mode)
    {
        privilege.instruction_mode = static_cast<unsigned>(mode);
        events.push_back({'I', privilege});
        return true;
    }
    bool set_translation_permissions(ReferencePrivilegeMode mode,
        bool mxr = false, bool sum = false, bool vmxr = false, bool vsum = false)
    {
        privilege.data_mode = static_cast<unsigned>(mode);
        privilege.mxr = mxr; privilege.sum = sum;
        privilege.vmxr = vmxr; privilege.vsum = vsum;
        events.push_back({'P', privilege});
        return true;
    }
    bool activate_bare()
    {
        privilege.virtualized = false; privilege.data_mode = 1;
        events.push_back({'B', privilege});
        return true;
    }
    bool activate_host(std::uint64_t root, unsigned mode, unsigned asid)
    {
        privilege.virtualized = false; privilege.data_mode = 1;
        events.push_back({'H', privilege, root, asid, mode});
        return true;
    }
    bool activate_sv39(std::uint64_t root, unsigned asid)
    { return activate_host(root, 8, asid); }
    bool activate_sv48(std::uint64_t root, unsigned asid)
    { return activate_host(root, 9, asid); }
    bool activate_two_stage_modes(ReferencePageMode vs, ReferencePageMode g,
        std::uint64_t root, std::uint64_t guest_root, unsigned asid, unsigned vmid)
    {
        require(asid == 29 && vmid == 31, "activation-identifiers");
        privilege.virtualized = true; privilege.data_mode = 1;
        events.push_back({'N', privilege, root, guest_root,
            static_cast<unsigned>(vs), static_cast<unsigned>(g)});
        return true;
    }
    bool set_page_based_memory_types(bool machine, bool hypervisor)
    {
        events.push_back({'T', privilege, machine, hypervisor});
        return true;
    }
    bool issue_sfence(std::uint64_t address, std::uint64_t id, bool rs1,
        bool rs2, bool vv, bool gv)
    {
        require(address == 0 && id == 0 && rs1 && rs2 && !(vv && gv),
            "activation-fence-operands");
        events.push_back({vv ? 'V' : gv ? 'G' : 'F', privilege});
        return true;
    }
};

struct CountingRandom {
    std::mt19937_64 engine;
    std::vector<std::uint64_t> script;
    std::size_t calls = 0;
    explicit CountingRandom(std::uint64_t seed) : engine(seed) {}
    std::uint64_t operator()()
    {
        const auto index = calls++;
        return script.empty() ? engine() : script.at(index);
    }
};

struct Fixture : AddressConstants {
    HostEnvironment environment;
    RandomConstraints constraints;
    OrdinaryPageMappings ordinary_pages;
    CountingRandom random;
    std::string phase;
    TranslationContext current_translation;
    bool translation_context_valid = false, entry_allowed = true;
    unsigned entries = 0;
    struct { std::array<unsigned, 3> context_fences{}; } coverage;
    explicit Fixture(std::uint64_t seed = 17)
        : ordinary_pages(seed, permuted_physical_base), random(seed)
    { constraints.stream_access_weights = {0, 1, 1}; }
    static ReferencePageMode page_mode(unsigned index);
    bool map_stage_page(unsigned, std::uint64_t, std::uint64_t, std::uint64_t,
        bool = false, bool = false, bool = false, bool = false);
    bool map_g_page(unsigned, std::uint64_t, std::uint64_t, std::uint64_t,
        bool = false, bool = false, bool = false);
    bool map_stage_napot(unsigned, std::uint64_t, std::uint64_t,
        std::optional<std::uint64_t> = std::nullopt, bool = false,
        bool = false, bool = false);
    bool prepare_user_translation_pages();
    bool prepare_stream_permission_pages();
    std::uint64_t stream_permission_physical(const TranslationContext &, std::uint64_t) const;
    bool validate_stream_permission_load(const LoadTransaction &, const TranslationContext &);
    bool validate_translation_byte(std::uint64_t, std::uint64_t, bool,
        const TranslationContext &);
    bool activate_translation_context(const TranslationContext &);
    unsigned choose_stream_access();
    bool leave_stream_access_context();
    bool enter_translation_context(const TranslationContext &context) {
        ++entries;
        if (!entry_allowed || !activate_translation_context(context)) return false;
        current_translation = context; translation_context_valid = true; return true;
    }
};

// PRODUCTION_METHODS

// A separate literal PTE walk checks topology, byte offsets and required bits.
// It does not call any production walker/permission/address helper.
struct LiteralLeaf {
    std::uint64_t physical, pte, address;
    std::vector<std::uint64_t> table_pages;
};
LiteralLeaf literal_walk(const SparseMemory &memory, std::uint64_t root,
    std::uint64_t input, unsigned mode, bool guest = false)
{
    std::vector<std::uint64_t> tables;
    std::uint64_t table = root;
    for (int level = mode == 0 ? 2 : 3; level >= 0; --level) {
        tables.push_back(table);
        const auto top = mode == 0 ? 2 : 3;
        const auto mask = guest && level == top ? 0x7ffULL : 0x1ffULL;
        const auto address = table + ((input >> (12 + 9 * level)) & mask) * 8;
        const auto pte = memory.read_u64(address);
        require(pte & 1, "literal-walk-missing-pte");
        require(!((pte & 4) && !(pte & 2)), "literal-walk-invalid-rw");
        const auto ppn = (pte >> 10) & ((1ULL << 44) - 1);
        if (pte & 0xa) {
            const auto offset_bits = pte >> 63 ? 16 : 12 + 9 * level;
            const auto offset_mask = (1ULL << offset_bits) - 1;
            const auto physical = ((ppn << 12) & ~offset_mask) | (input & offset_mask);
            return {physical, pte, address, std::move(tables)};
        }
        require(!(pte & 0xd0), "literal-walk-nonleaf-permissions");
        table = ppn << 12;
    }
    throw std::runtime_error("literal-walk-no-leaf");
}

TranslationContext context(unsigned number, unsigned access)
{
    TranslationContext value;
    value.access = access;
    value.regime = number < 2 ? RandomConstraints::translation_stage1
                              : RandomConstraints::translation_nested;
    value.stage1_mode = number % 2;
    value.vs_mode = number < 2 ? 0 : (number - 2) / 2;
    value.g_mode = number < 2 ? 0 : (number - 2) % 2;
    return value;
}

void seed_baseline(Fixture &fixture, bool permuted)
{
    auto &environment = fixture.environment;
    auto leaf = [&](unsigned mode, bool g, std::uint64_t root,
                    std::uint64_t input, std::uint64_t physical, bool u) {
        require(environment.builder.map_reference_leaf(input, physical, root,
            mode == 0 ? ReferencePageMode::sv39 : ReferencePageMode::sv48,
            g, 0, true, true, true, u, false), "baseline-page-build");
    };
    fixture.constraints.translation_permuted_per_mille = permuted ? 1000 : 0;
    for (unsigned mode = 0; mode < 2; ++mode) {
        for (auto root : {fixture.random_stage1_roots[mode], fixture.random_vs_roots[mode]})
            leaf(mode, false, root, fixture.cache0_base + 0xf000,
                fixture.cache0_base + 0xf000, false);
        // Baseline G mappings are deliberately seeded independently. New VS
        // page-table pages are omitted: production must supply those mappings.
        for (unsigned page = 0; page < 132; ++page) {
            const auto input = fixture.cache0_base + 0xf000 + page * 4096ULL;
            leaf(mode, true, fixture.random_g_roots[mode], input, input, true);
        }
        for (unsigned page = 0; page < 64; ++page) {
            const auto input = fixture.stage1_napot_base + page * 4096ULL;
            leaf(mode, true, fixture.random_g_roots[mode], input, input, true);
        }
        if (permuted) for (unsigned page = 0; page < 132; ++page)
            leaf(mode, true, fixture.random_g_roots[mode],
                OrdinaryPageMappings::guest_base + page * 4096ULL,
                fixture.permuted_physical_base + fixture.ordinary_pages.g[mode][page] * 4096ULL, true);
    }
}

std::vector<std::uint64_t> normal_image(Fixture &fixture)
{
    std::vector<std::uint64_t> result;
    for (unsigned mode = 0; mode < 2; ++mode)
        for (auto root : {fixture.random_stage1_roots[mode], fixture.random_vs_roots[mode]})
            for (unsigned byte = 0; byte < 0x10000; byte += 8)
                result.push_back(fixture.environment.image.read_u64(root + byte));
    return result;
}

void prepare(Fixture &fixture, bool permuted = true)
{
    seed_baseline(fixture, permuted);
    const auto before = normal_image(fixture);
    const bool prepared = fixture.prepare_user_translation_pages();
    require(prepared, "user-setup-rejected:" + fixture.phase);
    require(fixture.environment.failure.empty(), "page-builder-failed");
    require(normal_image(fixture) == before, "normal-roots-mutated");
}

std::uint64_t declared_physical(Fixture &fixture, unsigned number, unsigned page)
{
    const unsigned physical = number < 2 ? fixture.ordinary_pages.host[number][page]
        : fixture.ordinary_pages.g[(number - 2) % 2][fixture.ordinary_pages.vs[(number - 2) / 2][page]];
    return fixture.permuted_physical_base + physical * 4096ULL;
}

void check_declared_byte(Fixture &fixture, const TranslationContext &translation,
    std::uint64_t va, std::uint64_t pa, bool store)
{
    auto &memory = fixture.environment.image;
    const bool nested = translation.regime == RandomConstraints::translation_nested;
    auto leaf = literal_walk(memory,
        nested ? fixture.user_vs_roots[translation.vs_mode]
               : fixture.user_stage1_roots[translation.stage1_mode],
        va, nested ? translation.vs_mode : translation.stage1_mode);
    require((leaf.pte & 0xd7) == 0xd7, "user-leaf-required-bits");
    if (nested) {
        const auto guest = literal_walk(memory, fixture.random_g_roots[translation.g_mode],
            leaf.physical, translation.g_mode, true);
        require((guest.pte & 0xd7) == 0xd7, "guest-leaf-required-bits");
        require(guest.physical == pa, "declared-nested-physical-relation");
        for (auto table : leaf.table_pages) {
            const auto implicit = literal_walk(memory,
                fixture.random_g_roots[translation.g_mode], table, translation.g_mode, true);
            require(implicit.physical == table && (implicit.pte & 0x53) == 0x53,
                "implicit-table-read-permission");
        }
    } else require(leaf.physical == pa, "declared-host-physical-relation");
    const bool accepted = fixture.validate_translation_byte(va, pa, store, translation);
    require(accepted,
        "production-byte-validation-rejected:" + fixture.phase);
}

void construction(bool permuted, std::uint64_t seed)
{
    Fixture fixture(seed);
    prepare(fixture, permuted);
    require(fixture.user_stage1_roots == std::array<std::uint64_t, 2>{0xea000000, 0xeb000000} &&
        fixture.user_vs_roots == std::array<std::uint64_t, 2>{0xe8000000, 0xe9000000},
        "user-root-identity");
    unsigned identity = 0, permutations = 0, napot = 0, implicit = 0;
    for (unsigned access : {1U, 2U}) for (unsigned number = 0; number < 6; ++number) {
        const auto translation = context(number, access);
        for (unsigned page = 0; page < 132; ++page)
            for (unsigned offset : {0U, 4095U}) for (bool store : {false, true}) {
                const auto va = fixture.cache0_base + 0xf000 + page * 4096ULL + offset;
                check_declared_byte(fixture, translation, va, va, store); ++identity;
                if (permuted) {
                    check_declared_byte(fixture, translation,
                        OrdinaryPageMappings::virtual_base + page * 4096ULL + offset,
                        declared_physical(fixture, number, page) + offset, store);
                    ++permutations;
                }
            }
        for (auto base : {fixture.stage1_napot_base, fixture.nested_vs_napot_base,
                          fixture.nested_g_napot_base, fixture.nested_both_napot_base})
            for (unsigned offset : {0U, 65535U}) for (bool store : {false, true}) {
                check_declared_byte(fixture, translation, base + offset, base + offset, store);
                ++napot;
            }
    }
    for (unsigned g_mode = 0; g_mode < 2; ++g_mode) for (auto root : fixture.user_vs_roots)
        for (unsigned page = 0; page < 512; ++page) {
            const auto address = root + page * 4096ULL;
            const auto leaf = literal_walk(fixture.environment.image,
                fixture.random_g_roots[g_mode], address, g_mode, true);
            require(leaf.physical == address && (leaf.pte & 0x53) == 0x53,
                "implicit-g-page-range"); ++implicit;
        }
    for (unsigned mode = 0; mode < 2; ++mode) {
        const auto host = literal_walk(fixture.environment.image,
            fixture.user_stage1_roots[mode], fixture.stage1_napot_base, mode);
        const auto guest = literal_walk(fixture.environment.image,
            fixture.user_vs_roots[mode], fixture.nested_vs_napot_base, mode);
        require((host.pte >> 63) && (guest.pte >> 63), "user-napot-encoding");
        require(!(literal_walk(fixture.environment.image, fixture.random_stage1_roots[mode],
            fixture.cache0_base + 0xf000, mode).pte & 16), "normal-leaf-u-bit-mutated");
    }
    std::cout << "contexts=12 identity_checks=" << identity
              << " permuted_checks=" << permutations << " napot_checks=" << napot
              << " implicit_pages=" << implicit << " immutable_normal_roots=4\n";
}

void permissions()
{
    Fixture fixture;
    prepare(fixture);
    unsigned checked = 0;
    const auto va = fixture.cache0_base + 0xf188;
    auto &memory = fixture.environment.image;
    for (unsigned access : {1U, 2U}) for (unsigned number = 0; number < 6; ++number) {
        const auto translation = context(number, access);
        const bool nested = number >= 2;
        auto leaf = literal_walk(memory, nested ? fixture.user_vs_roots[translation.vs_mode]
            : fixture.user_stage1_roots[translation.stage1_mode], va,
            nested ? translation.vs_mode : translation.stage1_mode);
        auto check = [&](std::uint64_t location, std::uint64_t replacement,
                         bool load_ok, bool store_ok, const char *message) {
            const auto original = memory.read_u64(location);
            memory.write_u64(location, replacement);
            for (bool store : {false, true}) {
                require(fixture.validate_translation_byte(va, va, store, translation) ==
                    (store ? store_ok : load_ok), message);
                ++checked;
            }
            memory.write_u64(location, original);
        };
        require(!fixture.validate_translation_byte(va, va ^ 0x1000, false, translation),
            "wrong-declared-pa-accepted"); ++checked;
        check(leaf.address, leaf.pte & ~16ULL, access == 2, access == 2, "stage-u-control");
        check(leaf.address, leaf.pte & ~64ULL, false, false, "stage-accessed-control");
        check(leaf.address, leaf.pte & ~128ULL, true, false, "stage-dirty-control");
        check(leaf.address, leaf.pte & ~4ULL, true, false, "stage-write-control");
        check(leaf.address, (leaf.pte & ~6ULL) | 8ULL, false, false, "stage-mxr-control");
        check(leaf.address, leaf.pte & ~2ULL, false, false, "stage-invalid-rw-control");
        check(leaf.address, leaf.pte & ~1ULL, false, false, "stage-valid-control");
        if (access == 2) {
            const auto declared = ReferencePtePermissions{.user = true};
            require(!reference_load_permitted(declared, ReferencePrivilegeMode::supervisor, false, false) &&
                    !reference_store_permitted(declared, ReferencePrivilegeMode::supervisor, false),
                "sum-disabled-control"); ++checked;
        }
        if (nested) {
            auto guest = literal_walk(memory, fixture.random_g_roots[translation.g_mode],
                leaf.physical, translation.g_mode, true);
            check(guest.address, guest.pte & ~16ULL, false, false, "guest-u-control");
            check(guest.address, guest.pte & ~64ULL, false, false, "guest-accessed-control");
            check(guest.address, guest.pte & ~128ULL, true, false, "guest-dirty-control");
            check(guest.address, guest.pte & ~4ULL, true, false, "guest-write-control");
            const auto implicit = literal_walk(memory, fixture.random_g_roots[translation.g_mode],
                leaf.table_pages.front(), translation.g_mode, true);
            check(implicit.address, (implicit.pte & ~6ULL) | 8ULL, false, false,
                "implicit-read-control");
            check(implicit.address, implicit.pte & ~128ULL, true, true,
                "implicit-dirty-not-required");
        }
    }
    std::cout << "permission_controls=" << checked << '\n';
}

OrdinaryPrivilegeContext expected(unsigned regime, unsigned access)
{
    const std::array<OrdinaryPrivilegeContext, 3> literal{{
        {1, 3, false, false, false, false, false},
        {0, 0, false, false, false, false, false},
        {1, 1, false, true, false, true, false},
    }};
    auto result = literal.at(access);
    result.virtualized = regime == RandomConstraints::translation_nested;
    return result;
}

void activation()
{
    unsigned checked = 0;
    for (unsigned access : {0U, 1U, 2U}) for (unsigned number = 0; number < 7; ++number) {
        Fixture fixture;
        auto translation = number == 6 ? TranslationContext{} : context(number, access);
        translation.access = access;
        // Begin in a dirty, incompatible context to prove explicit restoration.
        fixture.environment.privilege = {0, 0, true, true, true, true, true};
        require(translation.expected_privilege() == expected(translation.regime, access),
            "declared-privilege-contract");
        require(fixture.activate_translation_context(translation), "activation-rejected");
        const auto &events = fixture.environment.events;
        const bool nested = number >= 2 && number < 6;
        const bool host = number < 2;
        const auto root = host ? (access ? 0xea000000ULL : 0xe0000000ULL) + number * 0x1000000ULL
            : (access ? 0xe8000000ULL : 0xe2000000ULL) + translation.vs_mode * 0x1000000ULL;
        for (const auto &event : events) {
            if (event.kind == 'H' || event.kind == 'N') {
                require(event.first == root, "activation-root");
                require(event.mode == (host ? 8U + number : 8U + translation.vs_mode),
                    "activation-mode");
                require(host ? event.second == 23 : event.second ==
                    0xe4000000ULL + translation.g_mode * 0x1000000ULL &&
                    event.guest_mode == 8U + translation.g_mode, "activation-guest-root-mode");
            }
            if (event.kind == 'F' || event.kind == 'V' || event.kind == 'G')
                require(event.privilege.instruction_mode == 3 && event.privilege.data_mode == 1 &&
                    !event.privilege.sum && !event.privilege.mxr && !event.privilege.vsum && !event.privilege.vmxr,
                    "activation-fence-privilege");
            if (event.kind == 'T') require(event.first && event.second, "activation-pbmte");
        }
        std::string sequence;
        for (const auto &event : events) sequence.push_back(event.kind);
        const auto required = std::string("IP") + (nested ? "NTVG" : host ? "HF" : "B") +
            (access ? "PI" : "");
        require(sequence == required, "activation-order");
        require(fixture.environment.privilege == expected(translation.regime, access),
            "activation-final-permissions");
        require(fixture.coverage.context_fences == std::array<unsigned, 3>{host ? 1U : 0U,
            nested ? 1U : 0U, nested ? 1U : 0U}, "activation-fence-accounting");
        ++checked;
    }
    std::cout << "activation_contexts=" << checked << '\n';
}

void handoff()
{
    unsigned contexts = 0, noops = 0, rejected = 0;
    for (unsigned number = 0; number < 7; ++number) {
        const auto base = number < 6 ? context(number, 0) : TranslationContext{};
        for (unsigned access : {1U, 2U}) {
            Fixture fixture;
            auto active = base; active.access = access;
            fixture.current_translation = active; fixture.translation_context_valid = true;
            fixture.environment.privilege = expected(active.regime, access);
            require(fixture.leave_stream_access_context(), "handoff-rejected");
            require(fixture.entries == 1, "handoff-entry-count");
            require(fixture.current_translation == base && fixture.translation_context_valid,
                "handoff-legacy-context");
            require(fixture.environment.privilege == expected(base.regime, 0), "handoff-legacy-privilege");
            for (const auto &event : fixture.environment.events) {
                if (event.kind == 'F' || event.kind == 'V' || event.kind == 'G')
                    require(event.privilege.instruction_mode == 3, "handoff-unprivileged-fence");
                if (event.kind == 'H') require(event.first == fixture.random_stage1_roots[base.stage1_mode],
                    "handoff-user-root-retained");
                if (event.kind == 'N') require(event.first == fixture.random_vs_roots[base.vs_mode] &&
                    event.second == fixture.random_g_roots[base.g_mode], "handoff-nested-user-root-retained");
            }
            require(fixture.random.calls == 0, "handoff-consumed-random");
            ++contexts;
            Fixture failure;
            failure.current_translation = active; failure.translation_context_valid = true;
            failure.environment.privilege = expected(active.regime, access); failure.entry_allowed = false;
            require(!failure.leave_stream_access_context() && failure.current_translation == active &&
                failure.environment.privilege == expected(active.regime, access) && failure.environment.events.empty(),
                "handoff-entry-failure-suppressed");
            ++rejected;
        }
        Fixture legacy;
        legacy.current_translation = base; legacy.translation_context_valid = true;
        require(legacy.leave_stream_access_context() && legacy.entries == 0 && legacy.environment.events.empty(),
            "handoff-changed-legacy-context");
        ++noops;
    }
    Fixture invalid;
    invalid.current_translation.access = 1;
    require(invalid.leave_stream_access_context() && invalid.entries == 0 && invalid.environment.events.empty(),
        "handoff-activated-invalid-context");
    ++noops;
    std::cout << "handoff_contexts=" << contexts << " noop_contexts=" << noops
              << " rejected_entries=" << rejected << '\n';
}

void profiles()
{
    Fixture fixture;
    fixture.constraints.stream_access_weights = {1, 0, 0};
    require(fixture.prepare_user_translation_pages() && fixture.environment.mapping_calls == 0,
        "disabled-profile-wrote-user-tables");
    unsigned endpoints = 0;
    for (unsigned access = 0; access < 3; ++access) {
        fixture.constraints.stream_access_weights = {0, 0, 0};
        fixture.constraints.stream_access_weights[access] = 27;
        for (unsigned n = 0; n < 64; ++n)
            require(fixture.choose_stream_access() == access, "one-hot-profile-choice");
        require(fixture.random.calls == 0, "one-hot-profile-consumed-random"); ++endpoints;
    }
    fixture.constraints.stream_access_weights = {2, 3, 5};
    fixture.random.script.resize(100);
    std::iota(fixture.random.script.begin(), fixture.random.script.end(), 0);
    const std::array<unsigned, 10> choices{0, 0, 1, 1, 1, 2, 2, 2, 2, 2};
    std::array<unsigned, 3> counts{};
    for (unsigned n = 0; n < 100; ++n) {
        const auto selected = fixture.choose_stream_access();
        require(selected == choices[n % 10], "weighted-profile-endpoint"); ++counts[selected];
    }
    require(counts == std::array<unsigned, 3>{20, 30, 50} && fixture.random.calls == 100,
        "weighted-profile-counts");
    Fixture first(198), replay(198);
    first.constraints.stream_access_weights = replay.constraints.stream_access_weights = {3, 7, 11};
    for (unsigned n = 0; n < 1024; ++n)
        require(first.choose_stream_access() == replay.choose_stream_access(), "profile-replay");
    auto invalid = RandomConstraints::preset("spec");
    for (auto key : {"stream-access-s=0", "stream-access-u=0", "stream-access-sum=0"}) invalid.apply(key);
    bool rejected = false;
    try { invalid.validate(); }
    catch (const std::invalid_argument &error) {
        rejected = std::string(error.what()) == "stream access weights cannot all be zero";
    }
    require(rejected, "zero-profile-weights-accepted");
    for (unsigned access : {1U, 2U}) {
        TranslationContext bare; bare.access = access;
        for (bool store : {false, true}) {
            require(fixture.validate_translation_byte(0x80200188, 0x80200188, store, bare),
                "bare-identity-control");
            require(!fixture.validate_translation_byte(0x80200188, 0x80201188, store, bare),
                "bare-wrong-pa-control");
        }
    }
    std::cout << "one_hot_profiles=" << endpoints
              << " weighted_counts=20,30,50 replay_choices=1024 bare_controls=8\n";
}

// Pools are independently owned UT resources. Check their actual reachable
// table pages and complete PTE images, including all four x4 root pages;
// distinct root pointers alone do not prevent child/root allocation overlap.
void pool_isolation()
{
    using Pages = std::set<std::uint64_t>;
    unsigned cases = 0, pairs = 0, controls = 0;
    for (auto seed : {17ULL, 0x491acULL}) for (bool permuted : {false, true})
        for (bool boundary_first : {false, true}) {
        Fixture fixture(seed);
        auto &memory = fixture.environment.memory();
        const auto observe = [&](Pages &pages, std::uint64_t root,
                                 std::uint64_t address, unsigned mode, bool guest) {
            const auto leaf = literal_walk(memory, root, address, mode, guest);
            pages.insert(leaf.table_pages.begin(), leaf.table_pages.end());
            for (unsigned page = 0; page < (guest ? 4U : 1U); ++page)
                pages.insert(root + page * 0x1000);
            return leaf.physical;
        };
        const auto reachable = [&](std::uint64_t root, unsigned mode, bool guest) {
            Pages pages;
            std::map<std::uint64_t, int> visited;
            std::function<void(std::uint64_t, int, bool)> visit =
                [&](std::uint64_t table, int level, bool top) {
                const auto [entry, inserted] = visited.emplace(table, level);
                if (!inserted) {
                    require(entry->second == level, "pool-isolation:cyclic-table-path");
                    return;
                }
                const unsigned entries = top && guest ? 2048U : 512U;
                for (unsigned page = 0; page < entries / 512; ++page)
                    pages.insert(table + page * 4096);
                for (unsigned index = 0; index < entries; ++index) {
                    const auto pte = memory.read_u64(table + index * 8);
                    if (!(pte & 1) || (pte & 0xa)) continue;
                    require(level > 0 && !(pte & 0xd0), "pool-isolation:malformed-nonleaf");
                    visit(((pte >> 10) & ((1ULL << 44) - 1)) << 12, level - 1, false);
                }
            };
            visit(root, mode ? 3 : 2, true);
            return pages;
        };
        const auto image = [&](const Pages &pages) {
            std::vector<std::pair<std::uint64_t, std::uint64_t>> result;
            for (auto page : pages) for (unsigned offset = 0; offset < 4096; offset += 8)
                result.emplace_back(page + offset, memory.read_u64(page + offset));
            return result;
        };
        const auto build_boundary = [&]() {
            bool result = true;
            for (unsigned mode = 0; mode < 2; ++mode) {
                const auto limit = 1ULL << (mode ? 50U : 41U);
                for (unsigned side = 0; side < 2; ++side)
                    result &= fixture.map_g_page(mode, fixture.gstage_boundary_roots[mode],
                        side ? 0 : limit - 4096,
                        fixture.gstage_boundary_physical + (mode * 2U + side) * 4096, true);
            }
            return result;
        };
        const auto boundary_pages = [&]() {
            Pages pages;
            for (unsigned mode = 0; mode < 2; ++mode) for (unsigned side = 0; side < 2; ++side) {
                const auto limit = 1ULL << (mode ? 50U : 41U);
                const auto physical = observe(pages, fixture.gstage_boundary_roots[mode],
                    side ? 0 : limit - 4096, mode, true);
                require(physical == fixture.gstage_boundary_physical + (mode * 2U + side) * 4096,
                    "pool-boundary-declared-physical");
                ++controls;
            }
            for (unsigned mode = 0; mode < 2; ++mode) {
                const auto owned = reachable(fixture.gstage_boundary_roots[mode], mode, true);
                pages.insert(owned.begin(), owned.end());
            }
            return pages;
        };
        const auto user_pages = [&]() {
            Pages pages;
            for (unsigned mode = 0; mode < 2; ++mode)
                for (auto root : {fixture.user_stage1_roots[mode], fixture.user_vs_roots[mode]}) {
                    const auto owned = reachable(root, mode, false);
                    pages.insert(owned.begin(), owned.end());
                }
            return pages;
        };
        if (boundary_first) require(build_boundary(), "pool-boundary-build");
        seed_baseline(fixture, permuted);
        Pages normal_pages;
        for (unsigned mode = 0; mode < 2; ++mode)
            for (auto root : {fixture.random_stage1_roots[mode], fixture.random_vs_roots[mode]}) {
                const auto pages = reachable(root, mode, false);
                normal_pages.insert(pages.begin(), pages.end());
            }
        const auto normal_before = image(normal_pages);
        Pages protected_pages;
        if (boundary_first) protected_pages = boundary_pages();
        const auto before = image(protected_pages);
        const bool prepared = fixture.prepare_user_translation_pages();
        require(image(protected_pages) == before, "pool-isolation:boundary-image-mutated-by-user-setup");
        require(prepared, "pool-user-setup:" + fixture.phase);
        const auto user_before_pages = user_pages();
        const auto user_before = image(user_before_pages);
        const bool boundary_built = build_boundary();
        require(image(user_before_pages) == user_before, "pool-isolation:user-image-mutated-by-boundary-setup");
        require(boundary_built, "pool-boundary-rebuild");
        require(image(normal_pages) == normal_before, "pool-isolation:ordinary-image-mutated");
        boundary_pages();

        std::array<Pages, 12> owners;
        for (unsigned mode = 0; mode < 2; ++mode) {
            for (unsigned role = 0; role < 6; ++role) {
                const auto root = role == 0 ? fixture.user_stage1_roots[mode] :
                    role == 1 ? fixture.user_vs_roots[mode] :
                    role == 2 ? fixture.random_stage1_roots[mode] :
                    role == 3 ? fixture.random_vs_roots[mode] :
                    role == 4 ? fixture.random_g_roots[mode] : fixture.gstage_boundary_roots[mode];
                owners[role * 2 + mode] = reachable(root, mode, role >= 4);
            }
        }
        for (unsigned first = 0; first < owners.size(); ++first)
            for (unsigned second = first + 1; second < owners.size(); ++second) {
                for (auto page : owners[first]) require(!owners[second].count(page),
                    "pool-isolation:shared-table-page:" + std::to_string(first) + "/" + std::to_string(second));
                ++pairs;
            }
        for (unsigned access : {1U, 2U}) for (unsigned number = 0; number < 6; ++number)
            for (unsigned page : {0U, 131U}) for (bool store : {false, true}) {
                const auto translation = context(number, access);
                const auto address = fixture.cache0_base + 0xf000 + page * 4096ULL;
                check_declared_byte(fixture, translation, address, address, store);
                if (permuted) check_declared_byte(fixture, translation,
                    OrdinaryPageMappings::virtual_base + page * 4096ULL,
                    declared_physical(fixture, number, page), store);
            }
        ++cases;
    }
    std::cout << "pool_cases=" << cases << " disjoint_owner_pairs=" << pairs
              << " boundary_controls=" << controls << " construction_orders=2\n";
}

void permission_maps()
{
    unsigned checks = 0, denied = 0, mxr_reads = 0;
    std::set<std::uint64_t> signatures;
    for (auto seed : {17ULL, 0x491acULL}) {
        Fixture fixture(seed);
        prepare(fixture);
        // The ordinary initializer maps its own VS table storage through G.
        for (unsigned g = 0; g < 2; ++g) for (auto root : fixture.random_vs_roots)
            for (auto page = root; page < root + 0x200000; page += 4096)
                require(fixture.map_g_page(g, fixture.random_g_roots[g], page, page), "implicit-ordinary-build");
        require(fixture.prepare_stream_permission_pages(), "permission-setup:" + fixture.phase);
        for (unsigned access = 0; access < 3; ++access) for (unsigned pair = 0; pair < 6; ++pair)
            for (unsigned page = 0; page < fixture.permission_page_count; ++page) for (unsigned offset : {0U, 4095U}) {
                auto c = context(pair, access);
                const auto va = fixture.permission_virtual_base + 4096ULL * page + offset;
                const auto root = pair < 2 ? (access ? fixture.user_stage1_roots : fixture.random_stage1_roots)[c.stage1_mode]
                    : (access ? fixture.user_vs_roots : fixture.random_vs_roots)[c.vs_mode];
                const auto leaf = literal_walk(fixture.environment.image, root, va, pair < 2 ? c.stage1_mode : c.vs_mode);
                require((leaf.pte & 0xcf) == 0x49 && bool(leaf.pte & 16) == (access != 0),
                    "permission-X-only-leaf");
                const auto expected = fixture.permission_physical_base + pair * fixture.permission_context_stride +
                    (pair < 2 ? fixture.ordinary_pages.host[pair][page] :
                        fixture.ordinary_pages.g[c.g_mode][fixture.ordinary_pages.vs[c.vs_mode][page]]) * 4096ULL + offset;
                auto actual = leaf.physical;
                if (pair >= 2) {
                    const auto g = literal_walk(fixture.environment.image, fixture.random_g_roots[c.g_mode], actual, c.g_mode, true);
                    require((g.pte & 0xd7) == 0xd7, "permission-G-leaf");
                    actual = g.physical;
                    for (auto table : leaf.table_pages) {
                        const auto implicit = literal_walk(fixture.environment.image, fixture.random_g_roots[c.g_mode], table, c.g_mode, true);
                        require(implicit.physical == table && (implicit.pte & 0x53) == 0x53, "permission-implicit-G");
                    }
                }
                require(actual == expected && fixture.stream_permission_physical(c, va) == expected,
                    "permission-declared-physical");
                for (bool mxr : {false, true}) {
                    c.mxr = mxr;
                    LoadTransaction load; load.address = va; load.oracle_address = expected;
                    load.expected_exception_mask = mxr ? 0U : kExceptionLoadPageFault;
                    require(fixture.validate_stream_permission_load(load, c), "permission-oracle:" + fixture.phase);
                    ++checks; denied += !mxr; mxr_reads += mxr;
                }
                signatures.insert(expected);
            }
    }
    require(signatures.size() > 192, "permission-seeds-fixed-map");
    std::cout << "permission_map_checks=" << checks << " denied=" << denied
        << " mxr_reads=" << mxr_reads << " distinct_addresses=" << signatures.size() << std::endl;
}

int main(int argc, char **argv)
{
    try {
        require(argc == 2, "one-case-required");
        const std::string test = argv[1];
        if (test == "construction") construction(true, 17);
        else if (test == "construction-seed2") construction(true, 0x491ac);
        else if (test == "construction-no-permutation") construction(false, 17);
        else if (test == "permissions") permissions();
        else if (test == "activation") activation();
        else if (test == "profiles") profiles();
        else if (test == "pool-isolation") pool_isolation();
        else if (test == "handoff") handoff();
        else if (test == "permission-maps") permission_maps();
        else throw std::runtime_error("unknown-case");
        std::cout << test << " PASS\n";
    } catch (const std::exception &error) {
        std::cerr << error.what() << '\n'; return 1;
    }
}
