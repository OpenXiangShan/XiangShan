// Host witnesses for the actual translation configuration/context helpers.
// Tick records public CSR pins. SparseMemory/PMP transport and settle scheduling
// are bounded stubs: this fixture checks producer pulses and pin ordering, not
// page-table semantics, PMP behavior, DTLB operation, or fence sufficiency.
#include "memblock_env.hpp"
#include <array>
#include <cstdlib>
#include <iostream>
#include <string>
#include <utility>
#include <vector>

using namespace memblock;
struct Port {
    std::uint64_t data = 0;
    void set(std::uint64_t value) { data = value; }
    std::uint64_t value() const { return data; }
    bool boolean() const { return data != 0; }
};
struct CsrPins {
    Port satp_mode, satp_asid, satp_ppn, satp_changed;
    Port vsatp_mode, vsatp_asid, vsatp_ppn, vsatp_changed;
    Port hgatp_mode, hgatp_vmid, hgatp_ppn, hgatp_changed;
    Port priv_dmode, priv_virt, priv_virt_changed, priv_spvp;
    Port priv_mxr, priv_vmxr, priv_vsum, m_pbmte;
};
struct State {
    std::array<std::uint64_t, 3> modes{}, ids{}, roots{};
    std::uint64_t data_mode = 0, spvp = 0;
    bool virt = false, mxr = false, vmxr = false, vsum = false, pbmte = false;
    std::array<bool, 4> changed{};
};
struct Edge { State pins; bool monitor; };
struct MemoryWitness {
    std::vector<std::pair<std::uint64_t, std::uint64_t>> writes;
    void write_u64(std::uint64_t address, std::uint64_t data) { writes.emplace_back(address, data); }
};
struct Harness {
    struct { struct { CsrPins csr; } translation; } dut_;
    struct {
        bool failed = false;
        void record(const std::string &) { failed = true; }
    } error_;
    MemoryWitness memory_;
    std::vector<Edge> edges;
    std::vector<std::pair<std::uint16_t, std::uint64_t>> distributed_writes;
    std::vector<unsigned> settle_calls;
    bool check_components() const { return !error_.failed; }
    void tick(bool monitor = true) {
        const auto &c = dut_.translation.csr;
        edges.push_back({{{c.satp_mode.value(), c.vsatp_mode.value(), c.hgatp_mode.value()},
            {c.satp_asid.value(), c.vsatp_asid.value(), c.hgatp_vmid.value()},
            {c.satp_ppn.value(), c.vsatp_ppn.value(), c.hgatp_ppn.value()},
            c.priv_dmode.value(), c.priv_spvp.value(), c.priv_virt.boolean(),
            c.priv_mxr.boolean(), c.priv_vmxr.boolean(), c.priv_vsum.boolean(), c.m_pbmte.boolean(),
            {c.satp_changed.boolean(), c.vsatp_changed.boolean(), c.hgatp_changed.boolean(),
             c.priv_virt_changed.boolean()}}, monitor});
    }
    bool write_distributed_csr(std::uint16_t address, std::uint64_t data) {
        distributed_writes.emplace_back(address, data); tick(false); return true;
    }
    bool run_cycles(unsigned count) {
        settle_calls.push_back(count);
        for (unsigned i = 0; i < count; ++i) tick();
        return check_components();
    }
    bool configure_sv39(std::uint64_t, std::uint64_t, std::uint64_t, bool, bool);
    bool activate_stage_one(ReferencePageMode, std::uint64_t, std::uint16_t);
    bool activate_two_stage_modes(ReferencePageMode, ReferencePageMode, std::uint64_t,
                                  std::uint64_t, std::uint16_t, std::uint16_t);
    bool update_stage_one_context(ReferencePageMode, std::uint64_t, std::uint16_t);
    bool update_vs_context(ReferencePageMode, std::uint64_t, std::uint16_t);
    bool update_two_stage_context(ReferencePageMode, ReferencePageMode, std::uint64_t,
                                  std::uint64_t, std::uint16_t, std::uint16_t);
    bool update_g_context(ReferencePageMode, std::uint64_t, std::uint16_t);
    bool set_hypervisor_access_permissions(ReferencePrivilegeMode, bool, bool, bool,
                                           ReferencePrivilegeMode);
};
// PRODUCTION_METHODS

std::string case_name;
void require(bool condition, const std::string &detail) {
    if (condition) return;
    std::cerr << "translation-context-pulse-check case=" << case_name << " " << detail << '\n';
    std::abort();
}
void initialize(Harness &h, const State &s) {
    auto &c = h.dut_.translation.csr;
    c.satp_mode.set(s.modes[0]); c.vsatp_mode.set(s.modes[1]); c.hgatp_mode.set(s.modes[2]);
    c.satp_asid.set(s.ids[0]); c.vsatp_asid.set(s.ids[1]); c.hgatp_vmid.set(s.ids[2]);
    c.satp_ppn.set(s.roots[0]); c.vsatp_ppn.set(s.roots[1]); c.hgatp_ppn.set(s.roots[2]);
    c.priv_dmode.set(s.data_mode); c.priv_spvp.set(s.spvp); c.priv_virt.set(s.virt);
    c.priv_mxr.set(s.mxr); c.priv_vmxr.set(s.vmxr); c.priv_vsum.set(s.vsum); c.m_pbmte.set(s.pbmte);
    c.satp_changed.set(0); c.vsatp_changed.set(0); c.hgatp_changed.set(0); c.priv_virt_changed.set(0);
}
void state_is(const State &actual, const State &expected, const std::string &phase) {
    for (unsigned slot = 0; slot < 3; ++slot) {
        require(actual.modes[slot] == expected.modes[slot], phase + " mode=" + std::to_string(slot));
        require(actual.ids[slot] == expected.ids[slot], phase + " identity=" + std::to_string(slot));
        require(actual.roots[slot] == expected.roots[slot], phase + " root=" + std::to_string(slot));
    }
    require(actual.data_mode == expected.data_mode && actual.spvp == expected.spvp &&
            actual.virt == expected.virt && actual.mxr == expected.mxr && actual.vmxr == expected.vmxr &&
            actual.vsum == expected.vsum && actual.pbmte == expected.pbmte, phase + " privilege");
    for (unsigned slot = 0; slot < 4; ++slot)
        require(actual.changed[slot] == expected.changed[slot], phase + " changed=" + std::to_string(slot));
}
void verify(Harness &h, const State &before, State after, const std::array<bool, 4> &pulse,
            bool activation, bool writes_mapping) {
    const unsigned prefix = activation ? 2 : 0;
    require(h.check_components(), "helper rejected scripted input");
    require(h.edges.size() == prefix + 17, "exact write/pulse/settle edge count");
    require(h.settle_calls == std::vector<unsigned>{16}, "one sixteen-edge settle call");
    if (activation) {
        require(h.distributed_writes == std::vector<std::pair<std::uint16_t, std::uint64_t>>{
            {0x3b0, ~std::uint64_t{0}}, {0x3a0, 0x1f}}, "PMP setup ordering");
    } else require(h.distributed_writes.empty(), "context update unexpectedly performed PMP setup");
    require(h.memory_.writes.size() == unsigned(writes_mapping), "mapping stub write count");
    for (unsigned edge = 0; edge < prefix; ++edge) {
        require(!h.edges[edge].monitor, "PMP transport tick monitoring");
        state_is(h.edges[edge].pins, before, "PMP prefix edge=" + std::to_string(edge));
    }
    after.changed = pulse;
    require(!h.edges[prefix].monitor, "pulse tick monitoring");
    state_is(h.edges[prefix].pins, after, "update edge");
    after.changed.fill(false);
    for (unsigned edge = prefix + 1; edge < h.edges.size(); ++edge) {
        require(h.edges[edge].monitor, "settle tick monitoring");
        state_is(h.edges[edge].pins, after, "settle edge=" + std::to_string(edge - prefix));
    }
    // A caller's next edge must also see cleared pulses after the helper returns.
    h.tick(); state_is(h.edges.back().pins, after, "caller edge after return");
}

const std::array<std::string, 8> helpers{
    "configure_sv39", "activate_stage_one", "activate_two_stage_modes", "update_stage_one_context",
    "update_vs_context", "update_two_stage_context", "update_g_context", "set_hypervisor_access_permissions"};
void histories(unsigned helper) {
    unsigned cases = 0, id_pulses = 0, virt_pulses = 0, stable_ids = 0;
    for (unsigned selector = 0; selector < 4; ++selector)
        for (unsigned id_changes = 0; id_changes < 8; ++id_changes)
            for (bool old_virt : {false, true}) for (bool roots_equal : {false, true})
                for (bool modes_equal : {false, true}) {
        case_name = helpers[helper] + ":selector=" + std::to_string(selector) +
            ":ids=" + std::to_string(id_changes) + ":virt=" + std::to_string(old_virt) +
            ":roots_equal=" + std::to_string(roots_equal) + ":modes_equal=" + std::to_string(modes_equal);
        // Requested values and old pins are independently scripted. Each identity
        // register starts distinct, so reading a sibling register cannot pass.
        const std::array<std::uint64_t, 3> requested_modes{
            selector & 1 ? 8U : 0U, selector & 1 ? 8U : 0U, selector & 2 ? 8U : 0U};
        const std::array<std::uint64_t, 3> root_addresses{0x40400000, 0x50500000, 0x60600000};
        const std::array<std::uint64_t, 3> requested_roots{0x40400, 0x50500, 0x60600};
        const std::array<std::uint64_t, 3> requested_ids{
            id_changes & 1 ? 0x6dU : 0x15U, id_changes & 2 ? 0x7eU : 0x26U,
            id_changes & 4 ? 0x3dU : 0x37U};
        State before;
        if (!modes_equal)
            for (unsigned slot = 0; slot < 3; ++slot) before.modes[slot] = requested_modes[slot] == 8 ? 0 : 8;
        else before.modes = requested_modes;
        before.ids = {0x15, 0x26, 0x37};
        before.roots = roots_equal ? requested_roots : std::array<std::uint64_t, 3>{0x10100, 0x20200, 0x30300};
        before.data_mode = 3; before.spvp = 1; before.virt = old_virt;
        before.mxr = false; before.vmxr = true; before.vsum = false; before.pbmte = false;
        if (helper == 0) {
            before.ids[0] = id_changes & 1 ? 0x15 : 0;
            before.modes[0] = modes_equal ? 8 : 0;
        }
        State after = before;
        std::array<bool, 4> pulse{};
        Harness h; initialize(h, before); bool success = false;
        const auto mode = [&](unsigned slot) { return static_cast<ReferencePageMode>(requested_modes[slot]); };
        if (helper == 0) {
            after.modes[0] = 8; after.ids[0] = 0; after.roots[0] = requested_roots[0];
            after.data_mode = 1; after.virt = false; after.pbmte = true;
            pulse = {bool(id_changes & 1), false, false, old_virt};
            success = h.configure_sv39(0x40001234, 0x80001234, root_addresses[0], true, true);
        } else if (helper == 1) {
            after.modes[0] = requested_modes[0]; after.ids[0] = requested_ids[0]; after.roots[0] = requested_roots[0];
            after.data_mode = 1; after.virt = false; after.pbmte = true;
            pulse = {bool(id_changes & 1), false, false, old_virt};
            success = h.activate_stage_one(mode(0), root_addresses[0], requested_ids[0]);
        } else if (helper == 2 || helper == 5) {
            after.modes[1] = requested_modes[1]; after.ids[1] = requested_ids[1]; after.roots[1] = requested_roots[1];
            after.modes[2] = requested_modes[2]; after.ids[2] = requested_ids[2]; after.roots[2] = requested_roots[2];
            pulse = {false, bool(id_changes & 2), bool(id_changes & 4), helper == 2 && !old_virt};
            if (helper == 2) {
                after.modes[0] = 0; after.data_mode = 1; after.virt = true;
                success = h.activate_two_stage_modes(mode(1), mode(2), root_addresses[1], root_addresses[2],
                                                     requested_ids[1], requested_ids[2]);
            } else success = h.update_two_stage_context(mode(1), mode(2), root_addresses[1], root_addresses[2],
                                                        requested_ids[1], requested_ids[2]);
        } else if (helper == 3 || helper == 4 || helper == 6) {
            const unsigned slot = helper == 3 ? 0 : helper == 4 ? 1 : 2;
            after.modes[slot] = requested_modes[slot]; after.ids[slot] = requested_ids[slot]; after.roots[slot] = requested_roots[slot];
            pulse[slot] = id_changes & (1U << slot);
            if (helper == 3) success = h.update_stage_one_context(mode(0), root_addresses[0], requested_ids[0]);
            else if (helper == 4) success = h.update_vs_context(mode(1), root_addresses[1], requested_ids[1]);
            else success = h.update_g_context(mode(2), root_addresses[2], requested_ids[2]);
        } else {
            after.data_mode = selector & 1 ? 1 : 0; after.virt = false; after.spvp = selector & 2 ? 1 : 0;
            after.mxr = selector & 1; after.vmxr = !(selector & 1); after.vsum = selector & 2;
            pulse[3] = old_virt;
            success = h.set_hypervisor_access_permissions(static_cast<ReferencePrivilegeMode>(after.spvp),
                after.mxr, after.vmxr, after.vsum, static_cast<ReferencePrivilegeMode>(after.data_mode));
        }
        require(success, "helper return value");
        verify(h, before, after, pulse, helper <= 2, helper == 0);
        for (unsigned slot = 0; slot < 3; ++slot) id_pulses += pulse[slot];
        virt_pulses += pulse[3]; stable_ids += !pulse[0] && !pulse[1] && !pulse[2]; ++cases;
    }
    std::cout << "helper=" << helpers[helper] << " cases=" << cases << " identity_pulses=" << id_pulses
              << " dvirt_pulses=" << virt_pulses << " stable_identity_cases=" << stable_ids
              << " settle_edges_per_case=16 cleared_caller_edge=1\n";
}
int main(int argc, char **argv) {
    require(argc == 2, "one helper argument required");
    for (unsigned helper = 0; helper < helpers.size(); ++helper)
        if (argv[1] == helpers[helper]) { histories(helper); return 0; }
    require(false, "unknown helper");
}
