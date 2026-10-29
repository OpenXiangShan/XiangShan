#!/usr/bin/env python3
"""Compile the real unresolved-store graph and independently check its contracts."""
from __future__ import annotations

import hashlib
import json
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
HELPERS = tuple(
    ROOT / "cpp/scenarios/mixed" / name
    for name in (
        "unresolved_graph.hpp", "unresolved_dependency.hpp",
        "scalar_dependency.hpp", "scalar_load.hpp",
    )
)

CPP = r'''
#include "scenarios/mixed/unresolved_graph.hpp"
#include <iostream>
#include <limits>
#include <set>
#include <sstream>
#include <string>
using namespace memblock::scenarios::mixed;

static void require(bool condition, const char *reason) {
    if (!condition) throw std::runtime_error(reason);
}
static std::string shape(const UnresolvedStoreGraph &graph, bool payload=false) {
    std::ostringstream out;
    out << graph.load.offset << ':' << graph.load.op << ':' << graph.load.fp
        << ':' << graph.conflict;
    for (const auto &store : graph.stores) {
        out << 's' << store.bytes.offset << ':' << store.bytes.width
            << ':' << store.data_before_load;
        if (payload) out << ':' << store.bytes.data;
    }
    return out.str();
}
// Only event identities and the early-data mask contribute to this fingerprint.
// Addresses, payloads, widths and delays cannot inflate scheduler diversity.
static std::string schedule(const UnresolvedStoreGraph &graph, bool delays=false) {
    std::ostringstream out;
    for (unsigned store=0; store<graph.stores.size(); ++store)
        if (graph.stores[store].data_before_load) out << 'D' << store << ';';
    out << "L;";
    for (const auto &event : graph.after_provisional) {
        out << (event.kind==UnresolvedOperandKind::address ? 'A' : 'D')
            << event.store << ';';
        if (delays) out << event.delay << ';';
    }
    return out.str();
}
static std::string replay(const UnresolvedStoreGraph &graph) {
    return shape(graph,true) + schedule(graph,true);
}
// Count the obligations directly. Early STD contributes one data occurrence;
// every STA and remaining STD must occur once after the provisional load.
static bool operands_complete_once(const UnresolvedStoreGraph &graph) {
    if (graph.stores.empty() || graph.stores.size()>4) return false;
    std::array<unsigned,4> address{}, data{};
    for (unsigned store=0; store<graph.stores.size(); ++store)
        data[store] = graph.stores[store].data_before_load;
    for (const auto &event : graph.after_provisional) {
        if (event.store>=graph.stores.size() || event.delay>7) return false;
        switch (event.kind) {
        case UnresolvedOperandKind::address:
            if (++address[event.store]!=1) return false;
            break;
        case UnresolvedOperandKind::data:
            if (++data[event.store]!=1) return false;
            break;
        default: return false;
        }
    }
    for (unsigned store=0; store<graph.stores.size(); ++store)
        if (address[store]!=1 || data[store]!=1) return false;
    return true;
}
static bool intersects(const DependencyStore &store, const DependencyLoad &load) {
    // Enumerate byte identities, rather than copying the planner's interval test.
    for (unsigned s=0; s<store.width; ++s)
        for (unsigned l=0; l<load.width(); ++l)
            if (store.offset+s==load.offset+l) return true;
    return false;
}
// Independent reference: first paint a complete memory image in architectural
// age order, then gather the load and apply its architectural extension.
static std::uint64_t reference(const std::array<unsigned char,64> &initial,
    const UnresolvedStoreGraph &graph) {
    auto memory=initial;
    for (const auto &store : graph.stores) {
        auto remaining=store.bytes.data;
        for (unsigned byte=0; byte<store.bytes.width; ++byte) {
            memory.at(store.bytes.offset+byte)=remaining & 255U;
            remaining >>= 8;
        }
    }
    std::uint64_t value=0;
    for (unsigned byte=graph.load.width(); byte>0; --byte)
        value=value*256U+memory.at(graph.load.offset+byte-1U);
    if (graph.load.fp) {
        switch (graph.load.op) {
        case 1: return value | 0xffffffffffff0000ULL;
        case 2: return value | 0xffffffff00000000ULL;
        case 3: return value;
        default: throw std::runtime_error("invalid FP oracle input");
        }
    }
    switch (graph.load.op) {
    case 0: return value & 0x80U ? value | 0xffffffffffffff00ULL : value;
    case 1: return value & 0x8000U ? value | 0xffffffffffff0000ULL : value;
    case 2: return value & 0x80000000U ? value | 0xffffffff00000000ULL : value;
    case 3: case 4: case 5: case 6: return value;
    default: throw std::runtime_error("invalid integer oracle input");
    }
}
static std::array<unsigned char,64> initial_bytes() {
    std::array<unsigned char,64> bytes{};
    for (unsigned index=0; index<bytes.size(); ++index) bytes[index]=index;
    return bytes;
}
static void check_oracle(const std::array<unsigned char,64> &initial,
    const UnresolvedStoreGraph &graph) {
    const auto extracted=unresolved_graph_bytes(graph);
    require(extracted.size()==graph.stores.size(),"store extraction lost an age role");
    for (unsigned index=0; index<extracted.size(); ++index) {
        const auto &a=extracted[index]; const auto &b=graph.stores[index].bytes;
        require(a.offset==b.offset && a.width==b.width && a.data==b.data,
            "store extraction changed byte geometry, data or age order");
    }
    require(scalar_dependency_expected(initial,extracted,graph.load)==reference(initial,graph),
        "youngest-writer byte oracle disagrees with independently painted memory");
}
static void planner() {
    constexpr unsigned samples_per_seed=1536;
    unsigned samples=0, conflicts=0, controls=0, changed_seed=0, early=0, late=0;
    unsigned mixed_data=0, partial=0, multi_overlap=0, different_address_order=0;
    unsigned fixed_two_samples=0, fixed_four_samples=0;
    std::array<bool,4> counts{}, store_widths{}, cells{}, address_first{}, data_first{};
    std::array<bool,7> integer_ops{};
    std::array<bool,3> fp_ops{};
    std::array<bool,8> delays{};
    std::set<std::string> topologies, schedules, fixed_two, fixed_four, address_orders;
    const ScalarLoadPolicy policy{375,{1,1,1}};
    const auto initial=initial_bytes();
    for (const std::uint64_t seed : {0ULL,17ULL,0xdeadbeefULL}) {
        std::mt19937_64 random(seed), again(seed), other(seed+1U);
        for (unsigned iteration=0; iteration<samples_per_seed; ++iteration) {
            ++samples;
            const auto graph=make_unresolved_store_graph(random,500,policy);
            require(replay(graph)==replay(make_unresolved_store_graph(again,500,policy)),
                "same-seed graph replay changed payload, timing or topology");
            changed_seed += replay(graph)!=replay(make_unresolved_store_graph(other,500,policy));
            require(graph.stores.size()>=1 && graph.stores.size()<=4,"store count out of bounds");
            require(operands_complete_once(graph),"operand obligations are missing or duplicated");
            const auto &load=graph.load;
            require(load.offset%load.width()==0 && load.offset+load.width()<=64,
                "unaligned or out-of-line load");
            require(load.offset/16U==(load.offset+load.width()-1U)/16U,
                "load straddles the selected cell");
            if (load.fp) {
                require(load.op>=1 && load.op<=3,"illegal FP load class");
                fp_ops[load.op-1]=true;
            } else {
                require(load.op<=6,"illegal integer load class");
                integer_ops[load.op]=true;
            }
            counts[graph.stores.size()-1]=true;
            cells[load.offset/16U]=true;
            unsigned overlapping=0, early_count=0;
            for (const auto &store : graph.stores) {
                const auto &bytes=store.bytes;
                require(bytes.width==1 || bytes.width==2 || bytes.width==4 || bytes.width==8,
                    "illegal store width");
                require(bytes.offset%bytes.width==0 && bytes.offset+bytes.width<=64,
                    "unaligned or out-of-line store");
                require(bytes.offset/16U==load.offset/16U &&
                    (bytes.offset+bytes.width-1U)/16U==load.offset/16U,
                    "store escaped the load comparison cell");
                store_widths[std::countr_zero(bytes.width)]=true;
                overlapping += intersects(bytes,load);
                early_count += store.data_before_load;
                early += store.data_before_load;
                late += !store.data_before_load;
                partial += intersects(bytes,load) &&
                    (bytes.offset!=load.offset || bytes.width!=load.width());
            }
            require(graph.conflict==(overlapping!=0),"conflict flag is not a window byte predicate");
            conflicts += graph.conflict;
            controls += !graph.conflict;
            multi_overlap += overlapping>1;
            mixed_data += early_count>0 && early_count<graph.stores.size();
            std::array<bool,4> seen_address{}, seen_data{};
            std::vector<unsigned> address_order;
            std::ostringstream address_order_text;
            for (const auto &event : graph.after_provisional) {
                delays[event.delay]=true;
                if (event.kind==UnresolvedOperandKind::address) {
                    if (!graph.stores[event.store].data_before_load)
                        address_first[event.store]=address_first[event.store] || !seen_data[event.store];
                    seen_address[event.store]=true;
                    address_order.push_back(event.store);
                    address_order_text << event.store << ';';
                } else {
                    data_first[event.store]=data_first[event.store] || !seen_address[event.store];
                    seen_data[event.store]=true;
                }
            }
            different_address_order += !std::is_sorted(address_order.begin(),address_order.end());
            if (graph.stores.size()==4) address_orders.insert(address_order_text.str());
            topologies.insert(shape(graph));
            schedules.insert(schedule(graph));
            if (graph.stores.size()==2 && early_count==0) {
                ++fixed_two_samples; fixed_two.insert(schedule(graph));
            }
            if (graph.stores.size()==4 && early_count==0) {
                ++fixed_four_samples; fixed_four.insert(schedule(graph));
            }
            check_oracle(initial,graph);
            auto distinguished=graph;
            distinguish_unresolved_graph(distinguished,initial);
            check_oracle(initial,distinguished);
            require(shape(graph)==shape(distinguished) && schedule(graph,true)==schedule(distinguished,true),
                "distinguishability changed geometry, operand roles or event timing");
            auto no_stores=graph; no_stores.stores.clear();
            require(!graph.conflict || reference(initial,distinguished)!=reference(initial,no_stores),
                "conflict remains indistinguishable from pre-store memory");
            if (!graph.conflict)
                require(replay(graph)==replay(distinguished),"disjoint graph payload was mutated");
        }
    }
    for (bool seen:counts) require(seen,"store-count variation missing");
    for (bool seen:store_widths) require(seen,"store-width reachability missing");
    for (bool seen:integer_ops) require(seen,"integer load-class reachability missing");
    for (bool seen:fp_ops) require(seen,"FP load-class reachability missing");
    for (bool seen:cells) require(seen,"comparison-cell reachability missing");
    for (bool seen:delays) require(seen,"delay endpoint reachability missing");
    for (bool seen:address_first) require(seen,"late address-before-data reachability missing");
    for (bool seen:data_first) require(seen,"late data-before-address reachability missing");
    require(conflicts>samples/3 && controls>samples/3,"mixed per-window conflict classes missing");
    require(early>100 && late>100 && mixed_data>100,"early/late operand variation missing");
    require(partial>100 && multi_overlap>100,"partial or multiple overlapping writers missing");
    require(topologies.size()>1000 && schedules.size()>500,"structural graph diversity missing");
    require(fixed_two_samples>40 && fixed_two.size()>12,"fixed two-store schedule diversity missing");
    require(fixed_four_samples>25 && fixed_four.size()>20,"fixed four-store schedule diversity missing");
    require(address_orders.size()>12 && different_address_order>100,"STA age order is artificially fixed");
    require(changed_seed>samples/2,"different seeds failed to alter generated graphs");
    // Endpoints are tested with every maximum and with pure integer/pure FP policy.
    unsigned endpoint_samples=0;
    for (unsigned maximum=1; maximum<=4; ++maximum)
        for (unsigned rate : {0U,1000U})
            for (unsigned fp : {0U,1000U}) {
                std::mt19937_64 random(71U+maximum+rate+fp);
                const ScalarLoadPolicy endpoint_policy{fp,{1,1,1}};
                for (unsigned sample=0; sample<128; ++sample) {
                    const auto graph=make_unresolved_store_graph(random,rate,endpoint_policy,maximum);
                    require(graph.stores.size()>=1 && graph.stores.size()<=maximum,
                        "maximum store constraint ignored");
                    require(graph.conflict==(rate==1000U),"conflict rate endpoint ignored");
                    require(graph.load.fp==(fp==1000U),"FP class endpoint ignored");
                    require(operands_complete_once(graph),"endpoint operand obligation failed");
                    unsigned overlap=0;
                    for (const auto &store:graph.stores) overlap += intersects(store.bytes,graph.load);
                    require((overlap!=0)==graph.conflict,"endpoint conflict geometry wrong");
                    ++endpoint_samples;
                }
            }
    for (unsigned selected_width=0; selected_width<3; ++selected_width) {
        ScalarLoadPolicy single_fp{1000,{0,0,0}};
        single_fp.fp_width_weights[selected_width]=1;
        std::mt19937_64 random(104+selected_width);
        for (unsigned sample=0; sample<32; ++sample) {
            const auto graph=make_unresolved_store_graph(random,500,single_fp);
            require(graph.load.fp && graph.load.op==selected_width+1U,
                "zero-weight FP formats were selected");
        }
    }
    std::cout << "{\"kind\":\"planner\",\"samples\":" << samples
        << ",\"endpoint_samples\":" << endpoint_samples
        << ",\"unique_topologies\":" << topologies.size()
        << ",\"unique_schedules_without_payload_address_width_delay\":" << schedules.size()
        << ",\"fixed_two_all_late_schedules\":" << fixed_two.size()
        << ",\"fixed_two_all_late_samples\":" << fixed_two_samples
        << ",\"fixed_four_all_late_schedules\":" << fixed_four.size()
        << ",\"fixed_four_all_late_samples\":" << fixed_four_samples
        << ",\"four_store_address_orders\":" << address_orders.size()
        << ",\"conflicts\":" << conflicts << ",\"controls\":" << controls
        << ",\"partial_overlaps\":" << partial << ",\"multiple_overlaps\":" << multi_overlap
        << ",\"seeds\":[0,17,3735928559],\"store_counts\":[1,2,3,4],"
           "\"store_widths\":[1,2,4,8],\"integer_ops\":[0,1,2,3,4,5,6],"
           "\"fp_ops\":[1,2,3],\"delay_endpoints\":[0,7]}\n";
}
static void literal(const std::array<unsigned char,64> &initial,
    UnresolvedStoreGraph graph, DependencyLoad load, std::uint64_t expected) {
    graph.load=load;
    require(reference(initial,graph)==expected,"literal and independent memory oracle disagree");
    require(scalar_dependency_expected(initial,unresolved_graph_bytes(graph),load)==expected,
        "literal youngest-writer/extension expectation failed");
}
static void distinguished_only(const std::array<unsigned char,64> &initial,
    UnresolvedStoreGraph graph, unsigned writer, std::uint64_t changed_data,
    std::uint64_t expected) {
    const auto original=graph;
    distinguish_unresolved_graph(graph,initial);
    require(shape(original)==shape(graph) && schedule(original,true)==schedule(graph,true),
        "distinguishability changed a non-payload input");
    for (unsigned index=0; index<graph.stores.size(); ++index)
        require(graph.stores[index].bytes.data==(index==writer ? changed_data : original.stores[index].bytes.data),
            "distinguishability modified the wrong architectural writer or byte");
    literal(initial,graph,graph.load,expected);
    auto no_stores=graph; no_stores.stores.clear();
    require(reference(initial,graph)!=reference(initial,no_stores),"literal collision was not distinguished");
    const auto once=replay(graph);
    distinguish_unresolved_graph(graph,initial);
    require(once==replay(graph),"distinguishability is not idempotent");
}
static void oracle() {
    const auto initial=initial_bytes();
    UnresolvedStoreGraph graph{{
        {{8,8,0x8877665544332211ULL},false}, {{10,2,0xbeef},true},
        {{12,1,0x80},false}, {{14,2,0xa1b2},true}}, {8,3,false}, true, {}};
    literal(initial,graph,{8,3,false},0xa1b26680beef2211ULL);
    literal(initial,graph,{12,0,false},0xffffffffffffff80ULL);
    literal(initial,graph,{12,4,false},0x80ULL);
    literal(initial,graph,{10,1,false},0xffffffffffffbeefULL);
    literal(initial,graph,{10,5,false},0xbeefULL);
    literal(initial,graph,{8,2,false},0xffffffffbeef2211ULL);
    literal(initial,graph,{8,6,false},0xbeef2211ULL);
    literal(initial,graph,{10,1,true},0xffffffffffffbeefULL);
    literal(initial,graph,{8,2,true},0xffffffffbeef2211ULL);
    literal(initial,graph,{8,3,true},0xa1b26680beef2211ULL);
    literal(initial,{}, {0,1,true},0xffffffffffff0100ULL);
    literal(initial,{}, {0,2,true},0xffffffff03020100ULL);
    literal(initial,{}, {0,3,true},0x0706050403020100ULL);
    literal(initial,graph,{0,3,false},0x0706050403020100ULL);
    // The younger store completely hides the older writer; changing the older
    // writer alone cannot make the architectural load distinguishable.
    UnresolvedStoreGraph hidden{{{{8,8,0x8877665544332211ULL},false},
        {{8,8,0x0f0e0d0c0b0a0908ULL},true}, {{0,8,0x5555555555555555ULL},false}},
        {8,3,false},true,{{UnresolvedOperandKind::address,2,7},
        {UnresolvedOperandKind::data,0,1},{UnresolvedOperandKind::address,1,0},
        {UnresolvedOperandKind::address,0,3},{UnresolvedOperandKind::data,2,5}}};
    literal(initial,hidden,hidden.load,0x0f0e0d0c0b0a0908ULL);
    distinguished_only(initial,hidden,1,0x0f0e0d0c0b0a0909ULL,0x0f0e0d0c0b0a0909ULL);
    UnresolvedStoreGraph partial{{{{8,8,0x0f0e0d0c0b0a0908ULL},false},
        {{8,1,0x08},true},{{12,4,0x0f0e0d0c},false}}, {8,3,false},true,{}};
    distinguished_only(initial,partial,1,0x09,0x0f0e0d0c0b0a0909ULL);
    UnresolvedStoreGraph middle{{{{12,4,0x0f0e0d0c},false}}, {8,3,false},true,{}};
    distinguished_only(initial,middle,0,0x0f0e0d0d,0x0f0e0d0d0b0a0908ULL);
    UnresolvedStoreGraph single{{{{8,8,0x0f0e0d0c0b0a0908ULL},false}}, {8,3,false},true,{}};
    distinguished_only(initial,single,0,0x0f0e0d0c0b0a0909ULL,0x0f0e0d0c0b0a0909ULL);
    // All integer and FP extensions preserve a one-bit byte distinction.
    unsigned collisions=0;
    for (unsigned fp : {0U,1U}) {
        const unsigned limit=fp ? 4U : 7U;
        for (unsigned op=fp ? 1U : 0U; op<limit; ++op) {
            auto collision=single;
            collision.load={8,op,fp!=0};
            const auto old_value=reference(initial,collision);
            distinguish_unresolved_graph(collision,initial);
            require(old_value!=reference(initial,collision),"load extension hid the injected distinction");
            check_oracle(initial,collision);
            ++collisions;
        }
    }
    const auto original=replay(graph);
    distinguish_unresolved_graph(graph,initial);
    require(replay(graph)==original,"already-distinct youngest writer was mutated");
    auto control=single; control.load.offset=32; control.conflict=false;
    const auto unchanged=replay(control);
    distinguish_unresolved_graph(control,initial);
    require(replay(control)==unchanged,"disjoint control was mutated");
    control.conflict=true;
    const auto inconsistent=replay(control);
    bool rejected=false;
    try { distinguish_unresolved_graph(control,initial); }
    catch (const std::logic_error &) { rejected=true; }
    require(rejected && replay(control)==inconsistent,
        "inconsistent conflict was accepted or mutated before rejection");
    std::cout << "{\"kind\":\"oracle\",\"literal_expectations\":14,"
        "\"extension_collision_classes\":" << collisions
        << ",\"distinguishability_cases\":[\"hidden_older_write\",\"partial_overlay\","
           "\"first_overlap_after_load_start\",\"single_writer_collision\","
           "\"already_distinct_unchanged\",\"disjoint_unchanged\",\"invalid_conflict_rejected\"]}\n";
}
static void arguments_and_destinations() {
    unsigned rejected=0, destination_samples=0;
    for (const auto &[rate,maximum] : std::array<std::pair<unsigned,unsigned>,5>{{
        {1001,4},{std::numeric_limits<unsigned>::max(),4},{500,0},{500,5},
        {500,std::numeric_limits<unsigned>::max()}}}) {
        std::mt19937_64 random(17), unchanged(17);
        bool invalid=false;
        try { (void)make_unresolved_store_graph(random,rate,{},maximum); }
        catch (const std::invalid_argument &) { invalid=true; }
        require(invalid && random==unchanged,"invalid graph bounds accepted or consumed RNG");
        ++rejected;
    }
    for (const auto policy : {ScalarLoadPolicy{1001,{1,1,1}},ScalarLoadPolicy{1,{0,0,0}}}) {
        std::mt19937_64 random(17);
        bool invalid=false;
        try { (void)make_unresolved_store_graph(random,500,policy); }
        catch (const std::invalid_argument &) { invalid=true; }
        require(invalid,"invalid load policy accepted");
        ++rejected;
    }
    for (const auto &[count,registers] : std::array<std::pair<unsigned,unsigned>,9>{{
        {0,0},{0,224},{1,0},{1,1},{4,4},{257,256},{1,257},
        {std::numeric_limits<unsigned>::max(),256},{1,std::numeric_limits<unsigned>::max()}}}) {
        std::mt19937_64 random(17), unchanged(17);
        bool invalid=false;
        try { (void)unresolved_graph_destinations(random,count,registers); }
        catch (const std::invalid_argument &) { invalid=true; }
        require(invalid && random==unchanged,"invalid destination domain accepted or consumed RNG");
        ++rejected;
    }
    for (unsigned registers : {memblock::kIntegerPhysicalRegisters,memblock::kFpPhysicalRegisters,256U})
        for (unsigned count : {1U,3U,6U}) {
            std::mt19937_64 random(registers+count), again(registers+count);
            std::set<unsigned> first_roles;
            for (unsigned sample=0; sample<512; ++sample) {
                const auto roles=unresolved_graph_destinations(random,count,registers);
                require(roles==unresolved_graph_destinations(again,count,registers),
                    "destination replay changed");
                require(roles.size()==count,"destination role lost");
                const std::set<unsigned> unique(roles.begin(),roles.end());
                require(unique.size()==count,"destination aliases another live role");
                for (unsigned role:roles) require(role>0 && role<registers,"destination out of physical domain");
                first_roles.insert(roles.front());
                ++destination_samples;
            }
            require(first_roles.size()>50,"destination selection lacks variation");
        }
    for (unsigned count : {1U,3U,6U,255U}) {
        std::mt19937_64 random(27);
        const auto roles=unresolved_graph_destinations(random,count,count+1U);
        const std::set<unsigned> unique(roles.begin(),roles.end());
        require(unique.size()==count && *unique.begin()==1 && *unique.rbegin()==count,
            "minimal destination domain lost a nonzero role");
    }
    // The independent occurrence checker must reject omissions, duplicates,
    // early/late double issue, invalid identities and invalid event delays.
    UnresolvedStoreGraph valid{{{{0,1,1},false},{{2,2,2},true}}, {8,3,false},false,
        {{UnresolvedOperandKind::data,0,7},{UnresolvedOperandKind::address,1,0},
         {UnresolvedOperandKind::address,0,3}}};
    require(operands_complete_once(valid),"legal hand-written obligations rejected");
    auto missing=valid; missing.after_provisional.pop_back();
    require(!operands_complete_once(missing),"missing STA escaped independent checker");
    auto duplicate=valid; duplicate.after_provisional.push_back(duplicate.after_provisional.front());
    require(!operands_complete_once(duplicate),"duplicate STD escaped independent checker");
    auto twice=valid; twice.after_provisional.push_back({UnresolvedOperandKind::data,1,0});
    require(!operands_complete_once(twice),"early STD reissued after load escaped independent checker");
    auto wrong_role=valid; wrong_role.after_provisional[0].store=2;
    require(!operands_complete_once(wrong_role),"out-of-range event escaped independent checker");
    auto wrong_delay=valid; wrong_delay.after_provisional[0].delay=8;
    require(!operands_complete_once(wrong_delay),"out-of-range delay escaped independent checker");
    auto wrong_kind=valid; wrong_kind.after_provisional[0].kind=static_cast<UnresolvedOperandKind>(9);
    require(!operands_complete_once(wrong_kind),"unknown operand kind escaped independent checker");
    std::cout << "{\"kind\":\"arguments_and_destinations\",\"invalid_cases_rejected\":" << rejected
        << ",\"destination_samples\":" << destination_samples
        << ",\"integer_registers\":" << memblock::kIntegerPhysicalRegisters
        << ",\"fp_registers\":" << memblock::kFpPhysicalRegisters
        << ",\"nonzero_256_register_endpoint\":255,\"corrupt_obligations_rejected\":6}\n";
}
int main(int argc, char **argv) {
    try {
        require(argc==2,"one fixture mode required");
        const std::string mode=argv[1];
        if (mode=="planner") planner();
        else if (mode=="oracle") oracle();
        else if (mode=="arguments") arguments_and_destinations();
        else require(false,"unknown fixture mode");
    } catch (const std::exception &error) {
        std::cerr << error.what() << '\n'; return 1;
    }
}
'''


class UnresolvedGraphTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.before = cls._hashes()
        cls.directory = tempfile.TemporaryDirectory(prefix="memblock-unresolved-graph-")
        cls.addClassCleanup(cls.directory.cleanup)
        cls.addClassCleanup(cls._unchanged_sources)
        cls.folder = Path(cls.directory.name)
        cls.source = cls.folder / "fixture.cpp"
        cls.source.write_text(CPP)
        cls.binary = cls.folder / "fixture"
        cls._compile(cls.binary)

    @staticmethod
    def _hashes():
        return {str(path.relative_to(ROOT)): hashlib.sha256(path.read_bytes()).hexdigest()
                for path in HELPERS}

    @classmethod
    def _unchanged_sources(cls):
        if cls.before != cls._hashes():
            raise AssertionError("production graph/oracle headers changed during compiled tests")

    @classmethod
    def _compile(cls, binary, override=None):
        command = ["c++", "-std=c++20", "-O0"]
        if override is not None:
            command += ["-I", str(override)]
        command += ["-I", str(ROOT / "cpp"), str(cls.source), "-o", str(binary)]
        built = subprocess.run(command, capture_output=True, text=True, timeout=60)
        if built.returncode:
            raise AssertionError("compiled graph fixture failed:\n" + built.stdout + built.stderr)

    def _run(self, mode):
        ran = subprocess.run([str(self.binary), mode], capture_output=True, text=True, timeout=15)
        self.assertEqual(ran.returncode, 0, ran.stdout + ran.stderr)
        evidence = json.loads(ran.stdout)
        print("unresolved_graph_evidence=" + json.dumps(evidence, sort_keys=True))
        return evidence

    def test_randomized_graph_replay_geometry_and_exactly_once_operands(self):
        evidence = self._run("planner")
        self.assertEqual(evidence["store_counts"], [1, 2, 3, 4])
        self.assertEqual(evidence["fp_ops"], [1, 2, 3])

    def test_literal_byte_oracle_and_youngest_writer_distinguishability(self):
        evidence = self._run("oracle")
        self.assertEqual(evidence["extension_collision_classes"], 10)

    def test_invalid_inputs_destination_domains_and_corrupt_obligations(self):
        evidence = self._run("arguments")
        self.assertEqual(evidence["corrupt_obligations_rejected"], 6)

    def test_private_planner_and_oracle_mutants_are_detected(self):
        mutations = (
            ("missing_sta", "unresolved_graph.hpp",
             "ready.push_back({UnresolvedOperandKind::address, index, 0});",
             "if (index!=0) ready.push_back({UnresolvedOperandKind::address, index, 0});",
             "planner", "operand obligations"),
            ("duplicate_std", "unresolved_graph.hpp",
             "ready.push_back({UnresolvedOperandKind::data, index, 0});",
             "{ ready.push_back({UnresolvedOperandKind::data, index, 0}); "
             "ready.push_back({UnresolvedOperandKind::data, index, 0}); }",
             "planner", "operand obligations"),
            ("fixed_store_count", "unresolved_graph.hpp",
             "const unsigned count = 1U + random() % maximum_stores;",
             "const unsigned count = maximum_stores;", "planner", "store-count variation"),
            ("fixed_event_order", "unresolved_graph.hpp",
             "const unsigned choice = random() % ready.size();", "const unsigned choice = 0;",
             "planner", "late data-before-address"),
            ("distinguish_oldest_writer", "unresolved_graph.hpp",
             "for (auto it = graph.stores.rbegin(); it != graph.stores.rend(); ++it)",
             "for (auto it = graph.stores.begin(); it != graph.stores.end(); ++it)",
             "oracle", "wrong architectural writer"),
            ("overlay_oldest_writer", "scalar_dependency.hpp",
             "for (const auto &store : older) {",
             "for (auto it=older.rbegin(); it!=older.rend(); ++it) { const auto &store=*it;",
             "oracle", "literal youngest-writer/extension"),
        )
        detected = []
        for name, filename, old, new, mode, diagnostic in mutations:
            with self.subTest(mutant=name):
                override = self.folder / name
                target = override / "scenarios/mixed"
                target.mkdir(parents=True)
                for original in HELPERS:
                    shutil.copyfile(original, target / original.name)
                path = target / filename
                text = path.read_text()
                self.assertEqual(text.count(old), 1, "private mutant source anchor changed")
                path.write_text(text.replace(old, new))
                binary = override / "fixture"
                self._compile(binary, override)
                ran = subprocess.run([str(binary), mode], capture_output=True, text=True, timeout=15)
                self.assertNotEqual(ran.returncode, 0, "private mutant escaped the production-contract tests")
                self.assertIn(diagnostic, ran.stderr)
                detected.append({"name": name, "mode": mode, "diagnostic": ran.stderr.strip()})
        print("unresolved_graph_mutation_evidence=" + json.dumps(detected, sort_keys=True))


if __name__ == "__main__":
    unittest.main()
