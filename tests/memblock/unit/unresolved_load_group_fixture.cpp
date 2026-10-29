#include "scenarios/mixed/unresolved_load_group.hpp"
#include <iostream>
#include <set>
#include <sstream>

using namespace memblock::scenarios::mixed;
static void require(bool valid, const char *message) {
    if (!valid) throw std::runtime_error(message);
}
static std::string signature(const UnresolvedStoreGraph &graph, const UnresolvedLoadGroup &group,
    bool schedule_only = false) {
    std::ostringstream out;
    out << graph.stores.size() << ':' << group.loads.size() << ':' << group.victim << ':';
    if (!schedule_only) {
        for (const auto &load:group.loads) {
            for (const auto &store:graph.stores) out << unresolved_overlap(store.bytes,load);
            out << ';';
        }
    }
    for (auto index:group.issue_order) out << index;
    out << '/';
    for (auto index:group.refetch_order) out << index;
    return out.str();
}
static bool intersects(const DependencyStore &store, const DependencyLoad &load) {
    for (unsigned s=store.offset; s<store.offset+store.width; ++s)
        for (unsigned l=load.offset; l<load.offset+load.width(); ++l) if (s==l) return true;
    return false;
}
static bool permutation(std::vector<unsigned> values, unsigned begin, unsigned end) {
    std::sort(values.begin(),values.end());
    if (values.size()!=end-begin) return false;
    for (auto value:values) if (value!=begin++) return false;
    return true;
}
static std::uint64_t expected(const std::array<unsigned char,64> &initial,
    const UnresolvedStoreGraph &graph, const DependencyLoad &load) {
    std::uint64_t value=0;
    for (unsigned byte=0; byte<load.width(); ++byte) {
        const unsigned address=load.offset+byte;
        unsigned char actual=initial[address];
        for (unsigned index=graph.stores.size(); index>0; --index) {
            const auto &store=graph.stores[index-1].bytes;
            if (address>=store.offset && address-store.offset<store.width) {
                actual=store.data>>((address-store.offset)*8U); break;
            }
        }
        value|=std::uint64_t{actual}<<(8U*byte);
    }
    const auto bits=load.width()*8U;
    if (bits<64 && (load.fp || ((load.op&4U)==0 && ((value>>(bits-1U))&1U))))
        value|=(~std::uint64_t{0})<<bits;
    return value;
}
static void planner() {
    std::set<std::string> shapes,schedules,fixed_four_issue,fixed_three_refetch;
    std::array<bool,4> counts{},victims{};
    unsigned samples=0,competing=0,disjoint_suffix=0,survivors=0,issue_inversions=0,mixed_classes=0;
    for (unsigned seed:{0U,17U,3735928559U}) {
        std::mt19937_64 random(seed), again(seed);
        for (unsigned step=0; step<1536; ++step) {
            auto graph=make_unresolved_store_graph(random,500,{500,{1,1,1}});
            const auto replay_graph=make_unresolved_store_graph(again,500,{500,{1,1,1}});
            const auto group=make_unresolved_load_group(random,graph,{500,{1,1,1}});
            const auto replay=make_unresolved_load_group(again,replay_graph,{500,{1,1,1}});
            require(random==again && signature(graph,group)==signature(replay_graph,replay),"seed replay failed");
            const unsigned count=group.loads.size();
            require(count>=1 && count<=4 && group.primary<count,"load count/primary out of range");
            require(group.victim==(graph.conflict?group.primary:count),"wrong victim/control identity");
            counts[count-1]=true;
            if (graph.conflict) victims[group.victim]=true;
            require(permutation(group.issue_order,0,count),"missing or duplicate provisional issue");
            require(permutation(group.refetch_order,group.victim,count),"missing or duplicate suffix refetch");
            require(group.issue_delays.size()==count,"missing issue delays");
            for (auto delay:group.issue_delays) require(delay<8,"invalid issue delay");
            for (unsigned first=0; first<count; ++first)
                for (unsigned second=first+1; second<count; ++second)
                    issue_inversions+=group.issue_order[first]>group.issue_order[second];
            bool fp=false,integer=false;
            for (unsigned index=0; index<count; ++index) {
                const auto &load=group.loads[index];
                require(load.offset%load.width()==0 && load.offset+load.width()<=64,"illegal load geometry");
                fp|=load.fp; integer|=!load.fp;
                bool hit=false;
                for (const auto &store:graph.stores) {
                    const bool overlap=intersects(store.bytes,load);
                    require(overlap==unresolved_overlap(store.bytes,load),"overlap helper disagrees with byte oracle");
                    if (index<group.victim || !graph.conflict)
                        require(!overlap,"surviving prefix/control overlaps an older store");
                    if (overlap) require(intersects(store.bytes,group.loads[group.primary]),
                        "store could select a younger load before the primary victim");
                    hit|=overlap;
                }
                if (index>group.victim) { competing+=hit; disjoint_suffix+=!hit; }
            }
            mixed_classes+=fp&&integer;
            survivors+=graph.conflict&&group.victim>0;
            for (const auto &store:graph.stores) {
                unsigned oldest=count;
                for (unsigned index=0; index<count; ++index)
                    if (intersects(store.bytes,group.loads[index])) { oldest=index; break; }
                require(oldest==count || oldest==group.victim,"oldest byte-conflict identity changed");
            }
            std::array<unsigned char,64> initial{};
            for (unsigned byte=0; byte<64; ++byte) initial[byte]=byte;
            const auto shape=signature(graph,group);
            distinguish_unresolved_load_group(graph,group,initial);
            require(shape==signature(graph,group),"distinguishability changed a dependency edge");
            for (const auto &load:group.loads) {
                bool hit=false;
                for (const auto &store:graph.stores) hit|=intersects(store.bytes,load);
                const auto value=expected(initial,graph,load);
                require(value==scalar_dependency_expected(initial,unresolved_graph_bytes(graph),load),
                    "youngest-writer byte/format oracle disagrees");
                auto empty=graph; empty.stores.clear();
                require((value!=expected(initial,empty,load))==hit,"stale conflicting value remains indistinguishable");
            }
            shapes.insert(signature(graph,group)); schedules.insert(signature(graph,group,true));
            if (count==4 && graph.conflict && group.victim==1) {
                std::string issue,refetch;
                for (auto index:group.issue_order) issue+=std::to_string(index);
                for (auto index:group.refetch_order) refetch+=std::to_string(index);
                fixed_four_issue.insert(issue); fixed_three_refetch.insert(refetch);
            }
            ++samples;
        }
    }
    for (bool seen:counts) require(seen,"load count variation missing");
    for (bool seen:victims) require(seen,"victim age variation missing");
    require(competing>100 && disjoint_suffix>100 && survivors>100,"suffix/prefix dependency classes missing");
    require(issue_inversions>100 && mixed_classes>100,"issue-order or destination-class variation missing");
    require(fixed_four_issue.size()==24 && fixed_three_refetch.size()==6,
        "fixed-size issue/refetch permutation variation missing");
    require(shapes.size()>1000 && schedules.size()>500,"structural schedule variation missing");
    for (unsigned maximum=1; maximum<=4; ++maximum)
        for (unsigned conflict:{0U,1000U}) for (unsigned fp:{0U,1000U}) {
            std::mt19937_64 random(maximum+conflict+fp);
            for (unsigned step=0; step<64; ++step) {
                const auto graph=make_unresolved_store_graph(random,conflict,{fp,{1,1,1}});
                const auto group=make_unresolved_load_group(random,graph,{fp,{1,1,1}},maximum);
                require(group.loads.size()<=maximum,"maximum loads ignored");
                for (const auto &load:group.loads) require(load.fp==(fp==1000U),"FP endpoint ignored");
            }
        }
    std::cout << "{\"samples\":"<<samples<<",\"topologies\":"<<shapes.size()
        <<",\"schedules_without_address_data_width_delay\":"<<schedules.size()
        <<",\"fixed_four_issue_orders\":"<<fixed_four_issue.size()<<",\"fixed_three_refetch_orders\":"<<fixed_three_refetch.size()
        <<",\"competing_younger_loads\":"<<competing<<",\"disjoint_younger_loads\":"<<disjoint_suffix
        <<",\"surviving_prefix_windows\":"<<survivors<<",\"issue_inversions\":"<<issue_inversions<<"}\n";
}
struct Load { unsigned rob; bool rob_flag; std::uint64_t ftq_ptr; unsigned ftq_offset; bool predecode_rvc; };
struct Redirect { bool valid=true,level=true,ftq_flag=false; unsigned rob_value=0; bool rob_flag=false;
    std::uint64_t ftq_value=0; unsigned ftq_offset=0; bool is_rvc=false; };
static Redirect redirect_for(const Load &load) {
    return {true,true,false,load.rob,load.rob_flag,load.ftq_ptr,load.ftq_offset,load.predecode_rvc};
}
static void literal_repairs() {
    std::array<unsigned char,64> initial{};
    for (unsigned byte=0; byte<64; ++byte) initial[byte]=byte;
    UnresolvedStoreGraph graph{{{{8,8,0x0f0e0d0c0b0a0908ULL},false},
        {{10,2,0x0b0a},true},{{12,1,0x0c},false}}, {8,3,false},true,{}};
    const UnresolvedLoadGroup group{{{8,3,false},{10,1,true},{12,4,false},{32,3,false}},0,0,{},{},{}};
    distinguish_unresolved_load_group(graph,group,initial);
    const std::array<std::uint64_t,4> literals{0x0f0e0d0d0b0b0909ULL,
        0xffffffffffff0b0bULL,0x0dULL,0x2726252423222120ULL};
    for (unsigned index=0; index<group.loads.size(); ++index)
        require(expected(initial,graph,group.loads[index])==literals[index],
            "multiple repaired loads disagree with literal expected bytes");
    require(graph.stores[0].bytes.data==0x0f0e0d0c0b0a0909ULL &&
        graph.stores[1].bytes.data==0x0b0b && graph.stores[2].bytes.data==0x0d,
        "multiple-load repair changed a hidden writer or wrong byte");
    const auto saved=graph;
    distinguish_unresolved_load_group(graph,group,initial);
    for (unsigned index=0; index<graph.stores.size(); ++index)
        require(graph.stores[index].bytes.data==saved.stores[index].bytes.data,
            "multiple-load repair undid a previous distinction");
}
static void obligations() {
    literal_repairs();
    const std::vector<Load> loads{{254,false,1,0,true},{255,false,2,1,false},
        {0,true,3,2,true},{1,true,4,3,false}};
    unsigned legal=0,rejected=0;
    for (unsigned victim=0; victim<=loads.size(); ++victim) {
        UnresolvedLoadGroupObligation ledger(loads,victim);
        for (unsigned index:{2U,0U,3U,1U}) require(ledger.provisional(index),"legal provisional rejected");
        if (victim<loads.size()) {
            require(ledger.redirect(redirect_for(loads[victim])) && ledger.returned(),"legal redirect rejected");
            for (unsigned index=loads.size(); index>victim; --index)
                require(ledger.refetched(index-1),"legal suffix refetch rejected");
        }
        require(ledger.resolved() && ledger.readback() && ledger.done(),"legal terminal obligations rejected");
        ++legal;
    }
    auto provision=[&](auto &ledger) { for (unsigned i=0;i<loads.size();++i) require(ledger.provisional(i),"setup failed"); };
    auto recover=[&](auto &ledger) { provision(ledger); require(ledger.redirect(redirect_for(loads[1]))&&ledger.returned(),"setup recovery failed"); };
    for (unsigned mutation=0; mutation<17; ++mutation) {
        UnresolvedLoadGroupObligation ledger(loads,1);
        bool accepted=false;
        if (mutation==0) accepted=ledger.resolved();
        else if (mutation==1) accepted=ledger.readback();
        else if (mutation==2) accepted=ledger.refetched(1);
        else if (mutation==3) accepted=ledger.returned();
        else if (mutation==4) { provision(ledger); accepted=ledger.provisional(0); }
        else if (mutation==5) accepted=ledger.provisional(4);
        else if (mutation>=6 && mutation<=12) {
            provision(ledger); auto event=redirect_for(loads[1]);
            if (mutation==6) event=redirect_for(loads[2]);
            if (mutation==7) event.rob_flag=!event.rob_flag;
            if (mutation==8) event.ftq_value++;
            if (mutation==9) event.ftq_offset++;
            if (mutation==10) event.is_rvc=!event.is_rvc;
            if (mutation==11) event.level=false;
            if (mutation==12) event.ftq_flag=true;
            accepted=ledger.redirect(event);
        } else if (mutation==13) { recover(ledger); accepted=ledger.refetched(0); }
        else if (mutation==14) { recover(ledger); require(ledger.refetched(1),"setup refetch failed"); accepted=ledger.refetched(1); }
        else if (mutation==15) { recover(ledger); require(ledger.refetched(1),"setup refetch failed"); accepted=ledger.resolved(); }
        else { recover(ledger); accepted=ledger.returned(); }
        require(!accepted && !ledger.done(),"invalid obligation transition escaped"); ++rejected;
    }
    // A correct victim writeback/refetch alone must not discharge its younger
    // disjoint suffix, even when those provisional values were already correct.
    UnresolvedLoadGroupObligation missing(loads,1); recover(missing);
    require(missing.refetched(1) && missing.refetched(2) && !missing.resolved() && !missing.done(),
        "missing disjoint younger refetch escaped"); ++rejected;
    std::cout << "{\"legal_layouts\":"<<legal<<",\"invalid_transitions_rejected\":"<<rejected<<"}\n";
}
int main(int argc,char **argv) {
    try { require(argc==2,"fixture mode required");
        if (std::string(argv[1])=="planner") planner(); else obligations();
    } catch (const std::exception &error) { std::cerr<<error.what()<<'\n'; return 1; }
}
