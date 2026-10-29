#include "scenarios/mixed/unresolved_epochs.hpp"
#include <iostream>
#include <set>
#include <sstream>

using namespace memblock::scenarios::mixed;
static void require(bool valid, const char *message) { if (!valid) throw std::runtime_error(message); }
static bool hit(const DependencyStore &store, const DependencyLoad &load) {
    for (unsigned s=store.offset; s<store.offset+store.width; ++s)
        for (unsigned l=load.offset; l<load.offset+load.width(); ++l) if (s==l) return true;
    return false;
}
static std::uint64_t expected(const std::array<unsigned char,64> &initial,
    const UnresolvedStoreGraph &graph, const std::vector<bool> &known, const DependencyLoad &load) {
    std::uint64_t value=0;
    for (unsigned byte=0; byte<load.width(); ++byte) {
        const unsigned address=load.offset+byte;
        unsigned char actual=initial[address];
        for (unsigned index=graph.stores.size(); index>0; --index) {
            const auto &store=graph.stores[index-1].bytes;
            if (known[index-1] && address>=store.offset && address-store.offset<store.width) {
                actual=store.data>>((address-store.offset)*8U); break;
            }
        }
        value|=std::uint64_t{actual}<<(8U*byte);
    }
    const unsigned bits=load.width()*8U;
    if (bits<64 && (load.fp || ((load.op&4U)==0 && ((value>>(bits-1U))&1U))))
        value|=(~std::uint64_t{0})<<bits;
    return value;
}
static bool permutation(std::vector<unsigned> values,unsigned begin,unsigned end) {
    std::sort(values.begin(),values.end());
    if (values.size()!=end-begin) return false;
    for (auto value:values) if (value!=begin++) return false;
    return true;
}
static std::string signature(const UnresolvedStoreGraph &graph,const UnresolvedLoadGroup &group,
    bool schedule_only=false, bool full=false) {
    std::ostringstream out;
    out<<graph.stores.size()<<':'<<group.loads.size()<<':'<<group.victim<<':';
    if (!schedule_only) for (const auto &load:group.loads) {
        for (const auto &store:graph.stores) out<<hit(store.bytes,load);
        out<<';';
        if (full) out<<load.offset<<','<<load.op<<','<<load.fp<<';';
    }
    for (auto index:group.issue_order) out<<index;
    for (const auto &event:graph.after_provisional) out<<'/'<<unsigned(event.kind)<<event.store;
    for (const auto &round:group.rounds) {
        out<<'|'<<round.store<<':'<<round.victim<<':';
        for (auto index:round.issue_order) out<<index;
        if (full) for (auto delay:round.issue_delays) out<<','<<delay;
    }
    if (full) {
        for (const auto &store:graph.stores) out<<':'<<store.bytes.offset<<','<<store.bytes.width<<','<<store.bytes.data<<','<<store.data_before_load;
        for (const auto &event:graph.after_provisional) out<<':'<<event.delay;
        for (auto delay:group.issue_delays) out<<':'<<delay;
    }
    return out.str();
}
static void planner() {
    std::set<std::string> shapes,schedules,four_issue,three_refetch;
    std::array<bool,4> counts{},round_counts{},victims{};
    std::array<unsigned,3> moves{};
    unsigned samples=0,multi_round=0,changed=0,unchanged=0,issue_inversions=0,mixed_classes=0;
    for (unsigned seed:{0U,17U,3735928559U}) {
        std::mt19937_64 random(seed),again(seed);
        for (unsigned step=0; step<1536; ++step) {
            auto graph=make_unresolved_store_graph(random,500,{500,{1,1,1}});
            const auto replay_graph=make_unresolved_store_graph(again,500,{500,{1,1,1}});
            const auto group=make_unresolved_load_group(random,graph,{500,{1,1,1}});
            const auto replay=make_unresolved_load_group(again,replay_graph,{500,{1,1,1}});
            require(random==again && signature(graph,group,false,true)==signature(replay_graph,replay,false,true),"seed replay failed");
            const unsigned count=group.loads.size();
            require(count>=1 && count<=4 && group.primary<count,"load count/primary invalid");
            counts[count-1]=true;
            require(permutation(group.issue_order,0,count),"missing or duplicate initial issue");
            require(group.issue_delays.size()==count,"missing issue delays");
            for (auto delay:group.issue_delays) require(delay<8,"illegal initial delay");
            bool fp=false,integer=false;
            for (const auto &load:group.loads) {
                fp|=load.fp; integer|=!load.fp;
                require(load.offset%load.width()==0 && load.offset+load.width()<=64,"illegal load geometry");
                for (const auto &store:graph.stores) {
                    require(hit(store.bytes,load)==unresolved_overlap(store.bytes,load),"overlap helper mismatch");
                    if (!graph.conflict) require(!hit(store.bytes,load),"control load overlaps a store");
                }
            }
            mixed_classes+=fp&&integer;
            for (unsigned first=0; first<count; ++first) for (unsigned second=first+1; second<count; ++second)
                issue_inversions+=group.issue_order[first]>group.issue_order[second];
            if (count==4) {
                std::string order; for (auto index:group.issue_order) order+=std::to_string(index);
                four_issue.insert(order);
            }
            std::array<unsigned char,64> initial{};
            for (unsigned byte=0; byte<64; ++byte) initial[byte]=byte;
            const auto geometry=signature(graph,group);
            distinguish_unresolved_epochs(graph,group,initial);
            require(signature(graph,group)==geometry,"input repair changed a dependency edge/schedule");
            UnresolvedEpochImage image(initial,unresolved_graph_bytes(graph));
            std::vector<bool> known(graph.stores.size());
            unsigned ordinal=0,minimum=count,previous=count;
            for (const auto &load:group.loads)
                require(image.expected(load)==expected(initial,graph,known,load),"initial unknown-store oracle mismatch");
            for (const auto &event:graph.after_provisional) {
                if (event.kind!=UnresolvedOperandKind::address) continue;
                std::vector<std::uint64_t> before;
                for (const auto &load:group.loads) before.push_back(expected(initial,graph,known,load));
                known[event.store]=true;
                require(image.resolve(event.store),"legal address rejected");
                unsigned oldest=count;
                for (unsigned index=0; index<count; ++index)
                    if (hit(graph.stores[event.store].bytes,group.loads[index])) { oldest=index; break; }
                if (oldest<count) {
                    require(ordinal<group.rounds.size(),"missing recovery round");
                    const auto &round=group.rounds[ordinal++];
                    require(round.store==event.store && round.victim==oldest,"wrong oldest victim/STA identity");
                    require(permutation(round.issue_order,oldest,count),"missing or duplicate suffix issue");
                    require(round.issue_delays.size()==count-oldest,"missing refetch delays");
                    for (auto delay:round.issue_delays) require(delay<8,"illegal refetch delay");
                    if (count-oldest==3) {
                        std::string order; for (auto index:round.issue_order) order+=std::to_string(index-oldest);
                        three_refetch.insert(order);
                    }
                    if (previous!=count) ++moves[oldest<previous?0U:oldest==previous?1U:2U];
                    previous=oldest;minimum=std::min(minimum,oldest);victims[oldest]=true;
                }
                for (unsigned index=0; index<count; ++index) {
                    const auto &load=group.loads[index];
                    const auto value=expected(initial,graph,known,load);
                    require(image.expected(load)==value,"known-address youngest-writer oracle mismatch");
                    bool visible=false;
                    for (unsigned byte=load.offset; byte<load.offset+load.width(); ++byte) {
                        const auto &new_store=graph.stores[event.store].bytes;
                        if (byte<new_store.offset || byte>=new_store.offset+new_store.width) continue;
                        bool hidden=false;
                        for (unsigned younger=event.store+1; younger<known.size(); ++younger) {
                            const auto &store=graph.stores[younger].bytes;
                            hidden|=known[younger] && byte>=store.offset && byte<store.offset+store.width;
                        }
                        visible|=!hidden;
                    }
                    require(!visible || value!=before[index],"visible writer left stale epoch value indistinguishable");
                    if (oldest<count && index>=oldest) { changed+=value!=before[index];unchanged+=value==before[index]; }
                }
            }
            require(ordinal==group.rounds.size() && group.victim==minimum,"extra round or wrong earliest victim");
            require(graph.conflict==!group.rounds.empty(),"conflict/control round accounting mismatch");
            if (ordinal) round_counts[ordinal-1]=true;
            multi_round+=ordinal>1;
            shapes.insert(signature(graph,group));schedules.insert(signature(graph,group,true));++samples;
        }
    }
    for (bool seen:counts) require(seen,"load count variation missing");
    for (bool seen:round_counts) require(seen,"recovery round count variation missing");
    for (bool seen:victims) require(seen,"victim age variation missing");
    for (auto count:moves) require(count>20,"victim movement class missing");
    require(issue_inversions>100 && mixed_classes>100,"issue-order or class variation missing");
    require(multi_round>500 && changed>100 && unchanged>100,"repeated/visible/hidden suffix classes missing");
    require(four_issue.size()==24 && three_refetch.size()==6,"fixed-size permutation variation missing");
    require(shapes.size()>1000 && schedules.size()>1000,"structural graph variation missing");
    for (unsigned maximum=1; maximum<=4; ++maximum) for (unsigned conflict:{0U,1000U}) for (unsigned fp:{0U,1000U}) {
        std::mt19937_64 random(maximum+conflict+fp);
        for (unsigned step=0; step<64; ++step) {
            const auto graph=make_unresolved_store_graph(random,conflict,{fp,{1,1,1}});
            const auto group=make_unresolved_load_group(random,graph,{fp,{1,1,1}},maximum);
            require(group.loads.size()<=maximum,"maximum load bound ignored");
            require(group.rounds.empty()==(conflict==0),"conflict endpoint ignored");
            for (const auto &load:group.loads) require(load.fp==(fp==1000),"FP endpoint ignored");
        }
    }
    std::cout<<"{\"samples\":"<<samples<<",\"topologies\":"<<shapes.size()<<",\"schedules_without_address_data_width_delay\":"<<schedules.size()
        <<",\"four_load_issue_orders\":"<<four_issue.size()<<",\"three_load_refetch_orders\":"<<three_refetch.size()
        <<",\"multi_round_windows\":"<<multi_round<<",\"changed_refetches\":"<<changed<<",\"unchanged_refetches\":"<<unchanged
        <<",\"victim_moves\":["<<moves[0]<<','<<moves[1]<<','<<moves[2]<<"]}\n";
}
static void literal_oracle() {
    std::array<unsigned char,64> initial{};for (unsigned b=0;b<64;++b) initial[b]=b;
    UnresolvedStoreGraph graph{{{{8,8,0x8877665544332211ULL},false},
        {{8,8,0x1020304050607080ULL},true},{{10,2,0xbeef},false}}, {8,3,false},true,
        {{UnresolvedOperandKind::address,1,0},{UnresolvedOperandKind::address,0,0},
         {UnresolvedOperandKind::address,2,0}}};
    UnresolvedEpochImage image(initial,unresolved_graph_bytes(graph));
    const DependencyLoad word{8,3,false},fp{10,1,true};
    require(image.expected(word)==0x0f0e0d0c0b0a0908ULL && image.expected(fp)==0xffffffffffff0b0aULL,"unknown stores leaked into initial image");
    require(image.resolve(1) && image.expected(word)==0x1020304050607080ULL && image.expected(fp)==0xffffffffffff5060ULL,"first known writer literal mismatch");
    require(image.resolve(0) && image.expected(word)==0x1020304050607080ULL,"hidden older writer changed known image");
    require(image.resolve(2) && image.expected(word)==0x10203040beef7080ULL && image.expected(fp)==0xffffffffffffbeefULL,"last known partial writer/boxing mismatch");
    require(image.known_count()==3 && !image.resolve(1) && !image.resolve(3),"duplicate/invalid address transition accepted");
    graph.stores[0].bytes.data=0x0f0e0d0c0b0a0908ULL;
    graph.stores[1].bytes.data=0x0f0e0d0c0b0a0908ULL;
    graph.stores[2].bytes.data=0x0b0b;
    UnresolvedLoadGroup group{{word,fp,{32,3,false}},0,0,{},{},{}};
    distinguish_unresolved_epochs(graph,group,initial);
    require(graph.stores[0].bytes.data==0x0f0e0d0c0b0a0908ULL && graph.stores[1].bytes.data==0x0f0e0d0c0b0b0909ULL &&
            graph.stores[2].bytes.data==0x0b0a,"epoch repair changed wrong writer or byte");
    UnresolvedEpochImage repaired(initial,unresolved_graph_bytes(graph));
    require(repaired.resolve(1) && repaired.expected(word)==0x0f0e0d0c0b0b0909ULL,"repaired intermediate literal mismatch");
    require(repaired.resolve(0) && repaired.expected(word)==0x0f0e0d0c0b0b0909ULL,"hidden repair changed value");
    require(repaired.resolve(2) && repaired.expected(word)==0x0f0e0d0c0b0a0909ULL && repaired.expected(fp)==0xffffffffffff0b0aULL,
        "legal final-equals-initial FP payload was rejected or changed");
    const auto once=graph;distinguish_unresolved_epochs(graph,group,initial);
    for (unsigned s=0;s<graph.stores.size();++s) require(graph.stores[s].bytes.data==once.stores[s].bytes.data,"epoch repair is not idempotent");
    std::cout<<"{\"literal_versions\":10,\"hidden_overwrite\":true,\"final_equals_initial\":true}\n";
}
struct Load { unsigned rob;bool rob_flag;std::uint64_t ftq_ptr;unsigned ftq_offset;bool predecode_rvc;unsigned pdest;std::uint64_t pc; };
struct Redirect { bool valid=true,level=true,ftq_flag=false;unsigned rob_value=0;bool rob_flag=false;std::uint64_t ftq_value=0;unsigned ftq_offset=0;bool is_rvc=false; };
static Redirect redirect_for(const Load &load) {return {true,true,false,load.rob,load.rob_flag,load.ftq_ptr,load.ftq_offset,load.predecode_rvc};}
static std::vector<Load> loads() {return {{254,false,10,0,false,1,0x1000},{255,false,10,2,true,2,0x1004},
    {0,true,10,3,false,3,0x1006},{1,true,10,5,true,4,0x100a}};}
static void provisional(UnresolvedLoadGroupObligation &ledger) {for (auto i:{2U,0U,3U,1U}) require(ledger.provisional(i),"legal provisional rejected");}
static void obligations() {
    auto current=loads();UnresolvedLoadGroupObligation ledger(current,4);provisional(ledger);
    for (unsigned victim:{2U,0U,3U,1U}) {
        const auto before=current;
        require(ledger.redirect(redirect_for(current[victim]),victim) && ledger.returned(),"legal redirect rejected");
        reset_unresolved_ftq_suffix(current,victim);
        for (unsigned index=0;index<current.size();++index) {
            require(current[index].rob==before[index].rob && current[index].pdest==before[index].pdest && current[index].pc==before[index].pc,"FTQ helper changed architectural identity");
            if (index<victim) require(current[index].ftq_ptr==before[index].ftq_ptr && current[index].ftq_offset==before[index].ftq_offset,"surviving prefix FTQ changed");
        }
        for (unsigned index=current.size();index>victim;--index) require(ledger.refetched(index-1,current[index-1]),"legal suffix refetch rejected");
        require(ledger.finish_round(),"legal completed round rejected");
    }
    require(ledger.rounds()==4 && ledger.executions(0)==2 && ledger.executions(1)==3 && ledger.executions(2)==4 && ledger.executions(3)==5,"epoch executions lost or duplicated");
    require(ledger.resolved()&&ledger.readback()&&ledger.done(),"legal terminal obligations rejected");
    auto initial=loads();UnresolvedLoadGroupObligation control(initial,4);provisional(control);
    require(control.resolved()&&control.readback()&&control.done(),"all-disjoint control rejected");
    unsigned rejected=0;
    for (unsigned bad=0;bad<20;++bad) {
        auto values=loads();UnresolvedLoadGroupObligation invalid(values,4);bool accepted=false;
        if (bad==0) accepted=invalid.resolved();
        else if (bad==1) accepted=invalid.readback();
        else if (bad==2) accepted=invalid.returned();
        else if (bad==3) accepted=invalid.refetched(1,values[1]);
        else if (bad==4) {provisional(invalid);accepted=invalid.provisional(0);}
        else if (bad==5) accepted=invalid.provisional(4);
        else if (bad<=12) {
            provisional(invalid);auto event=redirect_for(values[1]);
            if (bad==6) event=redirect_for(values[2]);
            if (bad==7) event.rob_flag=!event.rob_flag;
            if (bad==8) event.ftq_value++;
            if (bad==9) event.ftq_offset++;
            if (bad==10) event.is_rvc=!event.is_rvc;
            if (bad==11) event.level=false;
            if (bad==12) event.ftq_flag=true;
            accepted=invalid.redirect(event,1);
        } else {
            provisional(invalid);require(invalid.redirect(redirect_for(values[1]),1)&&invalid.returned(),"recovery setup failed");
            if (bad==13) accepted=invalid.refetched(0,values[0]);
            if (bad==14) accepted=invalid.refetched(1,values[1]); // stale FTQ epoch
            reset_unresolved_ftq_suffix(values,1);
            if (bad==15) {values[1].pdest++;accepted=invalid.refetched(1,values[1]);}
            if (bad==16) {require(invalid.refetched(1,values[1]),"refetch setup failed");accepted=invalid.refetched(1,values[1]);}
            if (bad==17) {require(invalid.refetched(1,values[1]),"refetch setup failed");accepted=invalid.finish_round();}
            if (bad==18) accepted=invalid.returned();
            if (bad==19) accepted=invalid.redirect(redirect_for(values[2]),2);
        }
        require(!accepted&&!invalid.done(),"invalid obligation transition escaped");++rejected;
    }
    std::cout<<"{\"legal_rounds\":4,\"invalid_transitions_rejected\":"<<rejected<<",\"executions\":[2,3,4,5]}\n";
}
int main(int argc,char **argv) {
    try {require(argc==2,"fixture mode required");const std::string mode=argv[1];
        if (mode=="planner") planner();else if(mode=="oracle") literal_oracle();else obligations();
    }catch(const std::exception &error){std::cerr<<error.what()<<'\n';return 1;}
}
