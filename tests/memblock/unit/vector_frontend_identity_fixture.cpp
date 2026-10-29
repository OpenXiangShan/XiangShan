#include "scenarios/mixed/state.hpp"
#include "scenarios/mixed/vector_indices.hpp"
#include "scenarios/mixed/vector_operands.hpp"
#include <set>
#include <tuple>

using namespace memblock;
using namespace memblock::scenarios;
using namespace memblock::scenarios::mixed;
static void require(bool value, const char *message) {
    if (!value) throw std::runtime_error(message);
}
namespace memblock::scenarios::mixed {
class MixedScenario {
public:
    RandomConstraints constraints = RandomConstraints::preset("coverage");
    ConstraintCoverage constraint_coverage;
    MixedCoverage coverage;
    std::mt19937_64 random{17};
    std::uint64_t rob_offset=0, sq_offset=0, lq_offset=0;
    struct {
        SparseMemory image;
        SparseMemory &memory() { return image; }
    } environment;
    std::shared_ptr<const ReferencePageMap> reference_pages_for(std::uint64_t) const { return {}; }
    static unsigned vector_shape_uop_count(const RandomVectorShape &);
    static unsigned vector_shape_flow_num(const RandomVectorShape &);
    bool choose_binary_class(unsigned, const std::array<std::uint64_t,2> &);
    std::vector<VectorMemoryTransaction> make_random_vector_uops(bool, std::uint64_t, const RandomVectorShape &, bool);
    std::vector<VectorMemoryTransaction> former_vector_uops(bool, std::uint64_t, const RandomVectorShape &, bool);
};
// PRODUCTION_BUILDERS
}

static auto non_frontend_fields(const VectorMemoryTransaction &u) {
    return std::tie(u.store,u.segment,u.address,u.reference_pages,u.stride,u.addressing,
        u.eew,u.vsew,u.vl,u.vstart,u.vm,u.mask_bits,u.mask_high,u.vma,u.vta,u.rob,u.rob_flag,
        u.lq,u.lq_flag,u.sq,u.sq_flag,u.pdest,u.lane,u.flow_num,u.vlmul,u.vuop_idx,
        u.last_uop,u.nf,u.data,u.index,u.oracle_index_group);
}
static void instruction_identity(const std::vector<VectorMemoryTransaction> &uops) {
    require(!uops.empty(),"empty instruction");
    const auto &first=uops.front();
    for(const auto &u:uops) {
        require(u.rob==first.rob && u.rob_flag==first.rob_flag,"builder split instruction ROB identity");
        require(u.ftq_ptr==first.ftq_ptr && u.ftq_offset==first.ftq_offset,
                "same-instruction uops have different frontend identities");
    }
}
static std::vector<VectorMemoryTransaction> segment_metadata(std::mt19937_64 &random,unsigned count) {
    std::vector<VectorMemoryTransaction> uops;
    for(unsigned i=0;i<count;++i) {
        uops.push_back(VectorMemoryTransaction{.segment=true,.rob=37,.rob_flag=true,
            .vuop_idx=static_cast<std::uint8_t>(i),.last_uop=i+1==count,.nf=1});
        auto &segment_uop=uops.back();
        // PRODUCTION_SEGMENT_METADATA
    }
    return uops;
}
static std::vector<VectorMemoryTransaction> former_segment_metadata(std::mt19937_64 &random,unsigned count) {
    std::vector<VectorMemoryTransaction> uops;
    for(unsigned i=0;i<count;++i) {
        uops.push_back(VectorMemoryTransaction{.segment=true,.rob=37,.rob_flag=true,
            .vuop_idx=static_cast<std::uint8_t>(i),.last_uop=i+1==count,.nf=1});
        auto &segment_uop=uops.back();
        // FORMER_SEGMENT_METADATA
    }
    return uops;
}
int main(int argc,char **argv) {
    try {
        require(argc==2,"one mode required");
        const std::string mode=argv[1];
        std::set<std::uint64_t> pointers,offsets;
        unsigned instructions=0,multiple=0;
        if(mode=="ordinary" || mode=="former-ordinary") {
            MixedScenario current,former;
            current.random.seed(6037);former.random.seed(6037);
            for(unsigned a=0;a<4;++a) for(unsigned e=0;e<4;++e)
              for(unsigned s=0;s<4;++s) for(int l=-3;l<=3;++l) {
                const int m=l+int(e)-int(s);
                if(l<int(s)-3 || m < -3 || m>3) continue;
                const RandomVectorShape shape{a,e,s,l,m,1};
                for(unsigned repeat=0;repeat<4;++repeat) for(bool store:{false,true}) {
                    auto uops=current.make_random_vector_uops(store,0x80001000,shape,true);
                    auto legacy=former.former_vector_uops(store,0x80001000,shape,true);
                    require(current.random==former.random,"frontend constraint changed PRNG draw consumption");
                    require(std::tie(current.rob_offset,current.lq_offset,current.sq_offset)==
                        std::tie(former.rob_offset,former.lq_offset,former.sq_offset),"frontend constraint changed queue histories");
                    require(uops.size()==legacy.size(),"frontend constraint changed decomposition");
                    for(unsigned i=0;i<uops.size();++i)
                        require(non_frontend_fields(uops[i])==non_frontend_fields(legacy[i]),
                            "frontend constraint changed unrelated vector fields");
                    require(uops.front().ftq_ptr==legacy.front().ftq_ptr &&
                        uops.front().ftq_offset==legacy.front().ftq_offset,"instruction frontend draw changed");
                    instruction_identity(mode=="former-ordinary"?legacy:uops);
                    pointers.insert(uops.front().ftq_ptr);offsets.insert(uops.front().ftq_offset);
                    ++instructions;multiple+=uops.size()>1;
                }
              }
        } else if(mode=="segment" || mode=="former-segment") {
            std::mt19937_64 current(8041),former(8041);
            for(unsigned seed=0;seed<128;++seed) for(unsigned count:{1U,2U,3U,8U,32U}) {
                auto uops=segment_metadata(current,count);
                auto legacy=former_segment_metadata(former,count);
                require(current==former,"segment frontend constraint changed PRNG draw consumption");
                require(uops.front().ftq_ptr==legacy.front().ftq_ptr &&
                    uops.front().ftq_offset==legacy.front().ftq_offset,"segment instruction frontend draw changed");
                instruction_identity(mode=="former-segment"?legacy:uops);
                pointers.insert(uops.front().ftq_ptr);offsets.insert(uops.front().ftq_offset);
                ++instructions;multiple+=count>1;
            }
        } else throw std::runtime_error("unknown mode");
        require(multiple>0 && pointers.size()>=32 && offsets.size()==8,
                "frontend identities did not vary between instructions");
        std::cout<<"instructions="<<instructions<<" multi_uop="<<multiple
            <<" ftq_pointers="<<pointers.size()<<" offsets="<<offsets.size()<<'\n';
        return 0;
    } catch(const std::exception &error) { std::cerr<<error.what()<<'\n'; return 1; }
}
