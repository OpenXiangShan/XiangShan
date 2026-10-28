#include "scenarios/shared.hpp"
#include "scenarios/mixed/scalar_dependency.hpp"
#include "scenarios/mixed/unresolved_dependency.hpp"
namespace memblock {
#include "environment/scoreboards.inc"
}
using namespace memblock;
using namespace memblock::scenarios;
using namespace memblock::scenarios::mixed;

void require(bool value, const char *message)
{
    if (!value) throw std::runtime_error(message);
}
template<class Callback> void rejects(Callback callback)
{
    try { callback(); } catch (const std::invalid_argument &) { return; }
    throw std::runtime_error("invalid FP stimulus accepted");
}

namespace memblock::scenarios::mixed {
struct MixedScenario {
    std::uint64_t rob_offset=0, lq_offset=0, sq_offset=0;
    std::mt19937_64 random{91};
    std::shared_ptr<const ReferencePageMap> reference_pages_for(std::uint64_t) { return {}; }
    LoadTransaction make_load(std::uint64_t, LoadOp, unsigned, bool fp=false);
};
// PRODUCTION_FACTORY
}

struct Pattern { LoadOp op; std::uint64_t raw, boxed; };
const std::array<Pattern,13> patterns{{
    {LoadOp::lh,0x0000,0xffffffffffff0000ULL},
    {LoadOp::lh,0x3c01,0xffffffffffff3c01ULL},
    {LoadOp::lh,0x8001,0xffffffffffff8001ULL},
    {LoadOp::lh,0x7c01,0xffffffffffff7c01ULL},
    {LoadOp::lh,0x7e55,0xffffffffffff7e55ULL},
    {LoadOp::lw,0x00000000,0xffffffff00000000ULL},
    {LoadOp::lw,0x3f800001,0xffffffff3f800001ULL},
    {LoadOp::lw,0x80000001,0xffffffff80000001ULL},
    {LoadOp::lw,0x7f800001,0xffffffff7f800001ULL},
    {LoadOp::lw,0x7fc12345,0xffffffff7fc12345ULL},
    {LoadOp::ld,0x0123456789abcdefULL,0x0123456789abcdefULL},
    {LoadOp::ld,0x7ff0000000000001ULL,0x7ff0000000000001ULL},
    {LoadOp::ld,0xfff8123456789abcULL,0xfff8123456789abcULL},
}};

void memory_values()
{
    SparseMemory memory;
    for (const auto &p:patterns) {
        LoadTransaction tx;
        tx.address=0x80200000; tx.op=p.op; tx.fp_wen=true; tx.rf_wen=false;
        for (unsigned b=0;b<8;++b) memory.write_reference_byte(tx.address+b,p.raw>>(8*b));
        require(memory.expected_load(tx)==p.boxed,"FP payload/boxing differs from literal ISA expectation");
        tx.fp_wen=false; tx.rf_wen=true;
        if (p.op==LoadOp::lh && p.raw==0x3c01)
            require(memory.expected_load(tx)==0x3c01,"integer LH boxing changed");
        if (p.op==LoadOp::lw && p.raw==0x80000001)
            require(memory.expected_load(tx)==0xffffffff80000001ULL,"integer LW extension changed");
    }
    LoadTransaction bad;
    bad.fp_wen=true;
    rejects([&]{ (void)memory.expected_load(bad); });
    bad.rf_wen=false;
    for (auto op:{LoadOp::lb,LoadOp::lbu,LoadOp::lhu,LoadOp::lwu,LoadOp::hlvh}) {
        bad.op=op; rejects([&]{ (void)memory.expected_load(bad); });
    }
    constexpr std::uint64_t va=0x2000000000ULL, first=0x80405000, second=0x80401000;
    LoadTransaction crossing;
    crossing.address=va+0xffe; crossing.op=LoadOp::lw;
    crossing.fp_wen=true; crossing.rf_wen=false;
    crossing.reference_pages=std::make_shared<const ReferencePageMap>(va,
        std::vector<std::uint64_t>{first,second});
    const std::array<std::uint64_t,4> physical{first+0xffe,first+0xfff,second,second+1};
    for (unsigned b=0;b<4;++b) memory.write_reference_byte(physical[b],0x11*(b+1));
    require(memory.expected_load(crossing)==0xffffffff44332211ULL,"FP noncontiguous page bytes lost");
    require(memory.expected_load(first+0xffe,LoadOp::lwu)!=0x44332211,"crossing control is indistinguishable");
}

void policies()
{
    for (const auto &[name,rate]:std::array<std::pair<const char*,unsigned>,4>{{
        {"coverage",250},{"spec",200},{"corner",500},{"cmo-stress",0}}}) {
        auto c=RandomConstraints::preset(name);
        require(c.scalar_fp_per_mille==rate,"profile FP rate changed");
        c.apply("scalar-fp=1000"); c.apply("scalar-fp16=1");
        c.apply("scalar-fp32=1"); c.apply("scalar-fp64=1"); c.validate();
        require(c.summary().find("target_scalar_fp=1000")!=std::string::npos,"FP rate absent from replay");
        for (unsigned width=0;width<3;++width) {
            auto policy=c.scalar_load_policy(); policy.fp_width_weights={0,0,0};
            policy.fp_width_weights[width]=7;
            std::mt19937_64 random(8+width);
            for (unsigned i=0;i<256;++i) {
                const auto shape=policy.choose(random);
                require(shape.fp && shape.op==width+1,"zero-weight FP width generated");
            }
        }
        c.apply("scalar-fp=1001"); rejects([&]{c.validate();});
        c.apply("scalar-fp=1000"); c.apply("scalar-fp16=0"); c.apply("scalar-fp32=0");
        c.apply("scalar-fp64=0"); rejects([&]{c.validate();});
        c.apply("scalar-fp=0"); c.validate();
        std::mt19937_64 random(7);
        for (unsigned i=0;i<256;++i) require(!c.scalar_load_policy().choose(random).fp,"disabled FP selected");
    }
    ScalarLoadPolicy mixed{500,{1,1,1}};
    std::mt19937_64 first(51),replay(51),other(52);
    std::string a,b,c;
    std::array<bool,3> fp_widths{}; std::array<bool,7> int_ops{};
    for (unsigned i=0;i<512;++i) {
        auto x=mixed.choose(first),y=mixed.choose(replay),z=mixed.choose(other);
        a+=std::to_string(x.op)+std::to_string(x.fp);
        b+=std::to_string(y.op)+std::to_string(y.fp);
        c+=std::to_string(z.op)+std::to_string(z.fp);
        if (x.fp) fp_widths[x.op-1]=true; else int_ops[x.op]=true;
    }
    require(a==b && a!=c,"FP format replay/seed variation failed");
    require(std::all_of(fp_widths.begin(),fp_widths.end(),[](bool x){return x;}) &&
            std::all_of(int_ops.begin(),int_ops.end(),[](bool x){return x;}),"sample misses FP/int formats");
}

void factory()
{
    MixedScenario scenario;
    for (unsigned i=0;i<2048;++i) {
        const bool fp=(i&1)!=0;
        const auto tx=scenario.make_load(0x80200000+(i%64),LoadOp::lw,i%3,fp);
        require(tx.fp_wen==fp && tx.rf_wen!=fp,"factory destination class wrong");
        require(tx.pdest>=1 && tx.pdest<(fp?192:224),"factory destination outside class domain");
    }
    std::mt19937_64 random(99);
    for (unsigned n=0;n<1024;++n) {
        const auto ds=unresolved_load_destinations(random,192);
        require(ds[0]!=ds[1] && ds[1]!=ds[2] && ds[0]!=ds[2] && ds[2]<192,
                "unresolved FP roles overlap/exceed destination domain");
    }
}

void dependencies()
{
    std::array<unsigned char,64> snapshot{};
    for (unsigned b=0;b<64;++b) snapshot[b]=b;
    std::vector<DependencyStore> older{{8,8,0x8877665544332211ULL}};
    require(scalar_dependency_expected(snapshot,older,{8,1,true})==0xffffffffffff2211ULL,"forwarded FLH wrong");
    require(scalar_dependency_expected(snapshot,older,{8,2,true})==0xffffffff44332211ULL,"forwarded FLW wrong");
    require(scalar_dependency_expected(snapshot,older,{8,3,true})==0x8877665544332211ULL,"forwarded FLD wrong");
    require(scalar_dependency_expected(snapshot,{{10,1,0x80}},{8,2,true})==0xffffffff0b800908ULL,
            "partial forwarded FLW bytes wrong");
    require(scalar_dependency_expected(snapshot,{}, {8,2,true})==0xffffffff0b0a0908ULL,
            "provisional FP version wrong");
    ScalarLoadPolicy fp{1000,{1,1,1}};
    std::mt19937_64 random(91), replay(91);
    std::set<std::string> topologies;
    for (unsigned i=0;i<512;++i) {
        auto plan=make_scalar_dependency_plan(random,fp);
        auto same=make_scalar_dependency_plan(replay,fp);
        std::ostringstream key;
        require(plan.loads.size()==same.loads.size(),"dependency replay changed");
        for (const auto &store:plan.older) key<<'s'<<store.offset<<':'<<store.width;
        for (unsigned j=0;j<plan.loads.size();++j) {
            const auto &load=plan.loads[j];
            require(load.fp && load.op>=1 && load.op<=3 && load.offset%load.width()==0,
                    "dependency FP class/width/alignment invalid");
            require(load.op==same.loads[j].op && load.offset==same.loads[j].offset,"FP dependency replay differs");
            key<<'f'<<load.offset<<':'<<load.op;
        }
        topologies.insert(key.str());
        auto unresolved=make_unresolved_dependency_plan(random,500,fp);
        auto same_unresolved=make_unresolved_dependency_plan(replay,500,fp);
        require(unresolved.load.fp && unresolved.load.op==same_unresolved.load.op &&
                unresolved.load.offset==same_unresolved.load.offset,"FP unresolved plan invalid");
    }
    require(topologies.size()>480,"FP traffic collapsed into a few fixed dependency templates");
    std::cout<<"unique_fp_dependency_topologies="<<topologies.size()<<"/512\n";
}

bool accepts(const LoadTransaction &tx,std::uint64_t expected,std::uint64_t returned,
             bool rf=false,bool fp=true,int pdest=-1)
{
    LoadScoreboard scoreboard;
    scoreboard.expect(tx,expected);
    generated::ScalarLoadWriteback wb;
    wb.valid=true; wb.rob_value=tx.rob; wb.rob_flag=tx.rob_flag;
    wb.pdest=pdest<0?tx.pdest:pdest; wb.data=returned; wb.rf_wen=rf; wb.fp_wen=fp;
    wb.exception_mask=tx.expected_exception_mask;
    scoreboard.observe(tx.lane,wb);
    return scoreboard.done() && scoreboard.error().empty();
}
void scoreboards()
{
    LoadTransaction tx;
    tx.fp_wen=true; tx.rf_wen=false; tx.rob=7; tx.pdest=191; tx.lane=2;
    for (const auto &p:patterns) {
        tx.op=p.op;
        require(accepts(tx,p.boxed,p.boxed),"correct FP completion rejected");
        require(!accepts(tx,p.boxed,p.boxed^1),"wrong FP payload accepted");
        require(!accepts(tx,p.boxed,p.boxed,true,false),"integer write substituted for FP");
        require(!accepts(tx,p.boxed,p.boxed,false,true,190),"wrong FP destination accepted");
        if (p.raw!=p.boxed) require(!accepts(tx,p.boxed,p.raw),"unboxed FP result accepted");
        if (p.op==LoadOp::lw && p.raw==0x3f800001)
            require(!accepts(tx,p.boxed,0x3f800001),"positive integer extension accepted as FP");
    }
    tx.op=LoadOp::ld; tx.pdest=0;
    require(accepts(tx,0x0123456789abcdefULL,0x0123456789abcdefULL),"FP physical zero treated as integer zero");

}

int main(int argc,char **argv)
{
    try {
        const std::string test=argc>1?argv[1]:"memory";
        if(test=="memory") memory_values();
        else if(test=="policy") policies();
        else if(test=="factory") factory();
        else if(test=="dependency") dependencies();
        else if(test=="scoreboard") scoreboards();
        else throw std::runtime_error("unknown test");
        return 0;
    } catch(const std::exception &error) { std::cerr<<error.what()<<'\n';return 1; }
}
