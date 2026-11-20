// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
// Licensed under Mulan PSL v2. See LICENSE for details.
// Compile against GEM5's source-banked sdbp_core.cc; pass "hash" or "sampler".
#include <cassert>
#include <array>
#include <iostream>
#include <random>
#include <string>
#include "mem/cache/replacement_policies/sdbp_core.hh"

void emitHash() {
    using gem5::replacement_policy::SDBPCore;
    SDBPCore::Config cfg;
    cfg.numSets = 512;
    cfg.samplerNum = 128;
    cfg.samplerAssoc = 6;
    SDBPCore core(cfg);
    auto emit = [&](uint64_t pc) {
        auto signature = core.signature(pc);
        std::cout << std::hex << pc << std::dec << ',' << signature;
        for (unsigned t = 0; t < 3; ++t)
            std::cout << ',' << core.index(signature, t);
        std::cout << '\n';
    };
    emit(0); emit(1); emit(0xffffffffffffffffULL); emit(0x80000000);
    std::mt19937_64 random(0x5db0);
    for (int i = 0; i < 128; ++i) emit(random());
    auto signature = core.signature(0x81234567);
    for (int i = 0; i < 4; ++i) core.train(signature, true);
    assert(core.confidence(signature) == 9);
    core.train(signature, false);
    assert(core.confidence(signature) == 5);
    core.train(signature, false);
    assert(core.confidence(signature) == 2);
    core.train(signature, false);
    assert(core.confidence(signature) == 0);
}

// Columns: set,tag,source,hasPC,newSig,newDead,sampled,hit,evicted,
// previousDead,previousSignatureValid,previousSignature,previousSource.
void emitSampler() {
    using gem5::replacement_policy::SDBPCore;
    SDBPCore::Config cfg;
    cfg.numSets = 512; cfg.samplerNum = 128; cfg.samplerAssoc = 6;
    cfg.requestTypes = 35; // GEM5: Demand plus two levels of 17 algorithm IDs.
    SDBPCore core(cfg);
    std::mt19937 random(0x5db0);
    constexpr std::array<unsigned, 13> rtlBanks = {
        0, 1, 2, 3, 11, 12, 18, 19, 20, 21, 22, 28, 34
    };
    for (unsigned i = 0; i < 600; ++i) {
        const unsigned set = i % 13 == 0 ? 1 : 0;
        const unsigned tag = random() % 15;
        const auto source = static_cast<SDBPCore::RequestType>(
            rtlBanks[i % rtlBanks.size()]);
        const bool hasPc = i % 7 != 0;
        const uint64_t pc = 0x80000000 + (random() % 4) * 4;
        const auto result = core.access(set, tag, source, hasPc, pc);
        std::cout << set << ',' << tag << ',' << unsigned(source) << ',' << hasPc << ',' <<
            core.signature(pc) << ',' << (hasPc && core.predict(core.signature(pc), source)) << ',' <<
            result.sampled << ',' << result.hit << ',' << result.evicted << ',' <<
            result.previousDead << ',' << result.previousSignatureValid << ',' << result.previousSignature << ',' <<
            unsigned(result.previousType) << '\n';
    }
}

int main(int argc, char **argv) {
    if (argc != 2) return 1;
    const std::string mode(argv[1]);
    if (mode == "hash") emitHash();
    else if (mode == "sampler") emitSampler();
    else return 1;
}
