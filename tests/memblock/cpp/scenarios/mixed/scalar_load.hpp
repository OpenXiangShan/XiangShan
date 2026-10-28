#pragma once
#include <array>
#include <cstdint>
#include <numeric>
#include <stdexcept>

namespace memblock::scenarios::mixed {
struct ScalarLoadShape {
    unsigned op;
    bool fp;
};

struct ScalarLoadPolicy {
    unsigned fp_per_mille = 0;
    std::array<unsigned, 3> fp_width_weights{1, 1, 1}; // FLH, FLW, FLD

    void validate() const
    {
        if (fp_per_mille > 1000 || (fp_per_mille != 0 &&
            std::accumulate(fp_width_weights.begin(), fp_width_weights.end(),
                std::uint64_t{0}) == 0)) {
            throw std::invalid_argument("invalid scalar FP rate or width weights");
        }
    }

    template<class Random>
    ScalarLoadShape choose(Random &random) const
    {
        validate();
        if (fp_per_mille == 0 || random() % 1000U >= fp_per_mille)
            return {static_cast<unsigned>(random() % 7U), false};
        auto choice = random() % std::accumulate(fp_width_weights.begin(),
            fp_width_weights.end(), std::uint64_t{0});
        for (unsigned width = 0; width < fp_width_weights.size(); ++width) {
            if (choice < fp_width_weights[width]) return {width + 1U, true};
            choice -= fp_width_weights[width];
        }
        throw std::logic_error("scalar FP selection lost its weight");
    }
};
} // namespace memblock::scenarios::mixed
