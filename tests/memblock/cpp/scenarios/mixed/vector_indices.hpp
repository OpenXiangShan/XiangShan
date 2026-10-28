#pragma once

#include <algorithm>
#include <array>
#include <cstdint>
#include <numeric>
#include <random>
#include <stdexcept>
#include <vector>

namespace memblock::scenarios::mixed {

struct RandomVectorIndices {
    std::vector<std::uint64_t> offsets;
    std::array<unsigned char, 128> bytes{};
};

// Generate one architectural index operand, then slice it for the issued uops.
// A store's complete field groups never overlap, so its post-state is exact
// even when the ISA permits arbitrary element/field execution order.
inline RandomVectorIndices make_random_vector_indices(
    std::mt19937_64 &random, unsigned elements, unsigned index_bytes,
    unsigned data_bytes, unsigned fields, bool store, unsigned repeat_per_mille)
{
    if (elements == 0 || elements * index_bytes > 128 ||
        (index_bytes != 1 && index_bytes != 2 && index_bytes != 4 && index_bytes != 8) ||
        (data_bytes != 1 && data_bytes != 2 && data_bytes != 4 && data_bytes != 8) ||
        fields == 0 || fields > 8 || repeat_per_mille > 1000) {
        throw std::invalid_argument("invalid random vector index shape");
    }
    const unsigned group_bytes = data_bytes * fields;
    const unsigned limit = index_bytes == 1 ? 256U : 2048U;
    const std::array<unsigned, 4> spans{{64, 256, 1024, 2048}};
    std::vector<unsigned> eligible_spans;
    for (auto span : spans) {
        if (span <= limit && span / group_bytes >= elements) {
            eligible_spans.push_back(span);
        }
    }
    if (eligible_spans.empty()) {
        throw std::invalid_argument("random vector index arena is too small");
    }
    const unsigned span = eligible_spans[random() % eligible_spans.size()];
    std::vector<unsigned> slots(span / group_bytes);
    std::iota(slots.begin(), slots.end(), 0U);
    std::shuffle(slots.begin(), slots.end(), random);
    RandomVectorIndices result;
    result.offsets.reserve(elements);
    for (unsigned element = 0; element < elements; ++element) {
        const std::uint64_t offset = !store && element != 0 &&
                random() % 1000 < repeat_per_mille
            ? result.offsets[random() % element]
            : static_cast<std::uint64_t>(slots[element]) * group_bytes;
        result.offsets.push_back(offset);
        for (unsigned byte = 0; byte < index_bytes; ++byte) {
            result.bytes[element * index_bytes + byte] =
                static_cast<unsigned char>(offset >> (8U * byte));
        }
    }
    return result;
}

} // namespace memblock::scenarios::mixed
