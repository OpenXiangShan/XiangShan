#pragma once

#include <array>
#include <cstdint>
#include <stdexcept>

namespace memblock::scenarios::mixed {

// An architectural generation killed by a mixed-suffix redirect. Input
// acceptances and admitted vector uops remain observations after cancellation;
// retirement is a distinct lifetime. This never changes a data expectation.
struct StreamCanceledGeneration {
    enum class AddressState { unissued, translated, retry_ready, feedback_pending };
    unsigned kind = 0;
    unsigned vector_uops = 0, accepted_vector_uops = 0, address_retries = 0;
    unsigned vector_dependencies = 0, unconsumed_vector_dependencies = 0;
    bool scalar_issued = false, address_issued = false, data_issued = false;
    bool unflushed_vstart = false;
    bool unconsumed_address_dependency = false, unconsumed_data_dependency = false;
    AddressState address_state = AddressState::unissued;

    void sample(std::array<std::uint64_t, 29> &counts) const {
        const bool vector = kind == 1 || kind == 3;
        const bool address_accepted = scalar_issued || address_issued || accepted_vector_uops != 0;
        if (kind > 4 || accepted_vector_uops > vector_uops ||
            (vector && (vector_uops == 0 || vector_uops > 8 || vector_dependencies >= vector_uops)) ||
            (!vector && (vector_dependencies || unconsumed_vector_dependencies)) ||
            (kind == 3 && unconsumed_vector_dependencies) ||
            unconsumed_vector_dependencies > vector_dependencies ||
            (kind == 1 && (unconsumed_vector_dependencies > vector_uops - accepted_vector_uops ||
                vector_dependencies - unconsumed_vector_dependencies >
                    (accepted_vector_uops == 0 ? 0 : accepted_vector_uops - 1U))) ||
            (address_retries && !address_issued) ||
            (unconsumed_address_dependency && address_accepted) ||
            (unconsumed_data_dependency && data_issued) ||
            static_cast<unsigned>(address_state) > static_cast<unsigned>(AddressState::feedback_pending) ||
            (kind != 1 && kind != 3 && (vector_uops || accepted_vector_uops || unflushed_vstart)) ||
            (kind != 2 && (address_issued || data_issued || address_retries ||
                address_state != AddressState::unissued || unconsumed_data_dependency)) ||
            (kind == 2 && address_issued == (address_state == AddressState::unissued)))
            throw std::logic_error("invalid canceled stream generation");
        ++counts[1U + kind];
        if (kind == 0) counts[6] += scalar_issued;
        if (kind == 4) counts[11] += scalar_issued;
        if (kind == 1 || kind == 3) {
            const unsigned direction = kind == 3;
            counts[12U + direction] += vector_uops;
            counts[9U + direction] += accepted_vector_uops;
            counts[14U + direction] += unflushed_vstart;
            counts[26U + direction] += vector_dependencies;
            counts[28] += unconsumed_vector_dependencies;
        }
        if (kind == 2) {
            counts[7] += address_issued;
            counts[8] += data_issued;
            counts[18] += address_state == AddressState::translated;
            counts[19] += address_state == AddressState::retry_ready;
            counts[20] += address_state == AddressState::feedback_pending;
            counts[21] += address_retries;
        }
        counts[16] += unconsumed_address_dependency;
        counts[17] += unconsumed_data_dependency;
    }
};

} // namespace memblock::scenarios::mixed
