#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>

namespace memblock::scenarios::mixed {
// Backend progress for one accepted STA at a time. Feedback is matched only
// after the attempt's sample cursor, by lane and full circular SQ identity.
// This schedules retries; address/data values remain independently checked by
// the store scoreboard and the admission-order byte oracle.
class StoreAddressReplay {
public:
    enum class Observation { none, hit, miss, invalid };

    bool issued() const { return issued_; }
    bool translated() const { return translated_; }
    bool waiting() const { return waiting_; }
    bool ready(std::uint64_t cycle) const {
        return !waiting_ && !translated_ && cycle >= ready_cycle_;
    }
    bool accept(std::size_t first_sample, std::uint64_t cycle) {
        if (!ready(cycle)) return false;
        issued_ = waiting_ = true;
        cursor_ = first_sample;
        accepted_cycle_ = cycle;
        return true;
    }
    bool timed_out(std::uint64_t cycle, unsigned timeout) const {
        return waiting_ && cycle >= accepted_cycle_ && cycle - accepted_cycle_ >= timeout;
    }
    template<class Samples, class Transaction>
    Observation observe(const Samples &samples, const Transaction &transaction) {
        if (!issued_) return Observation::none;
        if (cursor_ > samples.size()) return Observation::invalid;
        Observation result = Observation::none;
        for (; cursor_ < samples.size(); ++cursor_) {
            const auto &sample = samples[cursor_];
            if (!matches(sample, transaction)) continue;
            if (!waiting_ || sample.cycle < accepted_cycle_) return Observation::invalid;
            waiting_ = false;
            translated_ = sample.hit;
            ready_cycle_ = sample.cycle + 8U;
            result = sample.hit ? Observation::hit : Observation::miss;
        }
        return result;
    }
    // Count another accepted event only if its edge precedes this attempt's
    // feedback observation. A blocking helper may have advanced past feedback.
    template<class Samples, class Transaction>
    bool waiting_at(std::uint64_t edge, const Samples &samples,
                    const Transaction &transaction) const {
        if (!waiting_ || edge <= accepted_cycle_) return false;
        for (std::size_t index = cursor_; index < samples.size(); ++index)
            if (matches(samples[index], transaction) && samples[index].cycle <= edge) return false;
        return true;
    }
private:
    template<class Sample, class Transaction>
    static bool matches(const Sample &sample, const Transaction &transaction) {
        return sample.lane == transaction.address_lane &&
            sample.sq_flag == transaction.sq_flag && sample.sq_value == transaction.sq;
    }
    bool issued_ = false, waiting_ = false, translated_ = false;
    std::size_t cursor_ = 0;
    std::uint64_t accepted_cycle_ = 0, ready_cycle_ = 0;
};
} // namespace memblock::scenarios::mixed
