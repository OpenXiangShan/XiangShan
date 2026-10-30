// This file is included inside implementation.hpp's private memblock namespace.

class PtwMemoryAgent {
public:
    explicit PtwMemoryAgent(SparseMemory &memory) : memory_(memory) {}

    void inject_response_error_after(
        unsigned clean_requests, bool denied, bool corrupt,
        PtwCorruptBeat corrupt_beat = PtwCorruptBeat::all)
    {
        if (!denied && !corrupt) {
            throw std::invalid_argument(
                "PTW response error injection requires denied or corrupt");
        }
        if (!corrupt && corrupt_beat != PtwCorruptBeat::all) {
            throw std::invalid_argument(
                "PTW corrupt beat selection requires a corrupt injection");
        }
        pending_response_error_ = PendingResponseError{
            std::nullopt, clean_requests, denied, corrupt, corrupt_beat,
            false};
    }

    void inject_response_error_at(
        std::uint64_t address, bool denied, bool corrupt,
        PtwCorruptBeat corrupt_beat = PtwCorruptBeat::all)
    {
        if (!denied && !corrupt) {
            throw std::invalid_argument(
                "PTW response error injection requires denied or corrupt");
        }
        if ((!corrupt || denied) && corrupt_beat != PtwCorruptBeat::all) {
            throw std::invalid_argument(
                "PTW per-beat selection requires independent corrupt");
        }
        pending_response_error_ = PendingResponseError{
            address, 0, denied, corrupt, corrupt_beat, false};
    }

    void inject_persistent_response_error_at(
        std::uint64_t address, bool denied, bool corrupt,
        PtwCorruptBeat corrupt_beat = PtwCorruptBeat::all)
    {
        if (!denied && !corrupt) {
            throw std::invalid_argument(
                "persistent PTW response error requires denied or corrupt");
        }
        if ((!corrupt || denied) && corrupt_beat != PtwCorruptBeat::all) {
            throw std::invalid_argument(
                "persistent PTW per-beat selection requires independent corrupt");
        }
        pending_response_error_ = PendingResponseError{
            address, 0, denied, corrupt, corrupt_beat, true};
    }

    void clear_response_error() { pending_response_error_.reset(); }

    void configure_backpressure(
        std::uint64_t seed, bool enabled,
        ResponseLatencyProfile latency_profile = ResponseLatencyProfile::compact)
    {
        random_state_ = seed == 0 ? 1 : seed;
        random_backpressure_ = enabled;
        latency_profile_ = latency_profile;
        response_latency_stats_ = {};
        outstanding_requests_ = 0;
        max_outstanding_requests_ = 0;
        forced_next_response_delay_.reset();
        force_a_stall_ = enabled;
    }

    // Reorder complete messages only. Dedicated RNG state leaves legacy
    // backpressure/latency draws unchanged when this direction is disabled.
    void configure_response_order(std::uint64_t seed, unsigned per_mille)
    {
        if (per_mille > 1000 || !idle())
            throw std::invalid_argument("PTW response-order configuration requires idle link and rate <=1000");
        response_order_per_mille_ = per_mille;
        order_random_state_ = seed == 0 ? 1 : seed;
        response_order_stats_ = {};
    }

    const std::array<std::uint64_t, 8> &response_order_stats() const
    { return response_order_stats_; }

    void force_next_response_delay(unsigned cycles)
    {
        forced_next_response_delay_ = cycles;
    }

    void reset_link_state()
    {
        responses_.clear();
        request_.reset();
        a_fire_ = false;
        d_fire_ = false;
        d_gap_ = 0;
        d_presenting_ = false;
        response_message_active_ = false;
        active_sources_.fill(false);
        active_orders_.fill(std::nullopt);
        outstanding_requests_ = 0;
        forced_next_response_delay_.reset();
        force_a_stall_ = random_backpressure_;
        pending_response_error_.reset();
    }

    void drive(DutAdapter &dut)
    {
        const bool accept_a = !random_backpressure_ ||
                              (!force_a_stall_ && (next_random() & 3U) != 0);
        dut.ptw_memory.a.ready.set(accept_a);
        if (responses_.empty()) {
            dut.ptw_memory.d.valid.set(std::uint64_t{0});
            d_presenting_ = false;
            return;
        }
        if (!response_message_active_) {
            select_response_message();
            response_message_active_ = true;
        }
        if (!d_presenting_) {
            if (d_gap_ != 0) {
                --d_gap_;
                ++response_delay_cycles_;
                dut.ptw_memory.d.valid.set(
                    std::uint64_t{0});
                return;
            }
            d_presenting_ = true;
        }
        const Response &response = responses_.front();
        dut.ptw_memory.d.opcode.set(response.opcode);
        dut.ptw_memory.d.param.set(response.param);
        dut.ptw_memory.d.size.set(response.size);
        dut.ptw_memory.d.source.set(response.source);
        dut.ptw_memory.d.sink.set(std::uint64_t{0});
        dut.ptw_memory.d.denied.set(response.denied);
        auto data = response.data;
        dut.ptw_memory.d.data.set_bytes(data);
        dut.ptw_memory.d.corrupt.set(response.corrupt);
        dut.ptw_memory.d.valid.set(std::uint64_t{1});
    }

    void capture_before_tick(DutAdapter &dut)
    {
        d_fire_ = !responses_.empty() &&
                  dut.ptw_memory.d.valid.boolean() &&
                  dut.ptw_memory.d.ready.boolean();
        const std::optional<std::uint8_t> completing_source =
            d_fire_ && responses_.front().last_beat
            ? std::optional<std::uint8_t>{responses_.front().source}
            : std::nullopt;
        const bool a_valid = dut.ptw_memory.a.valid.boolean();
        const bool a_ready = dut.ptw_memory.a.ready.boolean();
        if (a_valid && !a_ready) {
            ++request_stall_cycles_;
            force_a_stall_ = false;
        }
        a_fire_ = a_valid && a_ready;
        if (a_fire_) {
            request_ = Request{
                static_cast<std::uint8_t>(
                    dut.ptw_memory.a.opcode.value()),
                static_cast<std::uint8_t>(
                    dut.ptw_memory.a.param.value()),
                static_cast<std::uint8_t>(
                    dut.ptw_memory.a.size.value()),
                static_cast<std::uint8_t>(
                    dut.ptw_memory.a.source.value()),
                dut.ptw_memory.a.address.value(),
            };
            if (request_->opcode != 4 && request_->opcode != 6) {
                error_.record("unsupported PTW TileLink A opcode");
            }
            if (request_->size > 6) {
                error_.record("oversized PTW TileLink A request");
            }
            if (request_->source >= active_sources_.size() ||
                (active_sources_[request_->source] &&
                 (!completing_source ||
                  *completing_source != request_->source))) {
                error_.record(
                    "PTW TileLink A reused an active source before D");
            }
        }
        if (d_fire_) {
            const Response &expected = responses_.front();
            const auto actual_data =
                dut.ptw_memory.d.data.bytes();
            if (dut.ptw_memory.d.opcode.value() != expected.opcode ||
                dut.ptw_memory.d.param.value() != expected.param ||
                dut.ptw_memory.d.size.value() != expected.size ||
                dut.ptw_memory.d.source.value() != expected.source ||
                dut.ptw_memory.d.sink.value() != 0 ||
                dut.ptw_memory.d.denied.boolean() != expected.denied ||
                dut.ptw_memory.d.corrupt.boolean() != expected.corrupt ||
                actual_data != expected.data) {
                error_.record("PTW TileLink D response identity or payload mismatch");
            }
        }
    }

    void update_after_tick()
    {
        if (d_fire_) {
            const bool completed_request = responses_.front().last_beat;
            const std::uint8_t source = responses_.front().source;
            if (completed_request) {
                if (source < active_orders_.size() && active_orders_[source]) {
                    const auto older = std::count_if(active_orders_.begin(), active_orders_.end(),
                        [&](const auto &order) { return order && *order < *active_orders_[source]; });
                    response_order_stats_[6] += older != 0;
                    response_order_stats_[7] = std::max<std::uint64_t>(response_order_stats_[7], older);
                    active_orders_[source].reset();
                }
                if (source >= active_sources_.size() ||
                    !active_sources_[source]) {
                    error_.record(
                        "PTW TileLink D completed an inactive source");
                } else {
                    active_sources_[source] = false;
                }
            }
            responses_.pop_front();
            if (completed_request) {
                --outstanding_requests_;
                ++response_order_stats_[4];
                response_message_active_ = false;
            }
            d_presenting_ = false;
            d_gap_ = responses_.empty() ? 0 : responses_.front().delay_before;
        }
        if (a_fire_ && request_) {
            request_->response_delay = respond(*request_);
            if (request_->source >= active_sources_.size()) {
                error_.record("PTW TileLink A source is out of range");
            } else {
                active_sources_[request_->source] = true;
                active_orders_[request_->source] = request_count_;
            }
            request_history_.push_back(*request_);
            ++request_count_;
            ++outstanding_requests_;
            max_outstanding_requests_ = std::max(
                max_outstanding_requests_, outstanding_requests_);
        }
        a_fire_ = false;
        d_fire_ = false;
        request_.reset();
    }

    const FailureState &failures() const { return error_; }
    void set_failure_cycle(std::uint64_t cycle) { error_.set_cycle(cycle); }

    bool ok() const { return error_.empty(); }
    const std::string &error() const { return error_.message(); }
    std::uint64_t request_count() const { return request_count_; }
    std::uint64_t request_stall_cycles() const { return request_stall_cycles_; }
    std::uint64_t response_delay_cycles() const { return response_delay_cycles_; }
    std::uint64_t error_response_requests() const
    {
        return error_response_requests_;
    }
    std::uint64_t denied_d_beats() const { return denied_d_beat_count_; }
    std::uint64_t corrupt_d_beats() const { return corrupt_d_beat_count_; }
    std::uint64_t last_error_response_address() const
    {
        return last_error_response_address_;
    }
    std::uint8_t last_error_response_source() const
    {
        return last_error_response_source_;
    }
    std::uint64_t max_outstanding_requests() const
    {
        return max_outstanding_requests_;
    }
    std::uint64_t outstanding_requests() const
    {
        return outstanding_requests_;
    }
    bool idle() const
    {
        return outstanding_requests_ == 0 && responses_.empty() &&
            !request_.has_value();
    }
    const ResponseLatencyStats &response_latency_stats() const
    {
        return response_latency_stats_;
    }
    bool request_covers_address_since(
        std::uint64_t address, std::uint64_t first_request) const
    {
        if (first_request >= request_history_.size()) {
            return false;
        }
        for (std::size_t index = static_cast<std::size_t>(first_request);
             index < request_history_.size(); ++index) {
            const Request &request = request_history_[index];
            const std::uint64_t bytes = std::uint64_t{1} << request.size;
            const std::uint64_t base = request.address & ~(bytes - 1);
            if (address >= base && address - base < bytes) {
                return true;
            }
        }
        return false;
    }

    std::uint64_t request_covering_count_since(
        std::uint64_t address, std::uint64_t first_request) const
    {
        std::uint64_t count = 0;
        for (std::size_t index = static_cast<std::size_t>(first_request);
             index < request_history_.size(); ++index) {
            const Request &request = request_history_[index];
            const std::uint64_t bytes = std::uint64_t{1} << request.size;
            const std::uint64_t base = request.address & ~(bytes - 1);
            count += address >= base && address - base < bytes;
        }
        return count;
    }

    bool request_covering_address_has_min_delay_since(
        std::uint64_t address,
        std::uint64_t first_request,
        unsigned min_response_delay) const
    {
        if (first_request >= request_history_.size()) {
            return false;
        }
        for (std::size_t index = static_cast<std::size_t>(first_request);
             index < request_history_.size(); ++index) {
            const Request &request = request_history_[index];
            const std::uint64_t bytes = std::uint64_t{1} << request.size;
            const std::uint64_t base = request.address & ~(bytes - 1);
            if (address >= base && address - base < bytes) {
                return request.response_delay >= min_response_delay;
            }
        }
        return false;
    }

private:
    static constexpr std::size_t kBeatBytes = 32;

    struct Request {
        std::uint8_t opcode;
        std::uint8_t param;
        std::uint8_t size;
        std::uint8_t source;
        std::uint64_t address;
        unsigned response_delay = 0;
    };

    struct Response {
        std::uint8_t opcode;
        std::uint8_t param;
        std::uint8_t size;
        std::uint8_t source;
        std::vector<unsigned char> data;
        bool denied = false;
        bool corrupt = false;
        bool last_beat = true;
        unsigned delay_before = 0;
        bool first_beat = false;
    };

    struct PendingResponseError {
        std::optional<std::uint64_t> address;
        unsigned clean_requests;
        bool denied;
        bool corrupt;
        PtwCorruptBeat corrupt_beat;
        bool persistent;
    };

    unsigned respond(const Request &request)
    {
        if (request.opcode != 4 && request.opcode != 6) {
            std::ostringstream message;
            message << "unsupported PTW TileLink A opcode "
                    << static_cast<unsigned>(request.opcode);
            error_.record(message.str());
            return 0;
        }
        const std::uint64_t transfer_bytes = std::uint64_t{1} << request.size;
        const std::uint64_t base = request.address & ~(transfer_bytes - 1);
        const std::size_t beats = static_cast<std::size_t>(
            transfer_bytes > kBeatBytes ? transfer_bytes / kBeatBytes : 1);
        bool denied = false;
        bool corrupt = false;
        PtwCorruptBeat corrupt_beat = PtwCorruptBeat::all;
        if (pending_response_error_) {
            const bool address_matches = !pending_response_error_->address ||
                ((*pending_response_error_->address &
                  ~(transfer_bytes - 1)) == base);
            if (address_matches &&
                pending_response_error_->clean_requests == 0) {
                denied = pending_response_error_->denied;
                corrupt = pending_response_error_->corrupt;
                corrupt_beat = pending_response_error_->corrupt_beat;
                if (!pending_response_error_->persistent) {
                    pending_response_error_.reset();
                }
                ++error_response_requests_;
                last_error_response_address_ = request.address;
                last_error_response_source_ = request.source;
            } else if (!pending_response_error_->address) {
                --pending_response_error_->clean_requests;
            }
        }
        unsigned first_response_delay = 0;
        for (std::size_t beat = 0; beat < beats; ++beat) {
            const bool selected_corrupt_beat =
                corrupt_beat == PtwCorruptBeat::all ||
                (corrupt_beat == PtwCorruptBeat::first && beat == 0) ||
                (corrupt_beat == PtwCorruptBeat::last && beat + 1 == beats);
            denied_d_beat_count_ += denied;
            corrupt_d_beat_count_ += denied ||
                (corrupt && selected_corrupt_beat);
            const unsigned delay = push_response(Response{
                static_cast<std::uint8_t>(request.opcode == 4 ? 1 : 5),
                static_cast<std::uint8_t>(request.opcode == 4 ? 0 : 1),
                request.size,
                request.source,
                memory_.read_beat(base + beat * kBeatBytes, kBeatBytes),
                denied,
                // denied is fixed across a multibeat D response and implies
                // corrupt on every beat carrying data.  Independent corrupt
                // errors may legally identify only the affected data beat.
                denied || (corrupt && selected_corrupt_beat),
                beat + 1 == beats,
            }, beat == 0);
            if (beat == 0) {
                first_response_delay = delay;
            }
        }
        return first_response_delay;
    }

    std::uint64_t next_random()
    {
        random_state_ ^= random_state_ << 13;
        random_state_ ^= random_state_ >> 7;
        random_state_ ^= random_state_ << 17;
        return random_state_;
    }

    void select_response_message()
    {
        // The active multibeat message is never interleaved, including D
        // backpressure and interbeat gaps. Select only before its first beat.
        std::vector<std::size_t> starts;
        for (std::size_t i = 0; i < responses_.size(); ++i)
            if (responses_[i].first_beat) starts.push_back(i);
        if (starts.empty() || starts.front() != 0) {
            error_.record("PTW response queue lost a whole-message boundary");
            return;
        }
        ++response_order_stats_[0];
        response_order_stats_[5] = std::max<std::uint64_t>(response_order_stats_[5], starts.size());
        if (starts.size() < 2) return;
        ++response_order_stats_[1];
        if (response_order_per_mille_ == 0) return;
        const auto draw = [&] {
            order_random_state_ ^= order_random_state_ << 13;
            order_random_state_ ^= order_random_state_ >> 7;
            order_random_state_ ^= order_random_state_ << 17;
            return order_random_state_;
        };
        if (response_order_per_mille_ != 1000 && draw() % 1000U >= response_order_per_mille_) return;
        ++response_order_stats_[2];
        // Selecting from every pending message includes FIFO controls and
        // permutations; no random choice may split or reorder message beats.
        const auto selected = draw() % starts.size();
        if (selected == 0) return;
        ++response_order_stats_[3];
        const auto begin = responses_.begin() + starts[selected];
        const auto end = selected + 1U < starts.size() ? responses_.begin() + starts[selected + 1U] : responses_.end();
        std::rotate(responses_.begin(), begin, end);
        d_gap_ = responses_.front().delay_before;
    }

    unsigned response_delay(bool first_beat)
    {
        if (first_beat && forced_next_response_delay_) {
            const unsigned delay = *forced_next_response_delay_;
            forced_next_response_delay_.reset();
            response_latency_stats_.sample(delay);
            return delay;
        }
        if (!random_backpressure_) {
            return 0;
        }
        if (!first_beat) {
            return static_cast<unsigned>(next_random() % 4);
        }
        const unsigned delay = sample_response_latency(
            latency_profile_, next_random(), response_latency_stats_.samples);
        response_latency_stats_.sample(delay);
        return delay;
    }

    unsigned push_response(Response response, bool first_beat)
    {
        const bool was_empty = responses_.empty();
        response.first_beat = first_beat;
        response.delay_before = response_delay(first_beat);
        const unsigned delay = response.delay_before;
        responses_.push_back(std::move(response));
        if (was_empty) {
            d_presenting_ = false;
            d_gap_ = responses_.front().delay_before;
        }
        return delay;
    }

    SparseMemory &memory_;
    std::deque<Response> responses_;
    std::vector<Request> request_history_;
    std::optional<Request> request_;
    bool a_fire_ = false;
    bool d_fire_ = false;
    std::uint64_t request_count_ = 0;
    std::uint64_t random_state_ = 1;
    std::uint64_t order_random_state_ = 1;
    unsigned response_order_per_mille_ = 0;
    bool response_message_active_ = false;
    // Message starts/multi-choice starts/random selections/nonfront selections/
    // final-D completions/maximum selectable messages/completions overtaking
    // at least one accepted older request/maximum pending-older overtaken. Counts are manager IO
    // scheduling observations, not DUT translation-correctness evidence.
    std::array<std::uint64_t, 8> response_order_stats_{};
    unsigned d_gap_ = 0;
    bool random_backpressure_ = false;
    ResponseLatencyProfile latency_profile_ = ResponseLatencyProfile::compact;
    ResponseLatencyStats response_latency_stats_;
    std::optional<unsigned> forced_next_response_delay_;
    std::optional<PendingResponseError> pending_response_error_;
    bool force_a_stall_ = false;
    bool d_presenting_ = false;
    std::array<bool, 16> active_sources_{};
    std::array<std::optional<std::uint64_t>, 16> active_orders_{};
    std::uint64_t outstanding_requests_ = 0;
    std::uint64_t max_outstanding_requests_ = 0;
    std::uint64_t request_stall_cycles_ = 0;
    std::uint64_t response_delay_cycles_ = 0;
    std::uint64_t error_response_requests_ = 0;
    std::uint64_t denied_d_beat_count_ = 0;
    std::uint64_t corrupt_d_beat_count_ = 0;
    std::uint64_t last_error_response_address_ = 0;
    std::uint8_t last_error_response_source_ = 0;
    FailureState error_{"ptw", "manager-contract"};
};
