// This file is included inside memblock_env.hpp's memblock namespace.

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
        outstanding_requests_ = 0;
        forced_next_response_delay_.reset();
        force_a_stall_ = random_backpressure_;
        pending_response_error_.reset();
    }

    void drive(UTMemBlock &dut)
    {
        const bool accept_a = !random_backpressure_ ||
                              (!force_a_stall_ && (next_random() & 3U) != 0);
        dut.auto_inner_ptw_to_l2_buffer_out_a_ready.ImmSet(accept_a);
        if (responses_.empty()) {
            dut.auto_inner_ptw_to_l2_buffer_out_d_valid.ImmSet(std::uint64_t{0});
            d_presenting_ = false;
            return;
        }
        if (!d_presenting_) {
            if (d_gap_ != 0) {
                --d_gap_;
                ++response_delay_cycles_;
                dut.auto_inner_ptw_to_l2_buffer_out_d_valid.ImmSet(
                    std::uint64_t{0});
                return;
            }
            d_presenting_ = true;
        }
        const Response &response = responses_.front();
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_opcode.ImmSet(response.opcode);
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_param.ImmSet(response.param);
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_size.ImmSet(response.size);
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_source.ImmSet(response.source);
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_sink.ImmSet(std::uint64_t{0});
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_denied.ImmSet(response.denied);
        auto data = response.data;
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_data.ImmSetBytes(data);
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_corrupt.ImmSet(response.corrupt);
        dut.auto_inner_ptw_to_l2_buffer_out_d_valid.ImmSet(std::uint64_t{1});
    }

    void capture_before_tick(UTMemBlock &dut)
    {
        const bool a_valid = dut.auto_inner_ptw_to_l2_buffer_out_a_valid.B();
        const bool a_ready = dut.auto_inner_ptw_to_l2_buffer_out_a_ready.B();
        if (a_valid && !a_ready) {
            ++request_stall_cycles_;
            force_a_stall_ = false;
        }
        a_fire_ = a_valid && a_ready;
        if (a_fire_) {
            request_ = Request{
                static_cast<std::uint8_t>(
                    dut.auto_inner_ptw_to_l2_buffer_out_a_bits_opcode.U()),
                static_cast<std::uint8_t>(
                    dut.auto_inner_ptw_to_l2_buffer_out_a_bits_param.U()),
                static_cast<std::uint8_t>(
                    dut.auto_inner_ptw_to_l2_buffer_out_a_bits_size.U()),
                static_cast<std::uint8_t>(
                    dut.auto_inner_ptw_to_l2_buffer_out_a_bits_source.U()),
                dut.auto_inner_ptw_to_l2_buffer_out_a_bits_address.U(),
            };
            if (request_->opcode != 4 && request_->opcode != 6) {
                error_ = "unsupported PTW TileLink A opcode";
            }
            if (request_->size > 6) {
                error_ = "oversized PTW TileLink A request";
            }
        }
        d_fire_ = !responses_.empty() &&
                  dut.auto_inner_ptw_to_l2_buffer_out_d_valid.B() &&
                  dut.auto_inner_ptw_to_l2_buffer_out_d_ready.B();
        if (d_fire_) {
            const Response &expected = responses_.front();
            const auto actual_data =
                dut.auto_inner_ptw_to_l2_buffer_out_d_bits_data.GetBytes();
            if (dut.auto_inner_ptw_to_l2_buffer_out_d_bits_opcode.U() != expected.opcode ||
                dut.auto_inner_ptw_to_l2_buffer_out_d_bits_param.U() != expected.param ||
                dut.auto_inner_ptw_to_l2_buffer_out_d_bits_size.U() != expected.size ||
                dut.auto_inner_ptw_to_l2_buffer_out_d_bits_source.U() != expected.source ||
                dut.auto_inner_ptw_to_l2_buffer_out_d_bits_sink.U() != 0 ||
                dut.auto_inner_ptw_to_l2_buffer_out_d_bits_denied.B() != expected.denied ||
                dut.auto_inner_ptw_to_l2_buffer_out_d_bits_corrupt.B() != expected.corrupt ||
                actual_data != expected.data) {
                error_ = "PTW TileLink D response identity or payload mismatch";
            }
        }
    }

    void update_after_tick()
    {
        if (d_fire_) {
            const bool completed_request = responses_.front().last_beat;
            responses_.pop_front();
            if (completed_request) {
                --outstanding_requests_;
            }
            d_presenting_ = false;
            d_gap_ = responses_.empty() ? 0 : responses_.front().delay_before;
        }
        if (a_fire_ && request_) {
            request_->response_delay = respond(*request_);
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

    bool ok() const { return error_.empty(); }
    const std::string &error() const { return error_; }
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
            error_ = message.str();
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
    unsigned d_gap_ = 0;
    bool random_backpressure_ = false;
    ResponseLatencyProfile latency_profile_ = ResponseLatencyProfile::compact;
    ResponseLatencyStats response_latency_stats_;
    std::optional<unsigned> forced_next_response_delay_;
    std::optional<PendingResponseError> pending_response_error_;
    bool force_a_stall_ = false;
    bool d_presenting_ = false;
    std::uint64_t outstanding_requests_ = 0;
    std::uint64_t max_outstanding_requests_ = 0;
    std::uint64_t request_stall_cycles_ = 0;
    std::uint64_t response_delay_cycles_ = 0;
    std::uint64_t error_response_requests_ = 0;
    std::uint64_t denied_d_beat_count_ = 0;
    std::uint64_t corrupt_d_beat_count_ = 0;
    std::uint64_t last_error_response_address_ = 0;
    std::uint8_t last_error_response_source_ = 0;
    std::string error_;
};
