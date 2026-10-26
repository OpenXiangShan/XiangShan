// This file is included inside implementation.hpp's private memblock namespace.

class UncacheMemoryAgent {
public:
    using DeviceAccess = UncacheDeviceAccess;

    explicit UncacheMemoryAgent(SparseMemory &memory) : memory_(memory) {}

    void configure_device_window(
        std::uint64_t base, std::uint64_t size, bool read_clear)
    {
        if (size == 0) {
            throw std::invalid_argument("uncache device window cannot be empty");
        }
        device_window_ = DeviceWindow{base, size, read_clear};
        device_accesses_.clear();
    }

    const std::vector<DeviceAccess> &device_accesses() const
    {
        return device_accesses_;
    }

    void inject_next_response_error(bool denied, bool corrupt)
    {
        inject_denied_ = denied;
        inject_corrupt_ = corrupt;
    }

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
        active_sources_.fill(false);
        outstanding_requests_ = 0;
        forced_next_response_delay_.reset();
        force_a_stall_ = random_backpressure_;
        inject_denied_ = false;
        inject_corrupt_ = false;
    }

    void drive(DutAdapter &dut)
    {
        const bool accept_a = !random_backpressure_ ||
                              (!force_a_stall_ && (next_random() & 3U) != 0);
        dut.uncache.a.ready.set(accept_a);
        if (responses_.empty()) {
            dut.uncache.d.valid.set(std::uint64_t{0});
            d_presenting_ = false;
            return;
        }
        if (!d_presenting_) {
            if (d_gap_ != 0) {
                --d_gap_;
                ++response_delay_cycles_;
                dut.uncache.d.valid.set(std::uint64_t{0});
                return;
            }
            d_presenting_ = true;
        }
        const Response &response = responses_.front();
        dut.uncache.d.opcode.set(response.opcode);
        dut.uncache.d.param.set(std::uint64_t{0});
        dut.uncache.d.size.set(response.size);
        dut.uncache.d.source.set(response.source);
        dut.uncache.d.sink.set(std::uint64_t{0});
        dut.uncache.d.denied.set(response.denied);
        dut.uncache.d.data.set(response.data);
        dut.uncache.d.corrupt.set(response.corrupt);
        dut.uncache.d.valid.set(std::uint64_t{1});
    }

    void capture_before_tick(DutAdapter &dut)
    {
        d_fire_ = !responses_.empty() &&
                  dut.uncache.d.valid.boolean() &&
                  dut.uncache.d.ready.boolean();
        const std::optional<std::uint8_t> completing_source =
            d_fire_ ? std::optional<std::uint8_t>{responses_.front().source}
                    : std::nullopt;
        const bool a_valid = dut.uncache.a.valid.boolean();
        const bool a_ready = dut.uncache.a.ready.boolean();
        if (a_valid && !a_ready) {
            ++request_stall_cycles_;
            force_a_stall_ = false;
        }
        a_fire_ = a_valid && a_ready;
        if (a_fire_) {
            request_ = Request{
                static_cast<std::uint8_t>(dut.uncache.a.opcode.value()),
                static_cast<std::uint8_t>(dut.uncache.a.size.value()),
                static_cast<std::uint8_t>(dut.uncache.a.source.value()),
                dut.uncache.a.address.value(),
                static_cast<std::uint8_t>(dut.uncache.a.mask.value()),
                dut.uncache.a.data.value(),
            };
            if (request_->opcode != 0 && request_->opcode != 1 &&
                request_->opcode != 4) {
                error_.record("unsupported uncache TileLink A opcode");
            }
            if (request_->source >= active_sources_.size() ||
                (active_sources_[request_->source] &&
                 (!completing_source ||
                  *completing_source != request_->source))) {
                error_.record(
                    "uncache TileLink A reused an active source before D");
            }
            if (request_->size > 6) {
                error_.record("oversized uncache TileLink A request");
            }
            if (request_->size > 3) {
                error_.record("uncache TileLink A request exceeds the 8-byte beat");
            } else {
                const unsigned transfer_bytes = 1U << request_->size;
                const unsigned beat_offset =
                    static_cast<unsigned>(request_->address & 7U);
                const std::uint8_t expected_mask = static_cast<std::uint8_t>(
                    ((1U << transfer_bytes) - 1U) << beat_offset);
                if ((request_->address & (transfer_bytes - 1U)) != 0 ||
                    beat_offset + transfer_bytes > 8 ||
                    request_->mask != expected_mask) {
                    error_.record("uncache TileLink A size/address/mask mismatch");
                }
            }
        }
        if (d_fire_) {
            const Response &expected = responses_.front();
            denied_d_beat_count_ += expected.denied;
            corrupt_d_beat_count_ += expected.corrupt;
            if (dut.uncache.d.opcode.value() != expected.opcode ||
                dut.uncache.d.size.value() != expected.size ||
                dut.uncache.d.source.value() != expected.source ||
                dut.uncache.d.sink.value() != 0 ||
                dut.uncache.d.denied.boolean() != expected.denied ||
                dut.uncache.d.corrupt.boolean() != expected.corrupt ||
                dut.uncache.d.data.value() != expected.data) {
                error_.record("uncache TileLink D response identity or payload mismatch");
            }
        }
    }

    void update_after_tick()
    {
        if (d_fire_) {
            const std::uint8_t source = responses_.front().source;
            if (source >= active_sources_.size() || !active_sources_[source]) {
                error_.record(
                    "uncache TileLink D completed an inactive source");
            } else {
                active_sources_[source] = false;
            }
            responses_.pop_front();
            --outstanding_requests_;
            d_presenting_ = false;
            d_gap_ = responses_.empty() ? 0 : responses_.front().delay_before;
        }
        if (a_fire_ && request_) {
            respond(*request_);
            if (request_->source >= active_sources_.size()) {
                error_.record("uncache TileLink A source is out of range");
            } else {
                active_sources_[request_->source] = true;
            }
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
    std::uint64_t max_outstanding_requests() const
    {
        return max_outstanding_requests_;
    }
    std::uint64_t outstanding_requests() const
    {
        return outstanding_requests_;
    }
    std::uint64_t error_response_requests() const
    {
        return error_response_requests_;
    }
    std::uint64_t denied_d_beat_count() const
    {
        return denied_d_beat_count_;
    }
    std::uint64_t corrupt_d_beat_count() const
    {
        return corrupt_d_beat_count_;
    }
    const ResponseLatencyStats &response_latency_stats() const
    {
        return response_latency_stats_;
    }

private:
    struct Request {
        std::uint8_t opcode;
        std::uint8_t size;
        std::uint8_t source;
        std::uint64_t address;
        std::uint8_t mask;
        std::uint64_t data;
    };

    struct Response {
        std::uint8_t opcode;
        std::uint8_t size;
        std::uint8_t source;
        std::uint64_t data;
        bool denied = false;
        bool corrupt = false;
        unsigned delay_before = 0;
    };

    struct DeviceWindow {
        std::uint64_t base;
        std::uint64_t size;
        bool read_clear;
    };

    bool is_device_request(const Request &request) const
    {
        if (!device_window_ || request.size > 3 ||
            request.address < device_window_->base) {
            return false;
        }
        const std::uint64_t transfer_bytes = std::uint64_t{1} << request.size;
        const std::uint64_t offset = request.address - device_window_->base;
        return transfer_bytes <= device_window_->size &&
               offset <= device_window_->size - transfer_bytes;
    }

    void record_device_access(
        const Request &request, std::uint64_t read_data,
        bool denied, bool corrupt)
    {
        device_accesses_.push_back(DeviceAccess{
            device_accesses_.size(), request.opcode != 4, request.size,
            request.source, request.address, request.mask, request.data,
            read_data, denied, corrupt,
        });
    }

    void respond(const Request &request)
    {
        const bool denied = inject_denied_;
        const bool corrupt = inject_corrupt_;
        inject_denied_ = false;
        inject_corrupt_ = false;
        error_response_requests_ += denied || corrupt;
        if (request.opcode == 4) {
            const bool response_corrupt = corrupt || denied;
            const std::uint64_t beat_base = request.address & ~std::uint64_t{7};
            const std::uint64_t read_data = memory_.read_u64(beat_base);
            if (is_device_request(request)) {
                record_device_access(
                    request, read_data, denied, response_corrupt);
                if (device_window_->read_clear &&
                    !denied && !response_corrupt) {
                    for (unsigned byte = 0; byte < 8; ++byte) {
                        if (((request.mask >> byte) & 1U) != 0) {
                            memory_.write_byte(beat_base + byte, 0);
                        }
                    }
                }
            }
            push_response(Response{
                1, request.size, request.source,
                // TileLink returns the complete 8-byte beat. LoadUnit selects
                // the requested byte lane later using the physical address.
                read_data,
                denied, response_corrupt,
            }, true);
            return;
        }
        if (request.opcode != 0 && request.opcode != 1) {
            std::ostringstream message;
            message << "unsupported uncache TileLink A opcode "
                    << static_cast<unsigned>(request.opcode);
            error_.record(message.str());
            return;
        }
        if (corrupt) {
            error_.record("cannot inject corrupt on a data-less Uncache AccessAck");
            return;
        }
        const std::uint64_t beat_base = request.address & ~std::uint64_t{7};
        const bool device_request = is_device_request(request);
        if (device_request) {
            record_device_access(request, 0, denied, false);
        }
        if (!denied) {
            for (unsigned byte = 0; byte < 8; ++byte) {
                if (((request.mask >> byte) & 1U) != 0) {
                    memory_.write_byte(
                        beat_base + byte,
                        static_cast<std::uint8_t>(request.data >> (8 * byte)));
                }
            }
        }
        push_response(
            Response{0, request.size, request.source, 0, denied, false}, true);
    }

    std::uint64_t next_random()
    {
        random_state_ ^= random_state_ << 13;
        random_state_ ^= random_state_ >> 7;
        random_state_ ^= random_state_ << 17;
        return random_state_;
    }

    unsigned response_delay()
    {
        if (forced_next_response_delay_) {
            const unsigned delay = *forced_next_response_delay_;
            forced_next_response_delay_.reset();
            response_latency_stats_.sample(delay);
            return delay;
        }
        if (!random_backpressure_) {
            return 0;
        }
        const unsigned delay = sample_response_latency(
            latency_profile_, next_random(), response_latency_stats_.samples);
        response_latency_stats_.sample(delay);
        return delay;
    }

    void push_response(Response response, bool first_beat)
    {
        const bool was_empty = responses_.empty();
        response.delay_before = first_beat ? response_delay() : 0;
        responses_.push_back(std::move(response));
        if (was_empty) {
            d_presenting_ = false;
            d_gap_ = responses_.front().delay_before;
        }
    }

    SparseMemory &memory_;
    std::deque<Response> responses_;
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
    bool force_a_stall_ = false;
    bool d_presenting_ = false;
    std::array<bool, 16> active_sources_{};
    std::uint64_t outstanding_requests_ = 0;
    std::uint64_t max_outstanding_requests_ = 0;
    std::uint64_t error_response_requests_ = 0;
    std::uint64_t denied_d_beat_count_ = 0;
    std::uint64_t corrupt_d_beat_count_ = 0;
    std::uint64_t request_stall_cycles_ = 0;
    std::uint64_t response_delay_cycles_ = 0;
    bool inject_denied_ = false;
    bool inject_corrupt_ = false;
    std::optional<DeviceWindow> device_window_;
    std::vector<DeviceAccess> device_accesses_;
    FailureState error_{"uncache", "manager-contract"};
};
