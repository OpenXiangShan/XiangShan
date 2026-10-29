// This file is included inside implementation.hpp's private memblock namespace.

class TileLinkMemoryAgent {
public:
    TileLinkMemoryAgent(SparseMemory &memory, const SparseMemory &reference_memory)
        : memory_(memory), reference_memory_(reference_memory)
    {}

    void configure_backpressure(
        std::uint64_t seed, bool enabled,
        ResponseLatencyProfile latency_profile = ResponseLatencyProfile::compact)
    {
        random_state_ = seed == 0 ? 1 : seed;
        e_random_state_ = (seed ^ 0xd1b54a32d192ed03ULL) == 0
            ? 1
            : seed ^ 0xd1b54a32d192ed03ULL;
        random_backpressure_ = enabled;
        latency_profile_ = latency_profile;
        response_latency_stats_ = {};
        forced_next_response_delay_.reset();
        forced_next_cmo_response_delay_.reset();
        forced_next_interbeat_delay_.reset();
        pending_held_response_addresses_.clear();
        hold_next_response_address_.reset();
        building_held_response_address_.reset();
        scheduled_held_response_release_.reset();
        release_ready_window_active_ = false;
        release_ready_window_first_complete_ = false;
        release_ready_window_target_selected_ = false;
        release_ready_window_bypass_non_target_ = false;
        release_ready_window_target_lines_.clear();
        release_ready_window_stall_remaining_ = 0;
        release_ready_window_stall_cycles_ = 0;
        release_ready_window_completed_releases_ = 0;
        release_channel_idle_cycles_ = 0;
        c_valid_sampled_ = false;
        force_a_stall_ = enabled;
        force_e_stall_ = enabled;
    }

    void inject_next_response_error(
        bool denied, bool corrupt,
        DcacheCorruptBeat corrupt_beat = DcacheCorruptBeat::all)
    {
        configure_response_error(std::nullopt, denied, corrupt, corrupt_beat);
    }

    void inject_response_error_at(
        std::uint64_t address, bool denied, bool corrupt,
        DcacheCorruptBeat corrupt_beat = DcacheCorruptBeat::all)
    {
        configure_response_error(address, denied, corrupt, corrupt_beat);
    }

    void inject_cmo_response_error_at(
        std::uint64_t address, bool denied, bool corrupt)
    {
        configure_response_error(
            address, denied, corrupt, DcacheCorruptBeat::all, true);
    }

    void force_next_response_delay(unsigned cycles)
    {
        forced_next_response_delay_ = cycles;
    }

    void force_next_cmo_response_delay(unsigned cycles)
    {
        forced_next_cmo_response_delay_ = cycles;
    }

    void force_next_interbeat_delay(unsigned cycles)
    {
        forced_next_interbeat_delay_ = cycles;
    }

    void hold_response_at(std::uint64_t address)
    {
        const std::uint64_t line = address & ~(kLineBytes - 1U);
        const bool pending = std::find(
                                 pending_held_response_addresses_.begin(),
                                 pending_held_response_addresses_.end(), line) !=
            pending_held_response_addresses_.end();
        const bool held = std::find_if(
                              held_responses_.begin(), held_responses_.end(),
                              [&](const HeldResponse &response) {
                                  return response.address == line;
                              }) != held_responses_.end();
        if (pending || held || hold_next_response_address_ == line ||
            building_held_response_address_ == line) {
            error_.record("DCache response hold address is already active");
            return;
        }
        pending_held_response_addresses_.push_back(line);
    }

    bool response_held() const { return !held_responses_.empty(); }
    bool response_held_at(std::uint64_t address) const
    {
        const std::uint64_t line = address & ~(kLineBytes - 1U);
        return std::find_if(
                   held_responses_.begin(), held_responses_.end(),
                   [&](const HeldResponse &response) {
                       return response.address == line;
                   }) != held_responses_.end();
    }
    std::size_t held_response_count() const { return held_responses_.size(); }

    void release_held_response()
    {
        if (held_responses_.size() != 1 ||
            building_held_response_address_) {
            error_.record("no complete held DCache response to release");
            return;
        }
        release_held_response_at(held_responses_.front().address);
    }

    void release_held_response_at(std::uint64_t address)
    {
        const std::uint64_t line = address & ~(kLineBytes - 1U);
        if (scheduled_held_response_release_ &&
            scheduled_held_response_release_->first == line) {
            error_.record("DCache held response already has a scheduled release");
            return;
        }
        const auto held = std::find_if(
            held_responses_.begin(), held_responses_.end(),
            [&](const HeldResponse &response) {
                return response.address == line;
            });
        if (held == held_responses_.end() ||
            building_held_response_address_ == line) {
            error_.record("no complete held DCache response at requested address");
            return;
        }
        const bool was_empty = d_beats_.empty();
        while (!held->beats.empty()) {
            d_beats_.push_back(std::move(held->beats.front()));
            held->beats.pop_front();
        }
        held_responses_.erase(held);
        if (was_empty) {
            d_presenting_ = false;
            d_gap_ = d_beats_.front().delay_before;
        }
    }

    void schedule_held_response_release_at(
        std::uint64_t address, unsigned after_ticks = 1)
    {
        const std::uint64_t line = address & ~(kLineBytes - 1U);
        const auto held = std::find_if(
            held_responses_.begin(), held_responses_.end(),
            [&](const HeldResponse &response) {
                return response.address == line;
            });
        if (after_ticks == 0) {
            error_.record("scheduled DCache response release requires at least one tick");
            return;
        }
        if (scheduled_held_response_release_) {
            error_.record("a DCache held response release is already scheduled");
            return;
        }
        if (held == held_responses_.end() || held->beats.empty() ||
            building_held_response_address_ == line) {
            error_.record("no complete held DCache response to schedule for release");
            return;
        }
        scheduled_held_response_release_ = std::make_pair(line, after_ticks);
    }

    void begin_release_ready_window(
        const std::vector<std::uint64_t> &target_lines,
        unsigned stall_valid_cycles)
    {
        if (release_ready_window_active_) {
            error_.record("DCache release ready window is already active");
            return;
        }
        if (target_lines.empty()) {
            error_.record("DCache release ready window has no target lines");
            return;
        }
        release_ready_window_active_ = true;
        release_ready_window_first_complete_ = false;
        release_ready_window_target_selected_ = false;
        release_ready_window_bypass_non_target_ = false;
        release_ready_window_target_lines_.clear();
        release_ready_window_target_lines_.reserve(target_lines.size());
        for (const std::uint64_t line : target_lines) {
            release_ready_window_target_lines_.push_back(
                line & ~(kLineBytes - 1U));
        }
        release_ready_window_stall_remaining_ = stall_valid_cycles;
        release_ready_window_stall_cycles_ = 0;
        release_ready_window_completed_releases_ = 0;
    }

    void end_release_ready_window()
    {
        if (!release_ready_window_active_ ||
            !release_ready_window_first_complete_ ||
            release_ready_window_stall_remaining_ != 0 ||
            release_ready_window_completed_releases_ != 1 ||
            release_ready_window_target_selected_ ||
            release_ready_window_bypass_non_target_ || release_data_) {
            std::ostringstream message;
            message << "incomplete DCache release ready window"
                    << " active=" << release_ready_window_active_
                    << " target_complete="
                    << release_ready_window_first_complete_
                    << " remaining="
                    << release_ready_window_stall_remaining_
                    << " completed="
                    << release_ready_window_completed_releases_
                    << " target_selected="
                    << release_ready_window_target_selected_
                    << " bypass="
                    << release_ready_window_bypass_non_target_
                    << " release_data=" << release_data_.has_value();
            error_.record(message.str());
            return;
        }
        release_ready_window_active_ = false;
        release_ready_window_target_lines_.clear();
    }

    void hold_c_until_probe_requests(std::uint64_t target_requests)
    {
        if (hold_c_until_probe_requests_ ||
            target_requests <= probe_request_count_) {
            error_.record("invalid DCache Probe C hold target");
            return;
        }
        hold_c_until_probe_requests_ = target_requests;
    }

    void reset_link_state()
    {
        b_beats_.clear();
        owners_.clear();
        pending_acquires_.clear();
        grants_.clear();
        d_beats_.clear();
        held_responses_.clear();
        probe_responses_.clear();
        expected_grant_acks_.clear();
        accepted_grant_ack_index_.reset();
        sink_busy_.fill(false);
        captured_a_.reset();
        captured_c_.reset();
        stalled_c_request_.reset();
        release_data_.reset();
        expected_release_lines_.clear();
        a_fire_ = false;
        b_fire_ = false;
        c_fire_ = false;
        d_fire_ = false;
        e_fire_ = false;
        d_gap_ = 0;
        d_presenting_ = false;
        forced_next_response_delay_.reset();
        forced_next_cmo_response_delay_.reset();
        forced_next_interbeat_delay_.reset();
        pending_held_response_addresses_.clear();
        hold_next_response_address_.reset();
        building_held_response_address_.reset();
        scheduled_held_response_release_.reset();
        release_ready_window_active_ = false;
        release_ready_window_first_complete_ = false;
        release_ready_window_target_selected_ = false;
        release_ready_window_bypass_non_target_ = false;
        release_ready_window_target_lines_.clear();
        release_ready_window_stall_remaining_ = 0;
        release_ready_window_stall_cycles_ = 0;
        release_ready_window_completed_releases_ = 0;
        release_channel_idle_cycles_ = 0;
        c_valid_sampled_ = false;
        hold_c_until_probe_requests_.reset();
        pending_response_error_.reset();
        force_a_stall_ = random_backpressure_;
        force_e_stall_ = random_backpressure_;
        outstanding_requests_ = 0;
        active_request_sources_.fill(false);
        probe_canceled_count_ += probe_request_count_ -
            probe_response_count_ - probe_canceled_count_;
        next_probe_source_ = 0;
        probe_sources_seen_.fill(false);
        last_accepted_probe_source_.reset();
    }

    void drive(DutAdapter &dut)
    {
        const bool accept_a = !random_backpressure_ ||
                              (!force_a_stall_ && (next_random() & 3U) != 0);
        const bool hold_c_for_probes = hold_c_until_probe_requests_ &&
            probe_request_count_ < *hold_c_until_probe_requests_;
        const bool accept_c = !hold_c_for_probes &&
            (release_ready_window_active_
                 ? release_ready_window_first_complete_ ||
                       release_ready_window_bypass_non_target_ ||
                       release_ready_window_stall_remaining_ == 0
                 : !random_backpressure_ || (next_random() & 3U) != 0);
        const bool accept_e = !random_backpressure_ ||
                              (!force_e_stall_ && (next_e_random() & 3U) != 0);
        dut.dcache.a.ready.set(accept_a);
        dut.dcache.c.ready.set(accept_c);
        dut.dcache.e.ready.set(accept_e);
        if (b_beats_.empty() || !probe_can_present(b_beats_.front())) {
            dut.dcache.b.valid.set(std::uint64_t{0});
        } else {
            const BBeat &beat = b_beats_.front();
            dut.dcache.b.opcode.set(beat.opcode);
            dut.dcache.b.param.set(beat.param);
            dut.dcache.b.size.set(beat.size);
            dut.dcache.b.source.set(beat.source);
            dut.dcache.b.address.set(beat.address);
            dut.dcache.b.mask.set(beat.mask);
            auto bytes = beat.data;
            dut.dcache.b.data.set_bytes(bytes);
            dut.dcache.b.corrupt.set(beat.corrupt);
            dut.dcache.b.valid.set(std::uint64_t{1});
        }
        if (d_beats_.empty()) {
            dut.dcache.d.valid.set(std::uint64_t{0});
            d_presenting_ = false;
            return;
        }

        if (!d_presenting_) {
            if (d_gap_ != 0) {
                --d_gap_;
                ++response_delay_cycles_;
                dut.dcache.d.valid.set(std::uint64_t{0});
                return;
            }
            d_presenting_ = true;
        }

        const DBeat &beat = d_beats_.front();
        dut.dcache.d.opcode.set(beat.opcode);
        dut.dcache.d.param.set(beat.param);
        dut.dcache.d.size.set(beat.size);
        dut.dcache.d.source.set(beat.source);
        dut.dcache.d.sink.set(beat.sink);
        dut.dcache.d.denied.set(beat.denied);
        dut.dcache.d.echo_is_keyword.set(beat.keyword);
        auto bytes = beat.data;
        dut.dcache.d.data.set_bytes(bytes);
        dut.dcache.d.corrupt.set(beat.corrupt);
        dut.dcache.d.valid.set(std::uint64_t{1});
    }

    void capture_before_tick(DutAdapter &dut)
    {
        const bool a_valid = dut.dcache.a.valid.boolean();
        const bool a_ready = dut.dcache.a.ready.boolean();
        if (a_valid && !a_ready) {
            ++request_stall_cycles_;
            force_a_stall_ = false;
        }
        a_fire_ = a_valid && a_ready;
        if (a_fire_) {
            captured_a_ = ARequest{
                .opcode = static_cast<std::uint8_t>(
                    dut.dcache.a.opcode.value()),
                .param = static_cast<std::uint8_t>(
                    dut.dcache.a.param.value()),
                .size = static_cast<std::uint8_t>(
                    dut.dcache.a.size.value()),
                .source = static_cast<std::uint8_t>(
                    dut.dcache.a.source.value()),
                .address = dut.dcache.a.address.value(),
                .keyword = dut.dcache.a.echo_is_keyword.boolean(),
                .request_source = static_cast<std::uint8_t>(
                    dut.dcache.a.user_req_source.value()),
                .virtual_address =
                    dut.dcache.a.user_vaddr.value(),
                .alias = static_cast<std::uint8_t>(
                    dut.dcache.a.user_alias.value()),
                .need_hint = dut.dcache.a.user_need_hint.boolean(),
                .mask = static_cast<std::uint32_t>(
                    dut.dcache.a.mask.value()),
                .corrupt = dut.dcache.a.corrupt.boolean(),
                .cycle = dut.cycle(),
                .index = request_count_,
            };
            if (const char *error = dcache_a_protocol_error(
                    *captured_a_, kDcacheMissEntries, kDcacheCmoSource,
                    kDcacheSourceCount)) {
                error_.record(std::string("DCache TileLink A contract: ") + error +
                    ':' + request_record_summary(*captured_a_));
            }
        }
        const bool b_valid = dut.dcache.b.valid.boolean();
        const bool b_ready = dut.dcache.b.ready.boolean();
        if (b_valid && !b_ready) {
            ++probe_stall_cycles_;
        }
        b_fire_ = b_valid && b_ready;
        const bool c_valid = dut.dcache.c.valid.boolean();
        const bool c_ready = dut.dcache.c.ready.boolean();
        c_valid_sampled_ = c_valid;
        std::optional<CRequest> current_c;
        if (c_valid) {
            current_c = CRequest{
                static_cast<std::uint8_t>(
                    dut.dcache.c.opcode.value()),
                static_cast<std::uint8_t>(
                    dut.dcache.c.param.value()),
                static_cast<std::uint8_t>(
                    dut.dcache.c.size.value()),
                static_cast<std::uint8_t>(
                    dut.dcache.c.source.value()),
                dut.dcache.c.address.value(),
                static_cast<std::uint8_t>(
                    dut.dcache.c.user_alias.value()),
                dut.dcache.c.user_vaddr.value(),
                static_cast<std::uint8_t>(
                    dut.dcache.c.user_req_source.value()),
                dut.dcache.c.user_need_hint.boolean(),
                dut.dcache.c.echo_is_keyword.boolean(),
                dut.dcache.c.corrupt.boolean(),
                dut.dcache.c.data.bytes(),
            };
        }
        if (current_c && release_ready_window_active_ &&
            !release_ready_window_first_complete_ &&
            !release_ready_window_target_selected_ &&
            !release_ready_window_bypass_non_target_) {
            if (is_release_ready_window_target(*current_c)) {
                release_ready_window_target_selected_ = true;
            } else {
                release_ready_window_bypass_non_target_ = true;
            }
        }
        if (stalled_c_request_) {
            if (!current_c) {
                error_.record("DCache TileLink C dropped valid while stalled");
            } else if (*current_c != *stalled_c_request_) {
                error_.record("DCache TileLink C changed payload while stalled");
            } else {
                ++c_payload_stability_checks_;
                if (release_ready_window_active_ &&
                    release_ready_window_target_selected_ &&
                    !release_ready_window_first_complete_) {
                    ++release_target_payload_stability_checks_;
                }
            }
        }
        c_fire_ = c_valid && c_ready;
        if (c_valid && !c_ready) {
            stalled_c_request_ = current_c;
            if (release_ready_window_active_ &&
                !release_ready_window_first_complete_ &&
                release_ready_window_target_selected_) {
                if (release_ready_window_stall_remaining_ != 0) {
                    --release_ready_window_stall_remaining_;
                    ++release_ready_window_stall_cycles_;
                }
            }
        } else {
            stalled_c_request_.reset();
        }
        if (c_fire_) {
            captured_c_ = std::move(current_c);
            if (captured_c_->opcode < 4 || captured_c_->opcode > 7) {
                error_.record("unsupported DCache TileLink C opcode");
            }
            if (captured_c_->size > 6) {
                error_.record("oversized DCache TileLink C request");
            }
        }
        const bool d_handshake = !d_beats_.empty() &&
            dut.dcache.d.valid.boolean() &&
            dut.dcache.d.ready.boolean();
        const bool e_valid = dut.dcache.e.valid.boolean();
        const bool e_ready = dut.dcache.e.ready.boolean();
        if (e_valid && !e_ready) {
            ++grant_ack_stall_cycles_;
            force_e_stall_ = false;
        }
        e_fire_ = e_valid && e_ready;
        if (e_fire_) {
            const std::uint16_t sink = static_cast<std::uint16_t>(
                dut.dcache.e.sink.value());
            const auto expected = std::find_if(
                expected_grant_acks_.begin(), expected_grant_acks_.end(),
                [&](const auto &entry) { return entry.first == sink; });
            if (expected == expected_grant_acks_.end()) {
                error_.record("unexpected DCache TileLink E GrantAck");
            } else {
                const bool sink_accepted =
                    sink < sink_busy_.size() && sink_busy_[sink];
                // TileLink legalizeDESink permits E in the same cycle as the
                // first accepted D beat, but not before any D acceptance.
                const bool same_cycle_first =
                    d_handshake && d_beats_.front().first_beat &&
                    d_beats_.front().sink == sink;
                if (!sink_accepted && !same_cycle_first) {
                    error_.record(
                        "DCache TileLink E GrantAck before first D acceptance");
                }
                accepted_grant_ack_index_ = static_cast<std::size_t>(
                    std::distance(expected_grant_acks_.begin(), expected));
            }
        }
        d_fire_ = !d_beats_.empty() &&
                  dut.dcache.d.valid.boolean() &&
                  dut.dcache.d.ready.boolean();
        if (d_fire_) {
            const DBeat &expected = d_beats_.front();
            const auto actual_data =
                dut.dcache.d.data.bytes();
            if (dut.dcache.d.opcode.value() != expected.opcode ||
                dut.dcache.d.param.value() != expected.param ||
                dut.dcache.d.size.value() != expected.size ||
                dut.dcache.d.source.value() != expected.source ||
                dut.dcache.d.sink.value() != expected.sink ||
                dut.dcache.d.echo_is_keyword.boolean() != expected.keyword ||
                dut.dcache.d.denied.boolean() != expected.denied ||
                dut.dcache.d.corrupt.boolean() != expected.corrupt ||
                actual_data != expected.data) {
                error_.record("DCache TileLink D response identity or payload mismatch");
            }
        }
    }

    void update_after_tick()
    {
        if (b_fire_) {
            const std::uint8_t source = b_beats_.front().source;
            const auto response = std::find_if(
                probe_responses_.begin(), probe_responses_.end(),
                [source](const auto &pending) {
                    return pending.b_source == source;
                });
            if (response == probe_responses_.end()) {
                error_.record("DCache TileLink accepted a B Probe without a "
                              "matching expectation");
            } else if (response->b_accepted) {
                error_.record("DCache TileLink accepted the same B Probe source twice");
            } else {
                response->b_accepted = true;
            }
            if (probe_sources_seen_[source]) {
                ++probe_source_reuse_count_;
            } else {
                probe_sources_seen_[source] = true;
                ++probe_source_count_;
            }
            if (last_accepted_probe_source_ ==
                    memblock::kDcacheProbeSourceCount - 1U &&
                source == 0) {
                ++probe_source_wrap_count_;
            }
            last_accepted_probe_source_ = source;
            b_beats_.pop_front();
            ++probe_request_count_;
            max_probe_outstanding_ = std::max(
                max_probe_outstanding_,
                probe_request_count_ - probe_response_count_ -
                    probe_canceled_count_);
            if (hold_c_until_probe_requests_ &&
                probe_request_count_ >= *hold_c_until_probe_requests_) {
                hold_c_until_probe_requests_.reset();
            }
        }
        if (d_fire_) {
            const DBeat &response = d_beats_.front();
            if (response.first_beat &&
                (response.opcode == 4 || response.opcode == 5)) {
                if (response.sink >= sink_busy_.size() ||
                    sink_busy_[response.sink]) {
                    error_.record(
                        "DCache TileLink sink reused before GrantAck");
                } else {
                    sink_busy_[response.sink] = true;
                }
            }
            if (d_beats_.front().opcode == 5) {
                ++grant_data_beat_count_;
            }
            denied_d_beat_count_ += response.denied;
            corrupt_d_beat_count_ += response.corrupt;
            const bool completes_a = response.opcode == 1 ||
                response.opcode == 4 ||
                response.opcode == 8 ||
                (response.opcode == 5 && response.last_beat);
            if (completes_a) {
                if (response.source >= active_request_sources_.size() ||
                    !active_request_sources_[response.source] ||
                    outstanding_requests_ == 0) {
                    error_.record("DCache TileLink D completed an inactive A source");
                } else {
                    active_request_sources_[response.source] = false;
                    --outstanding_requests_;
                }
            }
            if ((response.opcode == 4 || response.opcode == 5) &&
                response.last_beat) {
                for (auto &grant : grants_) {
                    if (grant.sink == response.sink) grant.final_d = true;
                }
            }
            d_beats_.pop_front();
            d_presenting_ = false;
            d_gap_ = d_beats_.empty() ? 0 : d_beats_.front().delay_before;
        }
        if (a_fire_ && captured_a_) {
            if (captured_a_->source >= active_request_sources_.size() ||
                active_request_sources_[captured_a_->source]) {
                error_.record("DCache TileLink A source reused before D completion");
            } else {
                active_request_sources_[captured_a_->source] = true;
                ++outstanding_requests_;
                max_outstanding_requests_ = std::max(
                    max_outstanding_requests_, outstanding_requests_);
            }
            last_request_address_ = captured_a_->address;
            request_history_.push_back(*captured_a_);
            ++request_classes_[request_class(*captured_a_)];
            if (captured_a_->opcode == 6 || captured_a_->opcode == 7) {
                pending_acquires_.push_back({*captured_a_, false});
            } else {
                respond(*captured_a_);
            }
            ++request_count_;
        }
        if (c_fire_ && captured_c_) {
            if (captured_c_->opcode == 4 || captured_c_->opcode == 5) {
                accept_probe_response(*captured_c_);
            } else {
                accept_release(*captured_c_);
            }
        }
        if (e_fire_ && accepted_grant_ack_index_) {
            const auto expected = expected_grant_acks_.begin() +
                static_cast<std::ptrdiff_t>(*accepted_grant_ack_index_);
            error_response_grant_ack_count_ += expected->second;
            if (expected->first < sink_busy_.size()) {
                sink_busy_[expected->first] = false;
            }
            for (auto &grant : grants_) {
                if (grant.sink == expected->first) grant.e = true;
            }
            expected_grant_acks_.erase(expected);
            ++grant_ack_count_;
        }
        std::erase_if(grants_, [](const auto &grant) {
            return grant.final_d && grant.e;
        });
        service_acquires();
        const bool probe_hold = hold_c_until_probe_requests_ &&
            probe_request_count_ < *hold_c_until_probe_requests_;
        const bool release_idle_sample = !c_valid_sampled_ &&
            !stalled_c_request_ && !release_data_ &&
            !release_ready_window_active_ && !probe_hold;
        release_channel_idle_cycles_ = release_idle_sample
            ? release_channel_idle_cycles_ + 1U : 0U;
        a_fire_ = false;
        b_fire_ = false;
        c_fire_ = false;
        d_fire_ = false;
        e_fire_ = false;
        accepted_grant_ack_index_.reset();
        captured_a_.reset();
        captured_c_.reset();
        if (scheduled_held_response_release_) {
            auto &ticks_remaining = scheduled_held_response_release_->second;
            --ticks_remaining;
            if (ticks_remaining == 0) {
                const std::uint64_t line =
                    scheduled_held_response_release_->first;
                scheduled_held_response_release_.reset();
                release_held_response_at(line);
            }
        }
    }

    const FailureState &failures() const { return error_; }
    void set_failure_cycle(std::uint64_t cycle) { error_.set_cycle(cycle); }

    bool ok() const { return error_.empty(); }
    const std::string &error() const { return error_.message(); }
    std::uint64_t request_count() const { return request_count_; }
    std::uint64_t last_request_address() const { return last_request_address_; }
    const std::array<std::uint64_t, 7> &request_classes() const
    {
        return request_classes_;
    }
    std::string request_summary_since(
        std::uint64_t first_request, unsigned maximum_records = 8) const
    {
        const std::size_t start = static_cast<std::size_t>(
            std::min<std::uint64_t>(first_request, request_history_.size()));
        const std::size_t count = request_history_.size() - start;
        const std::size_t shown = std::min<std::size_t>(
            count, std::min(maximum_records, 32U));
        std::ostringstream stream;
        stream << "count=" << count << ":records=[";
        for (std::size_t offset = 0; offset < shown; ++offset) {
            stream << (offset == 0 ? "" : ";")
                   << request_record_summary(request_history_[start + offset]);
        }
        stream << "]:omitted=" << count - shown;
        return stream.str();
    }
    const char *faulting_load_requests_error_since(
        std::uint64_t target_line, bool target_read_allowed,
        std::uint64_t first_request) const
    {
        for (std::size_t index = static_cast<std::size_t>(first_request);
             index < request_history_.size(); ++index) {
            const ARequest &request = request_history_[index];
            if (const char *error = dcache_faulting_load_request_error(
                    request, target_read_allowed, target_line)) {
                return error;
            }
        }
        return nullptr;
    }
    std::uint64_t request_covering_count_since(
        std::uint64_t address, std::uint64_t first_request) const
    {
        std::uint64_t count = 0;
        for (std::size_t index = static_cast<std::size_t>(first_request);
             index < request_history_.size(); ++index) {
            const ARequest &request = request_history_[index];
            const std::uint64_t bytes = std::uint64_t{1} << request.size;
            const std::uint64_t base = request.address & ~(bytes - 1);
            count += address >= base && address - base < bytes;
        }
        return count;
    }
    // Accepted external A-channel evidence for a cold full-line store acquire.
    // Source 3 is the generated interface's store request class; AcquireBlock
    // NtoT is TileLink opcode 6 / grow param 1, with size 6 for 64 bytes.
    std::uint64_t store_ntot_line_count_since(
        std::uint64_t line, std::uint64_t first_request) const
    {
        std::uint64_t count = 0;
        for (std::size_t index = static_cast<std::size_t>(first_request);
             index < request_history_.size(); ++index) {
            const ARequest &request = request_history_[index];
            count += request.address == line && request.opcode == 6 &&
                request.param == 1 && request.size == 6 &&
                request.request_source == 3;
        }
        return count;
    }
    std::uint64_t request_source_count_since(
        std::uint8_t request_source, std::uint64_t first_request) const
    {
        std::uint64_t count = 0;
        for (std::size_t index = static_cast<std::size_t>(first_request);
             index < request_history_.size(); ++index) {
            const ARequest &request = request_history_[index];
            count += request_source == 2
                ? dcache_is_cpu_demand_acquire(request)
                : request.request_source == request_source;
        }
        return count;
    }
    std::uint64_t request_source_covering_count_since(
        std::uint8_t request_source, std::uint64_t address,
        std::uint64_t first_request) const
    {
        std::uint64_t count = 0;
        for (std::size_t index = static_cast<std::size_t>(first_request);
             index < request_history_.size(); ++index) {
            const ARequest &request = request_history_[index];
            const std::uint64_t bytes = std::uint64_t{1} << request.size;
            const std::uint64_t base = request.address & ~(bytes - 1U);
            const bool source_matches = request_source == 2
                ? dcache_is_cpu_demand_acquire(request)
                : request.request_source == request_source;
            count += source_matches &&
                address >= base && address - base < bytes;
        }
        return count;
    }
    void begin_release_observation()
    {
        release_observation_.clear();
        observed_release_index_.clear();
        observe_releases_ = true;
    }
    void end_release_observation()
    {
        observe_releases_ = false;
        release_observation_.clear();
        observed_release_index_.clear();
    }
    std::uint64_t observed_release_covering_count(
        std::uint64_t address, bool data_only = false) const
    {
        if (const auto indexed =
                observed_release_index_.line_count(address, data_only)) {
            return *indexed;
        }
        std::uint64_t count = 0;
        for (const ReleaseRecord &release : release_observation_) {
            if (data_only && release.opcode != 7) {
                continue;
            }
            const std::uint64_t bytes = std::uint64_t{1} << release.size;
            const std::uint64_t base = release.address & ~(bytes - 1);
            count += address >= base && address - base < bytes;
        }
        return count;
    }
    std::uint64_t observed_stalled_release_covering_count(
        std::uint64_t address, bool data_only = false) const
    {
        std::uint64_t count = 0;
        for (const ReleaseRecord &release : release_observation_) {
            if ((data_only && release.opcode != 7) ||
                release.stall_cycles == 0) {
                continue;
            }
            const std::uint64_t bytes = std::uint64_t{1} << release.size;
            const std::uint64_t base = release.address & ~(bytes - 1);
            count += address >= base && address - base < bytes;
        }
        return count;
    }
    std::uint64_t observed_release_stall_cycles_covering(
        std::uint64_t address, bool data_only = false) const
    {
        std::uint64_t count = 0;
        for (const ReleaseRecord &release : release_observation_) {
            if (data_only && release.opcode != 7) {
                continue;
            }
            const std::uint64_t bytes = std::uint64_t{1} << release.size;
            const std::uint64_t base = release.address & ~(bytes - 1);
            if (address >= base && address - base < bytes) {
                count += release.stall_cycles;
            }
        }
        return count;
    }
    std::uint64_t get_count() const { return get_count_; }
    std::uint64_t refill_count() const { return refill_count_; }
    std::uint64_t keyword_refill_count() const
    {
        return keyword_refill_count_;
    }
    std::uint64_t nonkeyword_refill_count() const
    {
        return nonkeyword_refill_count_;
    }
    std::uint64_t acquire_perm_count() const { return acquire_perm_count_; }
    std::uint64_t cmo_count(unsigned operation) const
    {
        return operation < cmo_counts_.size() ? cmo_counts_[operation] : 0;
    }
    std::uint64_t last_cmo_address() const { return last_cmo_address_; }
    std::uint8_t last_cmo_source() const { return last_cmo_source_; }
    std::uint64_t request_stall_cycles() const { return request_stall_cycles_; }
    std::uint64_t response_delay_cycles() const { return response_delay_cycles_; }
    const ResponseLatencyStats &response_latency_stats() const
    {
        return response_latency_stats_;
    }
    std::uint64_t release_count() const { return release_count_; }
    std::uint64_t release_data_count() const { return release_data_count_; }
    std::uint64_t release_data_verified_count() const
    {
        return release_data_verified_count_;
    }
    std::uint64_t expected_release_data_verified_count() const
    {
        return expected_release_data_verified_count_;
    }
    std::uint64_t probe_request_count() const { return probe_request_count_; }
    std::uint64_t probe_response_count() const { return probe_response_count_; }
    std::uint64_t probe_data_count() const { return probe_data_count_; }
    std::uint64_t probe_source_count() const { return probe_source_count_; }
    std::uint64_t probe_source_reuse_count() const
    {
        return probe_source_reuse_count_;
    }
    std::uint64_t probe_source_wrap_count() const
    {
        return probe_source_wrap_count_;
    }
    std::uint64_t max_probe_outstanding() const
    {
        return max_probe_outstanding_;
    }
    std::uint64_t probe_stall_cycles() const { return probe_stall_cycles_; }
    std::uint64_t grant_ack_count() const { return grant_ack_count_; }
    std::uint64_t grant_data_beat_count() const
    {
        return grant_data_beat_count_;
    }
    std::uint64_t denied_d_beat_count() const
    {
        return denied_d_beat_count_;
    }
    std::uint64_t corrupt_d_beat_count() const
    {
        return corrupt_d_beat_count_;
    }
    std::uint64_t error_response_request_count() const
    {
        return error_response_request_count_;
    }
    std::uint64_t error_response_refill_count() const
    {
        return error_response_refill_count_;
    }
    std::uint64_t error_response_grant_ack_count() const
    {
        return error_response_grant_ack_count_;
    }
    std::uint64_t last_error_response_address() const
    {
        return last_error_response_address_;
    }
    bool last_error_response_keyword() const
    {
        return last_error_response_keyword_;
    }
    std::uint64_t max_outstanding_requests() const
    {
        return max_outstanding_requests_;
    }
    std::uint64_t outstanding_requests() const
    {
        return outstanding_requests_;
    }
    std::uint64_t grant_ack_stall_cycles() const
    {
        return grant_ack_stall_cycles_;
    }
    std::uint64_t c_payload_stability_checks() const
    {
        return c_payload_stability_checks_;
    }
    std::uint64_t release_target_payload_stability_checks() const
    {
        return release_target_payload_stability_checks_;
    }

    bool request_probe(
        std::uint64_t address, std::uint8_t cap, bool need_data,
        std::uint8_t expected_report,
        const std::vector<unsigned char> &expected_data = {})
    {
        if ((address & (kLineBytes - 1)) != 0 || cap > 2 ||
            expected_report > 5 ||
            (!expected_data.empty() && expected_data.size() != kLineBytes)) {
            error_.record("invalid DCache Probe request or expectation");
            return false;
        }
        const auto owner = owners_.find(address);
        const std::uint8_t alias = owner == owners_.end()
            ? static_cast<std::uint8_t>((address >> 12U) & 3U)
            : owner->second.alias;
        return enqueue_probe(address, cap, need_data, expected_report,
            expected_data, alias, false, 0);
    }

    // The caller has retired producers, drained SBuffer and all D/E traffic.
    // Freeze only independent architectural bytes, before the next context's
    // stores can advance the global reference beyond the old cache owner.
    bool checkpoint_alias_reference(std::uint64_t base, std::uint64_t bytes)
    {
        if (!responses_idle() || !grant_acks_idle() || !probes_idle()) {
            error_.record("DCache alias checkpoint requires an idle link");
            return false;
        }
        for (auto &[line, owner] : owners_) {
            if (line >= base && line - base < bytes && owner.present) {
                owner.expected_data = reference_memory_.read_beat(line, kLineBytes);
                owner.updates.clear();
            }
        }
        return true;
    }

    // These are independent committed inputs, not sampled cache contents.
    // Keep the drained baseline fixed: a voluntary eviction may precede a
    // committed same-alias store still propagating through SBuffer.
    void note_reference_store(std::uint64_t physical, std::uint64_t virtual_address,
                              const std::vector<unsigned char> &bytes)
    {
        const auto line = physical & ~(kLineBytes - 1U);
        const auto owner = owners_.find(line);
        if (owner == owners_.end() || !owner->second.present ||
            owner->second.alias != ((virtual_address >> 12U) & 3U) ||
            owner->second.expected_data.empty()) return;
        if (bytes.empty() || bytes.size() > kLineBytes - (physical - line)) {
            error_.record("mapped reference update crosses a cache line");
            return;
        }
        OwnerUpdate update{};
        for (unsigned b = 0; b < bytes.size(); ++b) {
            const unsigned offset = physical - line + b;
            update.mask |= std::uint64_t{1} << offset;
            update.bytes[offset] = bytes[b];
        }
        owner->second.updates.push_back(update);
    }

    void note_reference_write(std::uint64_t physical, std::uint64_t virtual_address,
                              unsigned char value)
    {
        note_reference_store(physical, virtual_address, {value});
    }

    std::uint64_t alias_recalls() const { return alias_recall_count_; }
    std::uint64_t alias_recall_responses() const { return alias_recall_response_count_; }

private:
    bool enqueue_probe(std::uint64_t address, std::uint8_t cap, bool need_data,
        std::uint8_t expected_report, const std::vector<unsigned char> &expected_data,
        std::uint8_t alias, bool automatic, std::uint8_t permission)
    {
        std::vector<unsigned char> data(kBeatBytes, 0);
        data[0] = static_cast<unsigned char>((need_data ? 1U : 0U) | (alias << 1U));
        const std::uint8_t source = next_probe_source_;
        if (std::any_of(
                probe_responses_.begin(), probe_responses_.end(),
                [source](const auto &response) {
                    return response.b_source == source;
                })) {
            error_.record("DCache Probe source reused while still outstanding");
            return false;
        }
        next_probe_source_ = static_cast<std::uint8_t>(
            (next_probe_source_ + 1U) &
            (memblock::kDcacheProbeSourceCount - 1U));
        b_beats_.push_back(BBeat{
            6, cap, 6, source, address, 0xffffffffU,
            std::move(data), false});
        probe_responses_.push_back(ProbeResponseState{
            source, address, expected_report, expected_data, 0,
            0, false, automatic, permission});
        return true;
    }

public:
    bool probes_idle() const
    {
        return b_beats_.empty() && probe_responses_.empty();
    }
    bool grant_acks_idle() const
    {
        return expected_grant_acks_.empty();
    }
    // Cacheable Acquire/Grant lifetime for one physical 64-byte line. A
    // Grant remains live until both final D and E, in either order; armed
    // holds also reserve the line before its A request arrives.
    bool line_grants_idle(std::uint64_t address) const
    {
        const std::uint64_t line = address & ~(kLineBytes - 1U);
        return std::none_of(
                   pending_acquires_.begin(), pending_acquires_.end(),
                   [line](const PendingAcquire &acquire) {
                       return (acquire.request.address & ~(kLineBytes - 1U)) == line;
                   }) &&
            !line_grant_busy(line) &&
            std::find(pending_held_response_addresses_.begin(),
                pending_held_response_addresses_.end(), line) ==
                pending_held_response_addresses_.end() &&
            hold_next_response_address_ != line &&
            building_held_response_address_ != line &&
            std::none_of(held_responses_.begin(), held_responses_.end(),
                [line](const HeldResponse &response) {
                    return response.address == line;
                }) &&
            (!scheduled_held_response_release_ ||
                scheduled_held_response_release_->first != line);
    }
    bool release_channel_idle() const
    {
        const bool probe_hold = hold_c_until_probe_requests_ &&
            probe_request_count_ < *hold_c_until_probe_requests_;
        return release_channel_idle_cycles_ >= 2 &&
            !stalled_c_request_ && !release_data_ &&
            !release_ready_window_active_ && !probe_hold;
    }
    bool responses_idle() const
    {
        return pending_acquires_.empty() && d_beats_.empty() && held_responses_.empty() &&
            pending_held_response_addresses_.empty() &&
            !hold_next_response_address_ && !building_held_response_address_ &&
            !scheduled_held_response_release_ &&
            !release_data_ && outstanding_requests_ == 0;
    }
    void expect_release_line(
        std::uint64_t base, const std::vector<unsigned char> &bytes)
    {
        if (bytes.empty() || bytes.size() % kBeatBytes != 0) {
            error_.record("expected ReleaseData line is not beat sized");
            return;
        }
        expected_release_lines_[base] = bytes;
    }

    void clear_release_line_expectations()
    {
        expected_release_lines_.clear();
    }

    bool verify_memory_bytes(
        std::uint64_t address, const std::vector<unsigned char> &expected)
    {
        for (std::size_t index = 0; index < expected.size(); ++index) {
            if (memory_.read_byte(address + index) != expected[index]) {
                std::ostringstream message;
                message << "post-commit store readback mismatch address=0x"
                        << std::hex << (address + index)
                        << " expected=0x" << static_cast<unsigned>(expected[index])
                        << " actual=0x"
                        << static_cast<unsigned>(memory_.read_byte(address + index));
                error_.record(message.str());
                return false;
            }
        }
        return true;
    }

    bool check_memory_bytes(
        std::uint64_t address, const std::vector<unsigned char> &expected)
    {
        return verify_memory_bytes(address, expected);
    }

private:
    static constexpr std::size_t kBeatBytes = 32;
    static constexpr std::size_t kLineBytes = 64;

    using ARequest = DcacheARequest;

    static unsigned request_class(const ARequest &request)
    {
        // CMO/Get user fields are unspecified. Classify by opcode first;
        // Acquire reqSource describes ancestry, not transaction identity.
        if (request.opcode >= 12 && request.opcode <= 14) {
            return 4;
        }
        if (request.opcode == 4) {
            return 5;
        }
        if (request.opcode == 6 || request.opcode == 7) {
            switch (request.request_source) {
            case 2: return 0;
            case 3: return 1;
            case 4: return 2;
            case 6: return 3;
            default: break;
            }
        }
        return 6;
    }

    static std::string request_record_summary(const ARequest &request)
    {
        std::ostringstream stream;
        stream << "index=" << request.index << ":cycle=" << request.cycle
               << ":opcode=" << unsigned(request.opcode)
               << ":param=" << unsigned(request.param)
               << ":size=" << unsigned(request.size)
               << ":source=" << unsigned(request.source)
               << ":address=0x" << std::hex << request.address
               << ":user_vaddr=0x" << request.virtual_address
               << ":mask=0x" << request.mask << std::dec
               << ":reqSource=" << unsigned(request.request_source)
               << ":alias=" << unsigned(request.alias)
               << ":needHint=" << request.need_hint
               << ":keyword=" << request.keyword
               << ":corrupt=" << request.corrupt;
        return stream.str();
    }

    struct DBeat {
        std::uint8_t opcode;
        std::uint8_t param;
        std::uint8_t size;
        std::uint8_t source;
        std::uint16_t sink;
        bool keyword;
        std::vector<unsigned char> data;
        bool denied = false;
        bool corrupt = false;
        bool last_beat = true;
        bool first_beat = false;
        unsigned delay_before = 0;
    };

    struct HeldResponse {
        std::uint64_t address;
        std::deque<DBeat> beats;
    };

    struct PendingResponseError {
        std::optional<std::uint64_t> address;
        bool denied;
        bool corrupt;
        DcacheCorruptBeat corrupt_beat;
        bool cmo_only;
    };

    struct BBeat {
        std::uint8_t opcode;
        std::uint8_t param;
        std::uint8_t size;
        std::uint8_t source;
        std::uint64_t address;
        std::uint32_t mask;
        std::vector<unsigned char> data;
        bool corrupt;
    };

    struct CRequest {
        std::uint8_t opcode;
        std::uint8_t param;
        std::uint8_t size;
        std::uint8_t source;
        std::uint64_t address;
        std::uint8_t alias;
        std::uint64_t virtual_address;
        std::uint8_t request_source;
        bool need_hint;
        bool keyword;
        bool corrupt;
        std::vector<unsigned char> data;

        bool operator==(const CRequest &) const = default;
    };

    struct ReleaseRecord {
        std::uint8_t opcode;
        std::uint8_t size;
        std::uint64_t address;
        std::uint64_t stall_cycles;
    };

    struct OwnerUpdate {
        std::uint64_t mask = 0;
        std::array<unsigned char, 64> bytes{};
    };

    struct ReleaseDataState {
        std::uint64_t base;
        std::uint64_t address;
        std::uint8_t size;
        std::uint8_t source;
        std::uint8_t param;
        std::size_t beats;
        std::size_t received = 0;
        std::vector<unsigned char> baseline;
        std::vector<OwnerUpdate> updates;
        std::vector<unsigned char> actual;
    };

    struct ProbeResponseState {
        std::uint8_t b_source;
        std::uint64_t base;
        std::uint8_t report;
        std::vector<unsigned char> expected_data;
        std::size_t received;
        std::uint8_t source = 0;
        bool b_accepted = false;
        bool automatic = false;
        std::uint8_t permission = 0; // granted maximum: T=0, B=1
        std::uint8_t response_opcode = 0;
        std::uint8_t response_report = 0;
    };

    struct Owner {
        std::uint8_t alias;
        std::uint8_t permission;
        bool present = true;
        std::vector<unsigned char> expected_data;
        std::vector<OwnerUpdate> updates;
    };
    struct PendingAcquire { ARequest request; bool recall_started; };
    struct GrantLifetime {
        std::uint64_t line;
        std::uint16_t sink;
        bool final_d = false;
        bool e = false;
    };

    bool line_grant_busy(std::uint64_t line) const
    {
        return std::any_of(grants_.begin(), grants_.end(),
            [line](const auto &grant) { return grant.line == line; });
    }

    bool probe_can_present(const BBeat &beat) const
    {
        return !line_grant_busy(beat.address) &&
            std::none_of(probe_responses_.begin(), probe_responses_.end(),
                [&](const auto &probe) {
                    return probe.base == beat.address && probe.b_accepted;
                });
    }

    void service_acquires()
    {
        std::set<std::uint64_t> earlier_lines;
        for (auto it = pending_acquires_.begin(); it != pending_acquires_.end();) {
            const auto &request = it->request;
            const auto line = request.address & ~(kLineBytes - 1U);
            const auto sink = static_cast<std::uint16_t>(1U + request.source);
            if (!earlier_lines.insert(line).second || line_grant_busy(line) ||
                (release_data_ && release_data_->base == line) ||
                std::any_of(expected_grant_acks_.begin(), expected_grant_acks_.end(),
                    [sink](const auto &ack) { return ack.first == sink; }) ||
                std::any_of(probe_responses_.begin(), probe_responses_.end(),
                    [line](const auto &probe) { return probe.base == line; })) {
                ++it;
                continue;
            }
            auto owner = owners_.find(line);
            const bool different_alias = owner != owners_.end() &&
                owner->second.present && owner->second.alias != request.alias;
            if (different_alias) {
                if (!it->recall_started) {
                    if (owner->second.expected_data.empty() || !owner->second.updates.empty()) {
                        error_.record("DCache cross-alias Acquire requires a drained reference checkpoint");
                        return;
                    }
                    if (!enqueue_probe(line, 2, true, 0,
                            owner->second.expected_data, owner->second.alias,
                            true, owner->second.permission)) return;
                    it->recall_started = true;
                    ++alias_recall_count_;
                }
                // NtoN can overtake an evicted line's voluntary ReleaseData.
                // Retain the old ownership obligation until that C completes.
                ++it;
                continue;
            }
            respond(request); // Capture backing bytes only after completed recall.
            it = pending_acquires_.erase(it);
        }
    }

    void shrink_owner(std::uint64_t line, std::uint8_t report)
    {
        const auto owner = owners_.find(line);
        if (owner == owners_.end()) return;
        if (report == 0 || report == 4) owner->second.permission = 1;
        if (report == 1 || report == 2) owner->second.present = false;
        // NtoN alone cannot retire a voluntary Release still queued at client.
    }

    void respond(const ARequest &request)
    {
        const std::uint64_t transfer_bytes = std::uint64_t{1} << request.size;
        const std::uint64_t base = request.address & ~(transfer_bytes - 1);
        const std::uint64_t request_line =
            request.address & ~(kLineBytes - 1U);
        const auto held_address = std::find(
            pending_held_response_addresses_.begin(),
            pending_held_response_addresses_.end(), request_line);
        if (held_address != pending_held_response_addresses_.end()) {
            hold_next_response_address_ = request_line;
            pending_held_response_addresses_.erase(held_address);
        }
        bool denied = false;
        bool corrupt = false;
        DcacheCorruptBeat corrupt_beat = DcacheCorruptBeat::all;
        if (pending_response_error_) {
            const bool address_matches = !pending_response_error_->address ||
                ((*pending_response_error_->address & ~(transfer_bytes - 1)) ==
                 base);
            const bool request_matches = !pending_response_error_->cmo_only ||
                (request.opcode >= 12 && request.opcode <= 14);
            if (address_matches && request_matches) {
                denied = pending_response_error_->denied;
                corrupt = pending_response_error_->corrupt;
                corrupt_beat = pending_response_error_->corrupt_beat;
                pending_response_error_.reset();
                ++error_response_request_count_;
                last_error_response_address_ = request.address;
                last_error_response_keyword_ = request.keyword;
            }
        }
        switch (request.opcode) {
        case 4: { // Get -> AccessAckData
            ++get_count_;
            const std::uint64_t beat_base = request.address & ~(kBeatBytes - 1);
            push_response(DBeat{
                1, 0, request.size, request.source, 0, request.keyword,
                memory_.read_beat(beat_base, kBeatBytes), denied,
                corrupt || denied,
            }, true);
            break;
        }
        case 6: { // AcquireBlock -> GrantData
            ++refill_count_;
            error_response_refill_count_ += denied || corrupt;
            if (request.keyword) {
                ++keyword_refill_count_;
            } else {
                ++nonkeyword_refill_count_;
            }
            const std::uint8_t cap = request.param == 0 ? 1 : 0;
            const std::uint16_t sink =
                static_cast<std::uint16_t>(1U + request.source);
            const std::size_t beats = static_cast<std::size_t>(
                transfer_bytes > kBeatBytes ? transfer_bytes / kBeatBytes : 1);
            for (std::size_t beat = 0; beat < beats; ++beat) {
                const std::size_t memory_beat = request.keyword ? beat ^ 1U : beat;
                const bool selected_corrupt_beat =
                    corrupt_beat == DcacheCorruptBeat::all ||
                    (corrupt_beat == DcacheCorruptBeat::first && beat == 0) ||
                    (corrupt_beat == DcacheCorruptBeat::last &&
                     beat + 1 == beats);
                push_response(DBeat{
                    5, cap, request.size, request.source, sink, request.keyword,
                    memory_.read_beat(base + memory_beat * kBeatBytes, kBeatBytes),
                    denied, denied || (corrupt && selected_corrupt_beat),
                    beat + 1 == beats,
                }, beat == 0);
            }
            expected_grant_acks_.emplace_back(sink, denied || corrupt);
            grants_.push_back({request_line, sink});
            if (!denied && !corrupt) {
                auto owner = owners_.find(request_line);
                if (owner == owners_.end() || !owner->second.present ||
                    owner->second.alias != request.alias) {
                    owners_[request_line] = Owner{request.alias, cap, true, {}, {}};
                } else {
                    owner->second.permission = cap;
                }
            }
            break;
        }
        case 7: { // AcquirePerm -> Grant
            ++acquire_perm_count_;
            if (corrupt) {
                error_.record("cannot inject corrupt on a data-less DCache Grant");
                return;
            }
            const std::uint16_t sink =
                static_cast<std::uint16_t>(1U + request.source);
            push_response(DBeat{
                4, 0, request.size, request.source, sink, request.keyword,
                std::vector<unsigned char>(kBeatBytes, 0),
                denied, false,
            }, true);
            expected_grant_acks_.emplace_back(sink, denied);
            grants_.push_back({request_line, sink});
            if (!denied) {
                auto &owner = owners_[request_line];
                owner.alias = request.alias;
                owner.permission = 0;
                owner.present = true;
            }
            break;
        }
        case 12: // CBOClean -> CBOAck
        case 13: // CBOFlush -> CBOAck
        case 14: { // CBOInval -> CBOAck
            const unsigned operation = request.opcode - 12U;
            ++cmo_counts_.at(operation);
            last_cmo_address_ = request.address;
            last_cmo_source_ = request.source;
            push_response(DBeat{
                8, 0, request.size, request.source, 0, request.keyword,
                std::vector<unsigned char>(kBeatBytes, 0), denied, corrupt,
            }, true, forced_next_cmo_response_delay_);
            forced_next_cmo_response_delay_.reset();
            break;
        }
        default: {
            std::ostringstream message;
            message << "unsupported DCache TileLink A opcode "
                    << static_cast<unsigned>(request.opcode);
            error_.record(message.str());
            break;
        }
        }
    }

    void configure_response_error(
        std::optional<std::uint64_t> address, bool denied, bool corrupt,
        DcacheCorruptBeat corrupt_beat, bool cmo_only = false)
    {
        if (!denied && !corrupt) {
            throw std::invalid_argument(
                "DCache response error injection requires denied or corrupt");
        }
        if ((!corrupt || denied) &&
            corrupt_beat != DcacheCorruptBeat::all) {
            throw std::invalid_argument(
                "DCache per-beat selection requires independent corrupt");
        }
        pending_response_error_ = PendingResponseError{
            address, denied, corrupt, corrupt_beat, cmo_only};
    }

    void accept_probe_response(const CRequest &response)
    {
        const auto expected_it = std::find_if(
            probe_responses_.begin(), probe_responses_.end(),
            [&](const auto &expected) {
                return expected.base == response.address &&
                       expected.b_accepted;
            });
        if (expected_it == probe_responses_.end()) {
            const auto unaccepted = std::find_if(
                probe_responses_.begin(), probe_responses_.end(),
                [&](const auto &expected) {
                    return expected.base == response.address;
                });
            error_.record(unaccepted == probe_responses_.end()
                ? "unexpected DCache ProbeAck address"
                : "DCache ProbeAck completed before its B Probe was accepted");
            return;
        }
        ProbeResponseState &expected = *expected_it;
        if (expected.received == 0) {
            expected.source = response.source;
        } else if (response.source != expected.source) {
            std::ostringstream message;
            message << "DCache ProbeAck multibeat source mismatch"
                    << " base=0x" << std::hex << response.address << std::dec
                    << " beat=" << expected.received
                    << " source=" << unsigned(response.source) << '/'
                    << unsigned(expected.source);
            error_.record(message.str());
            return;
        }
        const bool with_data = expected.automatic ? response.opcode == 5
            : !expected.expected_data.empty();
        const std::uint8_t expected_opcode = with_data ? 5 : 4;
        // Forced-data toN: a surviving T/B returns its full line; an evicted
        // copy may answer NtoN without data. Never accept a permission upgrade.
        const bool automatic_report = expected.automatic &&
            ((response.opcode == 4 && response.param == 5) ||
             (response.opcode == 5 &&
                (response.param == 2 ||
                 (response.param == 1 && expected.permission == 0))));
        if ((expected.automatic ? !automatic_report :
                response.opcode != expected_opcode || response.param != expected.report) ||
            (expected.received != 0 &&
                (response.opcode != expected.response_opcode ||
                 response.param != expected.response_report)) ||
            response.size != 6 || response.address != expected.base ||
            response.corrupt) {
            std::ostringstream message;
            message << "DCache ProbeAck identity or permission mismatch"
                    << " expected_opcode="
                    << static_cast<unsigned>(expected_opcode)
                    << " actual_opcode="
                    << static_cast<unsigned>(response.opcode)
                    << " expected_param="
                    << static_cast<unsigned>(expected.report)
                    << " actual_param="
                    << static_cast<unsigned>(response.param)
                    << " expected_size=6 actual_size="
                    << static_cast<unsigned>(response.size)
                    << " expected_address=0x" << std::hex << expected.base
                    << " actual_address=0x" << response.address << std::dec
                    << " corrupt=" << response.corrupt
                    << " beat=" << expected.received;
            error_.record(message.str());
            return;
        }
        expected.response_opcode = response.opcode;
        expected.response_report = response.param;
        if (with_data) {
            const std::size_t offset = expected.received * kBeatBytes;
            if (offset + kBeatBytes > expected.expected_data.size()) {
                error_.record("DCache ProbeAckData exceeded one cache line");
                return;
            }
            for (std::size_t byte = 0; byte < kBeatBytes; ++byte) {
                if (response.data.at(byte) != expected.expected_data[offset + byte]) {
                    std::ostringstream message;
                    message << "DCache ProbeAckData byte mismatch base=0x"
                            << std::hex << expected.base << " beat=" << std::dec
                            << expected.received << " byte=" << byte;
                    error_.record(message.str());
                    return;
                }
                memory_.write_byte(
                    expected.base + offset + byte, response.data.at(byte));
            }
            ++expected.received;
            if (expected.received != kLineBytes / kBeatBytes) {
                return;
            }
            ++probe_data_count_;
        }
        shrink_owner(expected.base, response.param);
        alias_recall_response_count_ += expected.automatic;
        probe_responses_.erase(expected_it);
        ++probe_response_count_;
        complete_release_ready_window_transaction();
    }

    // A cache-line eviction may observe a subset of still-propagating stores.
    // Each naturally aligned update is indivisible. If a later selected update
    // overlaps an older one, the older update must also have taken effect.
    // Disjoint updates may propagate independently. Payloads are input-derived.
    static bool permitted_release_image(const ReleaseDataState &state)
    {
        std::set<std::pair<std::size_t, std::uint64_t>> rejected;
        const auto search = [&](auto &&self, std::size_t count,
                                std::uint64_t resolved) -> bool {
            if (count == 0) {
                for (unsigned b = 0; b < kLineBytes; ++b) {
                    if (((resolved >> b) & 1U) == 0 &&
                        state.actual[b] != state.baseline[b]) return false;
                }
                return true;
            }
            const auto key = std::make_pair(count, resolved);
            if (rejected.contains(key)) return false;
            const auto &update = state.updates[count - 1U];
            const bool required = (update.mask & resolved) != 0;
            bool matches = true;
            for (unsigned b = 0; b < kLineBytes; ++b) {
                if (((update.mask & ~resolved) >> b) & 1U) {
                    matches &= state.actual[b] == update.bytes[b];
                }
            }
            if (matches && self(self, count - 1U, resolved | update.mask)) return true;
            if (!required && self(self, count - 1U, resolved)) return true;
            rejected.insert(key);
            return false;
        };
        return search(search, state.updates.size(), 0);
    }

    void accept_release(const CRequest &request)
    {
        if (request.opcode == 6) { // Release -> ReleaseAck
            // This DUT's voluntary releases are full-line with the shrink
            // report subset 0..2.  `corrupt` is not a data-less Release
            // validity field here (it is only meaningful on *Data messages).
            if (request.size != 6 ||
                (request.address & (kLineBytes - 1U)) != 0 ||
                request.param > 2) {
                std::ostringstream message;
                message << "invalid DCache Release identity"
                        << " size=" << unsigned(request.size)
                        << " addr=0x" << std::hex << request.address
                        << std::dec << " param="
                        << unsigned(request.param)
                        << " corrupt=" << request.corrupt;
                error_.record(message.str());
                return;
            }
            push_response(DBeat{
                6, 0, request.size, request.source, 0, request.keyword,
                std::vector<unsigned char>(kBeatBytes, 0),
            }, true);
            if (observe_releases_) {
                release_observation_.push_back(
                    {request.opcode, request.size, request.address,
                     current_release_ready_window_stalls()});
                observed_release_index_.record(
                    request.opcode, request.size, request.address);
            }
            shrink_owner(request.address, request.param);
            complete_release_ready_window_transaction();
            ++release_count_;
            return;
        }
        if (request.opcode != 7) { // ReleaseData -> ReleaseAck
            std::ostringstream message;
            message << "unsupported DCache TileLink C opcode "
                    << static_cast<unsigned>(request.opcode);
            error_.record(message.str());
            return;
        }

        const std::uint64_t transfer_bytes = std::uint64_t{1} << request.size;
        const std::uint64_t base = request.address & ~(transfer_bytes - 1);
        const std::size_t beats = static_cast<std::size_t>(
            transfer_bytes > kBeatBytes ? transfer_bytes / kBeatBytes : 1);
        if (!release_data_) {
            release_data_ = ReleaseDataState{
                base, request.address, request.size, request.source,
                request.param, beats, 0, {}, {}, {}};
            const auto owner = owners_.find(base);
            if (!expected_release_lines_.contains(base) && owner != owners_.end() &&
                !owner->second.expected_data.empty()) {
                release_data_->baseline = owner->second.expected_data;
                release_data_->updates = owner->second.updates;
            }
        }
        if (release_data_->base != base ||
            release_data_->address != request.address ||
            release_data_->size != request.size ||
            release_data_->source != request.source ||
            release_data_->param != request.param ||
            release_data_->beats != beats) {
            error_.record("interleaved or inconsistent DCache ReleaseData transaction");
            return;
        }
        const auto expected = expected_release_lines_.find(base);
        const bool has_expected_line = expected != expected_release_lines_.end();
        const std::size_t offset = release_data_->received * kBeatBytes;
        if (has_expected_line && offset + kBeatBytes > expected->second.size()) {
            error_.record("ReleaseData exceeded expected immutable line image");
            return;
        }
        for (std::size_t byte = 0; byte < kBeatBytes; ++byte) {
            const unsigned char expected_byte = has_expected_line
                ? expected->second[offset + byte]
                : reference_memory_.read_byte(base + offset + byte);
            if (release_data_->baseline.empty() && request.data.at(byte) != expected_byte) {
                std::ostringstream message;
                message << "ReleaseData byte mismatch base=0x" << std::hex
                        << base << " beat=" << std::dec
                        << release_data_->received << " byte=" << byte
                        << " expected=0x" << std::hex
                        << static_cast<unsigned>(expected_byte)
                        << " actual=0x"
                        << static_cast<unsigned>(request.data.at(byte));
                error_.record(message.str());
                return;
            }
        }
        release_data_->actual.insert(release_data_->actual.end(),
            request.data.begin(), request.data.end());
        ++release_data_->received;
        if (release_data_->received == release_data_->beats) {
            if (!release_data_->baseline.empty() &&
                (release_data_->actual.size() != kLineBytes ||
                 !permitted_release_image(*release_data_))) {
                error_.record("ReleaseData is not a permitted independent owner image");
                return;
            }
            for (unsigned byte = 0; byte < release_data_->actual.size(); ++byte) {
                memory_.write_byte(base + byte, release_data_->actual[byte]);
            }
            push_response(DBeat{
                6, 0, request.size, request.source, 0, request.keyword,
                std::vector<unsigned char>(kBeatBytes, 0),
            }, true);
            shrink_owner(base, request.param);
            release_data_.reset();
            ++release_data_verified_count_;
            if (has_expected_line) {
                ++expected_release_data_verified_count_;
                expected_release_lines_.erase(base);
            }
            if (observe_releases_) {
                release_observation_.push_back(
                    {request.opcode, request.size, request.address,
                     current_release_ready_window_stalls()});
                observed_release_index_.record(
                    request.opcode, request.size, request.address);
            }
            complete_release_ready_window_transaction();
            ++release_count_;
            ++release_data_count_;
        }
    }

    std::uint64_t current_release_ready_window_stalls() const
    {
        return release_ready_window_active_ &&
                !release_ready_window_first_complete_ &&
                release_ready_window_target_selected_
            ? release_ready_window_stall_cycles_
            : 0;
    }

    bool is_release_ready_window_target(const CRequest &request) const
    {
        if ((request.opcode != 6 && request.opcode != 7) ||
            request.size != 6) {
            return false;
        }
        const std::uint64_t line = request.address & ~(kLineBytes - 1U);
        return std::find(
                   release_ready_window_target_lines_.begin(),
                   release_ready_window_target_lines_.end(), line) !=
            release_ready_window_target_lines_.end();
    }

    void complete_release_ready_window_transaction()
    {
        if (!release_ready_window_active_) {
            return;
        }
        if (release_ready_window_target_selected_) {
            ++release_ready_window_completed_releases_;
            release_ready_window_first_complete_ = true;
            release_ready_window_target_selected_ = false;
        } else if (release_ready_window_bypass_non_target_) {
            release_ready_window_bypass_non_target_ = false;
        }
    }

    std::uint64_t next_random()
    {
        random_state_ ^= random_state_ << 13;
        random_state_ ^= random_state_ >> 7;
        random_state_ ^= random_state_ << 17;
        return random_state_;
    }

    std::uint64_t next_e_random()
    {
        e_random_state_ ^= e_random_state_ << 13;
        e_random_state_ ^= e_random_state_ >> 7;
        e_random_state_ ^= e_random_state_ << 17;
        return e_random_state_;
    }

    unsigned response_delay(bool first_beat)
    {
        if (first_beat && forced_next_response_delay_) {
            const unsigned delay = *forced_next_response_delay_;
            forced_next_response_delay_.reset();
            response_latency_stats_.sample(delay);
            return delay;
        }
        if (!first_beat && forced_next_interbeat_delay_) {
            const unsigned delay = *forced_next_interbeat_delay_;
            forced_next_interbeat_delay_.reset();
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

    void push_response(
        DBeat response, bool first_beat,
        std::optional<unsigned> forced_delay = std::nullopt)
    {
        response.first_beat = first_beat;
        if (first_beat && forced_delay) {
            response.delay_before = *forced_delay;
            response_latency_stats_.sample(*forced_delay);
        } else {
            response.delay_before = response_delay(first_beat);
        }
        if (first_beat && hold_next_response_address_) {
            held_responses_.push_back(
                HeldResponse{*hold_next_response_address_, {}});
            building_held_response_address_ = hold_next_response_address_;
            hold_next_response_address_.reset();
        }
        if (building_held_response_address_) {
            const auto held = std::find_if(
                held_responses_.begin(), held_responses_.end(),
                [&](const HeldResponse &candidate) {
                    return candidate.address ==
                        *building_held_response_address_;
                });
            if (held == held_responses_.end()) {
                error_.record("DCache held response builder lost its transaction");
                building_held_response_address_.reset();
                return;
            }
            const bool last_beat = response.last_beat;
            held->beats.push_back(std::move(response));
            if (last_beat) {
                building_held_response_address_.reset();
            }
            return;
        }
        const bool was_empty = d_beats_.empty();
        d_beats_.push_back(std::move(response));
        if (was_empty) {
            d_presenting_ = false;
            d_gap_ = d_beats_.front().delay_before;
        }
    }

    SparseMemory &memory_;
    const SparseMemory &reference_memory_;
    std::unordered_map<std::uint64_t, Owner> owners_;
    std::deque<PendingAcquire> pending_acquires_;
    std::vector<GrantLifetime> grants_;
    std::uint64_t alias_recall_count_ = 0;
    std::uint64_t alias_recall_response_count_ = 0;
    std::deque<BBeat> b_beats_;
    std::deque<DBeat> d_beats_;
    std::deque<HeldResponse> held_responses_;
    std::deque<ProbeResponseState> probe_responses_;
    std::deque<std::pair<std::uint16_t, bool>> expected_grant_acks_;
    std::array<bool, 1024> sink_busy_{};
    std::optional<std::size_t> accepted_grant_ack_index_;
    std::optional<ARequest> captured_a_;
    std::optional<CRequest> captured_c_;
    std::optional<CRequest> stalled_c_request_;
    std::vector<ARequest> request_history_;
    std::array<std::uint64_t, 7> request_classes_{};
    std::vector<ReleaseRecord> release_observation_;
    DcacheReleaseIndex observed_release_index_;
    bool observe_releases_ = false;
    std::optional<ReleaseDataState> release_data_;
    std::unordered_map<std::uint64_t, std::vector<unsigned char>>
        expected_release_lines_;
    bool a_fire_ = false;
    bool b_fire_ = false;
    bool c_fire_ = false;
    bool d_fire_ = false;
    bool e_fire_ = false;
    std::uint64_t request_count_ = 0;
    std::uint64_t last_request_address_ = 0;
    std::uint64_t get_count_ = 0;
    std::uint64_t refill_count_ = 0;
    std::uint64_t keyword_refill_count_ = 0;
    std::uint64_t nonkeyword_refill_count_ = 0;
    std::uint64_t acquire_perm_count_ = 0;
    std::array<std::uint64_t, 3> cmo_counts_{};
    std::uint64_t last_cmo_address_ = 0;
    std::uint8_t last_cmo_source_ = 0;
    std::uint64_t release_count_ = 0;
    std::uint64_t release_data_count_ = 0;
    std::uint64_t release_data_verified_count_ = 0;
    std::uint64_t expected_release_data_verified_count_ = 0;
    std::uint64_t probe_request_count_ = 0;
    std::uint64_t probe_response_count_ = 0;
    std::uint64_t probe_canceled_count_ = 0;
    std::uint64_t probe_data_count_ = 0;
    std::uint64_t probe_source_count_ = 0;
    std::uint64_t probe_source_reuse_count_ = 0;
    std::uint64_t probe_source_wrap_count_ = 0;
    std::uint64_t max_probe_outstanding_ = 0;
    std::uint64_t probe_stall_cycles_ = 0;
    std::uint64_t grant_ack_count_ = 0;
    std::uint64_t grant_data_beat_count_ = 0;
    std::uint64_t denied_d_beat_count_ = 0;
    std::uint64_t corrupt_d_beat_count_ = 0;
    std::uint64_t error_response_request_count_ = 0;
    std::uint64_t error_response_refill_count_ = 0;
    std::uint64_t error_response_grant_ack_count_ = 0;
    std::uint64_t last_error_response_address_ = 0;
    bool last_error_response_keyword_ = false;
    std::uint64_t outstanding_requests_ = 0;
    std::uint64_t max_outstanding_requests_ = 0;
    std::uint64_t grant_ack_stall_cycles_ = 0;
    std::uint64_t c_payload_stability_checks_ = 0;
    std::uint64_t release_target_payload_stability_checks_ = 0;
    std::uint8_t next_probe_source_ = 0;
    std::array<bool, memblock::kDcacheProbeSourceCount> probe_sources_seen_{};
    std::optional<std::uint8_t> last_accepted_probe_source_;
    std::uint64_t random_state_ = 1;
    std::uint64_t e_random_state_ = 1;
    unsigned d_gap_ = 0;
    bool random_backpressure_ = false;
    ResponseLatencyProfile latency_profile_ = ResponseLatencyProfile::compact;
    ResponseLatencyStats response_latency_stats_;
    bool force_a_stall_ = false;
    bool force_e_stall_ = false;
    bool d_presenting_ = false;
    std::array<bool, 64> active_request_sources_{};
    std::optional<PendingResponseError> pending_response_error_;
    std::optional<unsigned> forced_next_response_delay_;
    std::optional<unsigned> forced_next_cmo_response_delay_;
    std::optional<unsigned> forced_next_interbeat_delay_;
    std::vector<std::uint64_t> pending_held_response_addresses_;
    std::optional<std::uint64_t> hold_next_response_address_;
    std::optional<std::uint64_t> building_held_response_address_;
    std::optional<std::pair<std::uint64_t, unsigned>>
        scheduled_held_response_release_;
    std::optional<std::uint64_t> hold_c_until_probe_requests_;
    bool release_ready_window_active_ = false;
    bool release_ready_window_first_complete_ = false;
    bool release_ready_window_target_selected_ = false;
    bool release_ready_window_bypass_non_target_ = false;
    std::vector<std::uint64_t> release_ready_window_target_lines_;
    unsigned release_ready_window_stall_remaining_ = 0;
    std::uint64_t release_ready_window_stall_cycles_ = 0;
    std::uint64_t release_ready_window_completed_releases_ = 0;
    unsigned release_channel_idle_cycles_ = 0;
    bool c_valid_sampled_ = false;
    std::uint64_t request_stall_cycles_ = 0;
    std::uint64_t response_delay_cycles_ = 0;
    FailureState error_{"dcache", "manager-contract"};
};
