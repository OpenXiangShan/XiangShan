// This file is included inside memblock_env.hpp's memblock namespace.

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
            error_ = "DCache response hold address is already active";
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
            error_ = "no complete held DCache response to release";
            return;
        }
        release_held_response_at(held_responses_.front().address);
    }

    void release_held_response_at(std::uint64_t address)
    {
        const std::uint64_t line = address & ~(kLineBytes - 1U);
        if (scheduled_held_response_release_ &&
            scheduled_held_response_release_->first == line) {
            error_ = "DCache held response already has a scheduled release";
            return;
        }
        const auto held = std::find_if(
            held_responses_.begin(), held_responses_.end(),
            [&](const HeldResponse &response) {
                return response.address == line;
            });
        if (held == held_responses_.end() ||
            building_held_response_address_ == line) {
            error_ = "no complete held DCache response at requested address";
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
            error_ = "scheduled DCache response release requires at least one tick";
            return;
        }
        if (scheduled_held_response_release_) {
            error_ = "a DCache held response release is already scheduled";
            return;
        }
        if (held == held_responses_.end() || held->beats.empty() ||
            building_held_response_address_ == line) {
            error_ = "no complete held DCache response to schedule for release";
            return;
        }
        scheduled_held_response_release_ = std::make_pair(line, after_ticks);
    }

    void begin_release_ready_window(
        const std::vector<std::uint64_t> &target_lines,
        unsigned stall_valid_cycles)
    {
        if (release_ready_window_active_) {
            error_ = "DCache release ready window is already active";
            return;
        }
        if (target_lines.empty()) {
            error_ = "DCache release ready window has no target lines";
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
            error_ = message.str();
            return;
        }
        release_ready_window_active_ = false;
        release_ready_window_target_lines_.clear();
    }

    void hold_c_until_probe_requests(std::uint64_t target_requests)
    {
        if (hold_c_until_probe_requests_ ||
            target_requests <= probe_request_count_) {
            error_ = "invalid DCache Probe C hold target";
            return;
        }
        hold_c_until_probe_requests_ = target_requests;
    }

    void reset_link_state()
    {
        b_beats_.clear();
        d_beats_.clear();
        held_responses_.clear();
        probe_responses_.clear();
        expected_grant_acks_.clear();
        accepted_grant_ack_index_.reset();
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

    void drive(UTMemBlock &dut)
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
        dut.auto_inner_dcache_client_out_a_ready.ImmSet(accept_a);
        dut.auto_inner_dcache_client_out_c_ready.ImmSet(accept_c);
        dut.auto_inner_dcache_client_out_e_ready.ImmSet(accept_e);
        if (b_beats_.empty()) {
            dut.auto_inner_dcache_client_out_b_valid.ImmSet(std::uint64_t{0});
        } else {
            const BBeat &beat = b_beats_.front();
            dut.auto_inner_dcache_client_out_b_bits_opcode.ImmSet(beat.opcode);
            dut.auto_inner_dcache_client_out_b_bits_param.ImmSet(beat.param);
            dut.auto_inner_dcache_client_out_b_bits_size.ImmSet(beat.size);
            dut.auto_inner_dcache_client_out_b_bits_source.ImmSet(beat.source);
            dut.auto_inner_dcache_client_out_b_bits_address.ImmSet(beat.address);
            dut.auto_inner_dcache_client_out_b_bits_mask.ImmSet(beat.mask);
            auto bytes = beat.data;
            dut.auto_inner_dcache_client_out_b_bits_data.ImmSetBytes(bytes);
            dut.auto_inner_dcache_client_out_b_bits_corrupt.ImmSet(beat.corrupt);
            dut.auto_inner_dcache_client_out_b_valid.ImmSet(std::uint64_t{1});
        }
        if (d_beats_.empty()) {
            dut.auto_inner_dcache_client_out_d_valid.ImmSet(std::uint64_t{0});
            d_presenting_ = false;
            return;
        }

        if (!d_presenting_) {
            if (d_gap_ != 0) {
                --d_gap_;
                ++response_delay_cycles_;
                dut.auto_inner_dcache_client_out_d_valid.ImmSet(std::uint64_t{0});
                return;
            }
            d_presenting_ = true;
        }

        const DBeat &beat = d_beats_.front();
        dut.auto_inner_dcache_client_out_d_bits_opcode.ImmSet(beat.opcode);
        dut.auto_inner_dcache_client_out_d_bits_param.ImmSet(beat.param);
        dut.auto_inner_dcache_client_out_d_bits_size.ImmSet(beat.size);
        dut.auto_inner_dcache_client_out_d_bits_source.ImmSet(beat.source);
        dut.auto_inner_dcache_client_out_d_bits_sink.ImmSet(beat.sink);
        dut.auto_inner_dcache_client_out_d_bits_denied.ImmSet(beat.denied);
        dut.auto_inner_dcache_client_out_d_bits_echo_isKeyword.ImmSet(beat.keyword);
        auto bytes = beat.data;
        dut.auto_inner_dcache_client_out_d_bits_data.ImmSetBytes(bytes);
        dut.auto_inner_dcache_client_out_d_bits_corrupt.ImmSet(beat.corrupt);
        dut.auto_inner_dcache_client_out_d_valid.ImmSet(std::uint64_t{1});
    }

    void capture_before_tick(UTMemBlock &dut)
    {
        const bool a_valid = dut.auto_inner_dcache_client_out_a_valid.B();
        const bool a_ready = dut.auto_inner_dcache_client_out_a_ready.B();
        if (a_valid && !a_ready) {
            ++request_stall_cycles_;
            force_a_stall_ = false;
        }
        a_fire_ = a_valid && a_ready;
        if (a_fire_) {
            captured_a_ = ARequest{
                static_cast<std::uint8_t>(dut.auto_inner_dcache_client_out_a_bits_opcode.U()),
                static_cast<std::uint8_t>(dut.auto_inner_dcache_client_out_a_bits_param.U()),
                static_cast<std::uint8_t>(dut.auto_inner_dcache_client_out_a_bits_size.U()),
                static_cast<std::uint8_t>(dut.auto_inner_dcache_client_out_a_bits_source.U()),
                dut.auto_inner_dcache_client_out_a_bits_address.U(),
                dut.auto_inner_dcache_client_out_a_bits_echo_isKeyword.B(),
            };
            const bool cmo_opcode = captured_a_->opcode >= 12 &&
                captured_a_->opcode <= 14;
            if (captured_a_->opcode != 4 && captured_a_->opcode != 6 &&
                captured_a_->opcode != 7 && !cmo_opcode) {
                error_ = "unsupported DCache TileLink A opcode";
            }
            if (captured_a_->size > 6) {
                error_ = "oversized DCache TileLink A request";
            }
            if (cmo_opcode &&
                (captured_a_->size != 6 ||
                 captured_a_->source != kDcacheCmoSource ||
                 (captured_a_->address & (kLineBytes - 1)) != 0)) {
                error_ = "malformed DCache CMO request identity";
            }
        }
        const bool b_valid = dut.auto_inner_dcache_client_out_b_valid.B();
        const bool b_ready = dut.auto_inner_dcache_client_out_b_ready.B();
        if (b_valid && !b_ready) {
            ++probe_stall_cycles_;
        }
        b_fire_ = b_valid && b_ready;
        const bool c_valid = dut.auto_inner_dcache_client_out_c_valid.B();
        const bool c_ready = dut.auto_inner_dcache_client_out_c_ready.B();
        std::optional<CRequest> current_c;
        if (c_valid) {
            current_c = CRequest{
                static_cast<std::uint8_t>(
                    dut.auto_inner_dcache_client_out_c_bits_opcode.U()),
                static_cast<std::uint8_t>(
                    dut.auto_inner_dcache_client_out_c_bits_param.U()),
                static_cast<std::uint8_t>(
                    dut.auto_inner_dcache_client_out_c_bits_size.U()),
                static_cast<std::uint8_t>(
                    dut.auto_inner_dcache_client_out_c_bits_source.U()),
                dut.auto_inner_dcache_client_out_c_bits_address.U(),
                static_cast<std::uint8_t>(
                    dut.auto_inner_dcache_client_out_c_bits_user_alias.U()),
                dut.auto_inner_dcache_client_out_c_bits_user_vaddr.U(),
                static_cast<std::uint8_t>(
                    dut.auto_inner_dcache_client_out_c_bits_user_reqSource.U()),
                dut.auto_inner_dcache_client_out_c_bits_user_needHint.B(),
                dut.auto_inner_dcache_client_out_c_bits_echo_isKeyword.B(),
                dut.auto_inner_dcache_client_out_c_bits_corrupt.B(),
                dut.auto_inner_dcache_client_out_c_bits_data.GetBytes(),
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
                error_ = "DCache TileLink C dropped valid while stalled";
            } else if (*current_c != *stalled_c_request_) {
                error_ = "DCache TileLink C changed payload while stalled";
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
                error_ = "unsupported DCache TileLink C opcode";
            }
            if (captured_c_->size > 6) {
                error_ = "oversized DCache TileLink C request";
            }
        }
        const bool e_valid = dut.auto_inner_dcache_client_out_e_valid.B();
        const bool e_ready = dut.auto_inner_dcache_client_out_e_ready.B();
        if (e_valid && !e_ready) {
            ++grant_ack_stall_cycles_;
            force_e_stall_ = false;
        }
        e_fire_ = e_valid && e_ready;
        if (e_fire_) {
            const std::uint16_t sink = static_cast<std::uint16_t>(
                dut.auto_inner_dcache_client_out_e_bits_sink.U());
            const auto expected = std::find_if(
                expected_grant_acks_.begin(), expected_grant_acks_.end(),
                [&](const auto &entry) { return entry.first == sink; });
            if (expected == expected_grant_acks_.end()) {
                error_ = "unexpected DCache TileLink E GrantAck";
            } else {
                accepted_grant_ack_index_ = static_cast<std::size_t>(
                    std::distance(expected_grant_acks_.begin(), expected));
            }
        }
        d_fire_ = !d_beats_.empty() &&
                  dut.auto_inner_dcache_client_out_d_valid.B() &&
                  dut.auto_inner_dcache_client_out_d_ready.B();
        if (d_fire_) {
            const DBeat &expected = d_beats_.front();
            const auto actual_data =
                dut.auto_inner_dcache_client_out_d_bits_data.GetBytes();
            if (dut.auto_inner_dcache_client_out_d_bits_opcode.U() != expected.opcode ||
                dut.auto_inner_dcache_client_out_d_bits_param.U() != expected.param ||
                dut.auto_inner_dcache_client_out_d_bits_size.U() != expected.size ||
                dut.auto_inner_dcache_client_out_d_bits_source.U() != expected.source ||
                dut.auto_inner_dcache_client_out_d_bits_sink.U() != expected.sink ||
                dut.auto_inner_dcache_client_out_d_bits_echo_isKeyword.B() != expected.keyword ||
                dut.auto_inner_dcache_client_out_d_bits_denied.B() != expected.denied ||
                dut.auto_inner_dcache_client_out_d_bits_corrupt.B() != expected.corrupt ||
                actual_data != expected.data) {
                error_ = "DCache TileLink D response identity or payload mismatch";
            }
        }
    }

    void update_after_tick()
    {
        if (b_fire_) {
            const std::uint8_t source = b_beats_.front().source;
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
                    error_ = "DCache TileLink D completed an inactive A source";
                } else {
                    active_request_sources_[response.source] = false;
                    --outstanding_requests_;
                }
            }
            d_beats_.pop_front();
            d_presenting_ = false;
            d_gap_ = d_beats_.empty() ? 0 : d_beats_.front().delay_before;
        }
        if (a_fire_ && captured_a_) {
            if (captured_a_->source >= active_request_sources_.size() ||
                active_request_sources_[captured_a_->source]) {
                error_ = "DCache TileLink A source reused before D completion";
            } else {
                active_request_sources_[captured_a_->source] = true;
                ++outstanding_requests_;
                max_outstanding_requests_ = std::max(
                    max_outstanding_requests_, outstanding_requests_);
            }
            last_request_address_ = captured_a_->address;
            request_history_.push_back(*captured_a_);
            respond(*captured_a_);
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
            expected_grant_acks_.erase(expected);
            ++grant_ack_count_;
        }
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

    bool ok() const { return error_.empty(); }
    const std::string &error() const { return error_; }
    std::uint64_t request_count() const { return request_count_; }
    std::uint64_t last_request_address() const { return last_request_address_; }
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
    void begin_release_observation()
    {
        release_observation_.clear();
        observe_releases_ = true;
    }
    void end_release_observation()
    {
        observe_releases_ = false;
        release_observation_.clear();
    }
    std::uint64_t observed_release_covering_count(
        std::uint64_t address, bool data_only = false) const
    {
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
            error_ = "invalid DCache Probe request or expectation";
            return false;
        }
        std::vector<unsigned char> data(kBeatBytes, 0);
        // XiangShan carries the virtual-index alias in B.data[2:1] and the
        // manager's data request in B.data[0]. Bare mappings use PA == VA.
        data[0] = static_cast<unsigned char>(
            (need_data ? 1U : 0U) | ((address >> 11) & 0x6U));
        const std::uint8_t source = next_probe_source_;
        if (std::any_of(
                probe_responses_.begin(), probe_responses_.end(),
                [source](const auto &response) {
                    return response.b_source == source;
                })) {
            error_ = "DCache Probe source reused while still outstanding";
            return false;
        }
        next_probe_source_ = static_cast<std::uint8_t>(
            (next_probe_source_ + 1U) &
            (memblock::kDcacheProbeSourceCount - 1U));
        b_beats_.push_back(BBeat{
            6, cap, 6, source, address, 0xffffffffU,
            std::move(data), false});
        probe_responses_.push_back(ProbeResponseState{
            source, address, expected_report, expected_data, 0});
        return true;
    }

    bool probes_idle() const
    {
        return b_beats_.empty() && probe_responses_.empty();
    }
    bool grant_acks_idle() const
    {
        return expected_grant_acks_.empty();
    }
    bool responses_idle() const
    {
        return d_beats_.empty() && held_responses_.empty() &&
            pending_held_response_addresses_.empty() &&
            !hold_next_response_address_ && !building_held_response_address_ &&
            !scheduled_held_response_release_ &&
            !release_data_ && outstanding_requests_ == 0;
    }
    void expect_release_line(
        std::uint64_t base, const std::vector<unsigned char> &bytes)
    {
        if (bytes.empty() || bytes.size() % kBeatBytes != 0) {
            error_ = "expected ReleaseData line is not beat sized";
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
                error_ = message.str();
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

    struct ARequest {
        std::uint8_t opcode;
        std::uint8_t param;
        std::uint8_t size;
        std::uint8_t source;
        std::uint64_t address;
        bool keyword;
    };

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

    struct ReleaseDataState {
        std::uint64_t base;
        std::uint8_t size;
        std::uint8_t source;
        bool keyword;
        std::size_t beats;
        std::size_t received = 0;
    };

    struct ProbeResponseState {
        std::uint8_t b_source;
        std::uint64_t base;
        std::uint8_t report;
        std::vector<unsigned char> expected_data;
        std::size_t received;
    };

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
            break;
        }
        case 7: { // AcquirePerm -> Grant
            ++acquire_perm_count_;
            if (corrupt) {
                error_ = "cannot inject corrupt on a data-less DCache Grant";
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
            error_ = message.str();
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
                return expected.base == response.address;
            });
        if (expected_it == probe_responses_.end()) {
            error_ = "unexpected DCache ProbeAck address";
            return;
        }
        ProbeResponseState &expected = *expected_it;
        const bool with_data = !expected.expected_data.empty();
        const std::uint8_t expected_opcode = with_data ? 5 : 4;
        if (response.opcode != expected_opcode || response.param != expected.report ||
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
            error_ = message.str();
            return;
        }
        if (with_data) {
            const std::size_t offset = expected.received * kBeatBytes;
            if (offset + kBeatBytes > expected.expected_data.size()) {
                error_ = "DCache ProbeAckData exceeded one cache line";
                return;
            }
            for (std::size_t byte = 0; byte < kBeatBytes; ++byte) {
                if (response.data.at(byte) != expected.expected_data[offset + byte]) {
                    std::ostringstream message;
                    message << "DCache ProbeAckData byte mismatch base=0x"
                            << std::hex << expected.base << " beat=" << std::dec
                            << expected.received << " byte=" << byte;
                    error_ = message.str();
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
        probe_responses_.erase(expected_it);
        ++probe_response_count_;
        complete_release_ready_window_transaction();
    }

    void accept_release(const CRequest &request)
    {
        if (request.opcode == 6) { // Release -> ReleaseAck
            push_response(DBeat{
                6, 0, request.size, request.source, 0, request.keyword,
                std::vector<unsigned char>(kBeatBytes, 0),
            }, true);
            if (observe_releases_) {
                release_observation_.push_back(
                    {request.opcode, request.size, request.address,
                     current_release_ready_window_stalls()});
            }
            complete_release_ready_window_transaction();
            ++release_count_;
            return;
        }
        if (request.opcode != 7) { // ReleaseData -> ReleaseAck
            std::ostringstream message;
            message << "unsupported DCache TileLink C opcode "
                    << static_cast<unsigned>(request.opcode);
            error_ = message.str();
            return;
        }

        const std::uint64_t transfer_bytes = std::uint64_t{1} << request.size;
        const std::uint64_t base = request.address & ~(transfer_bytes - 1);
        const std::size_t beats = static_cast<std::size_t>(
            transfer_bytes > kBeatBytes ? transfer_bytes / kBeatBytes : 1);
        if (!release_data_) {
            release_data_ = ReleaseDataState{
                base, request.size, request.source, request.keyword, beats, 0};
        }
        if (release_data_->base != base || release_data_->size != request.size ||
            release_data_->source != request.source ||
            release_data_->beats != beats) {
            error_ = "interleaved or inconsistent DCache ReleaseData transaction";
            return;
        }
        const auto expected = expected_release_lines_.find(base);
        const bool has_expected_line = expected != expected_release_lines_.end();
        const std::size_t offset = release_data_->received * kBeatBytes;
        if (has_expected_line && offset + kBeatBytes > expected->second.size()) {
            error_ = "ReleaseData exceeded expected immutable line image";
            return;
        }
        for (std::size_t byte = 0; byte < kBeatBytes; ++byte) {
            const unsigned char expected_byte = has_expected_line
                ? expected->second[offset + byte]
                : reference_memory_.read_byte(base + offset + byte);
            if (request.data.at(byte) != expected_byte) {
                std::ostringstream message;
                message << "ReleaseData byte mismatch base=0x" << std::hex
                        << base << " beat=" << std::dec
                        << release_data_->received << " byte=" << byte
                        << " expected=0x" << std::hex
                        << static_cast<unsigned>(expected_byte)
                        << " actual=0x"
                        << static_cast<unsigned>(request.data.at(byte));
                error_ = message.str();
                return;
            }
        }
        for (std::size_t byte = 0; byte < kBeatBytes; ++byte) {
            memory_.write_byte(
                base + release_data_->received * kBeatBytes + byte,
                request.data.at(byte));
        }
        ++release_data_->received;
        if (release_data_->received == release_data_->beats) {
            push_response(DBeat{
                6, 0, request.size, request.source, 0, request.keyword,
                std::vector<unsigned char>(kBeatBytes, 0),
            }, true);
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
                error_ = "DCache held response builder lost its transaction";
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
    std::deque<BBeat> b_beats_;
    std::deque<DBeat> d_beats_;
    std::deque<HeldResponse> held_responses_;
    std::deque<ProbeResponseState> probe_responses_;
    std::deque<std::pair<std::uint16_t, bool>> expected_grant_acks_;
    std::optional<std::size_t> accepted_grant_ack_index_;
    std::optional<ARequest> captured_a_;
    std::optional<CRequest> captured_c_;
    std::optional<CRequest> stalled_c_request_;
    std::vector<ARequest> request_history_;
    std::vector<ReleaseRecord> release_observation_;
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
    std::uint64_t request_stall_cycles_ = 0;
    std::uint64_t response_delay_cycles_ = 0;
    std::string error_;
};
