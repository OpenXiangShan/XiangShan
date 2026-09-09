// This file is included inside memblock_env.hpp's memblock namespace.

class LoadScoreboard {
public:
    struct Expected {
        std::uint64_t data;
        std::uint8_t pdest;
        bool rob_flag;
        bool prefetch;
        std::uint32_t exception_mask;
        std::uint32_t allowed_additional_exception_mask;
        bool check_data;
        bool check_data_on_exception;
        bool rf_wen;
        bool fp_wen;
        std::uint8_t trigger;
        bool flush_pipe;
        std::optional<bool> debug_is_mmio;
        std::optional<bool> debug_is_ncio;
        std::optional<bool> debug_is_perf_cnt;
        std::uint64_t address;
        std::uint16_t op;
    };

    void expect(const LoadTransaction &transaction, std::uint64_t data)
    {
        if (!transaction.check_data &&
            transaction.expected_trigger != kTriggerDebugMode) {
            if (error_.empty()) {
                error_ = "load data checking may only be disabled for DebugMode trigger";
            }
            return;
        }
        const auto [_, inserted] = expected_.emplace(
            rob_identity(transaction.rob, transaction.rob_flag),
            Expected{
                data,
                transaction.pdest,
                transaction.rob_flag,
                false,
                transaction.expected_exception_mask,
                transaction.allowed_additional_exception_mask,
                transaction.check_data,
                transaction.check_data_on_exception,
                transaction.expected_exception_mask == 0 && transaction.rf_wen,
                transaction.expected_exception_mask == 0 && transaction.fp_wen,
                transaction.expected_trigger,
                transaction.input_flush_pipe,
                transaction.expected_debug_is_mmio,
                transaction.expected_debug_is_ncio,
                transaction.expected_debug_is_perf_cnt,
                transaction.address,
                static_cast<std::uint16_t>(transaction.op),
            });
        if (!inserted && error_.empty()) {
            std::ostringstream message;
            message << "duplicate outstanding scalar load ROB value rob="
                    << static_cast<unsigned>(transaction.rob)
                    << " flag=" << transaction.rob_flag;
            error_ = message.str();
        }
    }

    void expect_prefetch(const PrefetchTransaction &transaction)
    {
        const auto [_, inserted] = expected_.emplace(
            rob_identity(transaction.rob, transaction.rob_flag),
            Expected{
                0, 0, transaction.rob_flag, true, 0, 0, false, false, false,
                false,
                transaction.expected_trigger, transaction.input_flush_pipe,
                transaction.expected_debug_is_mmio,
                transaction.expected_debug_is_ncio,
                transaction.expected_debug_is_perf_cnt,
                transaction.address, static_cast<std::uint16_t>(transaction.op)});
        if (!inserted && error_.empty()) {
            error_ = "duplicate outstanding scalar load ROB value";
        } else if (inserted) {
            ++pending_prefetch_;
        }
    }

    void observe(unsigned lane, const generated::ScalarLoadWriteback &writeback)
    {
        if (!writeback.valid) {
            return;
        }
        ++observed_;
        const auto it = expected_.find(
            rob_identity(writeback.rob_value, writeback.rob_flag));
        if (it == expected_.end()) {
            fail("unexpected load writeback", lane, writeback);
            return;
        }
        if (it->second.prefetch) {
            if (writeback.exception_mask != 0 || writeback.replay ||
                writeback.rf_wen || writeback.fp_wen ||
                writeback.trigger != it->second.trigger ||
                writeback.flush_pipe != it->second.flush_pipe ||
                writeback.rob_flag != it->second.rob_flag ||
                optional_mismatch(
                    writeback.debug_is_mmio, it->second.debug_is_mmio) ||
                optional_mismatch(
                    writeback.debug_is_ncio, it->second.debug_is_ncio) ||
                optional_mismatch(
                    writeback.debug_is_perf_cnt,
                    it->second.debug_is_perf_cnt)) {
                fail("mismatched software-prefetch completion", lane, writeback,
                     &it->second);
                return;
            }
            ++prefetch_observed_;
            --pending_prefetch_;
            expected_.erase(it);
            return;
        }
        const std::uint32_t missing_exception =
            it->second.exception_mask & ~writeback.exception_mask;
        const std::uint32_t unexpected_exception = writeback.exception_mask &
            ~(it->second.exception_mask |
              it->second.allowed_additional_exception_mask);
        if (missing_exception != 0 || unexpected_exception != 0 ||
            writeback.replay || writeback.rf_wen != it->second.rf_wen ||
            writeback.fp_wen != it->second.fp_wen ||
            writeback.trigger != it->second.trigger ||
            writeback.flush_pipe != it->second.flush_pipe ||
            writeback.pdest != it->second.pdest ||
            writeback.rob_flag != it->second.rob_flag ||
            optional_mismatch(
                writeback.debug_is_mmio, it->second.debug_is_mmio) ||
            optional_mismatch(
                writeback.debug_is_ncio, it->second.debug_is_ncio) ||
            optional_mismatch(
                writeback.debug_is_perf_cnt,
                it->second.debug_is_perf_cnt) ||
            (it->second.check_data &&
             (it->second.exception_mask == 0 ||
              it->second.check_data_on_exception) &&
             writeback.data != it->second.data)) {
            fail("mismatched load writeback", lane, writeback, &it->second);
            return;
        }
        expected_.erase(it);
    }

    bool done() const { return expected_.empty(); }
    bool pending(const LoadTransaction &transaction) const
    {
        return expected_.find(
                   rob_identity(transaction.rob, transaction.rob_flag)) !=
            expected_.end();
    }
    std::size_t pending() const { return expected_.size(); }
    std::size_t pending_prefetch() const { return pending_prefetch_; }
    std::size_t pending_load() const { return expected_.size() - pending_prefetch_; }
    const Expected *expected_for(
        const generated::ScalarLoadWriteback &writeback) const
    {
        const auto it = expected_.find(
            rob_identity(writeback.rob_value, writeback.rob_flag));
        return it == expected_.end() ? nullptr : &it->second;
    }
    bool ok() const { return error_.empty(); }
    std::uint64_t observed() const { return observed_; }
    std::uint64_t prefetch_observed() const { return prefetch_observed_; }
    const std::string &error() const { return error_; }
    std::string pending_summary() const
    {
        std::ostringstream message;
        for (const auto &[identity, expected] : expected_) {
            message << " rob=" << static_cast<unsigned>(identity.value)
                    << ':' << identity.flag
                    << " address=0x" << std::hex << expected.address
                    << " op=0x" << expected.op << std::dec
                    << " prefetch=" << expected.prefetch;
        }
        return message.str();
    }

private:
    static bool optional_mismatch(
        bool actual,
        const std::optional<bool> &expected)
    {
        return expected.has_value() && actual != *expected;
    }

    void fail(
        const char *reason,
        unsigned lane,
        const generated::ScalarLoadWriteback &actual,
        const Expected *expected = nullptr)
    {
        if (!error_.empty()) {
            return;
        }
        std::ostringstream message;
        message << reason << " lane=" << lane
                << " rob=" << static_cast<unsigned>(actual.rob_value)
                << " pdest=" << static_cast<unsigned>(actual.pdest)
                << " data=0x" << std::hex << actual.data
                << " exception=0x" << actual.exception_mask
                << " replay=" << std::dec << actual.replay
                << " rf_wen=" << actual.rf_wen
                << " fp_wen=" << actual.fp_wen
                << " flush_pipe=" << actual.flush_pipe
                << " trigger=" << static_cast<unsigned>(actual.trigger)
                << " mmio=" << actual.debug_is_mmio
                << " ncio=" << actual.debug_is_ncio
                << " perf_cnt=" << actual.debug_is_perf_cnt;
        if (expected != nullptr) {
            message << " expected_address=0x" << std::hex
                    << expected->address
                    << " expected_op=0x" << expected->op << std::dec
                    << " expected_pdest=" << static_cast<unsigned>(expected->pdest)
                    << " expected_rob_flag=" << expected->rob_flag
                    << " expected_exception=0x" << std::hex
                    << expected->exception_mask
                    << " allowed_additional_exception=0x"
                    << expected->allowed_additional_exception_mask
                    << " expected_data=0x" << std::hex << expected->data
                    << " check_data=" << std::dec << expected->check_data
                    << " check_data_on_exception=" << std::dec
                    << expected->check_data_on_exception
                    << " expected_rf_wen=" << std::dec << expected->rf_wen
                    << " expected_fp_wen=" << expected->fp_wen
                    << " expected_flush_pipe=" << expected->flush_pipe
                    << " expected_trigger=" << static_cast<unsigned>(expected->trigger)
                    << " expected_mmio=";
            if (expected->debug_is_mmio.has_value()) {
                message << *expected->debug_is_mmio;
            } else {
                message << "unchecked";
            }
            message << " expected_ncio=";
            if (expected->debug_is_ncio.has_value()) {
                message << *expected->debug_is_ncio;
            } else {
                message << "unchecked";
            }
            message << " expected_perf_cnt=";
            if (expected->debug_is_perf_cnt.has_value()) {
                message << *expected->debug_is_perf_cnt;
            } else {
                message << "unchecked";
            }
        }
        error_ = message.str();
    }

    std::unordered_map<RobIdentity, Expected, RobIdentityHash> expected_;
    std::uint64_t observed_ = 0;
    std::uint64_t prefetch_observed_ = 0;
    std::size_t pending_prefetch_ = 0;
    std::string error_;
};

class StoreScoreboard {
public:
    void expect(const StoreTransaction &transaction)
    {
        const auto [_, inserted] = expected_.emplace(
            rob_identity(transaction.rob, transaction.rob_flag),
            Expected{
                transaction.rob_flag,
                transaction.expected_exception_mask,
                false,
                false,
                false,
                false,
                0,
                0,
                transaction.expected_trigger,
                transaction.expected_output_flush_pipe.value_or(
                    transaction.input_flush_pipe),
                transaction.expected_debug_is_mmio,
                transaction.expected_debug_is_ncio,
                transaction.allow_preliminary_success_writeback,
                false,
            });
        if (!inserted && error_.empty()) {
            error_ = "duplicate outstanding scalar store ROB value";
        }
    }

    void observe_address(
        unsigned lane, const generated::ScalarStoreWriteback &writeback,
        std::uint64_t sample_cycle)
    {
        if (!writeback.valid) {
            return;
        }
        const auto it = expected_.find(
            rob_identity(writeback.rob_value, writeback.rob_flag));
        if (it == expected_.end()) {
            fail("unexpected store-address writeback", lane, writeback);
            return;
        }
        // The store-address output has no ready signal.  A valid pulse can
        // therefore still be visible after an issue attempt that was stalled;
        // do not attribute that pulse until the input handshake was observed.
        if (!it->second.address_issued) {
            return;
        }
        if (sample_cycle < it->second.address_issue_cycle) {
            return;
        }
        const bool trigger_mismatch =
            (it->second.trigger.has_value() &&
             writeback.trigger != *it->second.trigger);
        const bool debug_mismatch =
            (it->second.debug_is_mmio.has_value() &&
             writeback.debug_is_mmio != *it->second.debug_is_mmio) ||
            (it->second.debug_is_ncio.has_value() &&
             writeback.debug_is_ncio != *it->second.debug_is_ncio);
        if (it->second.allow_preliminary_success_writeback &&
            it->second.exception_mask != 0 &&
            writeback.exception_mask == 0 &&
            writeback.rob_flag == it->second.rob_flag &&
            !trigger_mismatch &&
            writeback.flush_pipe == it->second.flush_pipe &&
            !debug_mismatch) {
            if (it->second.preliminary_success_seen) {
                fail("duplicate preliminary store-address writeback", lane,
                     writeback);
                return;
            }
            it->second.preliminary_success_seen = true;
            return;
        }
        if (writeback.exception_mask != it->second.exception_mask ||
            writeback.rob_flag != it->second.rob_flag ||
            trigger_mismatch ||
            writeback.flush_pipe != it->second.flush_pipe ||
            debug_mismatch) {
            if (error_.empty()) {
                std::ostringstream message;
                message << "mismatched store-address writeback lane=" << lane
                        << " rob=" << static_cast<unsigned>(writeback.rob_value)
                        << " rob_flag=" << writeback.rob_flag
                        << " exception=0x" << std::hex << writeback.exception_mask
                        << " trigger=" << std::dec
                        << static_cast<unsigned>(writeback.trigger)
                        << " flush_pipe=" << writeback.flush_pipe
                        << " expected_rob_flag=" << it->second.rob_flag
                        << " expected_exception=0x" << std::hex
                        << it->second.exception_mask
                        << " expected_trigger=" << std::dec
                        << static_cast<unsigned>(it->second.trigger.value_or(kTriggerNone))
                        << " expected_flush_pipe=" << it->second.flush_pipe
                        << " mmio=" << writeback.debug_is_mmio
                        << " ncio=" << writeback.debug_is_ncio
                        << " expected_mmio=";
                if (it->second.debug_is_mmio.has_value()) {
                    message << *it->second.debug_is_mmio;
                } else {
                    message << "unchecked";
                }
                message << " expected_ncio=";
                if (it->second.debug_is_ncio.has_value()) {
                    message << *it->second.debug_is_ncio;
                } else {
                    message << "unchecked";
                }
                error_ = message.str();
            }
            return;
        }
        if (it->second.address_seen) {
            fail("duplicate store-address writeback", lane, writeback);
            return;
        }
        it->second.address_seen = true;
        retire_if_complete(it);
    }

    void observe_data(
        unsigned lane, const generated::ScalarStoreWriteback &writeback,
        std::uint64_t sample_cycle)
    {
        if (!writeback.valid) {
            return;
        }
        Iterator match = expected_.end();
        for (auto it = expected_.begin(); it != expected_.end(); ++it) {
            if (it->first.value != writeback.rob_value ||
                !it->second.data_issued) {
                continue;
            }
            if (sample_cycle < it->second.data_issue_cycle) {
                continue;
            }
            if (match != expected_.end()) {
                fail("ambiguous store-data writeback", lane, writeback);
                return;
            }
            match = it;
        }
        if (match == expected_.end()) {
            // The RTL output is a bare valid pulse (no ready and no ROB flag)
            // and may retain the previous ROB value while the next data issue
            // is stalled.  With no accepted data outstanding there is no
            // architectural event to attribute, so ignore that stale pulse.
            return;
        }
        if (match->second.data_seen) {
            fail("duplicate store-data writeback", lane, writeback);
            return;
        }
        match->second.data_seen = true;
        retire_if_complete(match);
    }

    bool done() const { return expected_.empty(); }
    std::size_t pending() const { return expected_.size(); }
    bool ok() const { return error_.empty(); }
    std::uint64_t observed() const { return observed_; }
    const std::string &error() const { return error_; }

    bool mark_address_issued(
        const StoreTransaction &transaction, std::uint64_t issue_cycle)
    {
        const auto it = expected_.find(
            rob_identity(transaction.rob, transaction.rob_flag));
        if (it == expected_.end()) {
            if (error_.empty()) {
                error_ = "store-address issue was not expected";
            }
            return false;
        }
        // A TLB miss may require the same address uop to be replayed after
        // translation.  Recording the handshake is therefore intentionally
        // idempotent; duplicate *writebacks* remain an error below.
        it->second.address_issued = true;
        it->second.address_issue_cycle = issue_cycle;
        return true;
    }

    bool mark_data_issued(
        const StoreTransaction &transaction, unsigned lane,
        std::uint64_t issue_cycle)
    {
        if (lane >= kScalarStoreLanes) {
            if (error_.empty()) {
                error_ = "invalid scalar store-data issue lane";
            }
            return false;
        }
        const auto match = expected_.find(
            rob_identity(transaction.rob, transaction.rob_flag));
        if (match == expected_.end()) {
            if (error_.empty()) {
                error_ = "scalar store-data issue was not expected";
            }
            return false;
        }
        // Store-data can likewise be retried when the SQ entry is replayed.
        // Keep the issued bit as a monotonic fact rather than rejecting a
        // legal retry; duplicate output pulses are still checked separately.
        match->second.data_issued = true;
        match->second.data_issue_cycle = issue_cycle;
        return true;
    }

private:
    struct Expected {
        bool rob_flag;
        std::uint32_t exception_mask;
        bool address_seen;
        bool data_seen;
        bool address_issued;
        bool data_issued;
        std::uint64_t address_issue_cycle;
        std::uint64_t data_issue_cycle;
        std::optional<std::uint8_t> trigger;
        bool flush_pipe;
        std::optional<bool> debug_is_mmio;
        std::optional<bool> debug_is_ncio;
        bool allow_preliminary_success_writeback;
        bool preliminary_success_seen;
    };

    using Iterator = std::unordered_map<RobIdentity, Expected, RobIdentityHash>::iterator;

    void retire_if_complete(Iterator it)
    {
        // An exceptional scalar store reports its architectural disposition
        // on the address writeback and intentionally has no data writeback.
        // Normal stores still require both halves, so a missing data pulse
        // cannot be hidden by this exception rule.
        if (it->second.address_seen &&
            (it->second.data_seen || it->second.exception_mask != 0)) {
            ++observed_;
            expected_.erase(it);
        }
    }

    void fail(
        const char *reason,
        unsigned lane,
        const generated::ScalarStoreWriteback &writeback)
    {
        if (!error_.empty()) {
            return;
        }
        std::ostringstream message;
        message << reason << " lane=" << lane
                << " rob=" << static_cast<unsigned>(writeback.rob_value)
                << " rob_flag=" << writeback.rob_flag
                << " exception=0x" << std::hex << writeback.exception_mask
                << " trigger=" << static_cast<unsigned>(writeback.trigger)
                << " flush_pipe=" << std::dec << writeback.flush_pipe;
        if (reason == std::string("unexpected store-data writeback") ||
            reason == std::string("ambiguous store-data writeback")) {
            message << " pending_robs=";
            bool first = true;
            for (const auto &entry : expected_) {
                if (!first) {
                    message << ',';
                }
                first = false;
                message << static_cast<unsigned>(entry.first.value)
                        << ':' << entry.first.flag;
            }
        }
        error_ = message.str();
    }

    std::unordered_map<RobIdentity, Expected, RobIdentityHash> expected_;
    std::uint64_t observed_ = 0;
    std::string error_;
};

class VectorMemoryScoreboard {
public:
    struct Expected {
        bool store;
        bool segment;
        std::array<unsigned char, 16> data;
        std::uint16_t active_elements;
        std::uint16_t fu_op_type;
        std::uint8_t eew;
        std::uint8_t vsew;
        std::uint8_t vlmul;
        std::uint8_t vl;
        std::uint8_t vstart;
        std::uint8_t vuop_idx;
        bool last_uop;
        std::uint8_t nf;
        std::uint8_t pdest;
        bool rob_flag;
        std::uint8_t lq;
        bool lq_flag;
        std::uint8_t sq;
        bool sq_flag;
        std::uint8_t flow_num;
        std::uint32_t exception_mask;
        std::optional<std::uint8_t> trigger;
        std::optional<std::uint8_t> expected_writeback_vstart;
        bool check_data;
        bool vec_wen;
        bool v0_wen;
        bool vl_wen;
        bool flush_pipe;
        std::optional<bool> debug_is_mmio;
        std::optional<bool> debug_is_ncio;
        std::optional<bool> debug_is_perf_cnt;
        bool vma;
        bool vta;
        std::uint64_t address;
        VectorAddressingMode addressing;
        std::int64_t stride;
        std::uint16_t mask_bits;
        std::array<unsigned char, 16> index;
        std::uint16_t tail_elements;
        std::uint16_t mask_inactive_elements;
    };

    void expect(
        const VectorMemoryTransaction &transaction,
        const std::array<unsigned char, 16> &data)
    {
        if (!transaction.check_data &&
            transaction.expected_trigger != kTriggerDebugMode) {
            if (error_.empty()) {
                error_ = "vector data checking may only be disabled for DebugMode trigger";
            }
            return;
        }
        auto output_transaction = transaction;
        output_transaction.vl = transaction.expected_vl.value_or(
            vector_effective_vl(transaction));
        const RobIdentity identity = rob_identity(
            transaction.rob, transaction.rob_flag);
        const auto range = expected_.equal_range(identity);
        const bool duplicate = std::any_of(
            range.first, range.second, [&](const auto &entry) {
                return entry.second.vuop_idx == transaction.vuop_idx &&
                       entry.second.pdest == transaction.pdest;
            });
        if (duplicate) {
            if (error_.empty()) {
                error_ = "duplicate outstanding vector memory uop";
            }
            return;
        }
        expected_.emplace(
            identity,
            Expected{
                transaction.store,
                transaction.segment,
                data,
                vector_writeback_elements(output_transaction),
                vector_fu_op_type(transaction),
                transaction.eew,
                vector_vsew(transaction),
                transaction.vlmul,
                output_transaction.vl,
                transaction.vstart,
                transaction.vuop_idx,
                transaction.last_uop,
                transaction.nf,
                transaction.pdest,
                transaction.rob_flag,
                transaction.lq,
                transaction.lq_flag,
                transaction.sq,
                transaction.sq_flag,
                transaction.flow_num,
                transaction.expected_exception_mask,
                transaction.expected_trigger,
                transaction.expected_writeback_vstart,
                transaction.check_data,
                transaction.vec_wen.value_or(
                    !transaction.store && !transaction.vl_wen),
                false,
                transaction.vl_wen,
                transaction.input_flush_pipe,
                transaction.expected_debug_is_mmio,
                transaction.expected_debug_is_ncio,
                transaction.expected_debug_is_perf_cnt,
                transaction.vma,
                transaction.vta,
                transaction.address,
                transaction.addressing,
                transaction.stride,
                transaction.mask_bits,
                transaction.index,
                vector_tail_elements(output_transaction),
                vector_mask_inactive_elements(output_transaction),
            });
    }

    void observe(unsigned lane, const generated::VectorMemoryWriteback &writeback)
    {
        if (!writeback.valid) {
            return;
        }
        const auto range = expected_.equal_range(
            rob_identity(writeback.rob_value, writeback.rob_flag));
        const auto it = std::find_if(
            range.first, range.second, [&](const auto &entry) {
                return entry.second.vuop_idx == writeback.vuop_idx &&
                       entry.second.pdest == writeback.pdest;
            });
        if (it == range.second) {
            fail("unexpected vector memory writeback", lane, writeback);
            return;
        }
        const Expected &expected = it->second;
        const bool normal_progress = expected.exception_mask == 0;
        const bool exception_progress = expected.exception_mask != 0;
        const bool debug_mismatch =
            (expected.debug_is_mmio.has_value() &&
             writeback.debug_is_mmio != *expected.debug_is_mmio) ||
            (expected.debug_is_ncio.has_value() &&
             writeback.debug_is_ncio != *expected.debug_is_ncio) ||
            (expected.debug_is_perf_cnt.has_value() &&
             writeback.debug_is_perf_cnt != *expected.debug_is_perf_cnt);
        const bool trigger_mismatch =
            (expected.trigger.has_value() &&
             writeback.trigger != *expected.trigger);
        const bool vstart_mismatch = expected.expected_writeback_vstart
            ? writeback.vstart != *expected.expected_writeback_vstart
            : (!exception_progress && writeback.vstart != 0);
        if ((expected.segment && lane != 0) ||
            writeback.exception_mask != expected.exception_mask || writeback.replay ||
            writeback.flush_pipe != expected.flush_pipe ||
            trigger_mismatch ||
            writeback.vec_wen != expected.vec_wen ||
            writeback.v0_wen != expected.v0_wen ||
            writeback.vl_wen != expected.vl_wen ||
            debug_mismatch ||
            writeback.fu_op_type != expected.fu_op_type ||
            writeback.rob_flag != expected.rob_flag ||
            vstart_mismatch ||
            ((!exception_progress) &&
             (writeback.vsew != expected.vsew ||
              writeback.veew != expected.eew ||
              writeback.vlmul != expected.vlmul ||
              writeback.vl != expected.vl ||
              writeback.vuop_idx != expected.vuop_idx ||
              writeback.nf != expected.nf))) {
            fail("mismatched vector memory metadata", lane, writeback, &expected);
            return;
        }
        if (expected.vl_wen && expected.exception_mask == 0) {
            const bool vl_data_matches =
                writeback.data.size() == 16 &&
                writeback.data[0] == expected.vl &&
                std::all_of(
                    writeback.data.begin() + 1, writeback.data.end(),
                    [](unsigned char byte) { return byte == 0; });
            const bool mask_matches =
                writeback.vmask.size() == 16 &&
                std::all_of(
                    writeback.vmask.begin(), writeback.vmask.end(),
                    [](unsigned char byte) { return byte == 0xff; });
            if ((expected.segment ? lane != 0 : lane != 1) ||
                writeback.vec_wen ||
                writeback.pdest != expected.pdest ||
                !vl_data_matches || !mask_matches) {
                fail("mismatched vector FOF fix-VL writeback", lane, writeback,
                     &expected);
                return;
            }
            ++fof_fix_observed_;
        } else if (!expected.store && expected.exception_mask == 0 &&
                   expected.vec_wen) {
            if (!writeback.vec_wen || writeback.pdest != expected.pdest ||
                (expected.check_data &&
                 !matches_load_data(writeback.data, expected)) ||
                !matches_active_mask(writeback.vmask, expected.active_elements)) {
                fail("mismatched vector load data", lane, writeback, &expected);
                return;
            }
            ++load_observed_;
        } else if (!expected.store && expected.exception_mask == 0) {
            ++load_observed_;
        } else if (expected.store) {
            ++store_observed_;
        } else {
            ++load_observed_;
        }
        expected_.erase(it);
    }

    bool done() const { return expected_.empty(); }
    std::size_t pending() const { return expected_.size(); }
    std::size_t pending_loads() const
    {
        return static_cast<std::size_t>(std::count_if(
            expected_.begin(), expected_.end(),
            [](const auto &entry) { return !entry.second.store; }));
    }
    std::size_t pending_stores() const
    {
        return static_cast<std::size_t>(std::count_if(
            expected_.begin(), expected_.end(),
            [](const auto &entry) { return entry.second.store; }));
    }
    bool ok() const { return error_.empty(); }
    std::uint64_t load_observed() const { return load_observed_; }
    std::uint64_t store_observed() const { return store_observed_; }
    std::uint64_t fof_fix_observed() const { return fof_fix_observed_; }
    const std::string &error() const { return error_; }

    std::string pending_summary() const
    {
        std::vector<std::string> entries;
        entries.reserve(expected_.size());
        for (const auto &[identity, expected] : expected_) {
            std::ostringstream entry;
            entry << (expected.store ? "store" : "load")
                  << ":rob=" << static_cast<unsigned>(identity.value)
                  << '/' << identity.flag
                  << ":uop=" << static_cast<unsigned>(expected.vuop_idx)
                  << ":last=" << expected.last_uop
                  << ":pdest=" << static_cast<unsigned>(expected.pdest)
                  << ":lq=" << static_cast<unsigned>(expected.lq)
                  << '/' << expected.lq_flag
                  << ":sq=" << static_cast<unsigned>(expected.sq)
                  << '/' << expected.sq_flag
                  << ":flows=" << static_cast<unsigned>(expected.flow_num)
                  << ":addressing="
                  << static_cast<unsigned>(expected.addressing)
                  << ":address=0x" << std::hex << expected.address;
            entries.push_back(entry.str());
        }
        std::sort(entries.begin(), entries.end());
        std::ostringstream summary;
        summary << " vector_pending=" << entries.size();
        for (const auto &entry : entries) {
            summary << " [" << entry << ']';
        }
        return summary.str();
    }

private:
    static bool matches_load_data(
        const std::vector<unsigned char> &actual, const Expected &expected)
    {
        if (actual.size() != expected.data.size()) {
            return false;
        }
        const unsigned element_bytes = 1U <<
            (expected.addressing == VectorAddressingMode::indexed_unordered ||
             expected.addressing == VectorAddressingMode::indexed_ordered
                ? expected.vsew
                : expected.eew);
        const unsigned elements = expected.data.size() / element_bytes;
        for (unsigned element = 0; element < elements; ++element) {
            bool preserved = true;
            bool all_ones = true;
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                const unsigned offset = element * element_bytes + byte;
                preserved &= actual[offset] == expected.data[offset];
                all_ones &= actual[offset] == 0xff;
            }
            const bool active =
                ((expected.active_elements >> element) & 1U) != 0;
            const bool tail_agnostic =
                ((expected.tail_elements >> element) & 1U) != 0 && expected.vta;
            const bool mask_agnostic =
                ((expected.mask_inactive_elements >> element) & 1U) != 0 &&
                expected.vma;
            if (active || (!tail_agnostic && !mask_agnostic)) {
                if (!preserved) {
                    return false;
                }
            } else if (!preserved && !all_ones) {
                return false;
            }
        }
        return true;
    }

    static bool matches_active_mask(
        const std::vector<unsigned char> &actual, std::uint16_t expected)
    {
        if (actual.size() < 2) {
            return false;
        }
        const std::uint16_t value = static_cast<std::uint16_t>(actual[0]) |
                                    (static_cast<std::uint16_t>(actual[1]) << 8);
        if (value != expected) {
            return false;
        }
        for (std::size_t index = 2; index < actual.size(); ++index) {
            if (actual[index] != 0) {
                return false;
            }
        }
        return true;
    }

    void fail(
        const char *reason,
        unsigned lane,
        const generated::VectorMemoryWriteback &actual,
        const Expected *expected = nullptr)
    {
        if (!error_.empty()) {
            return;
        }
        std::ostringstream message;
        message << reason << " lane=" << lane
                << " rob=" << static_cast<unsigned>(actual.rob_value)
                << " op=0x" << std::hex << actual.fu_op_type
                << " exception=0x" << actual.exception_mask
                << " replay=" << std::dec << actual.replay
                << " flush_pipe=" << actual.flush_pipe
                << " trigger=" << static_cast<unsigned>(actual.trigger)
                << " vec_wen=" << actual.vec_wen
                << " pdest=" << static_cast<unsigned>(actual.pdest)
                << " uop=" << static_cast<unsigned>(actual.vuop_idx)
                << " vl=" << static_cast<unsigned>(actual.vl)
                << " vstart=" << static_cast<unsigned>(actual.vstart)
                << " sew/eew/lmul=" << static_cast<unsigned>(actual.vsew)
                << '/' << static_cast<unsigned>(actual.veew) << '/'
                << static_cast<unsigned>(actual.vlmul);
        if (expected != nullptr) {
            message << " expected_op=0x" << std::hex << expected->fu_op_type
                    << " expected_exception=0x" << expected->exception_mask
                    << " expected_trigger="
                    << static_cast<unsigned>(expected->trigger.value_or(kVectorWritebackTriggerNone))
                    << " check_data=" << std::dec << expected->check_data
                    << " expected_writeback_vstart=";
            if (expected->expected_writeback_vstart) {
                message << static_cast<unsigned>(
                    *expected->expected_writeback_vstart);
            } else {
                message << "default";
            }
            message
                    << " expected_active=0x" << std::hex
                    << expected->active_elements
                    << " address=0x" << expected->address << std::dec
                    << " expected_sew/eew/lmul="
                    << static_cast<unsigned>(expected->vsew) << '/'
                    << static_cast<unsigned>(expected->eew) << '/'
                    << static_cast<unsigned>(expected->vlmul)
                    << " expected_uop="
                    << static_cast<unsigned>(expected->vuop_idx)
                    << " last=" << expected->last_uop
                    << " lq=" << static_cast<unsigned>(expected->lq)
                    << '/' << expected->lq_flag
                    << " sq=" << static_cast<unsigned>(expected->sq)
                    << '/' << expected->sq_flag
                    << " addressing="
                    << static_cast<unsigned>(expected->addressing)
                    << " stride=" << expected->stride
                    << " mask=0x" << std::hex << expected->mask_bits
                    << " index=";
            for (const auto byte : expected->index) {
                message << std::setw(2) << std::setfill('0')
                        << static_cast<unsigned>(byte);
            }
            message << " actual_data=";
            for (const auto byte : actual.data) {
                message << std::setw(2) << std::setfill('0')
                        << static_cast<unsigned>(byte);
            }
            message << " expected_data=";
            for (const auto byte : expected->data) {
                message << std::setw(2) << std::setfill('0')
                        << static_cast<unsigned>(byte);
            }
        }
        error_ = message.str();
    }

    std::unordered_multimap<RobIdentity, Expected, RobIdentityHash> expected_;
    std::uint64_t load_observed_ = 0;
    std::uint64_t store_observed_ = 0;
    std::uint64_t fof_fix_observed_ = 0;
    std::string error_;
};
