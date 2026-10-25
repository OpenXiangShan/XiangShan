// Compiled by test_vector_scoreboard.py with the production model, scoreboard,
// and generated writeback structs.  No simulator or Picker build is needed.

static memblock::generated::VectorMemoryWriteback completion(
    const memblock::VectorMemoryTransaction &transaction, unsigned vd_index)
{
    memblock::generated::VectorMemoryWriteback result;
    result.valid = true;
    result.rob_value = transaction.rob;
    result.rob_flag = transaction.rob_flag;
    result.pdest = transaction.pdest;
    result.fu_op_type = memblock::vector_fu_op_type(transaction);
    result.vec_wen = transaction.vec_wen.value_or(
        !transaction.store && !transaction.vl_wen);
    result.vl_wen = transaction.vl_wen;
    result.exception_mask = transaction.expected_exception_mask;
    result.vsew = memblock::vector_vsew(transaction);
    result.veew = transaction.eew;
    result.vlmul = transaction.vlmul;
    result.vl = transaction.expected_vl.value_or(
        memblock::vector_effective_vl(transaction));
    result.vuop_idx = transaction.vuop_idx;
    result.nf = transaction.nf;
    result.vd_index_in_field = vd_index;
    result.data.assign(16, 0);
    result.vmask.assign(16, 0);
    auto output = transaction;
    output.vl = result.vl;
    const auto elements = memblock::vector_writeback_elements(output);
    result.vmask[0] = elements & 0xff;
    result.vmask[1] = elements >> 8;
    if (transaction.vl_wen) {
        result.data[0] = result.vl;
        result.vmask.assign(16, 0xff);
    }
    return result;
}

static void require(bool condition, const std::string &reason)
{
    if (!condition) {
        throw std::runtime_error(reason);
    }
}

static void check_scalar_reset_cancellation()
{
    const memblock::LoadTransaction old{
        .address = 0x80001018, .op = memblock::LoadOp::ld,
        .rob = 7, .pdest = 31, .lane = 0,
    };
    auto next = old;
    next.rob_flag = true;
    next.address += 0x1000;
    next.pdest = 32;
    memblock::generated::ScalarLoadWriteback wb{};
    wb.valid = true;
    wb.rob_value = old.rob;
    wb.pdest = old.pdest;
    wb.rf_wen = true;
    wb.trigger = old.expected_trigger;
    wb.data = 0x123456789abcdef0ULL;

    memblock::LoadScoreboard canceled;
    canceled.expect(old, wb.data);
    canceled.expect(next, wb.data);
    require(canceled.cancel_for_reset(old), "pending reset cancellation rejected");
    require(!canceled.pending(old) && canceled.pending(next),
            "reset cancellation removed unrelated wrapped identity");
    require(canceled.observed() == 0 && canceled.reset_canceled() == 1,
            "cancellation counted as a completion");
    canceled.observe(0, wb);
    require(!canceled.ok() && canceled.error().find("unexpected load writeback") !=
                std::string::npos, "stale canceled writeback was accepted");

    memblock::LoadScoreboard early;
    early.expect(old, wb.data);
    early.observe(0, wb);
    require(early.ok() && !early.pending(old) && early.observed() == 1,
            "legal early completion rejected");
    require(!early.cancel_for_reset(old) && early.reset_canceled() == 0,
            "completed identity was canceled a second time");

    for (unsigned mismatch = 0; mismatch < 4; ++mismatch) {
        memblock::LoadScoreboard wrong;
        wrong.expect(old, wb.data);
        auto requested = old;
        if (mismatch == 0) requested.rob_flag = !requested.rob_flag;
        if (mismatch == 1) ++requested.pdest;
        if (mismatch == 2) ++requested.address;
        if (mismatch == 3) requested.op = memblock::LoadOp::lw;
        require(!wrong.cancel_for_reset(requested) && wrong.pending(old) &&
                wrong.reset_canceled() == 0, "mismatched reset identity accepted");
    }
    memblock::LoadScoreboard invalid;
    invalid.expect(old, wb.data);
    wb.data ^= 1;
    invalid.observe(0, wb);
    require(!invalid.ok() && !invalid.cancel_for_reset(old) && invalid.pending(old),
            "reset cancellation hid an earlier data mismatch");
    memblock::LoadScoreboard prefetch;
    const memblock::PrefetchTransaction hint{.address = old.address, .rob = old.rob};
    prefetch.expect_prefetch(hint);
    require(!prefetch.cancel_for_reset(old) && prefetch.pending_prefetch() == 1,
            "scalar reset cancellation removed a prefetch");
}

static void check_index(
    const memblock::VectorMemoryTransaction &transaction, unsigned index,
    bool significant = true)
{
    const auto actual = completion(transaction, index);
    memblock::VectorMemoryScoreboard positive;
    positive.expect(transaction, {});
    positive.observe(transaction.vl_wen && !transaction.segment ? 1 : 0, actual);
    require(positive.ok() && positive.done(), positive.error());

    // Preserve all bytes, masks, identity, and other metadata while corrupting
    // only the field under test.  This used to be silently accepted.
    auto corrupted = actual;
    corrupted.vd_index_in_field = (index + 1) % 8;
    memblock::VectorMemoryScoreboard negative;
    negative.expect(transaction, {});
    negative.observe(transaction.vl_wen && !transaction.segment ? 1 : 0,
                     corrupted);
    if (significant) {
        require(!negative.ok(), "wrong vector destination index was accepted");
        require(negative.error().find("mismatched vector destination index") !=
                    std::string::npos,
                negative.error());
    } else {
        require(negative.ok() && negative.done(), negative.error());
    }
}

int main(int argc, char **argv)
{
    try {
        require(argc == 2, "one case name required");
        const std::string name = argv[1];
        if (name == "scalar-reset") {
            check_scalar_reset_cancellation();
            return 0;
        }
        if (name == "segment-exception-counts") {
            for (bool store : {false, true}) {
                memblock::VectorMemoryScoreboard scoreboard;
                memblock::VectorMemoryTransaction fault;
                fault.segment = true;
                fault.store = store;
                fault.nf = 1;
                fault.expected_exception_mask = store
                    ? memblock::kExceptionStorePageFault
                    : memblock::kExceptionLoadPageFault;
                auto fix = fault;
                fix.vuop_idx = 1;
                fix.pdest = 1;
                fix.vl_wen = true;
                fix.last_uop = true;
                scoreboard.expect(fault, {});
                scoreboard.expect(fix, {});
                scoreboard.observe(0, completion(fault, 0));
                require(scoreboard.ok() && scoreboard.pending() == 1,
                        "exceptional segment first completion rejected");
                require((store ? scoreboard.store_observed()
                               : scoreboard.load_observed()) == 1,
                        "first exceptional segment completion not counted");
                scoreboard.observe(0, completion(fix, 0));
                require(scoreboard.ok() && scoreboard.done(),
                        "exceptional segment fix completion rejected");
                require((store ? scoreboard.store_observed()
                               : scoreboard.load_observed()) == 2 &&
                            scoreboard.fof_fix_observed() == 0,
                        "exceptional segment fix must count in its direction");
            }
            return 0;
        }
        memblock::VectorMemoryTransaction transaction;
        if (name == "ordinary") {
            check_index(transaction, 0);
            transaction.vlmul = 1; // LMUL2, second register has four body bytes.
            transaction.vl = 20;
            transaction.vuop_idx = 1;
            check_index(transaction, 1);
            transaction.addressing = memblock::VectorAddressingMode::strided;
            transaction.stride = 3;
            check_index(transaction, 1);
        } else if (name == "indexed") {
            transaction.addressing =
                memblock::VectorAddressingMode::indexed_unordered;
            transaction.eew = 2; // EEW32 indices, SEW8 data, LMUL2, EMUL8.
            transaction.vsew = 0;
            transaction.vlmul = 1;
            transaction.flow_num = 4;
            transaction.vl = 20;
            transaction.vuop_idx = 5; // Four index uops per data register.
            check_index(transaction, 1);
            transaction.addressing =
                memblock::VectorAddressingMode::indexed_ordered;
            transaction.vlmul = 7; // LMUL1/2, EMUL2, two uops share vd index0.
            transaction.vl = 8;
            transaction.vuop_idx = 1;
            check_index(transaction, 0);
        } else if (name == "segment") {
            transaction.segment = true;
            transaction.nf = 1;
            transaction.vlmul = 1;
            transaction.vl = 20;
            transaction.vuop_idx = 3; // Field1, second data register.
            check_index(transaction, 1);
            transaction.vuop_idx = 2; // Field1 resets the register index.
            check_index(transaction, 0);
            transaction.addressing =
                memblock::VectorAddressingMode::indexed_ordered;
            transaction.eew = 1;
            transaction.vsew = 0;
            transaction.vuop_idx = 3;
            check_index(transaction, 1);
        } else if (name == "whole") {
            transaction.whole_register = true;
            transaction.nf = 7;
            transaction.vuop_idx = 7;
            check_index(transaction, 7);
        } else if (name == "fof") {
            transaction.fault_only_first = true;
            transaction.is_vleff = true;
            transaction.vlmul = 1;
            transaction.vl = 20;
            transaction.expected_vl = 18;
            transaction.vuop_idx = 1;
            // An optional page fault may be suppressed.  Actual successful
            // data completions still require the normal destination index.
            transaction.allowed_additional_exception_mask =
                memblock::kExceptionLoadPageFault;
            check_index(transaction, 1);
            transaction.segment = true;
            transaction.nf = 1;
            transaction.vuop_idx = 3;
            check_index(transaction, 1);
        } else if (name == "irrelevant") {
            transaction.expected_exception_mask =
                memblock::kExceptionLoadPageFault;
            check_index(transaction, 6, false);
            transaction.expected_exception_mask = 0;
            transaction.vec_wen = false;
            check_index(transaction, 6, false);
            transaction.vec_wen.reset();
            transaction.store = true;
            check_index(transaction, 6, false);
            transaction.store = false;
            transaction.fault_only_first = true;
            transaction.is_vleff = true;
            transaction.vl_wen = true;
            check_index(transaction, 6, false);
            transaction.segment = true;
            transaction.nf = 1;
            check_index(transaction, 6, false);
        } else {
            throw std::runtime_error("unknown test case");
        }
    } catch (const std::exception &error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
