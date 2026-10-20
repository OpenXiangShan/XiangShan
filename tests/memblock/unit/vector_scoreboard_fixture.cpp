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
