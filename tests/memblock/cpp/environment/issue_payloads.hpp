#pragma once
#include "memblock_env.hpp"
namespace memblock {
inline generated::ScalarLoadIssue ordinary_load_issue(const LoadTransaction &transaction) {
    generated::ScalarLoadIssue issue;
    issue.pc = transaction.pc.value_or(0x1000 + transaction.rob * 4);
    issue.predecode_rvc = transaction.predecode_rvc;
    issue.ftq_ptr = transaction.ftq_ptr;
    issue.ftq_offset = transaction.ftq_offset;
    issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
    issue.rf_wen = transaction.rf_wen;
    issue.fp_wen = transaction.fp_wen;
    issue.pdest = transaction.pdest;
    issue.rob_flag = transaction.rob_flag;
    issue.rob_value = transaction.rob;
    issue.lq_flag = transaction.lq_flag;
    issue.lq_value = transaction.lq;
    issue.sq_flag = transaction.sq_flag;
    issue.sq_value = transaction.sq;
    issue.store_set_hit = transaction.store_set_hit;
    issue.wait_for_rob_flag = transaction.wait_for_rob_flag;
    issue.wait_for_rob_value = transaction.wait_for_rob_value;
    issue.load_wait_bit = transaction.load_wait_bit;
    issue.load_wait_strict = transaction.load_wait_strict;
    issue.imm = static_cast<std::uint32_t>(transaction.immediate);
    issue.src = scalar_issue_base_address(
        transaction.address, transaction.immediate);
    return issue;
}
inline generated::ScalarLoadIssue ordinary_prefetch_issue(const PrefetchTransaction &transaction) {
    generated::ScalarLoadIssue issue;
    issue.pc = 0x1800 + transaction.rob * 4;
    issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
    issue.rf_wen = false;
    issue.fp_wen = false;
    issue.pdest = 0;
    issue.rob_flag = transaction.rob_flag;
    issue.rob_value = transaction.rob;
    issue.lq_flag = transaction.lq_flag;
    issue.lq_value = transaction.lq;
    issue.sq_flag = transaction.sq_flag;
    issue.sq_value = transaction.sq;
    issue.imm = static_cast<std::uint32_t>(transaction.immediate);
    issue.src = scalar_issue_base_address(
        transaction.address, transaction.immediate);
    return issue;
}
inline generated::ScalarStoreIssue ordinary_store_issue(const StoreTransaction &transaction, bool data) {
    generated::ScalarStoreIssue issue;
    issue.fu_type = kFuTypeStore;
    issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
    issue.rob_flag = transaction.rob_flag;
    issue.rob_value = transaction.rob;
    issue.sq_flag = transaction.sq_flag;
    issue.sq_value = transaction.sq;
    issue.imm = data ? 0 : static_cast<std::uint32_t>(transaction.immediate);
    issue.src = data ? transaction.data : scalar_issue_base_address(transaction.address, transaction.immediate);
    return issue;
}
} // namespace memblock
