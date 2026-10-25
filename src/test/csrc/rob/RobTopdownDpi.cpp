#include "topdown_rob_info.cpp"

extern "C" void topdown_rob_info_dpic_318_32(
    unsigned int iq_entries_num, unsigned int rob_entries_num,
    const svBitVecVal *in_bits, svBitVecVal *out_bits) {
  topdown_rob_info_dpic(iq_entries_num, rob_entries_num, in_bits, out_bits);
}
