#ifndef DFI_ADDRESS_MAPPING_H_
#define DFI_ADDRESS_MAPPING_H_

#include <array>
#include <cstdint>

namespace dfi {

enum class address_field : uint8_t {
    kChannel,
    kRank,
    kRow,
    kCol,
    kBank,
    kBg,
};

struct address_mapping_op {
    uint64_t mask;
    uint8_t source;
    uint8_t source_shift;
    uint8_t destination_shift;
};

struct address_mapping_config {
    static constexpr uint8_t kMaxOps = 32U;

    std::array<address_mapping_op, kMaxOps> ops{};
    uint64_t ram_size_bytes = 0U;
    uint8_t op_count = 0U;
};

struct dfi_address {
    uint8_t channel;
    uint8_t rank;
    uint8_t bank;
    uint32_t row;
    uint32_t col;
};

address_mapping_config load_address_mapping_config(uint64_t default_ram_size_bytes);

inline uint64_t compose_mapped_byte_address(const address_mapping_config& config,
                                            const dfi_address& address) {
    const std::array<uint64_t, 6> sources = {
        address.channel & 0x1U,
        address.rank & 0x1U,
        address.row & 0x3ffffU,
        (address.col >> 3U) & 0x7fU,
        address.bank & 0x3U,
        (address.bank >> 2U) & 0x3U,
    };

    uint64_t cacheline_address = 0U;
    for (uint8_t i = 0U; i < config.op_count; ++i) {
        const address_mapping_op& op = config.ops[i];
        cacheline_address |= ((sources[op.source] >> op.source_shift) & op.mask)
                             << op.destination_shift;
    }
    return (cacheline_address << 6U) | ((address.col & 0x1U) << 5U);
}

}  // namespace dfi

#endif  // DFI_ADDRESS_MAPPING_H_
