#pragma once
#include "vector_operands.hpp"
#include <set>

namespace memblock::scenarios::mixed {

// The caller admits instructions in architectural order and overlays each
// older store before freezing younger load expectations. This image belongs
// only to that prefix; neither store acceptance nor DUT data advances it.
class StreamByteOracle {
public:
    explicit StreamByteOracle(const memblock::SparseMemory &base) : base_(base) {}

    std::array<unsigned char, 64> line_snapshot(std::uint64_t base,
        const std::shared_ptr<const memblock::ReferencePageMap> &pages = {})
    {
        if (base & 63U) throw std::invalid_argument("unaligned stream snapshot");
        std::array<std::uint64_t, 64> addresses{};
        std::array<unsigned char, 64> result{};
        for (unsigned byte = 0; byte < addresses.size(); ++byte)
            addresses[byte] = pages ? pages->physical_address(base + byte) : base + byte;
        for (unsigned byte = 0; byte < addresses.size(); ++byte) {
            materialize(addresses[byte]);
            result[byte] = image_.read_byte(addresses[byte]);
        }
        return result;
    }

    std::uint64_t scalar_load(const memblock::LoadTransaction &transaction)
    {
        const auto encoding = static_cast<unsigned>(transaction.op);
        if (encoding > static_cast<unsigned>(memblock::LoadOp::lwu) ||
            (transaction.fp_wen && transaction.rf_wen)) {
            throw std::invalid_argument("unsupported stream scalar load");
        }
        // Validate FP widths before materializing any bytes.
        (void)memblock::scalar_load_value(transaction.op, transaction.fp_wen, 0);
        const unsigned width = 1U << (encoding & 3U);
        std::array<std::uint64_t, 8> addresses{};
        for (unsigned byte = 0; byte < width; ++byte) {
            addresses[byte] = memblock::reference_byte_address(
                transaction, transaction.address + byte);
        }
        std::uint64_t value = 0;
        for (unsigned byte = 0; byte < width; ++byte) {
            materialize(addresses[byte]);
            value |= std::uint64_t{image_.read_byte(addresses[byte])} << (8U * byte);
        }
        return memblock::scalar_load_value(transaction.op, transaction.fp_wen, value);
    }

    void prepare_vector_load(
        std::vector<memblock::VectorMemoryTransaction> &uops)
    {
        std::vector<std::uint64_t> addresses;
        for (const auto &uop : uops) {
            const auto bytes = vector_addresses(uop, false);
            addresses.insert(addresses.end(), bytes.begin(), bytes.end());
        }
        // All sibling bytes must be present before constructing any old-Vd
        // source. The producer policy may consume a preceding split's result.
        for (auto address : addresses) materialize(address);
        chain_indexed_vector_sources(uops, image_);
    }

    std::array<unsigned char, 16> vector_load(
        const memblock::VectorMemoryTransaction &transaction)
    {
        const auto addresses = vector_addresses(transaction, false);
        for (auto address : addresses) materialize(address);
        return image_.expected_vector_load(transaction);
    }

    void store(const memblock::StoreTransaction &transaction)
    {
        const auto encoding = static_cast<unsigned>(transaction.op);
        if (encoding > static_cast<unsigned>(memblock::StoreOp::sd)) {
            throw std::invalid_argument("unsupported stream scalar store");
        }
        const unsigned width = 1U << encoding;
        std::array<std::uint64_t, 8> addresses{};
        for (unsigned byte = 0; byte < width; ++byte) {
            addresses[byte] = memblock::reference_byte_address(
                transaction, transaction.address + byte);
        }
        for (unsigned byte = 0; byte < width; ++byte) {
            overlay(addresses[byte], static_cast<unsigned char>(
                transaction.data >> (8U * byte)));
        }
    }

    void store(const memblock::VectorMemoryTransaction &transaction)
    {
        const auto addresses = vector_addresses(transaction, true);
        const unsigned width = 1U << memblock::vector_data_eew(transaction);
        const auto active = memblock::active_vector_elements(transaction);
        std::size_t next = 0;
        for (unsigned element = 0; element < 16U / width; ++element) {
            if (((active >> element) & 1U) == 0) continue;
            for (unsigned byte = 0; byte < width; ++byte) {
                overlay(addresses.at(next++), transaction.data[element * width + byte]);
            }
        }
    }

private:
    static std::vector<std::uint64_t> vector_addresses(
        const memblock::VectorMemoryTransaction &transaction, bool store)
    {
        if (transaction.store != store) {
            throw std::invalid_argument("stream vector memory direction mismatch");
        }
        (void)memblock::vector_fu_op_type(transaction);
        (void)memblock::vector_lmul_log2(transaction);
        const unsigned width = 1U << memblock::vector_data_eew(transaction);
        const auto active = memblock::active_vector_elements(transaction);
        std::vector<std::uint64_t> result;
        for (unsigned element = 0; element < 16U / width; ++element) {
            if (((active >> element) & 1U) == 0) continue;
            for (unsigned byte = 0; byte < width; ++byte) {
                result.push_back(memblock::vector_byte_address(transaction, element, byte));
            }
        }
        return result;
    }

    void materialize(std::uint64_t address)
    {
        if (known_.insert(address).second) {
            image_.write_reference_byte(address, base_.read_byte(address));
        }
    }

    void overlay(std::uint64_t address, unsigned char value)
    {
        image_.write_reference_byte(address, value);
        known_.insert(address);
    }

    const memblock::SparseMemory &base_;
    memblock::SparseMemory image_;
    std::set<std::uint64_t> known_;
};

} // namespace memblock::scenarios::mixed
