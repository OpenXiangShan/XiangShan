#include "dfi/virtual_ram.h"

namespace dfi {

virtual_ram::virtual_ram(uint64_t size_bytes)
    : address_base_(0U),
      size_bytes_(size_bytes),
      context_(nullptr),
      read_(nullptr),
      write_(nullptr) {}

bool virtual_ram::bind_external(uint64_t address_base,
                                uint64_t size_bytes,
                                void* context,
                                read_callback read,
                                write_callback write) {
    if (read == nullptr || write == nullptr || size_bytes == 0U) {
        return false;
    }
    address_base_ = address_base;
    size_bytes_ = size_bytes;
    context_ = context;
    read_ = read;
    write_ = write;
    return true;
}

bool virtual_ram::read_beat(uint64_t byte_address, uint32_t* data) const {
    return data != nullptr && range_valid(byte_address) &&
           read_(context_, offset_of(byte_address), data);
}

bool virtual_ram::write_beat(uint64_t byte_address,
                             const uint32_t* data,
                             uint32_t byte_enable) {
    return data != nullptr && range_valid(byte_address) &&
           write_(context_, offset_of(byte_address), data, byte_enable);
}

bool virtual_ram::ready() const {
    return read_ != nullptr && write_ != nullptr && size_bytes_ != 0U;
}

bool virtual_ram::range_valid(uint64_t byte_address) const {
    if (!ready() || kBeatBytes > size_bytes_ || byte_address < address_base_) {
        return false;
    }
    return byte_address - address_base_ <= size_bytes_ - kBeatBytes;
}

uint64_t virtual_ram::offset_of(uint64_t byte_address) const {
    return byte_address - address_base_;
}

}  // namespace dfi
