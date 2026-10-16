#ifndef DFI_VIRTUAL_RAM_H_
#define DFI_VIRTUAL_RAM_H_

#include <cstdint>

namespace dfi {

// Adapts the DFI model to the simulator-owned memory. Keeping the storage in
// difftest gives the CPU, refill checker and DDR model one coherent backing RAM.
class virtual_ram {
public:
    static constexpr uint64_t kBeatBytes = 32U;

    using read_callback = bool (*)(void* context, uint64_t offset, uint32_t* data);
    using write_callback = bool (*)(void* context,
                                    uint64_t offset,
                                    const uint32_t* data,
                                    uint32_t byte_enable);
    explicit virtual_ram(uint64_t size_bytes);

    bool bind_external(uint64_t address_base,
                       uint64_t size_bytes,
                       void* context,
                       read_callback read,
                       write_callback write);

    bool read_beat(uint64_t byte_address, uint32_t* data) const;
    bool write_beat(uint64_t byte_address, const uint32_t* data, uint32_t byte_enable);

    bool ready() const;

private:
    bool range_valid(uint64_t byte_address) const;
    uint64_t offset_of(uint64_t byte_address) const;

    uint64_t address_base_;
    uint64_t size_bytes_;
    void* context_;
    read_callback read_;
    write_callback write_;
};

}  // namespace dfi

#endif  // DFI_VIRTUAL_RAM_H_
