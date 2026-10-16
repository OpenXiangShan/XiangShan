#ifndef DFI_DFI_BUS_H_
#define DFI_DFI_BUS_H_

#include <array>
#include <cstdint>

namespace dfi {

struct dfi_data_sample {
    uint32_t rddata_en;
    uint32_t wrdata_en;
    uint32_t wrdata_mask;
    std::array<uint32_t, 8> wrdata;
};

struct dfi_output_drive {
    std::array<uint32_t, 8> rddata;
    uint32_t rddata_valid;
    uint32_t rddata_dbi;
    uint32_t ctrlupd_ack;
    uint32_t ctrlupd_ack2;
    uint32_t lp_ack;

    void clear() {
        rddata.fill(0U);
        rddata_valid = 0U;
        rddata_dbi = 0U;
        ctrlupd_ack = 0U;
        ctrlupd_ack2 = 0U;
        lp_ack = 0U;
    }
};

}  // namespace dfi

#endif  // DFI_DFI_BUS_H_
