#ifndef PARAM_H_
#define PARAM_H_

// Delay/pulse width for dfi_ctrlupd_req -> dfi_ctrlupd_ack(+ack2).
#define SIM_DFI_CTRLUPD_ACK_DELAY_CORE_CYCLES 8ULL
#define SIM_DFI_CTRLUPD_ACK_PULSE_CORE_CYCLES 1ULL
// Delay/pulse width for dfi_lp_req -> dfi_lp_ack.
#define SIM_DFI_LP_ACK_DELAY_CORE_CYCLES 8ULL
#define SIM_DFI_LP_ACK_PULSE_CORE_CYCLES 1ULL
// DFI timing constraints configured by APB.
// PLDM DFITMG2 programs dfi_tphy_wrcslat = 9.
#define SIM_DFI_TPHY_WRLAT_CORE_CYCLES 9ULL
// PLDM DFITMG2 programs dfi_tphy_rdcslat = 10.
#define SIM_DFI_PHY_RDLAT_CORE_CYCLES 10ULL
#define SIM_DFI_DATA_EN_FULL_MASK 0xFFFFU
// dfi_rddata_valid value driven when a read beat is returned.
#define SIM_DFI_RDDATA_VALID_MASK 0xFFU
// Protocol mismatches must fail before pending DFI work can exhaust host memory.
#define SIM_DFI_MAX_PENDING_DEPTH 4096ULL

#endif  // PARAM_H_
