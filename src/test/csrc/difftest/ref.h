/**
 * Headers for C/C++ REF models
 */
#ifndef __DT_REF_H__
#define __DT_REF_H__

#include "difftest.h"

// REF Models
extern "C" uint8_t pte_helper(uint64_t satp, uint64_t vpn, uint64_t *pte, uint8_t *level);

#endif
