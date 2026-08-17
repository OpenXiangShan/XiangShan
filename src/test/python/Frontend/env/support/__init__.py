"""Shared environment configuration and utilities."""

from .pmp_pma import (
    PMA_ADDR_BASE,
    PMA_CFG_BASE,
    PMP_ADDR_BASE,
    PMP_CFG_BASE,
    PMP_PMA_PLATFORM_GRAIN_BYTES,
    PmpPmaConfig,
    csr_addresses_for_entry,
    encode_pmp_pma_addr,
    encode_pmp_pma_cfg,
    reconstruct_pmp_request_addr,
)

__all__ = [
    "PMA_ADDR_BASE",
    "PMA_CFG_BASE",
    "PMP_ADDR_BASE",
    "PMP_CFG_BASE",
    "PMP_PMA_PLATFORM_GRAIN_BYTES",
    "PmpPmaConfig",
    "csr_addresses_for_entry",
    "encode_pmp_pma_addr",
    "encode_pmp_pma_cfg",
    "reconstruct_pmp_request_addr",
]
