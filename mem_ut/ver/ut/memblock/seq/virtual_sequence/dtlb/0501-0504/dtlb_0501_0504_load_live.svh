// 仅接管FilterEntry请求及稳定CSR；ready/response由正式owner自然驱动。
// 真实FilterEntry input字段；clock/reset由真实管线提供。
drive({u,".io_csr_satp_asid"},0);
drive({u,".io_csr_vsatp_asid"},0);
drive({u,".io_csr_hgatp_vmid"},0);
drive({u,".io_tlb_req_0_valid"},0);
drive({u,".io_tlb_req_0_bits_vpn"},0);
drive({u,".io_tlb_req_0_bits_s2xlate"},0);
drive({u,".io_tlb_req_0_bits_getGpa"},0);
drive({u,".io_tlb_req_1_valid"},0);
drive({u,".io_tlb_req_1_bits_vpn"},0);
drive({u,".io_tlb_req_1_bits_s2xlate"},0);
drive({u,".io_tlb_req_1_bits_getGpa"},0);
drive({u,".io_tlb_req_2_valid"},0);
drive({u,".io_tlb_req_2_bits_vpn"},0);
drive({u,".io_tlb_req_2_bits_s2xlate"},0);
drive({u,".io_tlb_req_2_bits_getGpa"},0);
drive({u,".io_tlb_req_3_valid"},0);
drive({u,".io_tlb_req_3_bits_vpn"},0);
drive({u,".io_tlb_req_3_bits_s2xlate"},0);
drive({u,".io_tlb_req_3_bits_getGpa"},0);
