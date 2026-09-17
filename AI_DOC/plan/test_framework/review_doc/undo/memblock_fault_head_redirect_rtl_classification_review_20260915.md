# Fault head redirect 缺失的 RTL 分类独立复核（2026-09-15）

| 项目 | 内容 |
| --- | --- |
| 复核对象 | seed `856436350` 的 fault 后无进展现象 |
| 版本 | V2，`mem_ut_uvm_v2` |
| reviewer | `/root/rtl_liveness_review` |
| 结论 | 非 RTL；属于测试框架 fault recovery 语义缺失 |

## 术语与抽象功能说明

| 术语 | 当前含义 | 代码或波形落点 | 本次示例 |
| --- | --- | --- | --- |
| fault head | 已产生 exception、并到达 modeled ROB head 的 transaction。 | `lsq_commit_handler::mark_fault_rob_commit_uid()` | UID5，ROB `0/5`。 |
| redirect | 外部输入到 MemBlock 的 flush 边界，不是 DUT 从 writeback 自动产生的输出。 | `/top_tb/U_MEMBLOCK/io_redirect_valid` | 本次全程为 0。 |
| fault retire | 已满足 fault commit/deq 规则后的非成功终态收口。 | `consume_fault_retire()` | UID5 于 `1747.7ns` 完成。 |
| young entry | ROB 顺序晚于 redirect anchor 的活动 transaction。 | `rob_order_util::rob_need_flush()` | UID11、UID15。 |
| temporary RTL | 为本次验证显式指定的 DUT RTL 目录。 | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/test-rtl/build/rtl` | 编译日志逐文件确认。 |

抽象功能说明：本复核只判断 fault 后无进展的责任边界，检查 RTL 输入输出、波形和测试框架状态链路；不提出或执行 RTL 修改。

## 复核证据

- 编译日志 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/pbmt0_non_nc_100k_test_rtl_20260915/log/vcs_compile_rtl.log` 显示 VCS 解析 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/test-rtl/build/rtl/{LoadQueueReplay,StoreQueue,LsqWrapper,Sbuffer,MemBlock}.sv`。
- 运行日志 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/pbmt0_non_nc_100k_test_rtl_20260915/log/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=856436350_rtl_temp_rtl_seed856436350.log` 在 `747.7ns` 记录 UID5 的 STA `exception_vec=0x80`，并在 `1747.7ns` 记录其 fault retire。
- 波形 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/pbmt0_non_nc_100k_test_rtl_20260915/wave/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=856436350_rtl_temp_rtl_seed856436350.fsdb` 中 `/top_tb/U_MEMBLOCK/io_redirect_valid` 在 `700ns` 至 `146595.2ns` 维持 0。
- UID11/UID15 后续 replay 无法完成，日志于 `51.805us` 报 `terminal_done_uid=11`；没有更早 DUT assertion 或 DUT 输出协议违例。

## 责任判断

fault writeback、monitor 归属与 fault retire 已经正常完成，说明 DUT fault 输出链路没有首发异常。`io_redirect_valid` 是 DUT 输入，而当前测试框架没有在 fault head 时建立 redirect drive，因此 DUT 不会取消年轻 LSQ 项。结论是测试框架缺失 fault-head recovery 闭环，不是 RTL 问题。

本复核同时提示：不能在 `handle_fault_event()` 收到任意 fault 时立即调用 generic redirect，因为那里不保证 fault 已到 ROB head；后续实现应以 fault-head 时刻为入口，并明确年轻 UID 的测试框架恢复策略。
