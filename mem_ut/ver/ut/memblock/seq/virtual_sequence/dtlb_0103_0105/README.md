# 01.03～01.05 DTLB 有向补仓用例

本目录将请求取消、响应副本和 ROB 重定向关系用例集中隔离。配套配置位于 `seq/plus_cfg/dtlb_0103_0105/`，仿真结果位于 `sim/dtlb_0103_0105_20261030/`。

## 用例与范围

|序列类|配置|检查内容|
|---|---|---|
|`memblock_dtlb_0103_kill_ld_vseq`|`dtlb_0103_kill_ld`|LD0～LD2 请求取消、后拍取消与三种命中来源|
|`memblock_dtlb_0103_kill_query_vseq`|`dtlb_0103_kill_query`|PF1 请求取消与三种命中来源|
|`memblock_dtlb_0104_copies_vseq`|`dtlb_0104_copies`|LD0～LD2 实际地址副本一致性、全八口响应关联|
|`memblock_dtlb_0105_redirect_vseq`|`dtlb_0105_redirect`|全八口 ROB 等龄/较老/较新与两种重定向级别|

这些是 DTLB 模块输入边界上的请求级验证。只驱动声明输入，读取真实输出。翻译场景依据真实 miss 请求的 VPN/stage 建立单个返回凭据，提供匹配的 4KB PTW 返回。公共 L2TLB responder 关闭，下游 PTWNewFilter 的八个声明请求输入置 idle，以界定模块测试边界；所有公共检查器保持启用。结束时整体 DUT reset 清理并释放驱动。本组结果不代表完整指令提交或全局页表遍历验证。

## 已验证结果

2026-10-30 在 eda01（172.28.10.101）执行，最终四场景双 seed 共 8/8 PASS，UVM_ERROR/UVM_FATAL 均为零。含开发轮共 14 次运行、12 PASS/2 FAIL，按固定起点归为同一个网页任务 `dtlb_0103_0105_task_20261030`。

|功能组|起点|结果|新增|剩余说明|
|---|---|---|---|---|
|01.03|23/72|32/72|9|40 仓受当前来源常量或接口能力阻断|
|01.04|11/16|11/16|0|5 仓的采样条件对当前实例恒假|
|01.05|15/64|64/64|49|无剩余|

原覆盖模型、分母和 RTL 保持原样。完整补法、逐仓证据、成功失败路径和复现命令分别保存在：

- [01.03 总结](/nfs/home/wangyan/Mem_env_work_20260716/TLB_fcov/FCOV_complete/用例方案01/0103/仿真成功失败总结.md)
- [01.04 总结](/nfs/home/wangyan/Mem_env_work_20260716/TLB_fcov/FCOV_complete/用例方案01/0104/仿真成功失败总结.md)
- [01.05 总结](/nfs/home/wangyan/Mem_env_work_20260716/TLB_fcov/FCOV_complete/用例方案01/0105/仿真成功失败总结.md)
