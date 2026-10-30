# DTLB 01.01 请求来源专项

本目录保存6个隔离的请求级vseq；配置位于`../../plus_cfg/dtlb_0101/`。

完整补充指南与仿真记录：[/nfs/home/wangyan/Mem_env_work_20260716/TLB_fcov/FCOV_complete/用例方案01/0101/01_01用例补充指南.md](/nfs/home/wangyan/Mem_env_work_20260716/TLB_fcov/FCOV_complete/用例方案01/0101/01_01用例补充指南.md)、[仿真总结](/nfs/home/wangyan/Mem_env_work_20260716/TLB_fcov/FCOV_complete/用例方案01/0101/仿真成功失败总结.md)。

场景为l2_query、atomic_lr、atomic_amo、segment、stream、sms。使用`basicTest`，`ts=memblock_dtlb_0101_<场景>_vseq`、`cfg=dtlb_0101_<场景>`及`plus_file=../seq/plus_cfg/dtlb_0101`。

这些用例检查真实来源模块到DTLB响应；segment与预取使用模块输入级激励，响应后用DUT reset清理在途。它们不代表完整指令提交或预取训练算法验证通过。未修改公共scalar调度、未关闭X检查、未驱动DTLB响应/覆盖模型。

最终验收seed221108/221109，各场景2/2通过；13个原始缺口新增命中12仓。PF0 cmd1在当前RTL合法路径不可达，见输出目录专项证据。
