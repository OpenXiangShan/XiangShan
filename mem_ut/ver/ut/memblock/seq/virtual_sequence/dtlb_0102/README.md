# DTLB 01.02 请求流水专项

本目录隔离保存四类vseq：l2_query、store_dual、stream、sms；cfg在`../../plus_cfg/dtlb_0102/`。类名为`memblock_dtlb_0102_<场景>_vseq`，使用`basicTest`及`plus_file=../seq/plus_cfg/dtlb_0102`。

仅继承01.01的通用HDL/时钟/reset helper，body与逐拍检查独立。预取场景在过滤器输入端激励，每个有限窗口从空表开始，不强制TLB请求输出或响应。

最终8/8通过，01.02由43/65提升到65/65；两次开发轮失败保留。请求级检查不代表完整指令提交。

[补充指南](/nfs/home/wangyan/Mem_env_work_20260716/TLB_fcov/FCOV_complete/用例方案01/0102/01_02用例补充指南.md) · [成功失败总结](/nfs/home/wangyan/Mem_env_work_20260716/TLB_fcov/FCOV_complete/用例方案01/0102/仿真成功失败总结.md)。
