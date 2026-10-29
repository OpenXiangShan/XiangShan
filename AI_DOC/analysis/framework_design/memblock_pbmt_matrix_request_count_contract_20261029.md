# PBMT定向场景与批量请求数覆盖冲突分析

## 现象与证据

门禁已于`60dcca7c6`关闭，组8原10seed coverage严格PASS。组9入口为
`basicTest / memblock_l2tlb_pbmt_response_fault_vseq / tc_l2tlb_pbmt_response_fault`，
按历史矩阵参数覆盖`MEMBLOCK_MAIN_TRANS_NUM=1000`、CHECK_TRIGGER=0、timeout1e9ns。
8 FAIL、2 NOT_RUN、unknown0；首seed1476684907在201ns报`PBMT RF cfg snapshot does not match directed contract`。
全部controller/远端已收敛；该失败不重开已完成B1门禁，也不能继续组10等掩盖失败。

当前main sequence明确只建B Store/C Load两UID，validate必须MAIN=2；现cfg亦为2。
早期cfg曾是3，不能据历史误称当前3UID。`run_function_cfg_cov_20261022.sh`把该case放在default，
随后统一加TRANS_NUM覆盖，连本轮手工恢复也照搬历史覆盖，所以破坏了已修PBMT directed合同。
预期是保留固定场景的2请求及真实accepted/completed=2/2审计，不应放宽assert或灌1000个无关UID。

## 分类与最小方案

Turing独立责任/方案review已PASS；带波形原seed/原MAIN1000在201ns同fatal复现，原拒绝检查未放松。
方案只修脚本profile和路由测试：新增固定cfg数量profile，仍从seq/plus_cfg取配置，不加MAIN覆盖；
PBMT该行使用新profile，普通25项保持统一override，四CSR scalar保持独立cfg/10k路径。
补fake runner及真实dry-run断言MAIN2，并保持显式MAIN1000仍fatal作为负例。SV/RM/RTL/cfg不变。
修复后原失败seed按更正后的合同参数做专项，不伪称原1000参数通过；原8FAIL与2NOTRUN永不覆盖。

两脚本已按批准范围修正，单元先红后绿，正向原seed按cfg2严格1/1 PASS；独立实现review PASS，随本Bug单独提交。

本轮结果目录：
`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028/mem_ut/ver/ut/memblock/sim/mother_post_wip_cov_20261029_results/tc_l2tlb_pbmt_response_fault`。
原参数debug复现目录为同根`pbmt_contract_original_debug`。责任review完成后才写正式Buglist及编码。
