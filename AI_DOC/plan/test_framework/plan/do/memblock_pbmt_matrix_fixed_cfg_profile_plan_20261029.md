# PBMT定向回归保留cfg请求数方案

实现完成：两shell单元先红后绿，真实dry-run通过；原MAIN1000带波形负例保留fatal，
更正为cfg2的原seed1476684907严格1/1 PASS，独立FINAL_IMPLEMENTATION_REVIEW_PASS。
随本Bug独立提交归档；恢复10seed批次状态以母任务表为准，不提前记10/10。

## 1. 术语与问题

| 术语 | 含义与代码落点 |
|---|---|
| profile | 矩阵每行第四列，决定cfg目录和是否追加统一请求数覆盖。 |
| fixed_cfg | 新增profile，仍取seq/plus_cfg，但保持cfg自身MAIN_TRANS_NUM，不追加override。 |
| directed合同 | PBMT现行只建B Store/C Load两个UID及两笔真实L2TLB token；不等同普通1000请求随机场景。 |

组9原8FAIL/2NOT_RUN，首seed1476684907@201ns；带波形原参数重复同fatal。矩阵default统一MAIN1000
覆盖了cfg2，破坏`validate_pbmt_rf_cfg_snapshot()`。不是RTL/RM或B1问题，也不是cfg2写错。
Turing于2026-10-29独立`CLASSIFICATION_REVIEW_PASS / PLAN_REVIEW_PASS`，批准以下精确范围。

## 2. 修改职责

仅修改`mem_ut/ver/ut/memblock/scripts/run_function_cfg_cov_20261022.sh`及其已有routing shelltest。
PBMT response-fault一行改profile=fixed_cfg。`resolve_case_profile()`为该profile设置同一seq/plus_cfg路径，
`CASE_OVERRIDE_TRANS_NUM=0`；调用循环不追加MAIN。其余普通25项仍追加TRANS_NUM，scalar4保持独立cfg目录和10k。
不新增SV参数，不改cfg、SV、checker、RM、RTL，不改compile-once、seed数量、trigger或其它case路由。

行为概述：先从case第四列解析profile；fixed_cfg取与普通场景相同的cfg目录但保留字面数量；
构造runner参数时只有override开关为1才附MAIN；PBMT因此获得cfg2并走原有全部DUT/exception/token审计。
显式错误MAIN1000仍必须fatal，不能放松checker或称旧参数通过。

## 3. 测试与恢复

1. 先改既有shelltest，旧矩阵必须失败；再改矩阵源码后单元通过，保留30case/compile-once/路径校验。
2. 普通25项override4321；PBMT1项无MAIN override，scalar4亦无；cfg目录和CHECK_TRIGGER0保持。
3. 调真实runner --dry-run，PBMT有效MAIN唯一且为2、CHECK_TRIGGER唯一且为0，不调用远端。
4. 原seed1476684907、MAIN1000带波形负例仍201ns fatal、无TEST_PASS；保留结果，不计PASS。
5. 同seed更正profile后单颗专项，参数保留cfg2、trigger0、原timeout/verbosity/coverage；必须严格PASS。
6. 完成独立实现review和该Bug单独commit，再恢复组9：原8失败seed重跑，原2NOT_RUN另构2个未使用新seed；
   新空结果目录，10个逐日志严格PASS后更新原矩阵状态。原8FAIL/2NOT_RUN文件永不覆盖。

仅脚本变化不进simv，fingerprint/运行选项兼容时可复用原coverage二进制，不假装重编译过SV。
当前带失败数据的coverage是中间产物；最终报告须从通过的统一版本批次收集，不能把失败数据混称全PASS覆盖率。

## 4. 审查与授权

用户已授予本母任务测试框架coding权限，且要求每Bug独立提交、只在mother顺序修改。
分类/方案review通过后按本方案实施，不新建worktree、不修改无关soft_test，不推送未确认的公共上游。
