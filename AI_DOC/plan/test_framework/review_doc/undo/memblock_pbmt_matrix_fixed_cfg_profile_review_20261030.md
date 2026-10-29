# PBMT定向数量profile修复评审

## 1. 术语

profile是CASE第四列，用于选择cfg目录及是否追加统一MAIN覆盖；fixed_cfg是保留字面请求数的新profile。
当前directed合同是B Store/C Load两个UID和两笔真实token；旧三UID历史不代表现行合同。

## 2. 责任与方案review

Turing，原发现轮2026-10-29，`CLASSIFICATION_REVIEW_PASS / PLAN_REVIEW_PASS`：本轮任务参数照搬历史全局覆盖，
且现有矩阵同样错误地把PBMT放default。MAIN1000覆盖了正确cfg2，不是DUT/RM/B1问题。
批准只改矩阵脚本及routing测试，SV/cfg/checker/RM/RTL不动；手动恢复同步移除MAIN覆盖。

## 3. 修改前后与源码支撑

| 功能 | 修改前 | 修改后 | 正确性边界 |
|---|---|---|---|
| PBMT矩阵行 | default，追加TRANS_NUM1000 | fixed_cfg，不追加MAIN | cfg2及既有token/exception检查保持，不造1000请求 |
| profile解析 | default或scalar10k | 增fixed_cfg，同seq/plus_cfg目录，override=0 | 普通25及scalar4目录/数量策略不改，compile-once不改 |
| routing测试 | 前26全override当正确 | 普通25 override、PBMT1/scalar4不覆盖 | 真实runner dry-run检查唯一MAIN2/CHECK0 |

源码位置：`mem_ut/ver/ut/memblock/scripts/run_function_cfg_cov_20261022.sh`，`resolve_case_profile()`。
抽象功能描述：将显式profile转换成cfg路径和可选请求数覆盖，避免批量入口破坏定向合同。
函数功能简析：读取case第四列，写入两个已有临时变量；不改cfg内容，也不创建SV参数。

```bash
fixed_cfg)
    CASE_PLUS_FILE="$SIM_DIR/../seq/plus_cfg"
    CASE_OVERRIDE_TRANS_NUM=0
    ;;
```

中文伪代码：

本分支负责固定cfg数量场景的参数选择。先把cfg目录设为现有seq/plus_cfg，再把已有覆盖开关清零，
使后续runner构造跳过MAIN追加；PBMT因此从原cfg读取2。它不修改cfg、公共plus默认值或DUT行为。

## 4. 测试证据

- 修改前既有shelltest PASS，旧测试接受错误“前26全覆盖”策略。
- 仅先改routing测试，旧矩阵FAIL：`PBMT fixed cfg count was overridden`；同次trap有NFS临时目录清理stderr，
  未改无关cleanup逻辑。修matrix后全测试PASS，包含30case、一次compile、25/1/4、未知profile及路径校验。
- `bash -n`两脚本PASS；真实runner dry-run的MAIN唯一2、CHECK_TRIGGER唯一0。
- 原seed1476684907、显式MAIN1000、trigger0、timeout1e9/UVM_LOW/coverage，fresh compile+wave重现201ns原fatal，
  无TEST_PASS、controller FAIL/unknown0。这是保留的预期拒绝，不计真实DUT PASS。
- 正向同seed保持cfg2、不追加MAIN，1/1严格PASS、controller rc0/unknown0、六项计数全0、一次TEST_PASS/正常finish，
  真实PBMT阶段达到AUDIT_PASSED。结果在mother sim的`mother_post_wip_cov_20261029_results/pbmt_contract_fixed_root`，
  `strict_acceptance.json`已生成；Turing独立FINAL_IMPLEMENTATION_REVIEW_PASS，无剩余blocker。

## 5. 隔离、风险与提交门禁

没有编译进simv的源码变化，可在指纹/coverage能力兼容时复用原simv；不声称重新编译了SV。
单Bug提交只纳两脚本、本Bug分析/plan/review/Buglist/状态及已有profile方案后续修订提示。
原8FAIL/2NOT_RUN及原参数波形保留；MAIN2通过不能写成原1000请求已PASS。
原seed正向专项及独立实现review通过后单独commit，再恢复本组10seed；不修无关soft_test、不改RTL。

最终结论：原seed正向和独立实现review均已通过，可随本Bug提交。恢复采用原8FAIL seed加2个新seed，
原2NOT_RUN只替换名额；原失败归档保留，不能声称原1000参数或整个恢复批次已PASS。
