
# DUT 测试总结报告

## 项目概述

### DUT 基本信息
- **DUT 名称**: {DUT_NAME}
- **测试时间**: {TEST_DATE}
- **测试环境**: UCAgent 自动化验证框架
- **验证方法**: 基于 toffee 的功能覆盖率驱动验证
- **框架版本**: {UCAGENT_VERSION}
- **使用模型**: {MODEL_NAME}

### 验证概述

{在本章节描述创建了多少验证分组，构建了多少功能点，多少测试点，编写了多少测试用例，发现了多少bug。对重要边界测试和bug进行描述。}

## 测试执行汇总

### 测试用例统计
| 指标 | 数值 | 说明 |
|------|------|------|
| 总测试用例数 | {TOTAL_TESTS} | 包含所有功能点的测试用例 |
| 通过用例数 | {PASSED_TESTS} | 成功执行并验证通过的用例 |
| 失败用例数 | {FAILED_TESTS} | 执行失败或验证不通过的用例 |
| 跳过用例数 | {SKIPPED_TESTS} | 由于条件不满足而跳过的用例 |
| 错误用例数 | {ERROR_TESTS} | 执行过程中出现异常的用例 |
| 测试通过率 | {PASS_RATE}% | (通过用例数 / 总用例数) × 100% |

### 测试执行时间分析
| 阶段 | 耗时 | 说明 |
|------|------|------|
| DUT创建时间 | {DUT_CREATION_TIME}s | create_dut 和fixture初始化时间 |
| 测试准备时间 | {SETUP_TIME}s | 环境准备和数据初始化时间 |
| 测试执行时间 | {EXECUTION_TIME}s | 所有测试用例的实际执行时间 |
| 覆盖率统计时间 | {COVERAGE_TIME}s | 功能覆盖率采样和统计时间 |
| 结果分析时间 | {ANALYSIS_TIME}s | 测试结果分析和报告生成时间 |
| 总耗时 | {TOTAL_TIME}s | 完整验证流程总时间 |

## 功能覆盖率分析

### 覆盖率总览
| 覆盖率类型 | 目标值 | 实际值 | 状态 |
|------------|--------|--------|------|
| 功能组覆盖率 | 100% | {FG_COVERAGE}% | {FG_STATUS} |
| 功能点覆盖率 | 100% | {FC_COVERAGE}% | {FC_STATUS} |
| 检查点覆盖率 | 100% | {CK_COVERAGE}% | {CK_STATUS} |
| 边界值覆盖率 | 100% | {BOUNDARY_COVERAGE}% | {BOUNDARY_STATUS} |
| 异常情况覆盖率 | 100% | {EXCEPTION_COVERAGE}% | {EXCEPTION_STATUS} |

### 功能组覆盖详情
*按功能组 `<FG-*>` 统计覆盖情况*

#### 完全覆盖的功能组
{COMPLETED_FUNCTION_GROUPS}

#### 部分覆盖的功能组
{PARTIAL_FUNCTION_GROUPS}

#### 未覆盖的功能组
{UNCOMPLETED_FUNCTION_GROUPS}

### 检查点详细分析
*按检查点 `<CK-*>` 统计验证结果*

#### 通过的检查点
{PASSED_CHECKPOINTS}

#### 未通过的检查点
{FAILED_CHECKPOINTS}

### 功能覆盖率
- **总功能点数**: {TOTAL_FUNC}
- **总功检测点数**: {TOTAL_CHECK}
- **总Pass率**: {TOTAL_PASS_RATE}

## 缺陷分析

### 缺陷统计概览
| 严重程度 | 数量 | 平均置信度 | 占比 | 处理建议 |
|----------|------|------------|------|----------|
| 严重 (90-100%) | {CRITICAL_BUGS} | {CRITICAL_AVG}% | {CRITICAL_RATIO}% | 立即修复 |
| 重要 (70-89%) | {MAJOR_BUGS} | {MAJOR_AVG}% | {MAJOR_RATIO}% | 优先修复 |
| 一般 (50-69%) | {MINOR_BUGS} | {MINOR_AVG}% | {MINOR_RATIO}% | 进一步调查 |
| 待确认 (1-49%) | {UNCERTAIN_BUGS} | {UNCERTAIN_AVG}% | {UNCERTAIN_RATIO}% | 低优先级调查 |
| 可忽略 (0%) | {IGNORE_BUGS} | {IGNORE_AVG}% | {IGNORE_RATIO}% | 检查测试用例 |

### 按功能组分类的缺陷分布
{BUG_DISTRIBUTION_BY_FG}

### 主要缺陷详细分析
{MAJOR_BUG_ANALYSIS}

### 根因分析总结
{ROOT_CAUSE_ANALYSIS}

### 缺陷修复优先级排序
1. **高优先级** (置信度 ≥ 90%): {HIGH_PRIORITY_BUGS}
2. **中优先级** (置信度 70-89%): {MEDIUM_PRIORITY_BUGS}
3. **低优先级** (置信度 < 70%): {LOW_PRIORITY_BUGS}

## 测试质量评估

### 测试完整性评估
- **功能点覆盖完整性**: {FUNCTION_COMPLETENESS} - {FUNCTION_COMPLETENESS_DESC}
- **边界条件测试完整性**: {BOUNDARY_COMPLETENESS} - {BOUNDARY_COMPLETENESS_DESC}
- **异常情况测试完整性**: {EXCEPTION_COMPLETENESS} - {EXCEPTION_COMPLETENESS_DESC}
- **回归测试完整性**: {REGRESSION_COMPLETENESS} - {REGRESSION_COMPLETENESS_DESC}

### 测试有效性评估
- **缺陷检出能力**: {BUG_DETECTION_CAPABILITY} - {BUG_DETECTION_DESC}
- **误报率**: {FALSE_POSITIVE_RATE}% - {FALSE_POSITIVE_DESC}
- **测试用例质量得分**: {TEST_CASE_QUALITY}/10
- **断言覆盖率**: {ASSERTION_COVERAGE}%

### 代码质量评估
- **测试代码可读性**: {CODE_READABILITY}/10
- **API设计合理性**: {API_DESIGN_QUALITY}/10
- **覆盖率定义准确性**: {COVERAGE_DEFINITION_ACCURACY}/10
- **文档完整性**: {DOCUMENTATION_COMPLETENESS}/10

## 改进建议

### 设计改进建议
{DESIGN_IMPROVEMENT_SUGGESTIONS}

### 测试策略改进建议
{TEST_STRATEGY_IMPROVEMENTS}

### 验证流程优化建议
{PROCESS_OPTIMIZATION_SUGGESTIONS}

### 工具链改进建议
{TOOLCHAIN_IMPROVEMENTS}

## 经验教训总结

### 成功经验
{SUCCESS_EXPERIENCES}

### 遇到的挑战
{ENCOUNTERED_CHALLENGES}

### 解决方案总结
{SOLUTION_SUMMARY}

### 最佳实践提炼
{BEST_PRACTICES}

## 结论

### 验证结论
{VERIFICATION_CONCLUSION}

### DUT质量评估
- **整体质量等级**: {DUT_QUALITY_LEVEL}
- **功能正确性**: {FUNCTIONAL_CORRECTNESS}
- **设计鲁棒性**: {DESIGN_ROBUSTNESS}
- **接口规范性**: {INTERFACE_COMPLIANCE}

### 发布建议
{RELEASE_RECOMMENDATION}

### 后续工作建议
{NEXT_STEPS_RECOMMENDATION}

---
**报告生成信息**
- *报告生成时间*: {REPORT_GENERATION_TIME}
- *UCAgent框架版本*: {UCAGENT_VERSION}
- *使用的AI模型*: {AI_MODEL_INFO}
- *验证配置*: {VERIFICATION_CONFIG}
