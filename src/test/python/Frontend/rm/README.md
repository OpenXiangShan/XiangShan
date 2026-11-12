# Frontend 参考模型

本目录保存独立的 BPU 功能模型；DUT 信号采样和比较位于 `../env/checkers/`。当前接入默认 DUT 比较的模型有 FallThrough、uBTB、mBTB 和 RAS。

| 模型 | 当前比较范围 |
| --- | --- |
| FallThrough | S1 顺序预测目标和 CFI 位置。 |
| uBTB | 已知训练表项的预测方向、位置、目标和属性。 |
| mBTB | 已知训练表项的有效命中元数据，以及部分条件分支计数器写入关系；不完整检查未命中、最终方向或所有 bank 的计数器历史。 |
| RAS | 预测、提交、redirect 的事务级检查点模型；对已知栈顶的 S3 return 预测比较 RAS 返回地址。 |

mBTB 模型允许训练后的旧、新元数据版本在流水过渡期间出现，观察到新版本后淘汰旧版本；它不验证写入生效的准确周期。

RAS 的 `predict(event_id, ...)` 每次接受预测时保存动作前状态；`commit(event_id)` 回收该事件及更早检查点，不重复执行栈动作；`redirect(event_id, ...)` 恢复目标事件动作前状态，丢弃更年轻事件，并按纠正后的动作更新。DUT checker 以 S3 FTQ pointer 标识预测事件，以 RAS commit queue 和普通 FTQ commit 的 pointer 回收检查点，以 redirect FTQ pointer 恢复。对于 push，checker 从本次取指的 startPc 按指令长度解析程序字节，到 S3 `cfiPosition` 指向的指令末半字，取得真实 CFI PC 和长度；无法解析时把旧栈底标为未知。无法对应的恢复，或纠正动作需要 push 但当前 DUT 未暴露 redirect CFI PC 时，也会使旧栈底未知；后续已知 call 仍可建立可比较栈顶。模型最多信任 16 层已知返回地址；超过该界限时把旧栈顶及旧检查点标为未知，DUT 报告 speculative near-overflow 时也清除旧检查点并停止比较。这是保守的可比较范围，不推断硬件溢出后的替换内容。RAS 比较计数和不一致数在 bin 结束时输出。RAS CSR 默认开启，可通过 `set_bp_ctrl_enable(ras_enable=0)` 控制。

默认开启已接入的参考模型（`TB_ENABLE_REFERENCE_MODEL=1`）。比对不一致默认记录 warning，不使 case 失败；每类最多逐条打印前 10 次，bin 结束时汇总总数。设置 `TB_REFERENCE_MODEL_STRICT=1` 可将不一致升级为 error 并使 case 失败。设置 `TB_ENABLE_REFERENCE_MODEL=0` 可关闭参考模型比较，不影响其他 DUT/trace 检查。
