# memblock 测试框架与参考模型代码注释规则

## 触发条件

当修改 `mem_ut` 中测试框架、参考模型、公共数据结构或相关 agent/sequence 代码时，必须遵循本规则。

适用范围包括但不限于：

- `seq/base_seq`
- `seq/virtual_sequence`
- `common_data_transaction`
- `status_transaction`
- `lsq_ctrl_model`
- TLB/L2TLB responder 相关 sequence/model
- monitor event adapter
- reference model / scoreboard / checker
- agent xaction、driver、monitor 中新增或修改的测试框架控制字段

## 核心规则

凡是新增、删除、重命名或修改**核心字段**含义时，必须在源码中添加中文注释。

核心字段包括：

- 状态字段，例如 `active`、`success`、`flushed`、`replay_pending`、`refetch_pending`
- 队列、映射、索引字段，例如 issue queue、ROB/LQ/SQ map、uid map
- epoch、seq、cursor、counter 字段，例如 `issue_epoch`、`replay_seq`、`dynamic_epoch`、`route_scan_cursor`
- 配置字段和 plus 控制字段
- transaction/xaction 中影响 driver、monitor、scoreboard 或 reference model 行为的字段
- 与 DUT/Scala 源码语义对应的字段，例如 ROB redirect、LSQ commit/deq、TLB entry、CSR runtime state

## 注释要求

中文注释至少说明以下内容：

1. 字段表示什么语义。
2. 为什么需要该字段。
3. 该字段在当前流程中由谁设置、由谁清除或更新。
4. 字段为 1 或有效时，对后续流程有什么影响。
5. 如果字段有合法取值范围、互斥关系或依赖关系，需要写清楚。

示例：

```systemverilog
// 中文注释：当前 uid 是否等待 redirect 后重新取指/重新发射。
// 置位：apply_redirect_flush() 命中该 uid 且 uid 尚未 success。
// 清零：refetch 重新分配 ROB/LQ/SQ 并重新进入 issue 流程后清零。
// 作用：为 1 时该 uid 不是终态，all_transactions_success() 不能把它当完成。
bit refetch_pending;
```

枚举类型需要说明每个枚举值的含义：

```systemverilog
typedef enum int unsigned {
    // 无 LSQ 流程，用于非 load/store 或无效占位 transaction。
    MEMBLOCK_LSQ_FLOW_NONE   = 0,
    // load 流程，需要分配 LQ 并进入 load issue queue。
    MEMBLOCK_LSQ_FLOW_LOAD   = 1,
    // store 流程，需要分配 SQ，并根据操作拆成 STA/STD。
    MEMBLOCK_LSQ_FLOW_STORE  = 2
} memblock_lsq_flow_e;
```

复杂 task/function 如果修改了状态转移，也需要在函数头部或关键分支前添加中文注释，说明该函数承担的流程职责。

## 不需要强制注释的内容

以下内容不强制添加中文注释：

- 简单临时变量
- 明显的循环下标
- 纯机械赋值且字段含义已经在结构体声明处注释过的重复赋值
- 局部 debug 字符串

不要为了满足规则添加无意义注释，例如“给变量赋值”“循环 idx”。注释必须解释设计语义。

## 文档同步要求

如果代码修改改变了字段语义、状态机行为、约束关系或参数含义，必须同步更新对应设计文档。

常见同步位置：

- `AI_DOC/analysis/source_sv/dispatch_framework_sv/*.md`
- `AI_DOC/analysis/framework_design/*.md`
- `AI_DOC/project_management/*.md`
- 相关 feature/review plan 文档

## Review 检查项

提交或 review 前必须检查：

1. 新增核心字段是否有中文注释。
2. 修改字段含义后，旧注释是否同步更新。
3. 状态字段是否说明置位和清零时机。
4. queue/map/cursor/epoch 字段是否说明使用场景。
5. 与 DUT/Scala 语义相关的字段是否说明对应关系。
6. 文档是否同步更新。

如果 review 发现核心字段没有中文注释，应要求补充后再继续。
