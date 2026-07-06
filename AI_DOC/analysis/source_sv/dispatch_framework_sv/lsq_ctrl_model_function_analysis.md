# lsq_ctrl_model.sv 函数级源码说明

对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_ctrl_model.sv`

本文按源码函数顺序说明 `lsq_ctrl_model`。每个函数先附源码，再说明：

- 功能目的是什么
- 输入是什么
- 输出是什么
- 调用了哪些函数
- 被哪些上层流程调用
- 内部调用函数在本函数中承担什么作用

## 1. 文件定位

`lsq_ctrl_model` 是 dispatch 测试框架中的软件 LSQ 控制镜像。它不是完整 LSQ RTL 模型，只维护测试框架需要的 LSQ admission 信息：

- LQ/SQ enqueue 指针
- LQ/SQ dequeue 指针
- LQ/SQ free count
- 主表 transaction 分配到的 LQ/SQ key
- active uid 与 LQ/SQ key 的映射激活
- DUT deq 后软件侧资源释放

它主要被以下流程调用：

- `memblock_lsqenq_dispatch_sequence.sv`：入队前预测资源，入队后用 DUT response 校验并提交分配。
- `lsq_commit_handler.sv`：ctrl monitor 采到 DUT lqDeq/sqDeq 后，使用本模型释放 LQ/SQ 资源。
- `memblock_dispatch_base_sequence.sv`、`issue_queue_scheduler.sv`、`issue_field_assigner.sv`：通过 `derive_op_behavior()` 判断 load/store/atomic 的抽象行为。
- `soft_test_*` sequence：软件 smoke 中直接使用软件预测路径提交或释放资源。

## 2. class 字段

源码：

```systemverilog
class lsq_ctrl_model extends uvm_object;

    static lsq_ctrl_model m_inst;

    common_data_transaction data;

    memblock_lq_key_t lq_enq_ptr;
    memblock_sq_key_t sq_enq_ptr;
    memblock_lq_key_t lq_deq_ptr;
    memblock_sq_key_t sq_deq_ptr;
    int unsigned      lq_free_count;
    int unsigned      sq_free_count;

    `uvm_object_utils(lsq_ctrl_model)
```

说明：

- `m_inst`：单例对象。LSQ 指针和 free count 必须全局唯一，不能每个 sequence 各维护一份，否则 LSQ enqueue、deq 和 commit 会看到不同的软件状态。
- `data`：公共数据表单例，指向 `common_data_transaction::get()`。分配成功后要通过它写主表、状态表和 active map。
- `lq_enq_ptr/sq_enq_ptr`：软件预测的下一次 LQ/SQ 入队指针。
- `lq_deq_ptr/sq_deq_ptr`：软件预测的下一次 LQ/SQ 出队指针，用于和 DUT ctrl deq pointer 对齐。
- `lq_free_count/sq_free_count`：软件侧剩余可分配 LQ/SQ 项数。

## 3. new()

源码：

```systemverilog
    function new(string name = "lsq_ctrl_model");
        super.new(name);
        data = common_data_transaction::get();
        reset();
    endfunction:new
```

功能目的：

创建 `lsq_ctrl_model` 对象，并把软件 LSQ 状态初始化为空 LSQ。

输入：

- `name`：UVM object 名称，默认 `"lsq_ctrl_model"`。

输出：

- 无返回值。
- 副作用是设置 `data`，并调用 `reset()` 初始化指针和 free count。

调用函数：

- `common_data_transaction::get()`：获取公共数据单例。
- `reset()`：把 LQ/SQ enq/deq pointer 清 0，并把 free count 设为最大容量。

内部调用关系：

- `new()` 中调用 `reset()` 是为了保证对象一创建就处于空 LSQ 状态，避免后续 sequence 在未显式 reset 时读到未知指针。

上层调用：

- `get()` 中第一次创建单例时调用。

## 4. get()

源码：

```systemverilog
    static function lsq_ctrl_model get();
        if (m_inst == null) begin
            m_inst = new("lsq_ctrl_model_singleton");
        end
        return m_inst;
    endfunction:get
```

功能目的：

返回全局唯一的 `lsq_ctrl_model` 对象。

输入：

- 无。

输出：

- 返回 `lsq_ctrl_model` 单例句柄。

调用函数：

- `new("lsq_ctrl_model_singleton")`：当单例还不存在时创建对象。

被哪些上层调用：

- `memblock_dispatch_base_sequence::pre_body()` 中获取并 reset。
- `memblock_lsqenq_dispatch_sequence::ensure_helpers()` 中获取 LSQ 控制模型。
- `lsq_commit_handler::ensure_handles()` 中获取 LSQ 控制模型。
- soft smoke sequence 中获取软件 LSQ 状态。

设计意义：

LSQ 指针是全局资源，必须所有 sequence 看到同一份。`get()` 保证入队 sequence 和 deq/commit handler 操作的是同一套 `lq_enq_ptr/lq_deq_ptr/free_count`。

## 5. reset()

源码：

```systemverilog
    function void reset();
        lq_enq_ptr    = '{default:'0};
        sq_enq_ptr    = '{default:'0};
        lq_deq_ptr    = '{default:'0};
        sq_deq_ptr    = '{default:'0};
        lq_free_count = MEMBLOCK_LQ_SIZE;
        sq_free_count = MEMBLOCK_SQ_SIZE;
    endfunction:reset
```

功能目的：

把软件 LSQ 镜像恢复到空队列状态。

输入：

- 无。

输出：

- 无返回值。

状态修改：

- LQ/SQ enqueue pointer 清零。
- LQ/SQ dequeue pointer 清零。
- LQ free count 设为 `MEMBLOCK_LQ_SIZE`。
- SQ free count 设为 `MEMBLOCK_SQ_SIZE`。

调用函数：

- 无。

被哪些上层调用：

- `new()` 中调用。
- `memblock_dispatch_base_sequence::pre_body()` 中调用，用于每个 dispatch sequence 开始前清空软件 LSQ 镜像。

设计意义：

每个 testcase 或主表重新生成前，LSQ 软件模型必须回到空状态。否则上一轮残留的 enq/deq pointer 会导致后续 LQ/SQ index 预测错误。

## 6. is_vector_ls_futype()

源码：

```systemverilog
    static function bit is_vector_ls_futype(input bit [35:0] fuType);
        return fuType == MEMBLOCK_FUTYPE_VLDU    ||
               fuType == MEMBLOCK_FUTYPE_VSTU    ||
               fuType == MEMBLOCK_FUTYPE_VSEGLDU ||
               fuType == MEMBLOCK_FUTYPE_VSEGSTU;
    endfunction:is_vector_ls_futype
```

功能目的：

判断 `fuType` 是否属于 vector load/store 类。

输入：

- `fuType`：主表 transaction 中的功能单元类型。

输出：

- 返回 `1`：属于 vector LS。
- 返回 `0`：不是 vector LS。

调用函数：

- 无。

被哪些函数调用：

- `derive_op_behavior()`：如果发现 vector LS，当前简化模型直接 fatal。

在调用函数中的作用：

- `derive_op_behavior()` 用它做能力边界检查，避免把 vector load/store 错误按 scalar load/store 处理。

设计意义：

当前 LSQ 控制模型只完整支持 scalar LS。vector LS 可能一次占多个元素、拆分多个 uop，若按 scalar 处理会导致 LQ/SQ 分配数量和 issue 路径都不合法。

## 7. is_load_fuoptype()

源码：

```systemverilog
    static function bit is_load_fuoptype(input bit [8:0] fuOpType);
        return fuOpType == MEMBLOCK_LSUOP_LB  ||
               fuOpType == MEMBLOCK_LSUOP_LH  ||
               fuOpType == MEMBLOCK_LSUOP_LW  ||
               fuOpType == MEMBLOCK_LSUOP_LD  ||
               fuOpType == MEMBLOCK_LSUOP_LBU ||
               fuOpType == MEMBLOCK_LSUOP_LHU ||
               fuOpType == MEMBLOCK_LSUOP_LWU;
    endfunction:is_load_fuoptype
```

功能目的：

判断 `fuOpType` 是否是普通 load 操作。

输入：

- `fuOpType`：LSU 操作类型编码。

输出：

- `1`：普通 load。
- `0`：不是普通 load。

调用函数：

- 无。

被哪些函数调用：

- `derive_op_behavior()`：在 `fuType == MEMBLOCK_FUTYPE_LDU` 时用于区分普通 load 和非法 LDU op。
- `memblock_dispatch_base_sequence::is_load_fuoptype()`：对外提供基础判断包装。

在调用函数中的作用：

- 在 `derive_op_behavior()` 中，如果 LDU 且 `is_load_fuoptype()` 为真，则 behavior kind 设置为 `MEMBLOCK_OP_BEHAVIOR_LOAD`，并分配 LQ、route load。

## 8. is_prefetch_fuoptype()

源码：

```systemverilog
    static function bit is_prefetch_fuoptype(input bit [8:0] fuOpType);
        return fuOpType == MEMBLOCK_LSUOP_PREFETCH_I ||
               fuOpType == MEMBLOCK_LSUOP_PREFETCH_R ||
               fuOpType == MEMBLOCK_LSUOP_PREFETCH_W;
    endfunction:is_prefetch_fuoptype
```

功能目的：

判断 `fuOpType` 是否为 prefetch 操作。

输入：

- `fuOpType`：LSU 操作类型编码。

输出：

- `1`：prefetch。
- `0`：不是 prefetch。

调用函数：

- 无。

被哪些函数调用：

- `derive_op_behavior()`：在 LDU 路径中优先判断 prefetch。
- `memblock_dispatch_base_sequence::is_prefetch_fuoptype()`：基础 sequence 的包装函数。

在调用函数中的作用：

- 对 LDU 来说，prefetch 仍使用 LQ、route load，但 behavior kind 会标成 `MEMBLOCK_OP_BEHAVIOR_PREFETCH`，`is_prefetch=1`。

## 9. is_store_fuoptype()

源码：

```systemverilog
    static function bit is_store_fuoptype(input bit [8:0] fuOpType);
        return fuOpType == MEMBLOCK_LSUOP_SB ||
               fuOpType == MEMBLOCK_LSUOP_SH ||
               fuOpType == MEMBLOCK_LSUOP_SW ||
               fuOpType == MEMBLOCK_LSUOP_SD;
    endfunction:is_store_fuoptype
```

功能目的：

判断 `fuOpType` 是否为普通 store 操作。

输入：

- `fuOpType`：LSU 操作类型编码。

输出：

- `1`：普通 store。
- `0`：不是普通 store。

调用函数：

- 无。

被哪些函数调用：

- `derive_op_behavior()`：在 STU 路径中判断普通 store。
- `memblock_dispatch_base_sequence::is_store_fuoptype()`：基础 sequence 包装，同时会额外接受 CBO。
- `memblock_lintsissue_dispatch_sequence::mark_fired_items()` 附近用于判断 store 相关 issue 行为。

在调用函数中的作用：

- 在 `derive_op_behavior()` 中，普通 store 会设置 behavior 为 `STORE`，使用 SQ，并 route STA/STD 两条 issue 路径。

## 10. is_cbo_fuoptype()

源码：

```systemverilog
    static function bit is_cbo_fuoptype(input bit [8:0] fuOpType);
        bit [3:0] low4;

        low4 = fuOpType[3:0];
        return ((fuOpType[3:2] == 2'b11) && (fuOpType[6:4] == 3'b000)) ||
               (low4 == MEMBLOCK_LSUOP_CBO_ZERO[3:0]);
    endfunction:is_cbo_fuoptype
```

功能目的：

判断 `fuOpType` 是否为 CBO 类操作。

输入：

- `fuOpType`：LSU 操作类型编码。

输出：

- `1`：CBO 类操作。
- `0`：不是 CBO。

调用函数：

- 无。

内部逻辑：

- 先取 `low4 = fuOpType[3:0]`。
- 判断两类形式：
  - `fuOpType[3:2] == 2'b11 && fuOpType[6:4] == 3'b000`
  - 或低 4 bit 等于 `CBO_ZERO` 的低 4 bit。

被哪些函数调用：

- `derive_op_behavior()`：在 STU 路径中优先判断 CBO。
- `memblock_dispatch_base_sequence::is_store_fuoptype()`：将 CBO 也归为 store 类发射判断。

在调用函数中的作用：

- CBO 走 STU 类，使用 SQ，route STA/STD，但 behavior kind 标为 `MEMBLOCK_OP_BEHAVIOR_CBO`，`is_cbo=1`。

## 11. is_amocas_q_fuoptype()

源码：

```systemverilog
    static function bit is_amocas_q_fuoptype(input bit [8:0] fuOpType);
        return fuOpType[5:0] == MEMBLOCK_LSUOP_AMOCAS_Q_LO;
    endfunction:is_amocas_q_fuoptype
```

功能目的：

判断 AMO 操作是否为 AMOCAS_Q。

输入：

- `fuOpType`：LSU 操作类型编码。

输出：

- `1`：AMOCAS_Q。
- `0`：不是 AMOCAS_Q。

调用函数：

- 无。

被哪些函数调用：

- `derive_op_behavior()`：MOU/AMO 路径下设置 atomic STA/data uop 数量。

在调用函数中的作用：

- 如果是 AMOCAS_Q，`derive_op_behavior()` 设置：
  - `atomic_sta_uop_count = 2`
  - `atomic_data_uop_count = 4`

设计意义：

AMOCAS_Q 宽度更大，测试框架抽象成更多地址侧和数据侧 uop。

## 12. is_amocas_wd_fuoptype()

源码：

```systemverilog
    static function bit is_amocas_wd_fuoptype(input bit [8:0] fuOpType);
        return fuOpType[5:0] == MEMBLOCK_LSUOP_AMOCAS_W_LO ||
               fuOpType[5:0] == MEMBLOCK_LSUOP_AMOCAS_D_LO;
    endfunction:is_amocas_wd_fuoptype
```

功能目的：

判断 AMO 操作是否为 AMOCAS_W 或 AMOCAS_D。

输入：

- `fuOpType`：LSU 操作类型编码。

输出：

- `1`：AMOCAS_W 或 AMOCAS_D。
- `0`：不是这两类。

调用函数：

- 无。

被哪些函数调用：

- `derive_op_behavior()`。

在调用函数中的作用：

- 如果是 AMOCAS_W/D，`derive_op_behavior()` 设置：
  - `atomic_sta_uop_count = 1`
  - `atomic_data_uop_count = 2`

## 13. is_amo_fuoptype()

源码：

```systemverilog
    static function bit is_amo_fuoptype(input bit [8:0] fuOpType);
        return fuOpType == MEMBLOCK_LSUOP_LR_W     ||
               fuOpType == MEMBLOCK_LSUOP_SC_W     ||
               fuOpType == MEMBLOCK_LSUOP_AMOSWAP_W ||
               fuOpType == MEMBLOCK_LSUOP_AMOADD_W ||
               fuOpType == MEMBLOCK_LSUOP_AMOXOR_W ||
               fuOpType == MEMBLOCK_LSUOP_AMOAND_W ||
               fuOpType == MEMBLOCK_LSUOP_AMOOR_W  ||
               fuOpType == MEMBLOCK_LSUOP_AMOMIN_W ||
               fuOpType == MEMBLOCK_LSUOP_AMOMAX_W ||
               fuOpType == MEMBLOCK_LSUOP_AMOMINU_W ||
               fuOpType == MEMBLOCK_LSUOP_AMOMAXU_W ||
               fuOpType == MEMBLOCK_LSUOP_LR_D     ||
               fuOpType == MEMBLOCK_LSUOP_SC_D     ||
               fuOpType == MEMBLOCK_LSUOP_AMOSWAP_D ||
               fuOpType == MEMBLOCK_LSUOP_AMOADD_D ||
               fuOpType == MEMBLOCK_LSUOP_AMOXOR_D ||
               fuOpType == MEMBLOCK_LSUOP_AMOAND_D ||
               fuOpType == MEMBLOCK_LSUOP_AMOOR_D  ||
               fuOpType == MEMBLOCK_LSUOP_AMOMIN_D ||
               fuOpType == MEMBLOCK_LSUOP_AMOMAX_D ||
               fuOpType == MEMBLOCK_LSUOP_AMOMINU_D ||
               fuOpType == MEMBLOCK_LSUOP_AMOMAXU_D ||
               (fuOpType[5:2] == 4'b1011);
    endfunction:is_amo_fuoptype
```

功能目的：

判断 `fuOpType` 是否为 AMO/LR/SC/AMOCAS 类操作。

输入：

- `fuOpType`：LSU 操作类型编码。

输出：

- `1`：AMO 类。
- `0`：不是 AMO。

调用函数：

- 无。

被哪些函数调用：

- `derive_op_behavior()`：MOU 路径必须是 AMO，否则 fatal。
- `memblock_dispatch_base_sequence::is_amo_fuoptype()`：基础 sequence 包装。

在调用函数中的作用：

- `derive_op_behavior()` 用它确认 `MEMBLOCK_FUTYPE_MOU` 的 op 合法，再设置 atomic behavior。

## 14. make_default_behavior()

源码：

```systemverilog
    static function memblock_op_behavior_t make_default_behavior();
        memblock_op_behavior_t behavior;

        behavior.kind                   = MEMBLOCK_OP_BEHAVIOR_UNKNOWN;
        behavior.need_alloc             = 2'b00;
        behavior.uses_lq                = 1'b0;
        behavior.uses_sq                = 1'b0;
        behavior.route_load             = 1'b0;
        behavior.route_sta              = 1'b0;
        behavior.route_std              = 1'b0;
        behavior.commit_is_load         = 1'b0;
        behavior.commit_is_store        = 1'b0;
        behavior.commit_is_normal       = 1'b1;
        behavior.is_prefetch            = 1'b0;
        behavior.is_cbo                 = 1'b0;
        behavior.is_atomic              = 1'b0;
        behavior.num_ls_elem            = 5'd0;
        behavior.atomic_sta_uop_count   = 3'd0;
        behavior.atomic_data_uop_count  = 3'd0;
        return behavior;
    endfunction:make_default_behavior
```

功能目的：

构造一个安全默认的 `memblock_op_behavior_t`。

输入：

- 无。

输出：

- 返回默认 behavior。

调用函数：

- 无。

被哪些函数调用：

- `derive_op_behavior()`：先创建默认 behavior，再根据 fuType/fuOpType 打开相应字段。
- `memblock_lsqenq_dispatch_sequence::next_uid_needs_lsq_admission()`：无 candidate 时返回默认 behavior。
- `memblock_lsqenq_dispatch_sequence::admit_non_lsq_if_ready()` 等逻辑间接依赖默认 behavior。

设计意义：

默认 behavior 把所有 route/uses/atomic 字段清零，只保留 `commit_is_normal=1`。这样每条路径只需要设置自己需要的字段，避免结构体字段残留。

## 15. derive_op_behavior()

源码：

```systemverilog
    static function memblock_op_behavior_t derive_op_behavior(input main_control_transaction tr);
        memblock_op_behavior_t behavior;

        if (tr == null) begin
            `uvm_fatal("LSQ_CTRL", "derive_op_behavior got null transaction")
        end
        if (is_vector_ls_futype(tr.fuType)) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("uid=%0d vector LS is not supported by initial lsq_ctrl_model", tr.uid))
        end

        behavior = make_default_behavior();

        if (tr.fuType == MEMBLOCK_FUTYPE_LDU) begin
            behavior.need_alloc     = 2'b01;
            behavior.uses_lq        = 1'b1;
            behavior.route_load     = 1'b1;
            behavior.commit_is_load = 1'b1;
            behavior.commit_is_normal = 1'b0;
            behavior.num_ls_elem    = 5'd1;
            if (is_prefetch_fuoptype(tr.fuOpType)) begin
                behavior.kind        = MEMBLOCK_OP_BEHAVIOR_PREFETCH;
                behavior.is_prefetch = 1'b1;
            end else if (is_load_fuoptype(tr.fuOpType)) begin
                behavior.kind = MEMBLOCK_OP_BEHAVIOR_LOAD;
            end else begin
                `uvm_fatal("LSQ_CTRL", $sformatf("uid=%0d has illegal LDU fuOpType=%0d", tr.uid, tr.fuOpType))
            end
        end else if (tr.fuType == MEMBLOCK_FUTYPE_STU) begin
            behavior.need_alloc      = 2'b10;
            behavior.uses_sq         = 1'b1;
            behavior.route_sta       = 1'b1;
            behavior.route_std       = 1'b1;
            behavior.commit_is_store = 1'b1;
            behavior.commit_is_normal = 1'b0;
            behavior.num_ls_elem     = 5'd1;
            if (is_cbo_fuoptype(tr.fuOpType)) begin
                behavior.kind   = MEMBLOCK_OP_BEHAVIOR_CBO;
                behavior.is_cbo = 1'b1;
            end else if (is_store_fuoptype(tr.fuOpType)) begin
                behavior.kind = MEMBLOCK_OP_BEHAVIOR_STORE;
            end else begin
                `uvm_fatal("LSQ_CTRL", $sformatf("uid=%0d has illegal STU fuOpType=%0d", tr.uid, tr.fuOpType))
            end
        end else if (tr.fuType == MEMBLOCK_FUTYPE_MOU) begin
            if (!is_amo_fuoptype(tr.fuOpType)) begin
                `uvm_fatal("LSQ_CTRL", $sformatf("uid=%0d has illegal MOU fuOpType=%0d", tr.uid, tr.fuOpType))
            end
            behavior.kind             = MEMBLOCK_OP_BEHAVIOR_ATOMIC;
            behavior.need_alloc       = 2'b00;
            behavior.route_sta        = 1'b1;
            behavior.route_std        = 1'b1;
            behavior.commit_is_normal = 1'b1;
            behavior.is_atomic        = 1'b1;
            behavior.num_ls_elem      = 5'd0;
            if (is_amocas_q_fuoptype(tr.fuOpType)) begin
                behavior.atomic_sta_uop_count  = 3'd2;
                behavior.atomic_data_uop_count = 3'd4;
            end else if (is_amocas_wd_fuoptype(tr.fuOpType)) begin
                behavior.atomic_sta_uop_count  = 3'd1;
                behavior.atomic_data_uop_count = 3'd2;
            end else begin
                behavior.atomic_sta_uop_count  = 3'd1;
                behavior.atomic_data_uop_count = 3'd1;
            end
        end else begin
            `uvm_fatal("LSQ_CTRL", $sformatf("uid=%0d has unsupported fuType=0x%0h", tr.uid, tr.fuType))
        end

        return behavior;
    endfunction:derive_op_behavior
```

功能目的：

把主表 transaction 的 `fuType/fuOpType` 翻译成测试框架统一使用的 `memblock_op_behavior_t`。

输入：

- `tr`：主表 transaction，必须非空。使用其中的 `uid/fuType/fuOpType`。

输出：

- 返回 `memblock_op_behavior_t`，描述这条操作：
  - 是否需要 LSQ allocation
  - 使用 LQ 还是 SQ
  - route 到 load/STA/STD 哪些 issue queue
  - commit 类型
  - 是否 prefetch/CBO/atomic
  - atomic 拆成多少地址侧和数据侧 uop

调用函数：

- `is_vector_ls_futype()`：判断是否 unsupported vector LS。
- `make_default_behavior()`：生成默认 behavior。
- `is_prefetch_fuoptype()`：判断 LDU prefetch。
- `is_load_fuoptype()`：判断普通 load。
- `is_cbo_fuoptype()`：判断 CBO。
- `is_store_fuoptype()`：判断普通 store。
- `is_amo_fuoptype()`：判断 MOU 是否为合法 AMO。
- `is_amocas_q_fuoptype()`：判断 AMOCAS_Q。
- `is_amocas_wd_fuoptype()`：判断 AMOCAS_W/D。

内部调用函数的作用：

- 类型判断函数把编码识别集中在 `lsq_ctrl_model` 内，避免各个 sequence 自己散落判断。
- `make_default_behavior()` 保证所有字段从安全默认值开始设置。

被哪些上层调用：

- `memblock_lsqenq_dispatch_sequence::next_uid_needs_lsq_admission()`：判断下一个 uid 是否需要 LSQ admission。
- `memblock_lsqenq_dispatch_sequence::collect_lsq_candidates()`：收集一拍可入队 candidates。
- `issue_field_assigner.sv`、`issue_queue_scheduler.sv`：决定 issue route 和 uop 字段。
- `memblock_dispatch_base_sequence::derive_op_behavior()`：作为外部包装入口。

关键行为：

- LDU load/prefetch：
  - `uses_lq=1`
  - `need_alloc=2'b01`
  - `route_load=1`
  - `num_ls_elem=1`
- STU store/CBO：
  - `uses_sq=1`
  - `need_alloc=2'b10`
  - `route_sta=1`
  - `route_std=1`
  - `num_ls_elem=1`
- MOU atomic：
  - 当前不分配普通 LQ/SQ
  - `route_sta=1`
  - `route_std=1`
  - AMOCAS 根据宽度设置多个 uop count

## 16. advance_lq_key()

源码：

```systemverilog
    static function memblock_lq_key_t advance_lq_key(input memblock_lq_key_t base,
                                                     input int unsigned step);
        memblock_lq_key_t cur;

        if (base.value >= MEMBLOCK_LQ_SIZE) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("lqIdx value=%0d exceeds LQ size=%0d", base.value, MEMBLOCK_LQ_SIZE))
        end
        cur = base;
        repeat (step) begin
            if (cur.value == MEMBLOCK_LQ_SIZE - 1) begin
                cur.value = '0;
                cur.flag  = ~cur.flag;
            end else begin
                cur.value = cur.value + 1'b1;
            end
        end
        return cur;
    endfunction:advance_lq_key
```

功能目的：

按 LQ 环形队列规则从 `base` 前进 `step` 个元素。

输入：

- `base`：起始 LQ key，包含 `flag/value`。
- `step`：前进步数。

输出：

- 返回前进后的 LQ key。

调用函数：

- 无。

内部逻辑：

- 先检查 `base.value` 是否越过 `MEMBLOCK_LQ_SIZE`。
- 每前进一步：
  - 如果 value 到达最后一项，则 value 回到 0，同时翻转 flag。
  - 否则 value 加 1。

被哪些函数调用：

- `commit_allocate()`：分配 LQ 后推进 `lq_enq_ptr`。
- `commit_allocate_with_resp()`：真实入队确认后推进 `lq_enq_ptr`。
- `release_lq()`：DUT deq 后推进 `lq_deq_ptr`。
- `lsq_commit_handler::apply_dut_lq_deq()`：枚举 DUT deq 的每个 LQ key。
- `memblock_lsqenq_dispatch_sequence::collect_lsq_candidates()`：临时预测一拍多个 candidate 的 key。

在调用函数中的作用：

- 它是所有 LQ 指针前进的统一实现，保证入队预测、出队释放和批量 candidate 预览使用同一套 wrap/flag 规则。

## 17. advance_sq_key()

源码：

```systemverilog
    static function memblock_sq_key_t advance_sq_key(input memblock_sq_key_t base,
                                                     input int unsigned step);
        memblock_sq_key_t cur;

        if (base.value >= MEMBLOCK_SQ_SIZE) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("sqIdx value=%0d exceeds SQ size=%0d", base.value, MEMBLOCK_SQ_SIZE))
        end
        cur = base;
        repeat (step) begin
            if (cur.value == MEMBLOCK_SQ_SIZE - 1) begin
                cur.value = '0;
                cur.flag  = ~cur.flag;
            end else begin
                cur.value = cur.value + 1'b1;
            end
        end
        return cur;
    endfunction:advance_sq_key
```

功能目的：

按 SQ 环形队列规则从 `base` 前进 `step` 个元素。

输入：

- `base`：起始 SQ key。
- `step`：前进步数。

输出：

- 返回前进后的 SQ key。

调用函数：

- 无。

被哪些函数调用：

- `commit_allocate()`、`commit_allocate_with_resp()`：分配 SQ 后推进 `sq_enq_ptr`。
- `release_sq()`：DUT sqDeq 后推进 `sq_deq_ptr`。
- `lsq_commit_handler::apply_dut_sq_deq()`：枚举 DUT deq 的 SQ key。
- `memblock_lsqenq_dispatch_sequence::collect_lsq_candidates()`：预测批量 enqueue 的 SQ key。

在调用函数中的作用：

- 它是 SQ 指针前进的唯一规则入口，避免 SQ wrap/flag 在不同模块中实现不一致。

## 18. rewind_lq_key()

源码：

```systemverilog
    static function memblock_lq_key_t rewind_lq_key(input memblock_lq_key_t base,
                                                    input int unsigned step);
        memblock_lq_key_t cur;

        if (base.value >= MEMBLOCK_LQ_SIZE) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("lqIdx value=%0d exceeds LQ size=%0d", base.value, MEMBLOCK_LQ_SIZE))
        end
        cur = base;
        repeat (step) begin
            if (cur.value == 0) begin
                cur.value = MEMBLOCK_LQ_SIZE - 1;
                cur.flag  = ~cur.flag;
            end else begin
                cur.value = cur.value - 1'b1;
            end
        end
        return cur;
    endfunction:rewind_lq_key
```

功能目的：

按 LQ 环形队列规则从 `base` 回退 `step` 个元素。

输入：

- `base`：起始 LQ key。
- `step`：回退步数。

输出：

- 返回回退后的 LQ key。

调用函数：

- 无。

被哪些函数调用：

- `cancel_lq()`：取消已预留/已入队资源时回退 LQ enqueue pointer。
- `lsq_commit_handler::lq_deq_start_key()`：当 DUT deq pointer 表示 next pointer 时，回退 count 得到本次 deq 起点。

在调用函数中的作用：

- 在 `lq_deq_start_key()` 中，它把 DUT 返回的 next pointer 转成实际 deq 的第一个 key，用于和软件 `lq_deq_ptr` 对齐检查。

## 19. rewind_sq_key()

源码：

```systemverilog
    static function memblock_sq_key_t rewind_sq_key(input memblock_sq_key_t base,
                                                    input int unsigned step);
        memblock_sq_key_t cur;

        if (base.value >= MEMBLOCK_SQ_SIZE) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("sqIdx value=%0d exceeds SQ size=%0d", base.value, MEMBLOCK_SQ_SIZE))
        end
        cur = base;
        repeat (step) begin
            if (cur.value == 0) begin
                cur.value = MEMBLOCK_SQ_SIZE - 1;
                cur.flag  = ~cur.flag;
            end else begin
                cur.value = cur.value - 1'b1;
            end
        end
        return cur;
    endfunction:rewind_sq_key
```

功能目的：

按 SQ 环形队列规则从 `base` 回退 `step` 个元素。

输入：

- `base`：起始 SQ key。
- `step`：回退步数。

输出：

- 返回回退后的 SQ key。

调用函数：

- 无。

被哪些函数调用：

- `cancel_sq()`。
- `lsq_commit_handler::sq_deq_start_key()`。

在调用函数中的作用：

- 在 deq 处理时，如果 DUT 返回的是 deq 后的 next pointer，`rewind_sq_key()` 用于倒推出本次释放的起点。

## 20. can_allocate()

源码：

```systemverilog
    function bit can_allocate(input memblock_op_behavior_t behavior);
        if (behavior.uses_lq && lq_free_count < behavior.num_ls_elem) begin
            return 1'b0;
        end
        if (behavior.uses_sq && sq_free_count < behavior.num_ls_elem) begin
            return 1'b0;
        end
        return 1'b1;
    endfunction:can_allocate
```

功能目的：

根据当前软件 free count 判断一条 behavior 是否有足够 LQ/SQ 资源。

输入：

- `behavior`：由 `derive_op_behavior()` 生成的操作行为。

输出：

- `1`：资源足够。
- `0`：资源不足。

调用函数：

- 无。

被哪些函数调用：

- `preview_allocate()`。

在调用函数中的作用：

- `preview_allocate()` 调用它，资源不足时 fatal，而不是返回错误 key。

设计意义：

资源判断必须同时考虑：

- 是否使用 LQ：`behavior.uses_lq`
- 是否使用 SQ：`behavior.uses_sq`
- 需要几个元素：`behavior.num_ls_elem`

当前 scalar load/store 通常 `num_ls_elem=1`，但函数保留了多元素能力。

## 21. preview_allocate()

源码：

```systemverilog
    function void preview_allocate(input memblock_op_behavior_t behavior,
                                   output memblock_lq_key_t lq_key,
                                   output memblock_sq_key_t sq_key);
        lq_key = lq_enq_ptr;
        sq_key = sq_enq_ptr;
        if (!can_allocate(behavior)) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("preview_allocate lacks resources: kind=%0d lq_free=%0d sq_free=%0d need=%0d",
                                             behavior.kind, lq_free_count, sq_free_count, behavior.num_ls_elem))
        end
    endfunction:preview_allocate
```

功能目的：

预览当前 transaction 如果入队，软件预期它应获得的 LQ/SQ key。

输入：

- `behavior`：操作行为。

输出：

- `lq_key`：当前 `lq_enq_ptr`。
- `sq_key`：当前 `sq_enq_ptr`。

调用函数：

- `can_allocate(behavior)`。

内部调用函数作用：

- `can_allocate()` 检查 free count 是否足够。如果不足，说明 sequence 不应该继续入队，直接 fatal 暴露资源模型错误。

被哪些函数调用：

- `commit_allocate()`。
- `commit_allocate_with_resp()`。

设计意义：

preview 不改变 pointer/free count，只给出当前预期 key。真实 pointer 推进发生在 commit 阶段。

## 22. commit_allocate()

源码：

```systemverilog
    function void commit_allocate(input memblock_uid_t uid,
                                  input memblock_op_behavior_t behavior,
                                  input main_control_transaction tr);
        memblock_lq_key_t lq_key;
        memblock_sq_key_t sq_key;

        if (data == null) begin
            data = common_data_transaction::get();
        end
        if (tr == null) begin
            `uvm_fatal("LSQ_CTRL", "commit_allocate got null transaction")
        end
        if (tr.uid != uid) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("commit_allocate uid mismatch: arg=%0d tr.uid=%0d", uid, tr.uid))
        end
        preview_allocate(behavior, lq_key, sq_key);

        tr.lqIdx_flag  = lq_key.flag;
        tr.lqIdx_value = lq_key.value;
        tr.sqIdx_flag  = sq_key.flag;
        tr.sqIdx_value = sq_key.value;
        tr.numLsElem   = behavior.num_ls_elem;

        data.set_main_transaction(uid, tr);
        data.activate_uid(uid, behavior.uses_lq, behavior.uses_sq);
        data.set_status_field(uid, MEMBLOCK_STATUS_ENQ, 1'b1);

        if (behavior.uses_lq) begin
            lq_enq_ptr = advance_lq_key(lq_enq_ptr, behavior.num_ls_elem);
            lq_free_count -= behavior.num_ls_elem;
        end
        if (behavior.uses_sq) begin
            sq_enq_ptr = advance_sq_key(sq_enq_ptr, behavior.num_ls_elem);
            sq_free_count -= behavior.num_ls_elem;
        end
    endfunction:commit_allocate
```

功能目的：

提交一次软件预测的 LSQ allocation，并把分配结果写入主表和状态表。

输入：

- `uid`：主表 uid。
- `behavior`：该 transaction 的操作行为。
- `tr`：主表 transaction。

输出：

- 无返回值。

状态修改：

- 给 `tr` 写入 LQ/SQ key 和 `numLsElem`。
- 调用 `data.set_main_transaction()` 更新主表。
- 调用 `data.activate_uid()` 激活 uid，并建立 active ROB/LQ/SQ map。
- 设置状态 `MEMBLOCK_STATUS_ENQ=1`。
- 推进 LQ/SQ enqueue pointer。
- 减少 LQ/SQ free count。

调用函数：

- `common_data_transaction::get()`：data 为空时重新获取。
- `preview_allocate()`：得到当前预期 LQ/SQ key，并检查资源。
- `data.set_main_transaction()`：更新公共主表。
- `data.activate_uid()`：激活 uid 和 LQ/SQ mapping。
- `data.set_status_field()`：标记 enq 状态。
- `advance_lq_key()`：推进 LQ enqueue pointer。
- `advance_sq_key()`：推进 SQ enqueue pointer。

内部调用函数作用：

- `preview_allocate()` 只预览和检查，不改变 pointer。
- `activate_uid()` 是把 uid 放进公共 active 生命周期的关键步骤。
- `advance_*_key()` 在确认写表后推进软件 pointer，表示资源已经被占用。

被哪些上层调用：

- soft smoke sequence 直接使用。
- `commit_non_lsq_admission()` 间接调用。

注意：

真实 DUT LSQ enqueue flow 更常用 `commit_allocate_with_resp()`，因为它要校验 DUT 返回的 LQ/SQ key 是否等于软件预测。

## 23. commit_allocate_with_resp()

源码：

```systemverilog
    function void commit_allocate_with_resp(input memblock_uid_t uid,
                                            input memblock_op_behavior_t behavior,
                                            input main_control_transaction tr,
                                            input memblock_lq_key_t dut_lq_key,
                                            input memblock_sq_key_t dut_sq_key);
        memblock_lq_key_t expected_lq_key;
        memblock_sq_key_t expected_sq_key;

        if (data == null) begin
            data = common_data_transaction::get();
        end
        if (tr == null) begin
            `uvm_fatal("LSQ_CTRL", "commit_allocate_with_resp got null transaction")
        end
        if (tr.uid != uid) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("commit_allocate_with_resp uid mismatch: arg=%0d tr.uid=%0d", uid, tr.uid))
        end
        if (behavior.need_alloc == 2'b00) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("commit_allocate_with_resp got non-LSQ admission uid=%0d", uid))
        end

        preview_allocate(behavior, expected_lq_key, expected_sq_key);
        if (dut_lq_key.flag  != expected_lq_key.flag  ||
            dut_lq_key.value != expected_lq_key.value ||
            dut_sq_key.flag  != expected_sq_key.flag  ||
            dut_sq_key.value != expected_sq_key.value) begin
            `uvm_fatal("LSQ_CTRL",
                       $sformatf("uid=%0d LSQ enq resp mismatch: expected lq={%0d,%0d} sq={%0d,%0d}, got lq={%0d,%0d} sq={%0d,%0d}",
                                 uid,
                                 expected_lq_key.flag,
                                 expected_lq_key.value,
                                 expected_sq_key.flag,
                                 expected_sq_key.value,
                                 dut_lq_key.flag,
                                 dut_lq_key.value,
                                 dut_sq_key.flag,
                                 dut_sq_key.value))
        end

        tr.lqIdx_flag  = dut_lq_key.flag;
        tr.lqIdx_value = dut_lq_key.value;
        tr.sqIdx_flag  = dut_sq_key.flag;
        tr.sqIdx_value = dut_sq_key.value;
        tr.numLsElem   = behavior.num_ls_elem;

        data.set_main_transaction(uid, tr);
        data.activate_uid(uid, behavior.uses_lq, behavior.uses_sq);
        data.set_status_field(uid, MEMBLOCK_STATUS_ENQ, 1'b1);

        if (behavior.uses_lq) begin
            lq_enq_ptr = advance_lq_key(lq_enq_ptr, behavior.num_ls_elem);
            lq_free_count -= behavior.num_ls_elem;
        end
        if (behavior.uses_sq) begin
            sq_enq_ptr = advance_sq_key(sq_enq_ptr, behavior.num_ls_elem);
            sq_free_count -= behavior.num_ls_elem;
        end
    endfunction:commit_allocate_with_resp
```

功能目的：

真实 LSQ enqueue flow 中，用 DUT 返回的 LQ/SQ key 校验软件预测，并提交 allocation。

输入：

- `uid`：主表 uid。
- `behavior`：操作行为，必须需要 LSQ allocation。
- `tr`：主表 transaction。
- `dut_lq_key`：DUT enqueue response 返回的 LQ key。
- `dut_sq_key`：DUT enqueue response 返回的 SQ key。

输出：

- 无返回值。

调用函数：

- `common_data_transaction::get()`
- `preview_allocate()`
- `data.set_main_transaction()`
- `data.activate_uid()`
- `data.set_status_field()`
- `advance_lq_key()`
- `advance_sq_key()`

内部调用函数作用：

- `preview_allocate()` 生成软件预期 key。
- 本函数比较 DUT key 和 expected key，如果不一致 fatal。
- `activate_uid()` 建立 active map，让后续 writeback/deq 能通过 LQ/SQ key 找回 uid。
- `advance_*_key()` 在确认 DUT 与软件一致后推进软件 enqueue pointer。

被哪些上层调用：

- `memblock_lsqenq_dispatch_sequence::confirm_lsq_candidates()`：
  - 从 LSQ enqueue xaction 中读取 DUT response key。
  - 对每个 uid 调用 `commit_allocate_with_resp()`。
  - 然后执行 `complete_admission()` 建 TLB 并 route issue queue。

设计意义：

这是 real DUT admission 路径最关键的校验点。它保证测试框架的软件 LSQ 指针和 DUT 实际分配的 index 保持一致。如果不一致，后续 issue 使用的 LQ/SQ index、ctrl deq 释放、active map 反查都会错。

## 24. commit_non_lsq_admission()

源码：

```systemverilog
    function void commit_non_lsq_admission(input memblock_uid_t uid,
                                           input memblock_op_behavior_t behavior,
                                           input main_control_transaction tr);
        if (behavior.need_alloc != 2'b00 || behavior.uses_lq || behavior.uses_sq) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("commit_non_lsq_admission got LSQ allocating kind=%0d", behavior.kind))
        end
        commit_allocate(uid, behavior, tr);
    endfunction:commit_non_lsq_admission
```

功能目的：

提交不需要普通 LQ/SQ allocation 的 admission，例如当前模型中的部分 atomic/MOU 抽象路径。

输入：

- `uid`：主表 uid。
- `behavior`：操作行为，必须 `need_alloc==0` 且不使用 LQ/SQ。
- `tr`：主表 transaction。

输出：

- 无返回值。

调用函数：

- `commit_allocate(uid, behavior, tr)`。

内部调用函数作用：

- `commit_allocate()` 会写主表、激活 uid、设置 ENQ 状态。由于 `behavior.uses_lq/uses_sq` 为 0，不会推进 LQ/SQ pointer，也不会减少 free count。

被哪些上层调用：

- `memblock_lsqenq_dispatch_sequence::admit_non_lsq_if_ready()`。
- soft smoke sequence。

设计意义：

有些操作进入 dispatch 生命周期，但不占普通 LQ/SQ。这个函数为这类操作提供统一 admission 路径，同时防止真正需要 LSQ allocation 的行为误走非 LSQ 路径。

## 25. release_lq()

源码：

```systemverilog
    function void release_lq(input int unsigned count);
        if (count > (MEMBLOCK_LQ_SIZE - lq_free_count)) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("release_lq count=%0d exceeds allocated count=%0d",
                                             count, MEMBLOCK_LQ_SIZE - lq_free_count))
        end
        lq_deq_ptr = advance_lq_key(lq_deq_ptr, count);
        lq_free_count += count;
    endfunction:release_lq
```

功能目的：

DUT lqDeq 后，释放软件模型中的 LQ 资源。

输入：

- `count`：本次释放的 LQ 元素数量。

输出：

- 无返回值。

状态修改：

- 推进 `lq_deq_ptr`。
- 增加 `lq_free_count`。

调用函数：

- `advance_lq_key(lq_deq_ptr, count)`。

内部调用函数作用：

- `advance_lq_key()` 按环形队列规则推进 deq pointer，包含 wrap 和 flag 翻转。

被哪些上层调用：

- `lsq_commit_handler::apply_dut_lq_deq()`。

设计意义：

`release_lq()` 只维护软件 LSQ 资源，不直接释放 uid mapping。uid mapping 由 `common_data_transaction::release_uid_lq_mapping()` 在 `apply_dut_lq_deq()` 中逐 uid 释放。

## 26. release_sq()

源码：

```systemverilog
    function void release_sq(input int unsigned count);
        if (count > (MEMBLOCK_SQ_SIZE - sq_free_count)) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("release_sq count=%0d exceeds allocated count=%0d",
                                             count, MEMBLOCK_SQ_SIZE - sq_free_count))
        end
        sq_deq_ptr = advance_sq_key(sq_deq_ptr, count);
        sq_free_count += count;
    endfunction:release_sq
```

功能目的：

DUT sqDeq 后，释放软件模型中的 SQ 资源。

输入：

- `count`：本次释放的 SQ 元素数量。

输出：

- 无返回值。

状态修改：

- 推进 `sq_deq_ptr`。
- 增加 `sq_free_count`。

调用函数：

- `advance_sq_key(sq_deq_ptr, count)`。

被哪些上层调用：

- `lsq_commit_handler::apply_dut_sq_deq()`。

设计意义：

和 `release_lq()` 对称，用 DUT ctrl deq 作为真实资源释放源。释放后，`lsq_commit_handler` 会继续释放 uid 的 SQ active mapping，并尝试 retire 已 commit uid。

## 27. cancel_lq()

源码：

```systemverilog
    function void cancel_lq(input int unsigned count);
        if (count > (MEMBLOCK_LQ_SIZE - lq_free_count)) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("cancel_lq count=%0d exceeds allocated count=%0d",
                                             count, MEMBLOCK_LQ_SIZE - lq_free_count))
        end
        lq_enq_ptr = rewind_lq_key(lq_enq_ptr, count);
        lq_free_count += count;
    endfunction:cancel_lq
```

功能目的：

取消最近分配的 LQ 资源，回退 enqueue pointer。

输入：

- `count`：取消的 LQ 元素数量。

输出：

- 无返回值。

状态修改：

- 回退 `lq_enq_ptr`。
- 增加 `lq_free_count`。

调用函数：

- `rewind_lq_key(lq_enq_ptr, count)`。

内部调用函数作用：

- `rewind_lq_key()` 按环形队列规则回退 pointer，包含 value 回绕和 flag 翻转。

当前调用情况：

- 当前源码搜索中没有主 flow 直接调用 `cancel_lq()`。

设计意义：

这是为 redirect/cancel 类资源回退预留的基础能力。当前实际 redirect flush 更多是通过 active map 清理和 DUT deq/release 保守处理，cancel 函数尚未成为主路径。

## 28. cancel_sq()

源码：

```systemverilog
    function void cancel_sq(input int unsigned count);
        if (count > (MEMBLOCK_SQ_SIZE - sq_free_count)) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("cancel_sq count=%0d exceeds allocated count=%0d",
                                             count, MEMBLOCK_SQ_SIZE - sq_free_count))
        end
        sq_enq_ptr = rewind_sq_key(sq_enq_ptr, count);
        sq_free_count += count;
    endfunction:cancel_sq
```

功能目的：

取消最近分配的 SQ 资源，回退 enqueue pointer。

输入：

- `count`：取消的 SQ 元素数量。

输出：

- 无返回值。

状态修改：

- 回退 `sq_enq_ptr`。
- 增加 `sq_free_count`。

调用函数：

- `rewind_sq_key(sq_enq_ptr, count)`。

当前调用情况：

- 当前源码搜索中没有主 flow 直接调用 `cancel_sq()`。

设计意义：

和 `cancel_lq()` 对称，用于未来需要软件回退 SQ enqueue pointer 的场景。

## 29. 函数调用关系总览

### 29.1 LSQ enqueue real DUT 路径

```text
memblock_lsqenq_dispatch_sequence::collect_lsq_candidates()
  -> lsq_ctrl_model::derive_op_behavior()
  -> lsq_ctrl_model::advance_lq_key()/advance_sq_key()  临时预览批量 key

memblock_lsqenq_dispatch_sequence::confirm_lsq_candidates()
  -> lsq_ctrl_model::commit_allocate_with_resp()
     -> preview_allocate()
        -> can_allocate()
     -> data.set_main_transaction()
     -> data.activate_uid()
     -> data.set_status_field(ENQ)
     -> advance_lq_key()/advance_sq_key()
```

### 29.2 non-LSQ admission 路径

```text
memblock_lsqenq_dispatch_sequence::admit_non_lsq_if_ready()
  -> lsq_ctrl_model::commit_non_lsq_admission()
     -> commit_allocate()
        -> preview_allocate()
        -> data.set_main_transaction()
        -> data.activate_uid()
        -> data.set_status_field(ENQ)
```

### 29.3 DUT ctrl deq 释放路径

```text
io_mem_to_ooo_ctrl_agent monitor
  -> dispatch_monitor_event_adapter::apply_raw_ctrl_deq()
  -> lsq_commit_handler::apply_raw_ctrl_deq()
     -> apply_dut_lq_deq()
        -> lsq_ctrl_model::rewind_lq_key()  如果 DUT ptr 是 next ptr
        -> lsq_ctrl_model::advance_lq_key() 枚举 deq key
        -> lsq_ctrl.release_lq()
           -> advance_lq_key()
        -> data.release_uid_lq_mapping()
        -> data.try_retire_committed_uid()
     -> apply_dut_sq_deq()
        -> lsq_ctrl_model::rewind_sq_key()
        -> lsq_ctrl_model::advance_sq_key()
        -> lsq_ctrl.release_sq()
           -> advance_sq_key()
        -> data.release_uid_sq_mapping()
        -> data.try_retire_committed_uid()
```

## 30. 当前实现边界

- 当前 `derive_op_behavior()` 对 vector LS 直接 fatal，初版不支持 vector LS 资源拆分。
- 当前 atomic/MOU 抽象为不分配普通 LQ/SQ，但会 route STA/STD，并根据 AMOCAS 类型设置 uop 数量。
- `cancel_lq()` / `cancel_sq()` 当前是基础能力，主 flow 未直接使用。
- real DUT LSQ enqueue 路径以 `commit_allocate_with_resp()` 为准，软件预测必须和 DUT response key 完全一致。
- DUT ctrl deq 是 LQ/SQ 资源释放的真源，`release_lq()` / `release_sq()` 只在 deq handler 中释放 free count。
