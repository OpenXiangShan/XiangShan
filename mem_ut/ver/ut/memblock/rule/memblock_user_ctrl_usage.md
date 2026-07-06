# memblock user_ctrl 修改说明与使用方法

## 适用范围

本文说明 `mem_ut/ver/ut/memblock` 验证环境中的 `user_ctrl` 机制。

当前 `user_ctrl` 只支持通过本地个人配置文件控制，不再支持通过仿真命令行配置：

```text
mem_ut/ver/ut/memblock/cfg/user_cfg.local.sv
```

该文件被 git 忽略，用于个人固定配置；公共仓库只维护字段定义和默认模板。

## 设计目标

- 公共仓库中能看到所有可配置字段
- 每个字段都有明确默认值
- 默认不改变已有 env/testcase 行为
- 个人配置只写入本地 `user_cfg.local.sv`
- 缺失本地文件时自动生成一份所有字段 valid 为 0 的默认配置

## 文件分层

公共字段定义：

```text
mem_ut/ver/ut/memblock/cfg/user_cfg.sv
```

默认本地模板：

```text
mem_ut/ver/ut/memblock/cfg/user_cfg.local.default.sv
```

个人本地配置：

```text
mem_ut/ver/ut/memblock/cfg/user_cfg.local.sv
```

`user_cfg.local.sv` 会在 `memblock_user_cfg::load_local()` 中被 include。写配置时直接使用 `this` 访问 `memblock_user_cfg` 成员，不要在 local 文件中定义 class、module 或 package。

## 自动创建 local 文件

`mem_ut/ver/ut/memblock/sim/Makefile` 增加了 `ensure_user_cfg_local` 规则。

当执行 `compile`、`rtl`、`batch_run` 或远端封装的 `eda_compile`、`eda_run` 时，如果发现：

```text
mem_ut/ver/ut/memblock/cfg/user_cfg.local.sv
```

不存在，就会从：

```text
mem_ut/ver/ut/memblock/cfg/user_cfg.local.default.sv
```

自动复制生成一份本地文件。

生成的 local 文件列出所有当前支持的配置参数，并且所有 `<field>_valid` 默认都是 `1'b0`，因此默认不覆盖 env/testcase 配置。

## 覆盖流程

实际覆盖流程如下：

1. `memblock_env.build_phase()` 获取 testcase 传入的 `memblock_env_cfg`
2. 如果 testcase 未传 cfg，则 env 创建并 randomize 默认 cfg
3. `memblock_env.build_phase()` 调用 `this.cfg.apply_user_cfg()`
4. `apply_user_cfg()` 调用 `u_user_cfg.load_local()`
5. `load_local()` include `user_cfg.local.sv`
6. 如果 `enable=0`，退出，不覆盖任何 cfg
7. 先应用 `all_agent_ctrl`
8. 再应用每个 agent 自己的 `<agent>_ctrl`
9. 最终 cfg 再通过 `uvm_config_db` 下发给各 agent

最终优先级从低到高：

1. `memblock_env_cfg.post_randomize()` 中的环境默认配置
2. testcase 中手工设置的 cfg
3. 本地个人配置 `user_cfg.local.sv`

同一层内，`all_agent_ctrl` 先应用，单个 agent 的 `<agent>_ctrl` 后应用。

## class 结构

`memblock_user_agent_ctrl` 描述单个 agent 可被用户覆盖的字段。

每个字段采用 `<field>_valid + <field>` 的成对结构，例如：

```systemverilog
bit drv_mode_valid;
tcnt_dec_base::drv_mode_e drv_mode;
```

含义：

- `<field>_valid = 0`：该字段不参与覆盖，保持 env/testcase 原有配置
- `<field>_valid = 1`：使用 `user_ctrl` 中的 `<field>` 覆盖目标 agent cfg

`memblock_user_cfg` 汇总所有 agent 的用户配置入口：

- `all_agent_ctrl`：一次性覆盖所有 agent
- `u_<agent>_agent_ctrl`：只覆盖指定 agent

## 当前支持字段

每个 agent 支持以下覆盖字段：

| 字段 | 类型 | local 默认 valid | 说明 |
| --- | --- | --- | --- |
| `sqr_sw` | `tcnt_dec_base::switch_mode_e` | `0` | sequencer 开关 |
| `drv_sw` | `tcnt_dec_base::switch_mode_e` | `0` | driver 开关 |
| `mon_sw` | `tcnt_dec_base::switch_mode_e` | `0` | monitor 开关 |
| `xz_sw` | `tcnt_dec_base::switch_mode_e` | `0` | X/Z 检查开关 |
| `drv_mode` | `tcnt_dec_base::drv_mode_e` | `0` | driver idle/random 模式 |
| `channel_id` | `int` | `0` | agent channel id |

合法取值：

```text
sqr_sw/drv_sw/mon_sw/xz_sw: tcnt_dec_base::ON, tcnt_dec_base::OFF
drv_mode: tcnt_dec_base::DRV_0, tcnt_dec_base::DRV_1, tcnt_dec_base::DRV_X,
          tcnt_dec_base::DRV_RAND, tcnt_dec_base::DRV_LST,
          tcnt_dec_base::DRV_RAND_ONLY_DATA
channel_id: decimal int
```

## 当前 control 对象

`user_cfg.local.sv` 当前列出以下对象：

| 覆盖范围 | control 对象 |
| --- | --- |
| all agents | `all_agent_ctrl` |
| `backendToTopBypass` | `u_backendToTopBypass_agent_agent_ctrl` |
| `fence` | `u_fence_agent_agent_ctrl` |
| `csr_ctrl` | `u_csr_ctrl_agent_agent_ctrl` |
| `lsqcommit` | `u_lsqcommit_agent_agent_ctrl` |
| `lsqenq` | `u_lsqenq_agent_agent_ctrl` |
| `lintsissue` | `u_lintsissue_agent_agent_ctrl` |
| `vecissue` | `u_vecissue_agent_agent_ctrl` |
| `redirect` | `u_redirect_agent_agent_ctrl` |
| `sbuffer` | `u_sbuffer_agent_agent_ctrl` |
| `dcache` | `u_dcache_agent_agent_ctrl` |
| `int_sink` | `u_int_sink_agent_agent_ctrl` |
| `L2tlb` | `u_L2tlb_agent_agent_ctrl` |
| `itlb` | `u_itlb_agent_agent_ctrl` |
| `prefetch` | `u_prefetch_agent_agent_ctrl` |
| `io_mem_to_ooo_ctrl` | `u_io_mem_to_ooo_ctrl_agent_agent_ctrl` |
| `io_mem_to_ooo_int_wb` | `u_io_mem_to_ooo_int_wb_agent_agent_ctrl` |
| `io_mem_to_ooo_vec_wb` | `u_io_mem_to_ooo_vec_wb_agent_agent_ctrl` |
| `io_mem_to_ooo_wakeup` | `u_io_mem_to_ooo_wakeup_agent_agent_ctrl` |
| `io_mem_to_ooo_iq_feedback` | `u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl` |
| `other_ctrl` | `u_other_ctrl_agent_agent_ctrl` |

## 本地配置写法

示例：只把 `lsqenq` 的 driver 模式固定为 `DRV_0`：

```systemverilog
this.u_lsqenq_agent_agent_ctrl.drv_mode_valid = 1'b1;
this.u_lsqenq_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
```

示例：关闭 `prefetch` agent 的 driver：

```systemverilog
this.u_prefetch_agent_agent_ctrl.drv_sw_valid = 1'b1;
this.u_prefetch_agent_agent_ctrl.drv_sw = tcnt_dec_base::OFF;
```

示例：关闭所有 agent 的 X/Z 检查：

```systemverilog
this.all_agent_ctrl.xz_sw_valid = 1'b1;
this.all_agent_ctrl.xz_sw = tcnt_dec_base::OFF;
```

示例：修改某个 agent 的 channel id：

```systemverilog
this.u_lsqenq_agent_agent_ctrl.channel_id_valid = 1'b1;
this.u_lsqenq_agent_agent_ctrl.channel_id = 4;
```

## 编译生效规则

`user_cfg.local.sv` 是被 `` `include`` 进编译单元的 SystemVerilog 源码。

因此修改 `user_cfg.local.sv` 后必须重新编译：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

只重新运行仿真不会重新读取 local 文件的新内容。

## 新增字段扩展方法

如果后续要新增用户可覆盖字段，参考：

```text
mem_ut/ver/ut/memblock/rule/memblock_cfg_add_rule.md
```

基本步骤：

1. 在 `memblock_user_agent_ctrl` 中新增 `<field>_valid`
2. 在 `memblock_user_agent_ctrl` 中新增 `<field>`
3. 在 `set_default()` 中设置默认值
4. 在 `apply_to()` 中只在 `<field>_valid=1` 时覆盖目标 cfg
5. 同步更新 `user_cfg.local.default.sv`，为所有 control 对象补齐该字段，并把新增 `<field>_valid` 默认设为 `1'b0`
6. 同步更新当前环境中的 `user_cfg.local.sv`，补齐该字段，并把新增 `<field>_valid` 默认设为 `1'b0`
7. 更新本文档中的字段表、合法值和示例
8. 运行格式检查和必要的远端编译/仿真

禁止新增没有 valid 位的个人覆盖字段。

## 验证要求

修改 cfg 或 user_ctrl 后至少执行：

```bash
git diff --check -- mem_ut/ver/ut/memblock/cfg mem_ut/ver/ut/memblock/env mem_ut/ver/ut/memblock/sim/Makefile
```

如果影响编译路径、package include、class 定义或 env 接入，应执行远端编译：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```
