# MemBlock L2TLB WAITING UID 逻辑扇出容量修复 Plan

状态：coding、专项和原失败seed复验通过；100-seed回归恢复中

创建日期：2026-10-19

适用版本：V2 `mem_ut_uvm_v2`，基线`8e45befc10bc1ab93d465ebc3ac16a91d6b750c6`

## 1. 问题与责任

100-seed回归的`seed=292095580`在`793675.000ns`报：

```text
L2TLB WAITING index capacity overflow uid=3222 size=32 max=32
```

此时已有3144笔RM compare，未出现DUT assertion、协议或数据错误。测试框架把
`uid_waiting_by_vpn_s2xlate[{vpn,s2xlate}]`中的逻辑WAITING UID数量错误限制为
`MEMBLOCK_DUT_L2TLB_DFILTER_SIZE=32`。

V2 `PTWNewFilter`的32项是不同物理翻译请求的容量；相同`vpn/s2xlate`请求通过
`oldMatchVec/newMatchVec/canMerge()`合并到已有物理entry，一个物理entry可代表多个LSQ/ROB请求。
因此第33个同VPN逻辑UID合法，责任属于测试框架，不修改DUT RTL。

## 2. 修改方案

1. `prune_uid_waiting_index_bucket()`清除无效、COMPLETED、CANCELED索引后，使用
   `MEMBLOCK_ROB_SIZE`检查逻辑活跃窗口泄漏，不再使用DFilter物理容量。
2. `add_waiting_uid_to_index()`继续严格拒绝重复UID、null/非WAITING record和shape不一致；插入前的
   防泄漏上界改为`MEMBLOCK_ROB_SIZE`。
3. 不删除上界保护，不增加全表fallback，不改变request fire、response raw-hit、flush/reset、release语义。
4. 修正文档中“每个同VPN bucket最多32个WAITING UID”的旧结论，明确物理token容量和逻辑UID扇出是两层概念。

## 3. 定向验证

扩展`soft_test_rm_provenance_followup_sequence.sv`：

1. 建立33个相同`vpn/s2xlate`的WAITING record并逐个登记，证明第33项不再fatal。
2. 将其中2项转为CANCELED并通过公共remove helper移除，检查bucket剩31项且被取消UID不再存在。
3. 构造匹配response，调用既有multicast完成其余31项，检查全部COMPLETED且bucket删除。
4. 清理定向场景产生的record和status字段，不能污染后续检查。

## 4. 验证与回归

1. `git diff --check`和静态review。
2. 新mode编译并运行`memblock_rm_provenance_followup_directed_vseq`专项。
3. 原失败`seed=292095580`按原cfg完整执行10000请求，要求`TEST_PASS`、compare=10000、
   0 error/fatal、`Quit count=0/10`和正常`$finish`。
4. 原seed通过后建立新resume目录，继承此前完整PASS，继续100个唯一seed回归；首个新失败停止派发，
   不终止已运行任务。

## 5. 修改范围

- `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_rm_provenance_followup_sequence.sv`
- 本plan、plan review、implementation review、W43 buglist和相关L2TLB文档勘误

明确不修改DUT RTL/Scala、cfg、L2TLB物理pending token容量和response匹配语义。
