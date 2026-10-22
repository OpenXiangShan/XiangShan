# MemBlock L2TLB WAITING UID 逻辑扇出容量修复 Plan Review

评审日期：2026-10-19

关联plan：`AI_DOC/plan/test_framework/plan/undo/memblock_l2tlb_waiting_uid_logical_fanout_fix_plan_20261019.md`

评审结论：通过，可以coding。

## 1. 根因复核

`uid_waiting_by_vpn_s2xlate`保存逻辑UID候选，不保存DUT物理filter entry。RTL中相同
`vpn/s2xlate`请求可由`oldMatchVec/newMatchVec/canMerge()`合并，所以物理32项不能推导出单个shape
最多32个逻辑UID。失败点恰为第33项插入，且此前没有DUT首错，测试框架责任成立。

## 2. 边界复核

1. 使用`MEMBLOCK_ROB_SIZE=160`作为泄漏保护是保守上界：一个WAITING UID对应一个尚在ROB活跃窗口中的
   动态请求；完成、redirect/reset取消路径仍必须及时移除索引。
2. 方案没有取消容量检查；若索引生命周期泄漏导致同shape超过ROB容量仍会fatal。
3. 重复UID、record状态、record uid、lookup shape检查均保持，不能用扩大上界掩盖索引损坏。
4. response仍按全表raw-hit完成匹配UID；bucket只用于request-fire候选加速，修改不改变payload归属。
5. 33项专项同时覆盖插入、部分取消和response multicast清空，可发现只放宽插入但删除/完成不闭环的问题。

## 3. 遗漏场景检查

- 不同VPN各自bucket互不影响。
- 相同VPN但不同`s2xlate`仍为不同shape。
- stale索引先prune再计数，不会占用逻辑上界。
- 第160项允许登记，第161项继续fatal；不需要构造161项expected-fatal专项来吞掉其它fatal。
- release、sfence、redirect和reset继续调用现有cancel/remove路径，本修复不改变关闭顺序。

## 4. Review结论

方案仅纠正物理资源容量与逻辑扇出的错误等价关系，改动范围小且保留生命周期自检。没有发现会误完成
不匹配UID、隐藏DUT错误或造成无界索引增长的blocker，可以进入coding和专项验证。
