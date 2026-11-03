# B05 原 seed 的 ROB/SQ 生命周期动态复核（2026-10-31）

## 一、有效结论

本次真实 DUT 原 seed 1545815340 运行结果为 1/1 PASS，controller rc=0、remote_unknown_jobs=0。**PASS 只表示本次已有检查通过，不表示联合 LSQ admission 门槛合法，更不能追认已删除的历史 FAIL 刺激合法。**

1. 当前日志独立确认 UID492、UID652 均为 Store，完整身份分别为 ROB1/12、SQ1/20 与 ROB0/12、SQ0/15。UID 标记的 MAIN_TABLE、WB、模拟 commit、release 日志与入队波形交叉对应，不以 ROB value 单独猜 UID。
2. 当前重验重现历史记载的关键时间：UID492 STD WB 18168.200ns、模拟 ROB commit 19195.700ns；UID652 STD fire 19315.700ns、STD WB 19318.200ns。两目标身份及关键时序一致，不是换成另一种 UID 形状。此为新增当前证据，不是恢复了原 FAIL 日志。
3. 19318.200ns 的 DUT SQ 确有两个 allocated entry 同时保存上述不同 flag 的 ROB 标签。旧 UID492 entry20 已 committed、尚未 completed，新 UID652 entry15 未 committed。并非仅剩旧无效寄存器值，也非仅软件 active_sq_mapped 滞后。
4. 更早已出现联合门槛不满足：按 pre-edge 完整指针 distanceBetween，第一处有 request 而不满足 freeLQ>=6 && freeSQ>=4 是 **365.700ns**。UID652 于 **19295.700ns** 入队前，SQ distance=54、free=2，联合门槛同样不满足。不得把本次双标签轨迹推广成真实后端合法调度的证明。
5. UID492 SQ 驻留的直接原因是已提交 Store 的有序排队及 SBuffer 背压：目标时刻 head=1/17，SQ20 前面仍有 SQ17、18、19；SBuffer 两路 valid=1/ready=0。19500.700ns 路0接收 SQ20 后 entry20 才 completed，19505.700ns 出队。
6. UID492 软件 release 为19513.200ns，较真实 DUT 出队晚7.5ns；这段短延迟确实存在，但不能解释19318.200ns的双标签重叠，因为当时 DUT entry20 仍 allocated，尚未被 SBuffer 接收。没有旧 raw 在19318.200ns之前已出队却长期未消费的证据。

本任务没有修改生产源码、cfg、脚本、RTL、signal.rc、既有脏文件或冻结 coverage，没有新建 worktree，没有 git add/commit，没有启动整个回归。仅执行首 seed；已有目标证据后未执行邻接1396766687，也未为强行复现删除 B05 filter。

## 二、入口与编译追溯

工作区 R：/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028。HEAD=e275e8819b11b2329f01f9788c47655e8711d107。后文 R/ 表示此绝对根目录。

| 项目 | 实际值 |
|---|---|
| testcase / ts / cfg | tc_dispatch_real_smoke / virtual_base_sequence / default |
| seed / mode | 1545815340 / mother_b05_rob_sq_lifecycle_20261031 |
| runner | R/mem_ut/ver/ut/memblock/scripts/run_eda_regression.sh，经真实 eda01_entry.sh 在 eda01 执行 |
| controller | session77500，已退出rc0；本地PID3690648；远端worker/PGID/SID7979 |
| 波形/verbosity | wave=1，UVM_DEBUG，既有全dump；本轮是当前源码/current cfg的debug取证，不是原无波形批次严格原参数验收 |
| monitor / record | 均开启，error-limit=1；未关闭原expected检查 |
| plusargs | MAIN_TRANS_NUM=1000、CHECK_TRIGGER_EN=0、DISPATCH_ISSUE_SEQ_EN=1 |
| timeout | 1000000000ns，按用户历史入口要求设置；原config缺失，无法重新核实原件 |
| compile | 本轮no-compile；复制既有binary与daidir至新mode，未fresh compile |
| 正常结束 | TEST CASE PASSED；finish=40143.200ns；UVM_ERROR=0、UVM_FATAL=0、UVM_WARNING=2 |

每次EDA操作前source /nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.envrc。启动前核验eda01资源、controller与mode锁；未裸调用vcs，未覆盖旧结果。真实命令从 R/mem_ut/ver/ut/memblock/sim 执行：

```bash
source /nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.envrc
unset EDA_REGRESSION_REMOTE_ENTRY
../scripts/run_eda_regression.sh \
  --tc tc_dispatch_real_smoke --ts virtual_base_sequence --cfg default \
  --mode mother_b05_rob_sq_lifecycle_20261031 --seed 1545815340 --jobs 1 \
  --no-compile --wave --no-coverage --verbosity UVM_DEBUG \
  --timeout-ns 1000000000 --monitor --record --error-limit 1 \
  --plusarg +MEMBLOCK_MAIN_TRANS_NUM=1000 \
  --plusarg +MEMBLOCK_CHECK_TRIGGER_EN=0 \
  --plusarg +MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1 \
  --output-dir mother_b05_rob_sq_lifecycle_20261031_results
```

### binary和2540项输入

- source fingerprint：R/mem_ut/ver/ut/memblock/sim/mother_strict_nc_reproduce_20261031/exec/.eda_compile_fingerprint。编译提交d96427b89e8e68cce5c2f538cd459640874d6563，created_at=2026-10-31 10:46:15 CST。原source mode的strict-NC FAIL与本次B05不是同一个用例。
- source与本轮binary SHA256均为3dff1d3c3886312a59651ca966b1f09c193c8a581d9db3eedb983670f8f89043。新mode远端identity为65:2001656575:1252664:1793414396:1793434672，运行前后相同；summary simv_changed=0。
- 新mode fingerprint是复制binary后的重定位provenance，保留原编译提交，不伪造HEAD fresh compile。原fingerprint全文存于分析目录source_compile_fingerprint.txt。
- d96427b89e到当前HEAD仅文档变更；另重算production manifest的2540项当前实际输入，全部匹配，mismatches=[]。见分析目录current_2540_verification.json。
- baseline manifest为R/mem_ut/ver/ut/memblock/sim/mother_final_cancel_nonrtl_pass_coverage_20261030/actual_compile_inputs.json，SHA256=64a65c6d29455e0abca887fed0522fd75179cf7e3461822e48ce240ffb541d72。source既有验收为mother_strict_nc_reproduce_20261031_results/source_input_verification.json，未改冻结目录。

### 历史和当前cfg差异

旧closure的default_issue_enable_random10_20261023/config.txt及原FAIL final log现不可读，不能逐hash恢复旧effective参数。当前冻结default.cfg与当前源cfg SHA256均为2a49867261114c2c1c44c543736b27d3e18a63076529658ba75998494aceb789。以Buglist指明dadbd03e89对比当前tracked default.cfg，Git diff只有ISSUE_SEQ_EN从0改1；历史与本次均显式覆盖为1，此处effective值相同。框架源码已有后续变化，原config又缺失，不能据此保证整个历史刺激完全相同。

B05 filter仍保留于R/mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv:549的probe_std_candidate()，排除std_writeback || std_pass || std_fault；未删除、未改成伪造fatal。

## 三、证据定位及解析有效性

结果根：R/mem_ut/ver/ut/memblock/sim/mother_b05_rob_sq_lifecycle_20261031_results。稳定日志 L：该根下status/seed_1545815340.final.log。分析根 A：R/mem_ut/ver/ut/memblock/sim/mother_b05_rob_sq_lifecycle_20261031/analysis。

FSDB真实绝对路径：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028/mem_ut/ver/ut/memblock/sim/mother_b05_rob_sq_lifecycle_20261031/wave/tc=tc_dispatch_real_smoke_ts=virtual_base_sequence_cfg=default_seed=1545815340_rtl_eda_reg_20261031_162436_3690648_13669.fsdb
```

主FSDB约30MB，含辅助文件约36MB，L约5.9MB。时间单位1ps，范围0–40143200ps。使用现有只读reader/Verdi fsdb2vcd，未改connect或signal.rc。

| A下产物 | 作用 |
|---|---|
| lifecycle_dut.vcd | 有效DUT直接层级VCD，约61MB；同一FSDB转换，不是新仿真 |
| ctrl_sample.vcd | 19495–19520ns的ctrl VIF窗口，约11KB；同一FSDB |
| analyze_lifecycle.py | 完整scope/identifier解析；DUT clock；逐时间戳更新前状态作为pre-NBA |
| lifecycle_evidence.json | 最终有效逐边沿pre/post、request、cancel、SQ状态及指针门槛 |
| lifecycle_summary.json / cycle_occupancy.csv | 目标enqueue、issue、transition与逐拍数据；最终JSON门槛已改按指针distance |
| target_uid_logs.txt / commit_frontier_logs.json | 带源日志行号的独立UID证据和模拟提交记录 |
| current_2540_verification.json / assertion_audit.json | 输入和断言审计 |

首轮抽top_tb mirror时enqueue保持初值，得到enq_count=0、target_enq=[]、pendingPtr=0，该结果**全部作废**，不构成合法性证据。A/lifecycle.vcd仅保留为失败观察尝试。最终JSON/summary均由DUT直接层级重新生成；所有门控结论使用DUT clock边沿前状态，不用post-state指针判断。早期reader窄总线hex格式也不用于数字结论，以VCD位宽/二进制解码为准。

有效结果为1042次enqueue、最终next_uid=1000。2610.700ns存在ROB0/88、level1 redirect，UID88–129重入队42次；1042不等于1042个不同UID。独立L:2538在2598.200ns记录uid=88 dynamic_epoch=0、max_enqueued=129。解析器顺序跟踪admission并在该flush-itself回退到UID88，再以目标完整ROB/SQ和UID日志交叉检查。

## 四、UID492/652事件表

W表示上述FSDB。信号前缀D=top_tb.U_MEMBLOCK，S=D.inner_lsq.storeQueue，Q=D.inner_lsq.loadQueue.virtualLoadQueue。下表时间均为ns。框架消费WB可比DUTposedge晚2.5ns，表内分别注明。

| 时间 | 对象与事件 | 证据 |
|---:|---|---|
| 15255.700 | UID492入队，port3、ROB1/12、SQ1/20、needAlloc=2 | W D.enqLsq_req_3；S.allocated_20从0到1并写入完整ROB；顺序解析与后续UID日志交叉 |
| 18165.700 | UID492 STA0/STD0 fire | W valid&&ready，STA完整ROB1/12；STD只提供ROB value12及SQ1/20 |
| 18168.200 | UID492 STD真实WB框架消费 | L:12766 target3/source3/ROB1/12/SQ1/20 |
| 18178.200 | UID492 STA feedback success，等待真实WB | L:12769；feedback不等于STA WB |
| 18188.200 | UID492 STA真实WB框架消费 | L:12773；W STA0 WB在18185.700ns被采样 |
| 19195.700 | UID492框架模拟ROB commit，sq_mapped=1 | L:13522；不是真实后端ROB观测 |
| 19210.700 | DUT entry20 committed从0到1 | W S.committed_20 |
| 19295.700 | UID652入队，port0、ROB0/12、SQ0/15、needAlloc=2 | W req0，S.entryCanEnq_15=1、allocated_15写1；pre SQ free2 |
| 19315.700 | UID652 STD0 fire | W valid&&ready、SQ0/15，ROB value12 |
| 19318.200 | UID652 STD真实WB框架消费 | L:14376；UID492 STD早于此时完成，但DUT SQ20仍allocated |
| 19380.700 | SQ head1/17到1/18 | W S.deqPtrExt_0，sqDeq1 |
| 19430.700 | SQ head1/18到1/19 | 同上 |
| 19455.700 | SQ head1/19到1/20 | 同上；UID492尚未completed |
| 19500.700 | SBuffer0握手接收SQ20，entry20 completed写1 | W pre SBuffer0 valid1/ready1，dataBuffer0 sqPtr20 |
| 19505.700 | UID492真DUT出队，head1/20到1/21，allocated_20清0，sqDeq升1 | W pre completed1、readyDeqVec_0=1；post SQ占用56到55 |
| 19510.700 | ctrl monitor下一posedge可采到sqDeq1 | W ctrl_sample.vcd；DUT同拍输出回0 |
| 19513.200 | UID492软件preflight/release/retire | L:14519–14522；较DUT出队晚7.5ns |
| 19585.700 | UID652 STA1 fire | W valid&&ready、完整ROB0/12、SQ0/15 |
| 19608.200 | UID652 STA真实WB框架消费 | L:14589；W采样点19605.700ns |
| 23745.700 | UID652框架模拟ROB commit | L:16780 |
| 23760.700 | DUT entry15 committed写1 | W S.committed_15 |
| 24555.700 | DUT entry15 completed写1 | W S.completed_15 |
| 24560.700 | UID652真DUT出队，head0/15到0/16，allocated_15清0 | W S.deqPtrExt_0、allocated_15、sqDeq |
| 24568.200 | UID652软件release/retire | L:17399–17402，同样晚7.5ns |

主表独立身份：UID492见L:13638–13643，op_class3/kind STORE、fu_op0、VA0x8393e8c0、BYTE；UID652见L:17004–17009，op_class3/kind STORE、fu_op2、VA0x82ebc180、WORD。两个记录check_store=0是原入口已有配置，并非本任务关闭检查。

## 五、pre-edge联合门控核验

容量来自R/mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh:14–37：LQ72、SQ56、ROB160。门槛占用按完整指针计算，allocated总和只作交叉检查，不作为门控输入。

```text
distance(enq,deq,N) = enq.value-deq.value       当flag相同
                   = N+enq.value-deq.value     当flag不同
free = N-distance
LQ_canAccept_derived = freeLQ>=6
SQ_canAccept_derived = freeSQ>=4
joint_canAccept_derived = 两者逻辑与
```

**canAccept观测边界**：本次MemBlock/LsqWrapper的LSQ接口没有canAccept输出，有效VCD中没有该enqueue-controller wire。因此下列布尔值是由实测指针推导，不能标成实测canAccept。独立LsqEnqCtrl.sv文件存在不等于它实例化在本次MemBlock内。没有为补该量追加仿真或全量转换。

### 首个绝对门槛违例365.700ns

| pre-edge量 | 事实 |
|---|---|
| LQ enq/deq | 0/68、0/1，distance67、free5，LQ_canAccept_derived=0 |
| SQ enq/deq | 0/48、0/0，distance48、free8，SQ_canAccept_derived=1 |
| joint | 0 |
| request | port0..3 valid1，needAlloc依次2/1/1/1；UID116–119、ROB0/116–0/119；3 load+1 store |
| reset/取消 | reset0，D.redirect0、S.brqRedirect0、S.exceptionRedirect0、Q.redirect0，LQ/SQ cancel计数0/0 |
| 同拍deq | LQ/SQ deq均0，无同拍回收抵消 |
| post实际接收 | LQ distance70、SQ distance49，与allocated计数交叉一致 |

所以365.700ns是边沿前已freeLQ5而仍发请求，不是误把post占用70当pre。扫描全部有效posedge，按该绝对门槛共132个有请求的不满足边沿；最早是此点。

更强的独立控制器公式比较：R/build/rtl/LsqEnqCtrl.sv:662–663、1113的寄存器更新条件包含lqCounter>=lqAllocNumber+6、sqCounter>=sqAllocNumber+4和非t2_update。若直接比较本批需求加保留量，360.700ns已freeLQ8、本批load3，要求9而不足。但此控制器未直接观测，寄存器流水未重建，**360.700ns只作公式比较，不冒充canAccept_REG实测翻转点**；确定的绝对门槛首违例是365.700ns。

### UID652准入19295.700ns

| pre-edge量 | 事实 |
|---|---|
| LQ enq/deq | 1/53、0/59，distance=72+53-59=66，free6，LQ_canAccept_derived=1 |
| SQ enq/deq | 0/15、1/17，distance=56+15-17=54，free2，SQ_canAccept_derived=0 |
| joint | 0 |
| request | port0..3 valid1，needAlloc2/1/1/1，UID652–655；UID652 port0、ROB0/12、SQ0/15 |
| reset/redirect | reset0，D.redirect0、S.brqRedirect0、S.exceptionRedirect0、Q.redirect0 |
| entry取消与接收 | S.needCancel_15=0、needCancel_20=0，entryCanEnq_15=1；post entry15实际allocated并写入ROB0/12 |
| cancel计数 | LQ29、SQ13，保持历史redirect统计水平，**不是全零，也不是本拍再次取消29/13项** |
| 同拍deq | LQ/SQ deq均0 |
| post | LQ distance69、SQ distance55；旧entry20仍allocated、ROB1/12 |

唯一观测到的redirect是2610.700ns；目标时刻redirect为0、needCancel为0且entry15实际建立，held cancel计数不能作为该请求豁免。按enqCancelValid命名检索的逐port节点未出现，JSON保留null；不把未观测量宣称为0。

联合门槛要求两侧共同成立，不能因UID652只需求1项Store且freeSQ2就宣布合法。当前collect_lsq_candidates()只按本批实际load/store需求对软件free count预检，未实现上述联合保留门槛；代码R/mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv:365–465。V2无canAccept反馈，故它仍发batch，DUT queue按valid接受。

### 目标WB sample的真实占用和双标签

19318.200ns位于19315.700与19320.700两个posedge之间；相关指针和allocated在此窗口未变：LQ enq1/58、deq0/59，distance71/free1；SQ enq0/17、deq1/17，distance56/free0。S.cmtPtr=1/44。entry20为allocated1/committed1/completed0/addrvalid1/datavalid1、ROB1/12；entry15为allocated1/committed0/completed0、ROB0/12。旧STD已WB不等于SQ completed；这是DUT真实驻留的两个标签，不仅是软件旧记录。

## 六、UID492为何到19500.700ns才completed

19315.700ns的S.deqPtr为1/17，cmtPtr1/44，rdataPtr1/19；dataBuffer两路输出SQ17、18，valid均1，SBuffer ready均0。UID492 SQ20虽已提交且地址/数据ready，仍排在17/18/19后。head分别在19380.700、19430.700、19455.700ns推进到18、19、20。

19455.700ns dataBuffer输出已是SQ20/21，两路SBuffer ready仍0。19500.700ns pre-edge路0 valid1/ready1、dataBuffer sqPtr20，路1ready0；此拍后completed_20从0到1。19505.700ns pre-edge completed_20=1、readyDeqVec_0=1，此拍后head1/20到1/21、allocated_20清0。

源码支持：R/build/rtl/StoreQueue.sv:59393将io_sbuffer_0_valid接dataBuffer valid；:45191–45197以ready和valid、sqNeedDeq等构造完成条件；:47849更新completed_20；:4130–4133由allocated/completed形成readyDeqVec/sqDeqCnt；:40553寄存sqDeq输出。**STD WB/pass与SQ completed语义不同，前者不能作为已被SBuffer接收的证据。**

本次只证明可见的顺序排队与SQ到SBuffer背压，不猜测SBuffer下游ready0的更深根因。此前admission门槛已不满足，局部自然出队不能洗清全轨迹合法性。

## 七、软件raw消费、ROB和epoch边界

UID492 DUT sqDeq于19505.700ns升1，19510.700ns回0；ctrl_sample.vcd证明ctrl VIF也同步为1。monitor使用posedge clocking输入，下一采样边界19510.700ns取得非零输出；R/mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv:807–808写raw.cycle并push。19513.200ns日志明确preflight/release/retire，距物理出队7.5ns、距下一monitor采样边界2.5ns。

raw SV对象/FIFO深度没有dump，不能声称独立动态看到了push对象。源代码允许apply_deferred_ctrl_updates_batch()在联合预检失败时留FIFO队首，但本例没有长期重试或丢失证据；观测到的7.5ns与采样/service消费一致。不能把源码允许defer当成本次已发生长时间defer。19318.200ns旧entry20仍真实allocated且尚未完成SBuffer接收，已排除“此前真实出队、只剩软件map”的解释。19505.700到19513.200ns确有短暂软件滞后；entry20到19525.700ns被ROB0/22重用，此后不能再叫UID492。

**DUT只有MemBlock，没有真实后端ROB。** normal rob commit是框架模拟提交。R/mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv:107–121、797–810发布模拟frontier。按最后成功batch日志重建：UID492入队前最新提交UID401于15230.700ns，frontier402，candidate距离90；UID652入队前最新提交UID558于19235.700ns，frontier559，距离93，均小于ROB160。19318.200ns同样frontier559；pendingPtr实测1/79是框架驱动水位，不是真实ROB内部指针。

完整ROB/SQ有动态证据；目标dynamic_epoch、flush_epoch没有独立SV对象dump。按唯一redirect及初始化代码可推目标dynamic_epoch0、flush_epoch1，但仅为源码加观测的推导。数值issue_epoch、reservation launch_epoch没有直接证据，明确不猜。A/signals_software.txt检索epoch/frontier/raw_ctrl无匹配，保存此边界。

## 八、门控不满足为何仍PASS

只读审计本次2540输入绑定的生成MemBlock.sv、LsqWrapper.sv、StoreQueue.sv、VirtualLoadQueue.sv、LsqEnqCtrl.sv，assert关键字及$通配系统断言任务扫描均为0；实际正则检查的任务为$fatal、$error、$stop、$finish，见assertion_audit.json。must-accept文本也为0。**这份生成RTL未包含相应可执行must-accept assertion，日志无assert失败不能证明条件成立。**

source编译launcher/compile.log中的defines为TCNT_USE_UVM12、RTL_SIM、DEF_SETUP_TIME=1step、DEF_HOLD_TIME=1step、MEMBLOCK_UT；未发现SYNTHESIS或明确关闭assertion的define。内部编译日志还含-assert enable_hier。因此证据是相关assertion未进入生成RTL，而不是猜测某关闭宏压掉它。为何生成前未保留，超出本次生成RTL审计，不推测生成参数。

MemBlock直连queue输入无可观测LSQ canAccept拦截；现有软件检查只约束实际需求，未实现联合保留门槛，所以batch仍可建立并完成后续数据/异常检查。独立LsqEnqCtrl文件中的条件不能替代未实例化的后端。没有修改report catcher或expected配置；L:26802 TEST CASE PASSED、26821–26822 error/fatal为0，caught/demoted error/fatal也0。warning2条为210.700ns vector默认sequence未配置及25220.700ns L2TLB no-progress诊断，不是门控assertion。

**判定：当前seed重现同一关键双标签形状，DUT证据确认驻留与背压原因，同时证明更早pre-edge admission门槛已不满足。当前PASS不能证明历史刺激合法；B05 filter必要性也不能单独证明整个激励框架正确。** 原FAIL、原完整config/effective参数缺口保持未闭合。

## 九、收口和保护检查

controller77500已rc0退出；results为PASS/EXIT_CONFIRMED/rc0；summary remote_unknown_jobs0、simv_changed0。mode owners.tsv仅表头；未见本mode锁持有者；eda01 ps -p7979无进程。分析会话均已结束，不保留不必要session。

初始五个保护对象SHA256复核全OK：signal.rc及四份原知识库脏文件。仅新ignored mode/results和本指定report为本任务写入；主agent并发更新的状态文档/正式plan未回滚、未覆盖、未暂存。未触碰冻结coverage。报告交主审独立核验，不实施正式修复或追加仿真。
