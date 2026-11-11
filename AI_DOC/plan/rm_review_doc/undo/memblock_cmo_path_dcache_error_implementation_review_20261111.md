# V2 CMO 共用被动观察与 DCache 错误消费实现 Review（2026-11-11）

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| owner / UID | 一条动态指令身份；UID、ROB、LQ/SQ 与动态代共同限定 | `owner_t`、readonly API | 同一UID重发后不能取旧错误 |
| epoch / generation | 动态指令代、DUT reset代、TL source占用代分别隔离 | `owner.epoch`、batch.reset_generation、source.generation | source1复用不改变已冻结slot来源 |
| sample | CSR monitor唯一发布的真实DUT时钟序号 | `sample_seq`、watermark | WB样本40只能查询40之前的事实 |
| batch / probe | 同一sample的被动观察集合 / 一处物理观察口 | xaction.events、`cmo_path_probe_t` | 本拍A/D与S2一并归档，不依赖协程顺序 |
| token / beat | 框架冻结响应的唯一身份 / 32B响应拍 | `transport_t.token`、L2响应历史 | 首beat可证明低32B，不能证明未送达高32B |
| offer / fire | 真实valid展示 / valid且ready实际接受 | `DCACHE_TL_D_OFFER` / `DCACHE_TL_D` | ready=0仍可能被Load直转消费，但不能当MSHR已送达 |
| origin / version | 独立响应错误来源 / 槽位错误来源版本 | `origin_t`、origin_by_version | CLEAN refill建立新版本，旧错行不污染新命中 |
| slot / way / idx | 真实硬件槽 / 路 / 阵列索引 | `committed_version[{idx,way}]` | 不按PA推VIPT索引或模拟替换 |
| bypass / committed | S0/S1在途写旁路 / 真实延迟写已提交来源 | read_bypass_version、committed_version | 同拍S0写优先上一拍S1写 |
| AF / HE / raw | Access Fault、hardwareError、denied/corrupt两位原始贡献 | expected_load_af/he、raw[1:0] | 不同输入AF、HE可以同时成立 |
| split / prefix | MisalignBuffer真实分段 / 独立翻译的首页字节前缀 | first_split_fact、query_first_split_error | 首页错误停止高页翻译，不要求高段发生 |
| watermark / provenance | 已完整观察到的边界 / 错误来源证明 | watermark、proven | 水位未到返回等待；水位已到缺证据返回明确失败 |
| Prefetch / suppression | 软件或硬件预取 / 响应错误不追加同步异常 | query_prefetch、validate_prefetch_grant | 软件WB与后台GrantData分别核验，不构造返回数据 |
| FIFO / adapter | 单消费者事实队列 / RM集成转换层 | env.cmo_path_agent_mon2rm_fifo / observer_service_path_facts | 不恢复旧monitor无对端expected流量 |

本 review 负责完整 `cmo_path_agent`、只读 connect、公用 env 接线、DCache 来源观察以及 root RM集成的独立复查。CMO请求关联与 Uncache 错误细节分别由同日专项 review 覆盖；本文不重复实现其 checker。

## 2. 修改前后与逐文件覆盖

修改前RM按永久PA错误摘要推异常，无法区别真实消费token、首beat、替换后CLEAN refill与并行错误来源。内部CMO路径没有必要被动事实链，顶层monitor整包发布处于注释状态。

修改后先采真实物理接口，再沿独立响应token、真实errorArray来源版本与实际流水资格生成owner消费事实。CMO请求、DCache错误和Uncache错误共享一份批次与唯一FIFO消费；期望字节仍由既有RM提交底座维护。

| 文件/区域 | 本 review 覆盖逻辑 | 正确性检查 |
|---|---|---|
| `subagent/cmo_path_agent/*.f/*pkg`、`src/*` | 完整固定被动agent、值型事件、唯一monitor、水位与reset代 | 无驱动信号，无seq_pkg依赖，编译在seq_pkg前 |
| `tb/cmo_path_agent_connect.sv` | SQ/SB/CMO/Uncache、errorArray、Load/Atomic、外TL真实事实 | 当前生成RTL逐引用检查，握手/Valid区别显式 |
| `seq/base_seq/memblock_main_dispatch_cbo_directed_sequence.sv` | AUTO定向表导入后补齐父类control bootstrap | 只复用既有初始化，不放宽service fatal |
| `cfg/tb.f`、`tb/top_tb.sv` | 新package与只读探针编译接入 | 顶层没有内部force或RTL修改 |
| `env/memblock_env_pkg.sv`、`env/src/memblock_env.sv` | package导入、agent/cfg/FIFO例化与唯一RM交接 | config_db指向同一FIFO，禁止竞争消费者 |
| `env/src/memblock_rm/rm_dcache_error_observer.sv` | source身份、来源版本/旁路、实际消费、回收、软测 | expected来自独立L2 token，actual仅核对 |
| `env/src/memblock_rm/memblock_rm.sv` | Load/Atomic查询、CSR控制sample、first split调用、owner回收 | 本文独立复查，root负责修改 |
| `env/src/memblock_rm/rm_ls_core.sv` | CMO提交、zero64B及Uncache部分写、失败SC shadow | 独立复查，主体由CMO专项review展开 |

## 3. 字段与生命周期

| 对象 | 为什么需要/生产与读取 | 清理/上界 |
|---|---|---|
| xaction.sample/reset/events | monitor先复制同拍clocking快照，再等CSR锚点；RM唯一消费 | reset无DUT事件，只发布取消边界；每拍固定47口 |
| transport_by_source | 外TL真实A接受绑定source、accept sample与独立token代次 | 同source新A更新；已冻结来源不回查latest |
| origin_by_version / version_refcount | errorArray真实写产生版本，独立期望与actual写raw比较 | 仅真实slot、S0旁路、延迟写引用；引用归零删除 |
| committed_version | 真实延迟写提交后的idx/way来源 | 只有errorArray写提交改变，失效/tag写不擅自清raw |
| read_pending / read_bypass | S0每way保存同拍S0写优先、其次S1写的来源 | 每拍固定物理读口覆盖，旧引用先释放 |
| main_first_s1/main_s2/main_s3 | MainPipe首S1冻结、stall保持、真实S1/S2接受推进 | reset清除；固定流水状态 |
| forward_pending/hit_delay/load_s2_pending | S1直转/MSHR身份与LoadPipe延迟hit独立向S3推进 | 每拍固定Load口数，S2只有真实fire更新 |
| atomic_response_pending | 最终响应先冻结独立raw，到真实state6冻结内部消费边界和顶层前2拍CSR输入边界；actual内部enable只核对独立历史 | state6发布fact后清；miss/replay不形成fact |
| load/atomic/first_split事实与split聚合 | 同UID/epoch独立保存真实完成或段返回，查询验证ROB/LQ与sample | root提交后release_completed_owner；reset清当前查询索引 |

所有高频扫描均是固定物理口或最多16way容器；不扫描main_trans_num或完整历史。阵列实际idx/way尺寸来自当前生成RTL，容器显式检查容量；Load口数复用编译宏，connect对当前V2三口限定有明确fatal。

## 4. 被动采样与连接

### 4.1 monitor::run_phase

抽象功能描述：monitor 每个DUT时钟发布一份不可变观察批次，事件为空也推进完成水位。它不维护框架owner、不生成响应、不驱动DUT。

源码位置：`mem_ut/ver/ut/memblock/subagent/cmo_path_agent/src/cmo_path_agent_monitor.sv`，函数：`run_phase()`。
函数功能简析：输入clocking snapshot与CSR全局采样协调器；输出带reset代的xaction，供RM唯一消费者建立关联。

```systemverilog
    task run_phase(uvm_phase phase);
        cmo_path_agent_xaction tr;
        longint unsigned edge_time;
        super.run_phase(phase);
        forever begin
            @vif.mon_cb;
            edge_time=$time;
            tr=cmo_path_agent_xaction::type_id::create("path_sample");
            tr.reset_active=(vif.mon_cb.rst_n!==1'b1 ||
                memblock_sync_pkg::reset_backend_done!==1'b1 ||
                memblock_sync_pkg::l2tlb_reset_active());
            if(tr.reset_active&&!was_in_reset) reset_generation++;
            was_in_reset=tr.reset_active;
            tr.reset_generation=reset_generation;
            if(!tr.reset_active) begin
                for(int i=0;i<CMO_PATH_PROBE_NUM;i++)
                    if(vif.mon_cb.probes[i].valid===1'b1)
                        tr.events.push_back(vif.mon_cb.probes[i]);
            end
            // reset期CSR协调器暂停sample，无需请求不存在的时钟锚点。
            if(tr.reset_active) begin
                tr.reset_active=1;
                tr.events.delete();
                tr.sample_seq=memblock_sync_pkg::dut_sample_seq;
            end else begin
                memblock_sync_pkg::wait_for_l2tlb_sample_anchor(edge_time,tr.sample_seq);
            end
            mon_item_port.write(tr);
        end
    endtask
```
中文伪代码：
1. 本逻辑负责真实采样与完整批次水位，不负责错误预测。
2. 每拍先保存时刻与clocking输入；reset、backend reset未完成或协调器reset有效时，不采DUT事件，并在reset入口递增取消代。
3. 正常拍只扫描固定物理口，将valid事件复制进本拍新建对象；然后等待同拍CSR全局锚点，避免将下一拍或未锚定时刻当成本拍。
4. reset期间使用当前水位并清空事件；正常锚定后经唯一analysis port发布，发布后的sample表示该producer已完整采样。
5. `wait_for_l2tlb_sample_anchor()`只等待同一拍有界调度区域，不推进DUT时钟；analysis port写入同一FIFO，不创建第二消费者。

### 4.2 connect 与基础结构

抽象功能描述：connect 将当前生成RTL已有真实信号单向映射到观察口，不修改DUT，不建cache算法。完整agent基础类仅提供被动monitor与analysis端口。

源码位置：`tb/cmo_path_agent_connect.sv`；`subagent/cmo_path_agent/src/cmo_path_agent.sv`。
函数功能简析：固定被动agent在build创建cfg/monitor，在connect连接analysis port；driver/sequencer/default sequence只保留完整结构且不例化驱动。


抽象功能描述：`new()` 负责被动agent的对象构造。

源码位置：`subagent/cmo_path_agent/src/cmo_path_agent.sv`（相对memblock目录），函数：`new()`。
函数功能简析：输入UVM生命周期上下文，只创建或连接被动观察对象。

```systemverilog
    function new(string name,uvm_component parent);
        super.new(name,parent); mon_item_port=new("mon_item_port",this);
    endfunction
```
中文伪代码：
1. 本逻辑提供完整被动agent结构。
2. 构造时先调用UVM父类，然后创建analysis端口，供后续monitor发布；不创建driver。
3. UVM父类只处理公共组件生命周期，monitor独立负责真实采样。

抽象功能描述：`build_phase()` 负责被动agent的组件创建。

源码位置：`subagent/cmo_path_agent/src/cmo_path_agent.sv`（相对memblock目录），函数：`build_phase()`。
函数功能简析：输入UVM生命周期上下文，只创建或连接被动观察对象。

```systemverilog
    function void build_phase(uvm_phase phase);
        super.build_phase(phase);
        if(!uvm_config_db#(cmo_path_agent_cfg)::get(this,"","cfg",cfg))
            cfg=cmo_path_agent_cfg::type_id::create("cfg");
        is_active=UVM_PASSIVE;
        mon=cmo_path_agent_monitor::type_id::create("mon",this);
    endfunction
```
中文伪代码：
1. 本逻辑提供完整被动agent结构。
2. build先执行父类构建，再从config_db获取同一被动cfg；未配置时创建缺省对象；固定is_active为被动，随后只创建monitor。cfg本身无激励参数，不改变DUT。
3. UVM父类只处理公共组件生命周期，monitor独立负责真实采样。

抽象功能描述：`connect_phase()` 负责被动agent的事实端口接线。

源码位置：`subagent/cmo_path_agent/src/cmo_path_agent.sv`（相对memblock目录），函数：`connect_phase()`。
函数功能简析：输入UVM生命周期上下文，只创建或连接被动观察对象。

```systemverilog
    function void connect_phase(uvm_phase phase);
        super.connect_phase(phase); mon.mon_item_port.connect(mon_item_port);
    endfunction
```
中文伪代码：
1. 本逻辑提供完整被动agent结构。
2. connect先执行父类连接，然后把monitor唯一analysis输出连接到agent输出；环境连接FIFO是同一条事实流，不复制payload或额外消费。
3. UVM父类只处理公共组件生命周期，monitor独立负责真实采样。

连接分组：SQ enqueue保留真实ROB/SQ；SQ→SB保留DataBuffer sqPtr.value与wline/vecValid；accessIdx严格关联前拍接受；SB DCache请求/成功/replay保留真实id；CMO用锁存uncacheUop ROB；Uncache用实际id/slot选择。errorArray采真实Valid读写，LoadPipe真实way和Main首S1控制不从PA推算。外TL A/D使用顶层真实fire，direct另记真实valid offer；原子state5接受响应，state6采实际CSR控制边界。各probe中文注释给出qual/state/raw逐位语义，完整赋值位于源码，不在文档复制47组机械映射。

## 5. DCache 来源与消费核心 helper

以下均为新增实现。修改前缺少相应来源对象；修改后由同一class维护物理索引与独立token引用。每个helper先列外部职责，再给完整修改后源码与中文控制流；普通构造、slot_key/owner_key纯拼接另在对应调用说明中覆盖。

### 5.1 `clear_active()`

抽象功能描述：reset或首次初始化时结束当前物理来源与查询索引，独立L2历史由框架继续保存。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`clear_active()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function void clear_active();
        transport_by_source.delete();
        committed_version.delete();
        origin_by_version.delete();
        version_refcount.delete();
        load_fact_by_owner.delete();
        atomic_fact_by_owner.delete();
        first_split_fact_by_owner.delete();
        split_accumulator_by_owner.delete();
        previous_write_valid=0;
        previous_write_idx=0;
        previous_write_way=0;
        previous_write_version=0;
        main_first_s1='0;
        main_s2_flag='0;
        main_s3_flag='0;
        main_s3_refill='0;
        main_atomic_registered_valid=0;
        main_atomic_registered='0;
        main_atomic_registered_slot='0;
        main_atomic_registered_refill='0;
        atomic_owner='0;
        atomic_response_pending='0;
        foreach(read_pending_valid[p]) begin
            read_pending_valid[p]=0;
            read_pending_idx[p]=0;
            for(int w=0;w<16;w++) read_bypass_version[p][w]=0;
        end
        foreach(hit_delay1[p]) begin
            hit_delay1[p]='0;
            hit_delay2[p]='0;
            forward_pending[p]='0;
            load_s2_pending[p]='0;
        end
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是reset或首次初始化时结束当前物理来源与查询索引，独立L2历史由框架继续保存。
2. 按源码执行顺序，清空source、slot、版本引用和当前owner事实；清零延迟写、Main流水、原子pending和每个固定读口/Load口；不修改L2首次行选择、不创建动态epoch。构造函数先初始化计数和水位再调用此函数。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.2 `acquire_version()`

抽象功能描述：把真实slot或物理流水对来源版本的一次引用登记到小型计数表。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`acquire_version()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function void acquire_version(input longint unsigned version);
        if(version==0) return;
        if(!version_refcount.exists(version)) version_refcount[version]=0;
        version_refcount[version]++;
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是把真实slot或物理流水对来源版本的一次引用登记到小型计数表。
2. 按源码执行顺序，版本0无需登记；首次版本计数建0再加1，调用者必须在新slot、S0旁路或延迟写生效时调用，保证来源不会提前释放。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.3 `release_version()`

抽象功能描述：当真实slot或流水引用结束时回收不再可达的来源版本。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`release_version()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function void release_version(input longint unsigned version);
        if(version==0 || !version_refcount.exists(version)) return;
        if(version_refcount[version]>1) version_refcount[version]--;
        else begin
            version_refcount.delete(version);
            origin_by_version.delete(version);
        end
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是当真实slot或流水引用结束时回收不再可达的来源版本。
2. 按源码执行顺序，版本0或无计数直接结束；计数大于1只减1，否则删除计数和来源对象。已进入S1之后的消费保存纯值origin，不再持有版本引用。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.4 `release_completed_owner()`

抽象功能描述：root在完成同owner所有错误比较后释放指令消费索引，不影响硬件槽位来源。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`release_completed_owner()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function void release_completed_owner(input memblock_uid_t uid,input int unsigned epoch);
        load_fact_by_owner.delete(owner_key(uid,epoch));
        atomic_fact_by_owner.delete(owner_key(uid,epoch));
        first_split_fact_by_owner.delete(owner_key(uid,epoch));
        split_accumulator_by_owner.delete(owner_key(uid,epoch));
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是root在完成同owner所有错误比较后释放指令消费索引，不影响硬件槽位来源。
2. 按源码执行顺序，按UID与epoch拼接key，删除普通Load、原子、首页和split聚合事实；不清其它owner与slot，避免终检后继续增长。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.5 `observe_owner()`

抽象功能描述：在真实接受拍以readonly ROB映射冻结动态身份，供后续流水使用。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`observe_owner()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function automatic owner_t observe_owner(input cmo_path_probe_t event_item);
        memblock_rm_readonly_api ro;
        memblock_rm_readonly_api::status_view_t status;
        memblock_uid_t uid;
        owner_t result;
        result='0;
        ro=memblock_rm_readonly_api::get();
        if(!event_item.owner_valid || ro==null ||
            !ro.try_read_uid_by_rob_for_rm(memblock_rob_key_t'(event_item.rob),uid) ||
            !ro.read_status_for_rm(uid,status)) return result;
        if({status.rob_idx_flag,status.rob_idx_value}!=event_item.rob) return result;
        result.valid=1;
        result.uid=uid;
        result.epoch=status.dynamic_epoch;
        result.rob=event_item.rob;
        result.lq=event_item.lq;
        return result;
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是在真实接受拍以readonly ROB映射冻结动态身份，供后续流水使用。
2. 按源码执行顺序，先取readonly API；实际记录没有ROB身份、API缺失或ROB映射/状态缺失时返回无效身份；精确检查status ROB后冻结UID、epoch、ROB和LQ。不会用PA、最近CBO或最终异常猜owner。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.6 `unknown_origin()`

抽象功能描述：给来源证据不足场景构造不可证明的结果，不把缺记录当CLEAN。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`unknown_origin()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function automatic origin_t unknown_origin(input longint unsigned sample);
        origin_t result;
        result='0;
        result.sample=sample;
        return result;
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是给来源证据不足场景构造不可证明的结果，不把缺记录当CLEAN。
2. 按源码执行顺序，结果清零，仅记录查询sample；valid/proven保持0，后续query在水位到达后必须把它报告为确定缺证据。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.7 `clean_origin()`

抽象功能描述：为真实无命中、被kill或无该错误贡献的路径构造已证明CLEAN。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`clean_origin()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function automatic origin_t clean_origin(input longint unsigned sample);
        origin_t result;
        result='0;
        result.valid=1;
        result.proven=1;
        result.sample=sample;
        return result;
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是为真实无命中、被kill或无该错误贡献的路径构造已证明CLEAN。
2. 按源码执行顺序，结果清零后置valid/proven为1并保存sample；只有调用者已证明不消费此类错误时可使用，不替代未知有效hit。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.8 `resolve_delivered()`

抽象功能描述：依据独立L2 token和真实D.fire历史证明MSHR/refill实际可消费的错误输入。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`resolve_delivered()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function automatic origin_t resolve_delivered(
        input transport_t source,input bit [63:0] paddr,
        input longint unsigned cutoff,input bit require_full);
        mem_access_base_sequence::l2_response_history_t record;
        bit available;
        bit [1:0] needed;
        bit [63:0] coverage;
        origin_t result;
        result=unknown_origin(cutoff);
        if(!source.valid || cutoff==0) return result;
        if(!mem_access_base_sequence::query_l2_response_history(source.token,
                mem_access_base_sequence::mem_addr_t'(paddr),cutoff,2'b01,record,available) || !record.valid ||
                record.source_generation!=source.generation ||
                record.line_addr[47:6]!=paddr[47:6]) return result;
        coverage=0;
        if(record.beat_valid[0]) coverage|=record.beat0.byte_mask;
        if(record.beat_valid[1]) coverage|=record.beat1.byte_mask;
        if(require_full) begin
            needed=record.kind==2 ? 2'b11 : 2'b01;
            if((record.beat_valid&needed)!=needed) return result;
        end else if(!coverage[paddr[5:0]]) return result;
        result.valid=1;
        result.proven=1;
        result.token=source.token;
        result.line_addr=source.line_addr;
        // MissEntry真实raw是截至采样前所有已fire beat累积OR，不仅是数据选中的半行。
        if(record.beat_valid[0]) result.raw|={record.beat0.denied,record.beat0.corrupt};
        if(record.beat_valid[1]) result.raw|={record.beat1.denied,record.beat1.corrupt};
        return result;
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是依据独立L2 token和真实D.fire历史证明MSHR/refill实际可消费的错误输入。
2. 按源码执行顺序，先检查source和cutoff，再查询同token在cutoff前历史；要求source generation和物理line一致。根据真实beat字节覆盖证明当前PA，refill另需全部beat。成功后逐位OR所有已送达beat错误，这是MissEntry累积raw语义；未送达或未来beat不参与。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.9 `resolve_offer()`

抽象功能描述：证明Load direct真实广播的独立错误来源，允许valid且ready为0。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`resolve_offer()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function automatic origin_t resolve_offer(
        input transport_t source,input cmo_path_probe_t offer,
        input bit [63:0] paddr,input longint unsigned sample);
        mem_access_base_sequence::l2_response_history_t record;
        bit available;
        bit high_half;
        origin_t result;
        result=unknown_origin(sample);
        if(!source.valid || !offer.valid || offer.op!=9'd5 ||
            source.fired_beats>1 ||
            !mem_access_base_sequence::query_l2_response_history(source.token,
                mem_access_base_sequence::mem_addr_t'(paddr),sample,2'b01,record,available) || !record.valid ||
            record.kind!=2 || 32'(record.source)!=offer.id ||
            record.source_generation!=source.generation ||
            record.line_addr[47:6]!=paddr[47:6] ||
            record.echo_is_keyword!=offer.qual[0]) return result;
        high_half=record.echo_is_keyword^(source.fired_beats==1);
        if(paddr[5]!=high_half) return result;
        result.valid=1;
        result.proven=1;
        result.token=source.token;
        result.line_addr=source.line_addr;
        result.raw={record.denied,record.corrupt};
        return result;
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是证明Load direct真实广播的独立错误来源，允许valid且ready为0。
2. 按源码执行顺序，source、真实GrantData offer、token种类/line/source代次和关键词必须一致；以当前已fire beat数与关键词确定当前物理半行并检查PA。成功raw来自框架冻结响应，不读取actual raw或未来fire；覆盖/身份不符返回unknown。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.10 `fail()`

抽象功能描述：非UVM component类使用package报告入口输出功能错误。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`fail()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function void fail(input string report_id,input string detail);
        uvm_pkg::uvm_report_error(report_id,detail,UVM_NONE);
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是非UVM component类使用package报告入口输出功能错误。
2. 按源码执行顺序，调用uvm_pkg::uvm_report_error，避免普通class展开UVM宏时隐式访问不存在的component方法。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.11 `source_identity()`

抽象功能描述：为同拍producer调度未建立token的A接受记录延后补齐不可变传输身份。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`source_identity()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function automatic transport_t source_identity(input int unsigned source_id);
        transport_t result;
        longint unsigned token;
        mem_access_base_sequence::l2_response_history_t response;
        bit available;
        result='0;
        if(!transport_by_source.exists(source_id)) return result;
        result=transport_by_source[source_id];
        if(!result.valid && result.accept_sample!=0 &&
            mem_access_base_sequence::find_l2_response_at_accept(result.accept_sample,
                10'(source_id),0,token) &&
            mem_access_base_sequence::query_l2_response_history(token,mem_access_base_sequence::mem_addr_t'(result.line_addr),
                result.accept_sample,2'b01,response,available) && response.valid) begin
            result.valid=1;
            result.token=token;
            result.generation=response.source_generation;
            transport_by_source[source_id]=result;
        end
        return result;
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是为同拍producer调度未建立token的A接受记录延后补齐不可变传输身份。
2. 按源码执行顺序，先查真实source接受状态；仅在valid尚未建立但accept sample已冻结时按精确接受key查原token，核对line并取generation，然后写回source索引；不得拿当前新请求猜旧来源。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.11a `resolve_atomic_control()`

抽象功能描述：在原子state6实际消费时，把顶层CSR的两拍延迟关系转换成独立输入历史查询边界，并检查实际内部连接值。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`resolve_atomic_control()`。
函数功能简析：输入state6 sample及实际内部enable，输出顶层独立输入sample；actual只交叉核对，异常期望仍从顶层历史取得。

```systemverilog
    function bit resolve_atomic_control(input longint unsigned latch_sample,
        input bit actual_enable,output longint unsigned input_sample);
        memblock_sync_pkg::memblock_cache_error_control_t control;
        input_sample=0;
        if(latch_sample<=2) return 0;
        input_sample=latch_sample-2;
        if(!memblock_sync_pkg::read_cache_error_control(input_sample,control) || !control.valid)
            return 0;
        if(control.enable!=actual_enable) begin
            fail("RM_ATOMIC_CSR_DELAY",$sformatf(
                "state6 sample=%0d top_input_sample=%0d expected_enable=%0d actual_enable=%0d",
                latch_sample,input_sample,control.enable,actual_enable));
            return 0;
        end
        return 1;
    endfunction
```
中文伪代码：
1. 本函数区分实际门控消费时间和独立顶层输入采样时间，防止延迟束与顶层当拍混用。
2. 先清输出；消费sample不足两拍时返回无证据，禁止无符号下溢。否则将独立输入sample设为实际消费拍减2。
3. 调用共享CSR只读历史查该输入拍；记录缺失或无效直接返回失败。有效历史值与probe真实内部enable不同则通过fail报告连接/延迟错误，不以actual覆写独立答案。
4. 两边一致才返回成功；原子query继续输出顶层输入sample，root从该拍历史取enable形成异常，不能再减两拍。`read_cache_error_control()`仅查监视器独立输入事实，不读最终异常。

### 5.12 `check_raw()`

抽象功能描述：把DUT实际错误位与已独立证明的raw比较，actual永不参与expected构造。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`check_raw()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function void check_raw(input string point,input origin_t expected,input bit [1:0] actual);
        if(expected.proven && expected.raw!=actual)
            fail("RM_D_ERROR_RAW",$sformatf("%s token=%0d sample=%0d expected=%b actual=%b",
                point,expected.token,expected.sample,expected.raw,actual));
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是把DUT实际错误位与已独立证明的raw比较，actual永不参与expected构造。
2. 按源码执行顺序，只有expected已proven时比较两位raw；不同则用fail报告位置、token、sample与两边值。未知来源保留给query证据失败，不静默授权。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.13 `select_read_origin()`

抽象功能描述：在真实S1按命中way选择S0锁存旁路或当前已提交来源。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`select_read_origin()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function automatic origin_t select_read_origin(
        input int unsigned port,input bit [15:0] way_mask,input longint unsigned sample);
        origin_t result;
        longint unsigned version;
        result=clean_origin(sample);
        if(way_mask==0) return result;
        if(port>=CMO_PATH_ERROR_READ_PORTS || !read_pending_valid[port] || !$onehot(way_mask))
            return unknown_origin(sample);
        for(int w=0;w<16;w++) if(way_mask[w]) begin
            version=read_bypass_version[port][w];
            if(version==0 && committed_version.exists(slot_key(read_pending_idx[port],w)))
                version=committed_version[slot_key(read_pending_idx[port],w)];
            if(version==0 || !origin_by_version.exists(version)) return unknown_origin(sample);
            result=origin_by_version[version];
            result.sample=sample;
        end
        return result;
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是在真实S1按命中way选择S0锁存旁路或当前已提交来源。
2. 按源码执行顺序，无hit返回CLEAN；有效hit必须有真实读接受且way独热。命中way先查S0已锁存旁路版本，缺旁路才查当前committed slot。版本缺失返回unknown；成功复制origin并记录消费sample。Main首S1调用者自行跨stall保持。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.14 `observe_batch()`

抽象功能描述：唯一RM消费者在完整采样屏障后处理一份批次，发布独立owner-bound消费事实。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`observe_batch()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function void observe_batch(input cmo_path_agent_xaction tr);
        cmo_path_probe_t event_item;
        cmo_path_probe_t offer;
        origin_t current_write;
        longint unsigned current_write_version;
        bit current_write_valid;
        bit [15:0] current_write_idx,current_write_way;
        origin_t next_hit[CMO_PATH_LOAD_PORTS];
        origin_t main_atomic_expected,main_atomic_slot,main_atomic_refill;
        bit has_main_atomic;
        transport_t source;
        forward_t next_forward[CMO_PATH_LOAD_PORTS];
        consumption_t next_load_s2[CMO_PATH_LOAD_PORTS];
        bit has_next_load_s2[CMO_PATH_LOAD_PORTS];
        consumption_t combined;
        owner_t owner;
        origin_t selected;
        consumption_t accumulated;
        owner_key_t split_key;
        origin_t refill;
        origin_t next_main_s2;
        bit next_main_s2_valid;
        origin_t next_main_s3_flag,next_main_s3_refill;
        bit next_main_s3_valid;
        bit [1:0] selected_actual;
        longint unsigned token;
        mem_access_base_sequence::l2_response_history_t response;
        bit available;
        if(tr==null) return;
        if(!initialized || tr.reset_generation!=reset_generation || tr.reset_active) begin
            clear_active();
            reset_generation=tr.reset_generation;
            initialized=1;
            watermark=0;
            if(tr.reset_active) begin watermark=tr.sample_seq; return; end
        end
        if(tr.sample_seq<=watermark) return;
        offer='0;
        current_write='0;
        current_write_valid=0;
        current_write_version=0;
        current_write_idx=0;
        current_write_way=0;
        has_main_atomic=0;
        main_atomic_expected='0;
        main_atomic_slot='0;
        main_atomic_refill='0;
        next_main_s2_valid=0;
        next_main_s3_valid=0;
        next_main_s2='0;
        next_main_s3_flag='0;
        next_main_s3_refill='0;
        foreach(next_hit[p]) begin
            next_hit[p]=clean_origin(tr.sample_seq);
            next_forward[p]='0;
            next_load_s2[p]='0;
            has_next_load_s2[p]=0;
        end
        // TL A只创建当前source真实占用身份；当拍重用前的S3/refill引用已在早先流水冻结。
        foreach(tr.events[e]) begin
            event_item=tr.events[e];
            if(event_item.kind==CMO_PATH_DCACHE_TL_A) begin
                source='0;
                source.source_id=event_item.id;
                source.accept_sample=tr.sample_seq;
                source.line_addr={event_item.addr[63:6],6'b0};
                if(mem_access_base_sequence::find_l2_response_at_accept(tr.sample_seq,
                        10'(event_item.id),1'b0,token) &&
                    mem_access_base_sequence::query_l2_response_history(token,
                        mem_access_base_sequence::mem_addr_t'(event_item.addr),tr.sample_seq,2'b01,response,available) && response.valid) begin
                    source.valid=1;
                    source.token=token;
                    source.line_addr={event_item.addr[63:6],6'b0};
                    source.accept_sample=tr.sample_seq;
                    source.generation=response.source_generation;
                end
                transport_by_source[event_item.id]=source;
            end
            if(event_item.kind==CMO_PATH_DCACHE_TL_D_OFFER) offer=event_item;
        end
        // 先建立当前写版本，供同拍S0旁路引用；expected由已冻结S3来源提供。
        foreach(tr.events[e]) begin
            event_item=tr.events[e];
            if(event_item.kind==CMO_PATH_ERROR_ARRAY_WRITE) begin
                current_write=main_s3_refill;
                current_write.sample=tr.sample_seq;
                if(!event_item.state[0] || !current_write.valid ||
                    current_write.line_addr[47:6]!=event_item.addr[47:6])
                    current_write=unknown_origin(tr.sample_seq);
                if(current_write.proven &&
                    (!mem_access_base_sequence::query_l2_response_history(current_write.token,
                        mem_access_base_sequence::mem_addr_t'(event_item.addr),tr.sample_seq,2'b01,response,available) ||
                     !response.valid || 32'(response.source)!=event_item.id))
                    current_write=unknown_origin(tr.sample_seq);
                if(!current_write.proven) begin
                    source=source_identity(event_item.id);
                    fail("RM_D_ERROR_ORIGIN",$sformatf(
                        "errorArray写缺少独立refill来源 sample=%0d miss_id=%0d addr=0x%0h miss=%0d s3_origin(valid/proven/token/line/sample)=%0d/%0d/%0d/0x%0h/%0d transport(valid/token/accept/generation/beats)=%0d/%0d/%0d/%0d/%0d",
                        tr.sample_seq,event_item.id,event_item.addr,event_item.state[0],
                        main_s3_refill.valid,main_s3_refill.proven,main_s3_refill.token,
                        main_s3_refill.line_addr,main_s3_refill.sample,
                        source.valid,source.token,source.accept_sample,source.generation,source.fired_beats));
                end
                check_raw("errorArray.write",current_write,event_item.raw);
                current_write_valid=1;
                current_write_version=next_version++;
                origin_by_version[current_write_version]=current_write;
                current_write_idx=event_item.idx;
                current_write_way=event_item.way;
            end
        end
        // S1读、MainPipe流水与原子最终响应先取旧的已冻结状态。
        foreach(tr.events[e]) begin
            event_item=tr.events[e];
            case(event_item.kind)
                CMO_PATH_LOAD_PIPE_HIT: if(int'(event_item.port_id)<CMO_PATH_LOAD_PORTS) begin
                    selected=select_read_origin(int'(event_item.port_id),event_item.way,tr.sample_seq);
                    if(event_item.qual[0]) selected=clean_origin(tr.sample_seq);
                    next_hit[int'(event_item.port_id)]=selected;
                    selected_actual=0;
                    for(int w=0;w<16;w++) if(event_item.way[w])
                        selected_actual|=event_item.actual_by_way[w*2+:2];
                    if(!event_item.qual[0]) check_raw("LoadPipe.S1",selected,selected_actual);
                end
                CMO_PATH_MAIN_PIPE_S1: begin
                    selected=main_first_s1;
                    if(event_item.qual[0]) begin
                        selected=select_read_origin(CMO_PATH_LOAD_PORTS,event_item.way,tr.sample_seq);
                        main_first_s1=selected;
                        selected_actual=0;
                        for(int w=0;w<16;w++) if(event_item.way[w])
                            selected_actual|=event_item.actual_by_way[w*2+:2];
                        check_raw("MainPipe.firstS1",selected,selected_actual);
                    end
                    if(event_item.qual[1]) begin next_main_s2=selected; next_main_s2_valid=1; end
                end
                CMO_PATH_MAIN_PIPE_S2: if(event_item.qual[0]) begin
                    next_main_s3_flag=main_s2_flag;
                    next_main_s3_refill=clean_origin(tr.sample_seq);
                    check_raw("MainPipe.S2.flag",main_s2_flag,event_item.raw);
                    next_main_s3_valid=1;
                end
                CMO_PATH_MAIN_ATOMIC_RESPONSE: if(event_item.aux==2 && !event_item.state[2]) begin
                    if(event_item.state[1]!=event_item.state[0])
                        fail("RM_D_ERROR_ORIGIN","原子最终响应ack_miss_queue与真实s3_req.miss不一致");
                    main_atomic_expected=main_s3_flag;
                    main_atomic_slot=main_s3_flag;
                    main_atomic_refill=main_s3_refill;
                    main_atomic_expected.sample=tr.sample_seq;
                    if(event_item.state[0]) begin
                        main_atomic_expected.proven&=main_s3_refill.proven;
                        main_atomic_expected.raw|=main_s3_refill.raw;
                        check_raw("MainPipe.atomic.refill",main_s3_refill,event_item.raw);
                    end else if(event_item.raw!=0)
                        fail("RM_D_ERROR_RAW","non-refill atomic携带非零refill错误贡献");
                    check_raw("MainPipe.atomic.slot",main_s3_flag,event_item.raw_extra);
                    has_main_atomic=1;
                end
                default: begin end
            endcase
        end
        // S2 refill身份在source下一次重用前冻结，随后作为S3 error写及原子refill来源。
        foreach(tr.events[e]) begin
            event_item=tr.events[e];
            if(event_item.kind==CMO_PATH_MAIN_PIPE_REFILL && event_item.qual[0]) begin
                refill=unknown_origin(tr.sample_seq);
                if(transport_by_source.exists(event_item.id))
                    refill=resolve_delivered(source_identity(event_item.id),event_item.addr,
                        tr.sample_seq,1'b1);
                check_raw("MainPipe.refill",refill,event_item.raw);
                next_main_s3_refill=refill;
            end
        end
        // S1 forward每拍冻结真实source与此拍之前已送达范围；不会在S2查询后来复用的source。
        foreach(tr.events[e]) begin
            event_item=tr.events[e];
            if(event_item.kind==CMO_PATH_LOAD_FORWARD_SOURCE && int'(event_item.port_id)<CMO_PATH_LOAD_PORTS) begin
                next_forward[int'(event_item.port_id)].valid=1;
                next_forward[int'(event_item.port_id)].owner=observe_owner(event_item);
                next_forward[int'(event_item.port_id)].sample=tr.sample_seq;
                next_forward[int'(event_item.port_id)].direct=unknown_origin(tr.sample_seq);
                next_forward[int'(event_item.port_id)].mshr=unknown_origin(tr.sample_seq);
                if(transport_by_source.exists(event_item.id)) begin
                    source=source_identity(event_item.id);
                    next_forward[int'(event_item.port_id)].mshr=resolve_delivered(source,event_item.addr,
                        tr.sample_seq-1,1'b0);
                    if(event_item.qual[0] && event_item.qual[1] && event_item.aux==event_item.id &&
                        offer.valid && offer.id==event_item.aux)
                        next_forward[int'(event_item.port_id)].direct=resolve_offer(source,offer,
                            event_item.addr,tr.sample_seq);
                end
            end
        end
        // S2独立贡献合并后随真实fire推进，不因数据选择或SC失败关闭其它有效错误输入。
        foreach(tr.events[e]) begin
            event_item=tr.events[e];
            if(event_item.kind==CMO_PATH_LOAD_S2 && int'(event_item.port_id)<CMO_PATH_LOAD_PORTS && event_item.qual[5]) begin
                combined='0;
                combined.valid=1;
                combined.proven=1;
                combined.owner=forward_pending[int'(event_item.port_id)].owner;
                combined.sample=tr.sample_seq;
                combined.data_sample=tr.sample_seq;
                combined.is_prefetch=!event_item.qual[3];
                if(!combined.owner.valid || combined.owner.rob!=event_item.rob ||
                    combined.owner.lq!=event_item.lq) begin
                    combined.owner=observe_owner(event_item);
                    combined.proven=!(event_item.qual[0]||event_item.qual[1]);
                end
                if(event_item.qual[2]&&event_item.qual[3]&&event_item.qual[4]&&!event_item.qual[6]) begin
                    if(event_item.qual[0]) begin
                        selected=forward_pending[int'(event_item.port_id)].direct;
                        combined.proven&=selected.proven;
                        combined.raw|=selected.raw;
                        combined.expected_load_af|=selected.raw[1];
                        combined.expected_load_he|=selected.raw[0]&&!selected.raw[1];
                        check_raw("Load.S2.direct",selected,event_item.raw);
                    end
                    if(event_item.qual[1]) begin
                        selected=forward_pending[int'(event_item.port_id)].mshr;
                        combined.proven&=selected.proven;
                        combined.raw|=selected.raw;
                        combined.expected_load_af|=selected.raw[1];
                        combined.expected_load_he|=selected.raw[0]&&!selected.raw[1];
                        check_raw("Load.S2.mshr",selected,event_item.raw_extra);
                    end
                end
                next_load_s2[int'(event_item.port_id)]=combined;
                has_next_load_s2[int'(event_item.port_id)]=1;
            end
        end
        // 最终S3有效输出建立可查询事实；owner保留在S2接受时的epoch，不从最终exception倒推。
        foreach(tr.events[e]) begin
            event_item=tr.events[e];
            case(event_item.kind)
                CMO_PATH_LOAD_S3: if(int'(event_item.port_id)<CMO_PATH_LOAD_PORTS &&
                    ((event_item.qual[3]&&event_item.qual[4]) ||
                     (event_item.qual[6]&&event_item.qual[7]&&event_item.qual[8]&&event_item.qual[9]))) begin
                    combined=load_s2_pending[int'(event_item.port_id)];
                    if(!combined.owner.valid || combined.owner.rob!=event_item.rob ||
                        combined.owner.lq!=event_item.lq) begin
                        combined='0;
                        combined.owner=observe_owner(event_item);
                    end
                    if(event_item.qual[0]&&event_item.qual[1]&&event_item.qual[2]) begin
                        selected=event_item.qual[5] ? hit_delay2[int'(event_item.port_id)] : clean_origin(tr.sample_seq);
                        combined.proven&=selected.proven;
                        combined.raw|=selected.raw;
                        combined.expected_load_af|=selected.raw[1];
                        combined.expected_load_he|=selected.raw[0]&&!selected.raw[1];
                        check_raw("Load.S3.hit",selected,event_item.raw);
                    end
                    combined.valid=1;
                    combined.is_prefetch=!event_item.qual[1];
                    combined.sample=tr.sample_seq;
                    combined.paddr=event_item.addr;
                    combined.byte_mask=event_item.mask[15:0];
                    if(combined.owner.valid) begin
                        if(event_item.qual[6]&&event_item.qual[7]&&event_item.qual[8]&&event_item.qual[9]) begin
                            split_key=owner_key(combined.owner.uid,combined.owner.epoch);
                            // 首段与后段消费事实分别由真实mabuf返回驱动，未发出的段不会被模型补造。
                            if(!event_item.qual[10]) begin
                                first_split_fact_by_owner[split_key]=combined;
                                split_accumulator_by_owner[split_key]=combined;
                            end else begin
                                if(split_accumulator_by_owner.exists(split_key)) begin
                                    accumulated=split_accumulator_by_owner[split_key];
                                    combined.proven&=accumulated.proven;
                                    combined.raw|=accumulated.raw;
                                    combined.expected_load_af|=accumulated.expected_load_af;
                                    combined.expected_load_he|=accumulated.expected_load_he;
                                end else combined.proven=0;
                                split_accumulator_by_owner[split_key]=combined;
                            end
                            if(event_item.qual[10] || combined.expected_load_af || combined.expected_load_he)
                                load_fact_by_owner[split_key]=combined;
                        end else load_fact_by_owner[owner_key(combined.owner.uid,combined.owner.epoch)]=combined;
                    end
                end
                CMO_PATH_ATOMIC_REQUEST: atomic_owner=observe_owner(event_item);
                CMO_PATH_ATOMIC_RESPONSE: if(!event_item.state[0]&&!event_item.state[1]) begin
                    combined='0;
                    combined.valid=1;
                    combined.owner=atomic_owner;
                    combined.sample=tr.sample_seq;
                    combined.main_response_sample=main_atomic_registered.sample;
                    combined.atomic_proof[0]=main_atomic_registered_valid;
                    combined.atomic_proof[1]=main_atomic_registered.proven;
                    combined.atomic_proof[2]=main_atomic_registered.sample+1==tr.sample_seq;
                    combined.atomic_proof[3]=atomic_owner.valid && atomic_owner.rob==event_item.rob;
                    combined.proven=&combined.atomic_proof[3:0];
                    combined.raw=main_atomic_registered.raw;
                    combined.actual_raw=event_item.raw;
                    combined.atomic_slot_origin=main_atomic_registered_slot;
                    combined.atomic_refill_origin=main_atomic_registered_refill;
                    if(main_atomic_registered_valid && combined.atomic_proof[2])
                        check_raw("AtomicsUnit.final",main_atomic_registered,event_item.raw);
                    atomic_response_pending=combined;
                end
                CMO_PATH_ATOMIC_LATCH_CONTROL: begin
                    combined=atomic_response_pending;
                    combined.atomic_proof[4]=combined.valid && combined.owner.valid &&
                        combined.owner.rob==event_item.rob && combined.sample+1==tr.sample_seq;
                    combined.sample=tr.sample_seq;
                    combined.atomic_proof[5]=resolve_atomic_control(tr.sample_seq,event_item.qual[0],
                        combined.control_input_sample);
                    combined.actual_control_enable=event_item.qual[0];
                    combined.actual_raw=event_item.raw;
                    combined.proven&=combined.atomic_proof[4] && combined.atomic_proof[5];
                    selected=clean_origin(tr.sample_seq);
                    selected.proven=combined.proven;
                    selected.raw=combined.raw;
                    check_raw("AtomicsUnit.latch",selected,event_item.raw);
                    if(combined.owner.valid)
                        atomic_fact_by_owner[owner_key(combined.owner.uid,combined.owner.epoch)]=combined;
                    atomic_response_pending='0;
                end
                default: begin end
            endcase
        end
        // 本拍S1读取完成后再提交前拍延迟写；S0当前/上一拍写旁路优先于array版本。
        if(previous_write_valid)
            for(int w=0;w<16;w++) if(previous_write_way[w]) begin
                if(committed_version.exists(slot_key(previous_write_idx,w)))
                    release_version(committed_version[slot_key(previous_write_idx,w)]);
                committed_version[slot_key(previous_write_idx,w)]=previous_write_version;
                acquire_version(previous_write_version);
            end
        foreach(read_pending_valid[p]) begin
            read_pending_valid[p]=0;
            for(int w=0;w<16;w++) begin
                release_version(read_bypass_version[p][w]);
                read_bypass_version[p][w]=0;
            end
        end
        foreach(tr.events[e]) begin
            event_item=tr.events[e];
            if(event_item.kind==CMO_PATH_ERROR_ARRAY_READ && int'(event_item.port_id)<CMO_PATH_ERROR_READ_PORTS) begin
                read_pending_valid[int'(event_item.port_id)]=1;
                read_pending_idx[int'(event_item.port_id)]=event_item.idx;
                for(int w=0;w<16;w++) begin
                    read_bypass_version[int'(event_item.port_id)][w]=0;
                    if(current_write_valid && current_write_idx==event_item.idx && current_write_way[w])
                        read_bypass_version[int'(event_item.port_id)][w]=current_write_version;
                    else if(previous_write_valid && previous_write_idx==event_item.idx && previous_write_way[w])
                        read_bypass_version[int'(event_item.port_id)][w]=previous_write_version;
                    acquire_version(read_bypass_version[int'(event_item.port_id)][w]);
                end
            end
        end
        if(previous_write_valid) release_version(previous_write_version);
        if(current_write_valid) acquire_version(current_write_version);
        previous_write_valid=current_write_valid;
        previous_write_idx=current_write_idx;
        previous_write_way=current_write_way;
        previous_write_version=current_write_version;
        if(next_main_s2_valid) main_s2_flag=next_main_s2;
        if(next_main_s3_valid) begin
            main_s3_flag=next_main_s3_flag;
            main_s3_refill=next_main_s3_refill;
        end
        foreach(hit_delay1[p]) begin
            hit_delay2[p]=hit_delay1[p];
            hit_delay1[p]=next_hit[p];
            forward_pending[p]=next_forward[p];
            if(has_next_load_s2[p]) load_s2_pending[p]=next_load_s2[p];
        end
        // 最后推进真实D.fire beat计数；同拍direct offer使用计数推进前的物理beat。
        foreach(tr.events[e]) begin
            event_item=tr.events[e];
            if(event_item.kind==CMO_PATH_DCACHE_TL_D && transport_by_source.exists(event_item.id)) begin
                source=source_identity(event_item.id);
                if(event_item.op==9'd5 && !validate_prefetch_grant(source,event_item,tr.sample_seq))
                    fail("RM_PREFETCH_TOKEN",$sformatf("GrantData独立token/beat/raw不匹配 source=%0d token=%0d beat=%0d sample=%0d",
                        event_item.id,source.token,source.fired_beats,tr.sample_seq));
                if(event_item.op==9'd5) source.fired_beats++;
                transport_by_source[event_item.id]=source;
            end
        end
        main_atomic_registered_valid=has_main_atomic;
        if(has_main_atomic) begin
            main_atomic_registered=main_atomic_expected;
            main_atomic_registered_slot=main_atomic_slot;
            main_atomic_registered_refill=main_atomic_refill;
        end
        watermark=tr.sample_seq;
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是唯一RM消费者在完整采样屏障后处理一份批次，发布独立owner-bound消费事实。
2. 按源码执行顺序，本逻辑承担物理来源与指令消费的连接，先读旧拍状态再更新下一拍状态。空对象结束；reset代改变时清当前物理/owner索引；重复sample不处理。先建立本拍A身份和真实D offer，再用此前冻结的S3 refill建立当前error写版本并核对actual。随后完成各LoadPipe S1来源、Main首S1保持/真实fire推进、原子最终两项raw；S2 refill从已fire历史冻结来源，S1 forward冻结direct offer和MSHR已fire输入。Load S2只在真实fire存下一拍项，各有效direct/MSHR分别映射AF/HE；S3用旧S2及延迟hit合并，普通最终或真实mabuf返回分别归档。split首页与最终段按真实事件聚合，原子最终响应先pending、下一state6及顶层前2拍控制边界才发布fact。最后提交前拍延迟写slot、释放旧S0引用、逐way建立本拍S0旁路，推进物理流水；D.fire计数最后更新，水位最后前进。关键子helper分别负责身份、独立响应证明、raw核对和版本引用，不修改框架owner或期望字节。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.15 `query()`

抽象功能描述：RM按当前动态owner和完成上界查询普通Load或原子的独立消费期望。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`query()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function bit query(
        input memblock_uid_t uid,input int unsigned epoch,input bit [8:0] rob,
        input bit [7:0] lq,input longint unsigned access_sample,input bit atomic,
        output bit [1:0] expected_raw,output longint unsigned control_sample,
        output bit proven,output string detail);
        owner_key_t key;
        consumption_t fact;
        memblock_sync_pkg::memblock_cache_error_control_t control;
        bit control_found;
        key=owner_key(uid,epoch);
        expected_raw=0;
        control_sample=0;
        proven=0;
        detail="尚无owner-bound DCache错误消费事实";
        if(atomic) begin
            if(!atomic_fact_by_owner.exists(key)) return watermark>=access_sample;
            fact=atomic_fact_by_owner[key];
        end else begin
            if(!load_fact_by_owner.exists(key)) return watermark>=access_sample;
            fact=load_fact_by_owner[key];
        end
        if(!fact.valid || fact.sample>access_sample || fact.owner.rob!=rob ||
            (!atomic&&fact.owner.lq!=lq)) begin
            detail="DCache错误消费事实的sample/ROB/LQ不匹配";
            return watermark>=access_sample;
        end
        expected_raw=fact.raw;
        control_sample=atomic ? fact.control_input_sample : fact.sample;
        proven=fact.proven;
        detail=proven ? "独立token/slot来源已证明" : "实际消费存在，但独立错误来源缺失";
        control_found=0;
        if(atomic && !proven) begin
            control_found=memblock_sync_pkg::read_cache_error_control(fact.control_input_sample,control);
            detail=$sformatf(
            "原子证据proof[CSR/latch/owner/delay/raw/main]=%b uid=%0d epoch=%0d rob=%h main=%0d latch=%0d ctrl_input=%0d ctrl(found/expected/actual)=%0d/%0d/%0d expected_raw=%b actual_raw=%b slot(valid/proven/token/raw/sample)=%0d/%0d/%0d/%b/%0d refill=%0d/%0d/%0d/%b/%0d",
            fact.atomic_proof[5:0],fact.owner.uid,fact.owner.epoch,fact.owner.rob,
            fact.main_response_sample,fact.sample,fact.control_input_sample,control_found,control.enable,fact.actual_control_enable,
            fact.raw,fact.actual_raw,fact.atomic_slot_origin.valid,fact.atomic_slot_origin.proven,
            fact.atomic_slot_origin.token,fact.atomic_slot_origin.raw,fact.atomic_slot_origin.sample,
            fact.atomic_refill_origin.valid,fact.atomic_refill_origin.proven,fact.atomic_refill_origin.token,
            fact.atomic_refill_origin.raw,fact.atomic_refill_origin.sample);
        end
        return 1;
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是RM按当前动态owner和完成上界查询普通Load或原子的独立消费期望。
2. 按源码执行顺序，先清输出并按UID/epoch找对应fact；没有fact时仅用watermark判断是未到边界的等待还是已覆盖的缺证据。存在fact时检查sample上界、ROB及Load LQ；匹配才输出raw/control sample/proven。返回0仅表示可等待，返回1且proven0必须失败，不无限等待。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.16 `query_first_split_error()`

抽象功能描述：结合readonly首页context，对实际首段PA/mask及消费sample进行严格错误查询。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`query_first_split_error()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function bit query_first_split_error(
        input memblock_uid_t uid,input int unsigned epoch,input bit [8:0] rob,
        input bit [7:0] lq,input bit [63:0] first_pa,input int unsigned prefix_bytes,
        input longint unsigned data_sample,input longint unsigned response_sample,
        input longint unsigned access_sample,
        output bit expected_access_fault,output bit expected_hardware_error,
        output longint unsigned control_sample,output bit proven,output string detail);
        owner_key_t key;
        consumption_t fact;
        int unsigned offset;
        bit [15:0] required_mask;
        key=owner_key(uid,epoch);
        expected_access_fault=0;
        expected_hardware_error=0;
        control_sample=0;
        proven=0;
        detail="跨页首页缺少真实mabuf错误消费事实";
        if(watermark<access_sample) return 0;
        if(!first_split_fact_by_owner.exists(key)) return 1;
        fact=first_split_fact_by_owner[key];
        if(!fact.valid || fact.owner.rob!=rob || fact.owner.lq!=lq ||
            fact.sample!=response_sample || fact.data_sample!=data_sample || data_sample>=response_sample ||
            fact.sample>access_sample || prefix_bytes==0 || prefix_bytes>16 ||
            fact.paddr[63:6]!=first_pa[63:6]) return 1;
        offset=int'(first_pa[3:0]);
        if(offset+prefix_bytes>16) return 1;
        required_mask=(16'hffff>>(16-prefix_bytes))<<offset;
        if((fact.byte_mask&required_mask)!=required_mask) return 1;
        expected_access_fault=fact.expected_load_af;
        expected_hardware_error=fact.expected_load_he;
        control_sample=fact.sample;
        proven=fact.proven;
        detail=proven ? "首页实际PA/mask/消费sample均已证明" : "首页实际消费来源证据缺失";
        return 1;
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是结合readonly首页context，对实际首段PA/mask及消费sample进行严格错误查询。
2. 按源码执行顺序，先检查水位；必须存在首页owner fact且ROB/LQ相同。实际S2 data sample与S3 response sample分别精确匹配context，响应必须晚于取数且不超过WB。检查独立prefix大小、物理line和每个请求字节的16B mask覆盖；通过后输出分项AF/HE与proven，不补发高页请求。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.17 `query_load_exception_bits()`

抽象功能描述：普通Load以各输入先映射再合并的两位异常期望进入RM，防止合并raw丢HE。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_dcache_error_observer.sv`，函数：`query_load_exception_bits()`。
函数功能简析：由真实批次或RM查询调用；输入/副作用按第3章字段生命周期约束，以下源码显示全部当前函数。

```systemverilog
    function bit query_load_exception_bits(
        input memblock_uid_t uid,input int unsigned epoch,input bit [8:0] rob,
        input bit [7:0] lq,input longint unsigned access_sample,
        output bit expected_access_fault,output bit expected_hardware_error,
        output longint unsigned control_sample,output bit proven,output string detail);
        bit [1:0] raw;
        owner_key_t key;
        expected_access_fault=0;
        expected_hardware_error=0;
        if(!query(uid,epoch,rob,lq,access_sample,0,raw,control_sample,proven,detail)) return 0;
        key=owner_key(uid,epoch);
        if(!load_fact_by_owner.exists(key)) return 1;
        expected_access_fault=load_fact_by_owner[key].expected_load_af;
        expected_hardware_error=load_fact_by_owner[key].expected_load_he;
        return 1;
    endfunction
```
中文伪代码：
1. 本函数在当前流程的作用是普通Load以各输入先映射再合并的两位异常期望进入RM，防止合并raw丢HE。
2. 按源码执行顺序，先调用query确认身份/水位/证据；未到水位返回等待，确定缺记录保持proven0；存在fact才输出独立AF与HE。原子仍使用总raw后denied优先。
3. 输入来自真实probe、readonly身份或独立L2历史；调用到的关键子helper职责均在本章独立展开。失败只报告或返回证据状态，不改变DUT、框架owner与RM期望内存。

### 5.18 `test_atomic_registered_transport()`

抽象功能：以已知零贡献夹具独立检查真实 MainPipe 到 AtomicsUnit 的一拍寄存边界；同拍、空拍、重复、非原子 source 和 miss/replay 均不能形成可用证据。实际 raw 正向仍由前述 token 测试证明。

```systemverilog
    static function bit test_atomic_registered_transport(output string detail);
        rm_dcache_error_observer observer;
        cmo_path_agent_xaction tr;
        cmo_path_probe_t main_event,unit_event,latch_event;
        longint unsigned unit_sample,ctrl_sample;
        bit [1:0] raw;
        bit proven;
        string query_detail;
        for(int mode=0;mode<5;mode++) begin
            observer=new();
            observer.initialized=1;
            observer.atomic_owner='0;
            observer.atomic_owner.valid=1;
            observer.atomic_owner.uid=100+mode;
            observer.atomic_owner.epoch=3;
            observer.atomic_owner.rob=9'h40+9'(mode);
            // 此测试固定零贡献已知来源，单独检查寄存边界；真实raw在主测试由token推导。
            observer.main_s3_flag=observer.clean_origin(100);
            observer.main_s3_refill=observer.clean_origin(100);
            main_event='0;
            main_event.valid=1;
            main_event.kind=CMO_PATH_MAIN_ATOMIC_RESPONSE;
            main_event.aux=mode==3 ? 1 : 2;
            main_event.state=mode==4 ? 4 : 0;
            unit_event='0;
            unit_event.valid=1;
            unit_event.kind=CMO_PATH_ATOMIC_RESPONSE;
            unit_event.rob=observer.atomic_owner.rob;
            tr=batch(100);
            tr.events.push_back(main_event);
            if(mode==0) tr.events.push_back(unit_event);
            observer.observe_batch(tr);
            unit_sample=mode==0 ? 100 : mode==1 ? 102 : 101;
            if(mode!=0) begin
                if(mode==1) observer.observe_batch(batch(101));
                tr=batch(unit_sample);
                tr.events.push_back(unit_event);
                observer.observe_batch(tr);
            end
            memblock_sync_pkg::publish_cache_error_control(unit_sample-1,0,1);
            latch_event=unit_event;
            latch_event.kind=CMO_PATH_ATOMIC_LATCH_CONTROL;
            latch_event.qual=1;
            tr=batch(unit_sample+1);
            tr.events.push_back(latch_event);
            observer.observe_batch(tr);
            if(!observer.query(100+mode,3,unit_event.rob,0,unit_sample+1,1,
                    raw,ctrl_sample,proven,query_detail)) begin detail="原子负向测试水位缺失";return 0;end
            if(mode!=2 && proven) begin detail=$sformatf("非法原子寄存来源获准 mode=%0d",mode);return 0;end
            if(mode==2) begin
                if(!proven) begin detail="真实一拍Main到Unit未获证明";return 0;end
                // 完成后再重复Unit同一响应，前一拍Main Valid已清，不能复用旧raw。
                tr=batch(unit_sample+2);
                tr.events.push_back(unit_event);
                observer.observe_batch(tr);
                memblock_sync_pkg::publish_cache_error_control(unit_sample+1,0,1);
                tr=batch(unit_sample+3);
                tr.events.push_back(latch_event);
                observer.observe_batch(tr);
                if(!observer.query(100+mode,3,unit_event.rob,0,unit_sample+3,1,
                        raw,ctrl_sample,proven,query_detail)||proven) begin
                    detail="重复Unit响应复用了过期Main寄存来源";return 0;
                end
            end
        end
        detail="原子同拍/空拍/重复/source过滤/miss负向检查通过";
        return 1;
    endfunction
```

中文伪代码：

1. 对五种场景分别建立独立 observer、固定 owner 和已知 slot/refill 夹具，避免前一测试的来源泄漏。
2. 在 sample100 送 Main 响应，按场景选择 Unit 同拍、下一拍或隔一空拍，非原子 source 与 miss 均不登记 Main 最终响应。
3. Unit 接受后下一拍送 state6，独立发布其前两拍顶层 CSR；只有恰好隔一拍且 source2 最终响应可获证明。
4. 正向完成后重复同一 Unit 响应时，已清空的 Main 寄存 Valid 必须拒绝复用旧来源。
5. 任一负向误获证明，或正向未获证明，立即返回明确失败。

### 5.19 `validate_prefetch_grant()`

抽象功能：逐beat核对真实 L1 预取 GrantData 的独立 token、generation、raw 与 sample；其他合法来源由既有消费者检查。

```systemverilog
    function bit validate_prefetch_grant(input transport_t source,
        input cmo_path_probe_t event_item,input longint unsigned sample);
        mem_access_base_sequence::l2_response_history_t response;
        bit available;
        bit [1:0] required_beats,expected_raw;
        longint unsigned expected_sample;
        if(!source.valid || event_item.op!=9'd5) return 0;
        // 底座要求非零beat mask；首次01只取record元信息，不以available决定当前beat。
        if(!mem_access_base_sequence::query_l2_response_history(source.token,
            mem_access_base_sequence::mem_addr_t'(source.line_addr),sample,2'b01,response,available) ||
            !response.valid) begin
            return 0;
        end
        if(response.req_source!=MEMBLOCK_REQ_L1_PREFETCH) return 1;
        required_beats=source.fired_beats==0 ? 2'b01 : 2'b10;
        if(source.fired_beats>1 || response.kind!=2 ||
            response.source_generation!=source.generation || 32'(response.source)!=event_item.id ||
            !mem_access_base_sequence::query_l2_response_history(source.token,
                mem_access_base_sequence::mem_addr_t'(source.line_addr),sample,required_beats,response,available) ||
            !available) begin
            return 0;
        end
        expected_raw=source.fired_beats==0 ? {response.beat0.denied,response.beat0.corrupt} :
                                            {response.beat1.denied,response.beat1.corrupt};
        expected_sample=source.fired_beats==0 ? response.beat0.sample : response.beat1.sample;
        if(expected_sample!=sample || expected_raw!=event_item.raw) return 0;
        `uvm_info("RM_PREFETCH_GRANT",$sformatf(
            "token=%0d source=%0d generation=%0d beat=%0d sample=%0d raw=%0b 独立预取响应比较PASS",
            source.token,event_item.id,source.generation,source.fired_beats,sample,expected_raw),UVM_HIGH)
        return 1;
    endfunction
```

中文伪代码：先查询当前 source 的独立 token，非预取来源保持既有通路；预取来源按已 fire 数选择本 beat，核对 generation、source 和该拍已送达事实，再逐位比较 raw 与准确 sample；缺记录、未来 beat 或错拍一律返回失败。成功后以 `RM_PREFETCH_GRANT` 输出 token、source、generation、beat、sample 和 raw 诊断，返回成功；日志不改变分类或比较算法。

### 5.20 `query_prefetch()`

抽象功能：复用 Load 消费事实查询当前软件预取资格，保留 owner/epoch/ROB/LQ/sample 检查，不构造返回数据。

```systemverilog
    function bit query_prefetch(input memblock_uid_t uid,input int unsigned epoch,
        input bit [8:0] rob,input bit [7:0] lq,input longint unsigned access_sample,
        output bit proven,output string detail);
        bit [1:0] raw;
        longint unsigned control_sample;
        owner_key_t key;
        if(!query(uid,epoch,rob,lq,access_sample,0,raw,control_sample,proven,detail)) return 0;
        key=owner_key(uid,epoch);
        if(!proven) return 1;
        if(!load_fact_by_owner.exists(key) || !load_fact_by_owner[key].is_prefetch ||
            load_fact_by_owner[key].expected_load_af || load_fact_by_owner[key].expected_load_he) begin
            proven=0;detail="软件预取终态的prefetch资格或错误抑制不一致";
        end
        return 1;
    endfunction
```

中文伪代码：先执行公共 query 的身份与水位检查；未到水位可等待，缺证据明确失败。事实存在时要求 is_prefetch 且两个响应错误期望位均未追加；普通 Load 不能冒充预取完成。

### 5.21 `prefetch_suppression_matches()`

抽象功能：独立判断真实软件预取写回是否满足响应错误的同步异常抑制。

```systemverilog
    static function bit prefetch_suppression_matches(input bit [23:0] actual_exception);
        return !actual_exception[5] && !actual_exception[19];
    endfunction
```

中文伪代码：只核对同步 bit5 和 bit19 均为零，不读取或比较数据，其他异常仍由现有终态逻辑负责。

### 5.22 `test_prefetch_error_suppression()`

抽象功能：独立覆盖预取传输错误与软件抑制资格，不将 mock 结果当作真实 DUT 通过。

```systemverilog
    static function bit test_prefetch_error_suppression(output string detail);
        rm_dcache_error_observer observer;
        rm_dcache_error_observer::transport_t source;
        rm_dcache_error_observer::consumption_t fact;
        mem_access_base_sequence::l2_d_error_line_record_t choice;
        mem_access_base_sequence::l2_response_history_t response;
        mem_access_base_sequence::mem_addr_t line;
        cmo_path_probe_t event_item;
        bit candidate,available,proven;
        longint unsigned token;
        string query_detail;
        observer=new();
        line=48'h80005000;
        void'(mem_access_base_sequence::lookup_l2_line_choice(line,60,10'd3,2,MEMBLOCK_REQ_L1_PREFETCH,1,
            mem_access_base_sequence::L2_D_ERROR_DENIED,candidate,choice));
        token=mem_access_base_sequence::begin_l2_response_history(line,60,10'd3,0,2,4'd6,
            MEMBLOCK_REQ_L1_PREFETCH,1,1,0,0);
        mem_access_base_sequence::publish_l2_response_beat_history(token,0,61);
        mem_access_base_sequence::publish_l2_response_beat_history(token,1,62);
        void'(mem_access_base_sequence::query_l2_response_history(token,line,62,2'b11,response,available));
        source='0;source.valid=1;source.token=token;source.line_addr=64'(line);
        source.source_id=3;source.generation=response.source_generation;source.accept_sample=60;
        event_item='0;event_item.valid=1;event_item.op=5;event_item.id=3;event_item.raw=2'b10;
        if(!observer.validate_prefetch_grant(source,event_item,61)) begin detail="预取首beat独立传输未获证明";return 0;end
        if(observer.validate_prefetch_grant(source,event_item,62)) begin detail="预取错误sample被接受";return 0;end
        event_item.raw=0;
        if(observer.validate_prefetch_grant(source,event_item,61)) begin detail="预取错误raw被接受";return 0;end
        event_item.raw=2'b10;source.fired_beats=1;
        if(observer.validate_prefetch_grant(source,event_item,61)) begin detail="预取未来第二beat被提前使用";return 0;end
        if(!observer.validate_prefetch_grant(source,event_item,62)) begin detail="预取第二beat独立传输未获证明";return 0;end
        // 预取无返回数据比较，消费资格与同步bit5/19抑制必须独立证明。
        fact='0;fact.valid=1;fact.proven=1;fact.is_prefetch=1;fact.sample=64;
        fact.owner.valid=1;fact.owner.uid=200;fact.owner.epoch=3;fact.owner.rob=9'h122;fact.owner.lq=8'h91;
        observer.watermark=64;
        observer.load_fact_by_owner[observer.owner_key(200,3)]=fact;
        if(!observer.query_prefetch(200,3,9'h122,8'h91,64,proven,query_detail)||!proven ||
            !rm_dcache_error_observer::prefetch_suppression_matches(0) || rm_dcache_error_observer::prefetch_suppression_matches(24'h20) ||
            rm_dcache_error_observer::prefetch_suppression_matches(24'h80000)) begin detail="软件预取同步错误抑制检查失败";return 0;end
        fact.is_prefetch=0;observer.load_fact_by_owner[observer.owner_key(200,3)]=fact;
        if(!observer.query_prefetch(200,3,9'h122,8'h91,64,proven,query_detail)||proven) begin detail="普通Load冒充预取终态";return 0;end
        if(!observer.query_prefetch(201,3,9'h122,8'h91,64,proven,query_detail)||proven) begin detail="预取缺失owner被当成CLEAN";return 0;end
        detail="预取逐beat token/raw/sample与软件资格/同步抑制通过";
        return 1;
    endfunction
```

中文伪代码：构造合法预取 token 并分别发布两个 beat，正向检查 raw/sample，负向检查错 raw、错拍与未来 beat；随后构造软件消费身份，验证预取资格、同步位5/19抑制以及缺 owner、普通 Load 冒充均被拒绝。

### 5.23 `observer_check_prefetch_d_error_suppression()`

抽象功能：在软件预取已提交时比较真实 LDA 身份与响应错误同步抑制，保持 OTHER 的无数据效果语义。

```systemverilog
function bit memblock_rm::observer_check_prefetch_d_error_suppression(rm_ls_program_item_t item);
    memblock_rm_readonly_api::dut_writeback_view_t wb;
    memblock_rob_key_t rob_key;
    memblock_rm_readonly_api ro;
    bit proven;
    string detail;
    ro=memblock_rm_readonly_api::get();
    rob_key='{flag:item.rob.flag,value:item.rob.value};
    if(!ro.read_dut_load_writeback_for_rm(rob_key,wb)) begin
        observer_pending_evidence=1;
        ls_model.set_error(RM_LS_ERR_D_ERROR_NOT_READY,"软件预取缺少当前ROB的真实LDA终态");return 0;
    end
    if(!wb.valid || !wb.rob_valid || !wb.rob_flag_valid || !wb.exception_valid ||
        wb.source_kind!=1 || wb.rob_flag!=item.rob.flag || wb.rob_value!=item.rob.value ||
        wb.replay_inst || wb.flush_pipe || wb.sample_seq==0) begin
        ls_model.set_error(RM_LS_ERR_D_ERROR_NOT_READY,"软件预取LDA终态身份或属性不匹配");return 0;
    end
    if(!dcache_errors.query_prefetch(item.uid,item.dynamic_epoch,
        {item.rob.flag,item.rob.value[7:0]},{item.lq_flag,item.lq_value},wb.sample_seq,proven,detail)) begin
        observer_pending_evidence=1;return 0;
    end
    if(!proven || !rm_dcache_error_observer::prefetch_suppression_matches(wb.exception_vec)) begin
        ls_model.set_error(RM_LS_ERR_EXCEPTION_MISMATCH,
            $sformatf("软件预取响应错误抑制失败 uid=%0d sample=%0d exception=0x%h detail=%s",
                item.uid,wb.sample_seq,wb.exception_vec,detail));return 0;
    end
    `uvm_info("RM_PREFETCH_COMPARE",$sformatf("uid=%0d epoch=%0d sample=%0d 响应错误同步抑制PASS，无数据效果比较",
        item.uid,item.dynamic_epoch,wb.sample_seq),UVM_LOW)
    return 1;
endfunction
```

中文伪代码：读取当前 ROB 的真实 LDA；缺失时仅等待既有有界轮询，属性或 owner 错配立即失败。随后以 UID/epoch/ROB/LQ 查询共享消费事实，核对 prefetch 资格和 bit5/19，成功仅记录比较结果，不调用 backing、Load 返回值或写效果函数。

## 6. Root RM 集成独立复查

抽象功能描述：root集成层将纯值消费事实接入已有程序序commit，权限/翻译异常仍来自公共独立模型，只有证据和实际写回一致才读取backing或提交字节。

- `observer_service_path_facts()` 延迟处理本拍直到所有producer完成，唯一取FIFO并依次交给DCache、Uncache、CMO观察者；每个观察者只处理其职责，不生成额外响应。DCache查询在水位已覆盖但证据不足时立即失败，不因地址历史为错就预测当前异常。
- Load 使用 `query_load_exception_bits()` 分别追加bit5/19，翻译/PMA前置异常直接跳过数据错误查询；first split先用既有readonly首页context证明实际路径，再调用新首页PA/mask/sample接口，不查询未执行高页。
- 原子仅正常cacheable路径查询独立来源，两项raw先OR，CSR从真实state6前2拍的顶层输入sample读取，enable=0不追加响应异常；TLB/PMP/PMA异常不受此开关关闭。
- CMO权限zero为write、nonzero为read，但异常外显统一Store类；前置fault要求空请求集合。cacheablezero只在正确完成后清64B，复用`commit_memory_effect()`与失败SC shadow，不用永久错行拒绝正常Store/zero写入。
- MMIO Store后端已接受、随后注入响应错误时仍提交已接受字节；后端拒绝不造写效果；NC不补造已提交指令同步异常，由独立通知checker闭环。

独立复查曾发现并反馈root的事项：CMO fault ROB commit早于terminal_done需要等待；原子CSR应取state6而非响应接受拍；first split/正常split需要真实mabuf消费与PA范围证明。DCache与split部分已在本文件闭环，CMO终态等待由root源码与最终仿真记录确认。这些集成点已通过实际验证；nonzero CMO故障后的flushSb排空存在已复现SQ重入及排空阻塞，责任边界仍待复核，原场景保留BLOCKED，不能声称全部恢复组合通过。

### 6.1 CBO AUTO 定向构建的 bootstrap 修复

抽象功能描述：定向CBO builder 完成静态表导入后，为已经由父VSEQ启动的AUTO控制worker建立现有控制运行期握手。它仅补齐父构建入口被覆盖后遗漏的生命周期步骤。

修改前：builder从DISABLED扩展到AUTO时，导入7项定向表和最终check_store，但未执行父`memblock_dispatch_base_sequence::build_main_table()`末尾的bootstrap。真实zero用例在267.800ns进入主service首次worker shutdown检查时，因`bootstrap_started=0`直接fatal，尚未发出目标指令，不能判为CMO硬件功能失败。

修改后：在整表导入及策略检查后，active control topology复用同一服务入口；DISABLED无新增动作。正确性检查是父VSEQ仍唯一启动CSR/SFence worker，未放宽任何fatal、不引入第二reset owner、不重复构表。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_cbo_directed_sequence.sv`，函数：`build_main_table()`末尾。
函数功能简析：输入已冻结topology及完整导入表，唯一副作用是激活现有control runtime bootstrap。

```systemverilog
    import_manual_main_table();
    check_main_table_control_policy("memblock_main_dispatch_cbo_directed_sequence");
    // 中文注释：此定向builder覆盖了父类build_main_table，必须在整表导入后复用同一bootstrap。
    // AUTO的CSR/SFence worker由父VSEQ唯一启动；这里只建立控制运行期握手，不重建表或放宽检查。
    if (memblock_sync_pkg::uses_control_barrier_topology()) begin
        control_barrier_service.initialize_control_runtime_bootstrap();
    end
```
中文伪代码：
1. 本逻辑在定向表完成后建立与父AUTO表相同的运行期控制准备状态。
2. 先导入全部静态项，再检查本表是否满足冻结topology策略；检查失败沿既有fatal停止，不进入bootstrap。
3. topology启用控制barrier时，调用公用服务的初始化入口；它取得现有data/commit句柄，清未交付action、请求当前控制reset握手并置bootstrap_started，重复调用天然无副作用。
4. DISABLED不调用。父VSEQ继续唯一启动控制worker，主service随后按既有ready/ack规则推进，shutdown不再因漏初始化提前fatal。

初次失败日志：`/tmp/cmo_zero_run_20261111.log`。本修复仅涉及现有sequence文件，seq_pkg既有include不需新增filelist项；root统一重新编译并复测四种CBO，结果另行更新。

## 7. 验证、风险与测试入口

已执行：48口完整结构/连续索引检查；全部内部层级引用逐一对照当前`build/rtl`存在；connect dec/interface在匹配真实宽度的mock层级进行Verilator静态检查；来源class与软测在独立mock L2 API上实际编译执行通过，包含direct valid未ready、错误source/generation/半行、未fire/未来fire过滤、逐way旁路、Main首S1保持、原子双贡献/重放/state6及顶层前2拍控制边界、Load独立AF与HE、首页PA/mask/sample负向及正常split两段闭环。mock验证不能替代真实VCS/完整L2 API与DUT时序验证。

VCS发现的Q2020兼容问题已修：类作用域地址类型全部显式限定；函数返回struct先赋临时变量再读member。真实VCS统一编译及专项soft通过；Load CLEAN/denied/corrupt、原子gate0数据效果、gate1 AMO/LR/SC异常及软件PrefetchR真实双beat错误/同步抑制已通过。完整结果与限制见集成review第5章及逐场景CSV；未执行项不计通过。

风险：当前connect对应V2三Load口生成层级；profile变化须同步探针。无法证明的有效hit明确FAIL；首次reset后正常refill会建立CLEAN版本。新增来源模型不覆盖L1 ECC本身、不模拟数据/替换算法、不处理向量请求。错误source/generation、半行覆盖和未来样本均不得降级为CLEAN。

## 8. Plan 对齐检查

关联：

- `AI_DOC/plan/rm_plan/do/memblock_cmo_blackbox_rm_coding_plan_20261103.md`
- `AI_DOC/plan/rm_plan/do/memblock_nonvector_d_error_path_classification_plan_20261110.md`
- `AI_DOC/plan/test_framework/plan/do/memblock_l2_grantdata_first_line_weighted_error_plan_20261110.md`

### 8.1 实现与 Plan 不一致项

均已写入执行中 `IMPLEMENTATION_DELTA`，不能当成原plan已有：CBO AUTO定向表补齐父类控制bootstrap；Valid接口按实际采集；外TL必要事实统一进入cmo_path批次；direct真实valid offer允许ready0；原子CSR取state6；Load分项AF/HE与split真实返回索引。原因是当前生成RTL实际接口/控制时序要求，保持当前实现，不能回退到不准确的fire/末beat/PA摘要规则。源码及逐步中文伪代码分别见第4、5章相应helper，不另复制不连续代码。

### 8.2 Plan 未说明但 Coding 落实的细节

同拍A接受token的延后精确key补齐、48个固定probe容量、普通class使用UVM package报告、来源版本有界引用计数、reset当前索引清理、producer水位已到后的缺证据FAIL、完成owner回收均为必要工程细节。源码位置、完整函数及中文伪代码见`source_identity/acquire_version/release_version/clear_active/query/release_completed_owner`。已回写统一错误plan的执行中补充，未引入新的配置参数或隐藏cache算法。

## 9. 非本次修改的逻辑分析

本review覆盖第2章明确文件；git工作区同时存在以下本任务其它分工改动，不能当成本review已验证。

| 类别 | 文件/目录 | 判断与原因 |
|---|---|---|
| 其它功能逻辑 | `mem_base_sequence.sv`、readonly API、L2/Uncache responder/通知checker | 同三plan的feature1及Uncache分工，由对应review负责；本review仅独立检查feature1暂存快照的首次冻结/Store排除/逐beat事实无阻塞 |
| CMO请求/核心 | `rm_cmo_request_observer.sv`、`rm_ls_core.sv`、CMO directed sequence | 由CMO专项review完整展开，本文只独立复查集成契约与实际数据效果 |
| 参数/测试配置 | plus、CSR相关cfg、TC package/seq package及专项preset | 本任务其它分工创建/删除sticky与测试入口；本review只维护DCache专项CSV |
| 文档同步/整理 | responder flow、历史sticky/first split计划、其他implementation review | 由对应分工同步，不重复认领正确性 |
| 仿真/生成产物 | 远端VCS日志/本地临时mock产物 | 不提交mock产物，不改生成RTL；真实验证状态以root汇总为准 |
| 未知修改 | 无额外用户脏改动纳入本次实现 | coding前基线已由root独立提交；本agent未执行git add/commit或移动plan |

### 9.1 原子CSR延迟交叉复查补充

独立review指出MemBlock顶层输入到AtomicsUnit有2拍延迟；当前实现已保留实际state6 sample与独立顶层control_input_sample两种字段。此前只取state6同拍顶层历史会在输入变化时误判，已修为state6减2并采真实内部enable交叉验证。soft在两拍窗口内分别构造相反输入值，验证方向改变不影响身份和raw来源；sample<=2返回缺证据，未放宽checker。

errorArray缺来源错误诊断已补充当前写地址/真实miss与冻结S3 origin、source token/accept/generation/beat数，用来区分真实producer锚点错拍和流水引用缺失；诊断不会把未知来源降级为CLEAN。

### 9.2 原子 DCache 响应寄存边界修复

真实 AMO denied 联调暴露：`DCache.sv:20348` 将 `source==2` 的 MainPipe 原子响应寄存一拍才送到 AtomicsUnit。原实现要求两侧同拍，导致实际消费存在但来源 `proven=0`；原同拍 mock 未覆盖真实寄存边界。

修复后每批次先用上一批冻结来源服务 Unit，批次末只将当拍 source2 最终 Main 响应登记为下一拍有效来源，空拍、miss/replay 和其他 source 均清除 Valid。Unit 只接受 `Main sample+1`，state6 只接受 `Unit sample+1`，独立顶层 CSR 仍取 `state6 sample-2`。不补造来源，也不将缺证据视为 CLEAN。

失败查询保存并输出 Main/Unit/state6 证据位、owner、Main/latch/CSR sample、独立 CSR 是否存在及期望/实际门控值，以及 slot/refill 的 token、raw 和证明状态。新增五组寄存边界负向检查，本地真实 observer + mock provider 可执行检查通过；真实 DUT 复测由 root 汇总。

### 9.3 最终范围复查与预取闭环

统一错误方案第6.12节要求预取实际传输错误与软件抑制比较，最终复查发现原实现只有注入能力和 Load 资格屏蔽，未比较软件预取真实 LDA。此次补齐共享 observer 的逐beat预取 token 检查，以及 RM 的软件 LDA owner/资格/同步位5和19检查；硬件预取不创建 UID，软件提前写回不等待后台 refill，也不凭空要求每次预取均发下游请求。两个边界独立服务，软件预取无数据效果语义保持原 core 通路。

本地真实 observer 配 mock provider 的可执行测试已通过新增正负向用例；真实编译与 DUT 定向仍由 root 汇总。最终 reset/drain 只读复查未发现新增 P0/P1：CMO reset 取消旧请求历史并清活跃链，Uncache reset 清 token/通知关联，终态后的迟到请求继续检查；高频 helper 仅按 key 与固定物理口工作，无新增全历史扫描。

真实门控用例入口核验：普通 AUTO control topology 的静态基线没有调用 CSR initial randomizer，单独修改 `MEMBLOCK_CSR_ENABLE_CACHE_ERROR_*_WT` 不能证明内部 gate1。已有 DISABLED topology 的 `memblock_mmu_sv39_csr_sequence` 显式驱动 gate1，可在相同 smoke VSEQ 以 `MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=0` 复用；AUTO 默认 gate0 作为反向入口。两者都必须检查真实内部门控采样，不能按 plusarg 声称命中。

预取补充独立复核修正：底座 `query_l2_response_history()` 拒绝零 beat mask。首次分类查询现用合法 `01`，只读取元信息及 `record.valid`，不以首次 `available` 推断当前 beat；随后仍按实际 `01/10` 查询送达性并检查准确 sample。临时 mock 同步拒绝 sample0/mask00，修后重新编译执行；L2 agent 已独立只读复核该契约、软件资格及硬件无 UID 边界，无剩余必改项。

## 最终验证收口（2026-11-11）

真实统一VCS编译、软件检查及必要DUT验证已完成；具体命令、结果与未覆盖项见同目录`memblock_cmo_d_error_rm_integration_review_20261111.md`第5章。isolated三指令DISABLED场景denied/corrupt均确认实际CBOAck首次错误并完成CMO及两条Load比较，0 error/0 fatal；不覆盖Store或flushSb排空。原四指令恢复场景继续BLOCKED，详见`AI_DOC/analysis/testcase_flow/memblock_cmo_cboack_fault_sq_reentry_flush_stall_20261111.md`。该阻塞不通过RM跳过检查规避，也不登记为整例通过。
