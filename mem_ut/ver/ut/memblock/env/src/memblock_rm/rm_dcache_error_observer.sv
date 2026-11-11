`ifndef RM_DCACHE_ERROR_OBSERVER_SV
`define RM_DCACHE_ERROR_OBSERVER_SV
// 仅维护真实消费的错误来源身份，不保存数据、不查tag、不预测cache命中或替换。
class rm_dcache_error_observer;
    typedef struct packed {
        bit valid;
        bit proven;
        bit [1:0] raw;
        longint unsigned token;
        longint unsigned sample;
        bit [63:0] line_addr;
    } origin_t;
    typedef struct packed {
        bit valid;
        longint unsigned token;
        bit [63:0] line_addr;
        longint unsigned accept_sample;
        longint unsigned generation;
        int unsigned fired_beats;
        int unsigned source_id;
    } transport_t;
    typedef struct packed {
        bit valid;
        memblock_uid_t uid;
        int unsigned epoch;
        bit [8:0] rob;
        bit [7:0] lq;
    } owner_t;
    typedef struct packed {
        bit valid;
        bit proven;
        bit [1:0] raw;
        owner_t owner;
        // Load每项贡献先映射再合并；总denied不能抹掉另一来源的corrupt-only hardwareError。
        bit expected_load_af;
        bit expected_load_he;
        bit is_prefetch;
        bit [63:0] paddr;
        bit [15:0] byte_mask;
        // 对原子：state6消费sample与其前2拍顶层CSR输入sample分开保存。
        // top输入历史是独立答案，实际atomics enable仅作交叉核对。
        longint unsigned control_input_sample;
        // 原子来源诊断：Main寄存输入、Unit接受、state6控制分别留证。
        longint unsigned main_response_sample;
        bit [7:0] atomic_proof;
        bit actual_control_enable;
        bit [1:0] actual_raw;
        origin_t atomic_slot_origin;
        origin_t atomic_refill_origin;
        longint unsigned data_sample;
        longint unsigned sample;
    } consumption_t;
    typedef struct packed {
        bit valid;
        owner_t owner;
        origin_t direct;
        origin_t mshr;
        longint unsigned sample;
    } forward_t;
    typedef bit [63:0] owner_key_t;
    // TL source当前占用代次只在真实A接受更新；旧slot/流水引用保存独立origin版本。
    transport_t transport_by_source[int unsigned];
    origin_t origin_by_version[longint unsigned];
    // 版本只被真实slot、延迟写或S0旁路引用；S1之后复制纯值来源，无需保留无限历史。
    int unsigned version_refcount[longint unsigned];
    longint unsigned committed_version[bit [31:0]];
    longint unsigned next_version;
    // 物理读口和way上界；记录容器允许16way，实际connect仅当前V2的4way。
    // 每拍只扫描固定端口/way，不扫描指令或历史来源全集。
    bit read_pending_valid[CMO_PATH_ERROR_READ_PORTS];
    bit [15:0] read_pending_idx[CMO_PATH_ERROR_READ_PORTS];
    longint unsigned read_bypass_version[CMO_PATH_ERROR_READ_PORTS][16];
    bit previous_write_valid;
    bit [15:0] previous_write_idx;
    bit [15:0] previous_write_way;
    longint unsigned previous_write_version;
    origin_t main_first_s1;
    origin_t main_s2_flag;
    origin_t main_s3_flag;
    origin_t main_s3_refill;
    // DCache将source==2的MainPipe原子响应寄存一拍后送AtomicsUnit。
    bit main_atomic_registered_valid;
    origin_t main_atomic_registered,main_atomic_registered_slot,main_atomic_registered_refill;
    origin_t hit_delay1[CMO_PATH_LOAD_PORTS];
    origin_t hit_delay2[CMO_PATH_LOAD_PORTS];
    forward_t forward_pending[CMO_PATH_LOAD_PORTS];
    consumption_t load_s2_pending[CMO_PATH_LOAD_PORTS];
    owner_t atomic_owner;
    consumption_t atomic_response_pending;
    // 完成fact按owner索引；每个owner只保留最后成功attempt，重放不会形成终态fact。
    consumption_t load_fact_by_owner[owner_key_t];
    consumption_t atomic_fact_by_owner[owner_key_t];
    consumption_t first_split_fact_by_owner[owner_key_t];
    consumption_t split_accumulator_by_owner[owner_key_t];
    longint unsigned reset_generation;
    longint unsigned watermark;
    bit initialized;

    function new();
        initialized=0;
        next_version=1;
        watermark=0;
        clear_active();
    endfunction

    function void acquire_version(input longint unsigned version);
        if(version==0) return;
        if(!version_refcount.exists(version)) version_refcount[version]=0;
        version_refcount[version]++;
    endfunction
    function void release_version(input longint unsigned version);
        if(version==0 || !version_refcount.exists(version)) return;
        if(version_refcount[version]>1) version_refcount[version]--;
        else begin
            version_refcount.delete(version);
            origin_by_version.delete(version);
        end
    endfunction
    // 只在root完成owner全部错误比较后调用；不触碰cache slot或其它仍活跃指令的来源。
    function void release_completed_owner(input memblock_uid_t uid,input int unsigned epoch);
        load_fact_by_owner.delete(owner_key(uid,epoch));
        atomic_fact_by_owner.delete(owner_key(uid,epoch));
        first_split_fact_by_owner.delete(owner_key(uid,epoch));
        split_accumulator_by_owner.delete(owner_key(uid,epoch));
    endfunction

    function automatic bit [31:0] slot_key(input bit [15:0] idx,input int unsigned way);
        return {idx,16'(way)};
    endfunction
    function automatic owner_key_t owner_key(input memblock_uid_t uid,input int unsigned epoch);
        return {epoch[31:0],uid[31:0]};
    endfunction

    // reset结束物理slot/源/流水与当前owner查询索引；独立L2不可变历史保留旧generation诊断。
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

    // 只读动态身份在接受拍冻结；不会创建epoch，也不会由PA或最近一条指令猜owner。
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

    function automatic origin_t unknown_origin(input longint unsigned sample);
        origin_t result;
        result='0;
        result.sample=sample;
        return result;
    endfunction
    function automatic origin_t clean_origin(input longint unsigned sample);
        origin_t result;
        result='0;
        result.valid=1;
        result.proven=1;
        result.sample=sample;
        return result;
    endfunction

    // 独立L2响应记录是raw期望唯一来源；实际阵列raw只用于核对。
    // MSHR/refill只允许读取当前cutoff之前真实fire的beat，并检查物理半行覆盖。
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

    // direct广播使用真实valid offer，允许ready=0；期望来自独立冻结token，不能引用未来fire。
    // 关键词与已fire beat数共同确定当前物理半行，source/generation/覆盖任一不符均不proven。
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

    // 抽象职责：逐beat核对真实L1预取响应；不创建UID，不等待软件预取WB或补造请求。
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

    // 抽象职责：复用Load终态身份，证明当前软件预取资格；不将数据错误映射成架构异常。
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

    static function bit prefetch_suppression_matches(input bit [23:0] actual_exception);
        return !actual_exception[5] && !actual_exception[19];
    endfunction

    // 非UVM component使用package报告入口，避免宏隐式引用不存在的this.uvm_report_*。
    function void fail(input string report_id,input string detail);
        uvm_pkg::uvm_report_error(report_id,detail,UVM_NONE);
    endfunction

    // 同拍producer调度顺序不改变身份：A接受先登记sample，延后消费时按不可变accept key补齐token。
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

    // MemBlock将顶层csrCtrl延迟2拍后送AtomicsUnit；state6门控消费对应sample-2的独立输入。
    // actual_enable只用于检查连接/延迟，不能替代顶层monitor采集的期望值。
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

    function void check_raw(input string point,input origin_t expected,input bit [1:0] actual);
        if(expected.proven && expected.raw!=actual)
            fail("RM_D_ERROR_RAW",$sformatf("%s token=%0d sample=%0d expected=%b actual=%b",
                point,expected.token,expected.sample,expected.raw,actual));
    endfunction

    // 真实S1 hit-way选择S0各way已锁存旁路；无旁路时读取当前已提交slot版本。
    // MainPipe调用者仅在首S1拍保存此结果，LoadPipe固定流水在每次S1消费时选择。
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

    // 抽象职责：按全局sample消费一份内部被动批次，冻结独立来源并发布owner-bound终态事实。
    // 每阶段先读取本拍前状态，再更新下一拍状态；不依赖events入队顺序。
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

    // 返回0仅表示producer水位尚未覆盖access_sample；返回1且proven=0为确定缺证据，RM必须FAIL。
    // 查询只返回已由真实完成attempt证明的错误输入；缺证据不能默认为CLEAN。
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
    // 跨页首段仅使用真实mabuf返回消费记录；调用者还需以现有readonly context证明首页路径/未发后页。
    // PA及mask必须覆盖调用者已独立翻译的prefix，不能仅owner+error授权无关物理line。
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

    // Load异常按每个实际有效贡献独立映射后OR；不能对总raw统一执行denied优先。
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

endclass

// 仅software-only测试调用；测试前重置独立L2历史，禁止在真实DUT运行期调用。
// 提供class入口以便env之后编译的soft testcase复用，不依赖sequence->env反向import。
class rm_dcache_error_observer_soft_test;
    static function cmo_path_agent_xaction batch(input longint unsigned sample);
        cmo_path_agent_xaction tr;
        tr=new("d_error_soft_batch");
        tr.sample_seq=sample;
        tr.reset_generation=0;
        tr.reset_active=0;
        return tr;
    endfunction

    // 抽象职责：证明Main到Unit必须严格相隔一拍，旁路、空拍、重复及非原子响应不能授权。
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

    // 抽象职责：验证预取错误传输与软件同步抑制相互独立，缺失身份不能视为CLEAN。
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

    static function bit run(output string detail);
        rm_dcache_error_observer observer;
        rm_dcache_error_observer::transport_t source,bad_source;
        rm_dcache_error_observer::origin_t origin,corrupt_origin,clean;
        rm_dcache_error_observer::owner_t old_owner,new_owner;
        mem_access_base_sequence::l2_d_error_line_record_t choice;
        cmo_path_agent_xaction tr;
        cmo_path_probe_t event_item,offer;
        mem_access_base_sequence::mem_addr_t line;
        longint unsigned token,control_sample;
        bit needs_candidate,proven,expected_af,expected_he;
        bit [1:0] raw;
        string query_detail;
        longint unsigned csr_input_sample;
        memblock_sync_pkg::memblock_cache_error_control_t csr_control;
        detail="";
        line=48'h0000_8000_0000;
        observer=new();
        mem_access_base_sequence::reset_l2_line_history();
        if(!mem_access_base_sequence::lookup_l2_line_choice(line,10,10'd1,2,5'd1,1,
            mem_access_base_sequence::L2_D_ERROR_DENIED,needs_candidate,choice)) begin
            detail="建立source测试首次行选择失败"; return 0;
        end
        token=mem_access_base_sequence::begin_l2_response_history(line,10,10'd1,0,2,4'd6,
            5'd1,1,1,1,0);
        if(token==0) begin detail="建立source测试token失败"; return 0; end
        source='0;
        source.valid=1;
        source.source_id=1;
        source.token=token;
        source.line_addr=64'(line);
        source.accept_sample=10;
        source.generation=1;
        offer='0;
        offer.valid=1;
        offer.kind=CMO_PATH_DCACHE_TL_D_OFFER;
        offer.id=1;
        offer.op=5;
        offer.raw=2'b11;
        // valid=1、ready=0且尚无D.fire，direct真实消费仍可独立证明。
        origin=observer.resolve_offer(source,offer,64'(line),12);
        if(!origin.proven||origin.raw!=2'b11) begin detail="direct valid未ready来源证明失败"; return 0; end
        offer.id=2;
        origin=observer.resolve_offer(source,offer,64'(line),12);
        if(origin.proven) begin
            detail="错误source被当成direct证据"; return 0;
        end
        offer.id=1;
        bad_source=source;
        bad_source.generation=2;
        origin=observer.resolve_offer(bad_source,offer,64'(line),12);
        if(origin.proven) begin
            detail="错误source generation被当成direct证据"; return 0;
        end
        origin=observer.resolve_offer(source,offer,64'(line)+32,12);
        if(origin.proven) begin
            detail="direct错误物理半行被当成证据"; return 0;
        end
        origin=observer.resolve_delivered(source,64'(line),12,0);
        if(origin.proven) begin detail="MSHR错误地使用尚未fire的offer"; return 0; end
        mem_access_base_sequence::publish_l2_response_beat_history(token,0,15);
        origin=observer.resolve_delivered(source,64'(line),14,0);
        if(origin.proven) begin detail="未来D.fire污染较早消费"; return 0; end
        origin=observer.resolve_delivered(source,64'(line),15,0);
        if(!origin.proven||origin.raw!=2'b11) begin detail="首beat已fire来源遗漏"; return 0; end
        origin=observer.resolve_delivered(source,64'(line)+32,15,0);
        if(origin.proven) begin
            detail="MSHR使用未送达的另一物理半行"; return 0;
        end
        origin=observer.resolve_delivered(source,64'(line),15,1);
        if(origin.proven) begin
            detail="refill在末beat前被错误证明"; return 0;
        end
        mem_access_base_sequence::publish_l2_response_beat_history(token,1,20);
        origin=observer.resolve_delivered(source,64'(line),20,1);
        if(!origin.proven||origin.raw!=2'b11) begin detail="完整refill独立来源失败"; return 0; end
        mem_access_base_sequence::complete_l2_response_history(token);
        // S0 current写优先于previous s1写；未旁路的读在真实S1采用当前committed来源。
        observer.initialized=1;
        observer.reset_generation=0;
        observer.watermark=29;
        corrupt_origin=origin;
        corrupt_origin.raw=2'b01;
        clean=observer.clean_origin(29);
        observer.origin_by_version[1]=origin;
        observer.origin_by_version[2]=corrupt_origin;
        observer.acquire_version(2);
        observer.acquire_version(2);
        observer.origin_by_version[3]=clean;
        observer.next_version=4;
        observer.previous_write_valid=1;
        observer.previous_write_idx=7;
        observer.previous_write_way=1;
        observer.previous_write_version=2;
        observer.main_s3_refill=origin;
        tr=batch(30);
        event_item='0;
        event_item.valid=1;
        event_item.kind=CMO_PATH_ERROR_ARRAY_WRITE;
        event_item.idx=7;
        event_item.way=1;
        event_item.raw=2'b11;
        event_item.addr=64'(line);
        event_item.id=1;
        event_item.state=1;
        tr.events.push_back(event_item);
        event_item='0;
        event_item.valid=1;
        event_item.kind=CMO_PATH_ERROR_ARRAY_READ;
        event_item.idx=7;
        tr.events.push_back(event_item);
        observer.observe_batch(tr);
        origin=observer.select_read_origin(0,1,31);
        if(!origin.proven||origin.raw!=2'b11) begin detail="S0 current写未优先于s1写旁路"; return 0; end
        observer.committed_version[observer.slot_key(8,0)]=3;
        tr=batch(32);
        event_item.idx=8;
        tr.events.delete();
        tr.events.push_back(event_item);
        observer.observe_batch(tr);
        observer.committed_version[observer.slot_key(8,0)]=2;
        origin=observer.select_read_origin(0,1,33);
        if(!origin.proven||origin.raw!=2'b01) begin detail="无旁路S1未采用当前committed来源"; return 0; end
        // MainPipe首S1冻结后，即便array变化也必须跨stall保持。
        observer.read_pending_valid[3]=1;
        observer.read_pending_idx[3]=8;
        tr=batch(33);
        event_item='0;
        event_item.valid=1;
        event_item.kind=CMO_PATH_MAIN_PIPE_S1;
        event_item.way=1;
        event_item.qual=1;
        event_item.actual_by_way=1;
        tr.events.push_back(event_item);
        observer.observe_batch(tr);
        observer.committed_version[observer.slot_key(8,0)]=3;
        tr=batch(34);
        event_item.qual=2;
        tr.events.push_back(event_item);
        observer.observe_batch(tr);
        if(observer.main_s2_flag.raw!=2'b01||!observer.main_s2_flag.proven) begin
            detail="MainPipe首S1来源未跨stall保持"; return 0;
        end
        // 原子最终错误必须OR refill与slot两项，miss/replay不能提前建立终态。
        observer.transport_by_source[1]=source;
        observer.atomic_owner='0;
        observer.atomic_owner.valid=1;
        observer.atomic_owner.uid=13;
        observer.atomic_owner.epoch=3;
        observer.atomic_owner.rob=9'h23;
        tr=batch(35);
        event_item='0;
        event_item.valid=1;
        event_item.kind=CMO_PATH_MAIN_PIPE_S2;
        event_item.qual=1;
        event_item.raw=2'b01;
        tr.events.push_back(event_item);
        event_item.kind=CMO_PATH_MAIN_PIPE_REFILL;
        event_item.raw=2'b11;
        event_item.id=1;
        event_item.addr=64'(line);
        tr.events.push_back(event_item);
        observer.observe_batch(tr);
        tr=batch(36);
        event_item='0;
        event_item.valid=1;
        event_item.kind=CMO_PATH_MAIN_ATOMIC_RESPONSE;
        event_item.aux=2;
        event_item.state=3;
        event_item.raw=2'b11;
        event_item.raw_extra=2'b01;
        tr.events.push_back(event_item);
        observer.observe_batch(tr);
        if(!observer.query(13,3,9'h23,0,36,1,raw,control_sample,proven,query_detail)||proven) begin
            detail="原子在真实latch控制拍之前提前形成终态"; return 0;
        end
        // DCache实际寄存Main响应一拍后，AtomicsUnit才接受最终响应。
        tr=batch(37);
        event_item.kind=CMO_PATH_ATOMIC_RESPONSE;
        event_item.state=0;
        event_item.rob=9'h23;
        tr.events.push_back(event_item);
        observer.observe_batch(tr);
        memblock_sync_pkg::publish_cache_error_control(36,0,1'b1);
        // 顶层当拍已经变为0，但内部state6仍使用前2拍输入1。
        memblock_sync_pkg::publish_cache_error_control(38,0,1'b0);
        tr=batch(38);
        event_item.qual=1;
        event_item.kind=CMO_PATH_ATOMIC_LATCH_CONTROL;
        event_item.raw=2'b11;
        tr.events.push_back(event_item);
        observer.observe_batch(tr);
        if(!observer.query(13,3,9'h23,0,38,1,raw,control_sample,proven,query_detail)||
            !proven||raw!=2'b11||control_sample!=36) begin detail="原子refill与slot独立贡献未OR"; return 0; end
        if(!memblock_sync_pkg::read_cache_error_control(control_sample,csr_control)||!csr_control.enable)
            begin detail="原子没有使用state6前2拍的顶层CSR1";return 0;end
        // 相反方向：顶层当拍变1，state6仍应读取前2拍的0；实际gate仅作交叉检验。
        memblock_sync_pkg::publish_cache_error_control(50,0,1'b0);
        memblock_sync_pkg::publish_cache_error_control(52,0,1'b1);
        if(!observer.resolve_atomic_control(52,0,csr_input_sample)||csr_input_sample!=50 ||
            !memblock_sync_pkg::read_cache_error_control(csr_input_sample,csr_control)||csr_control.enable)
            begin detail="原子CSR0到1变化偏移两拍证明失败";return 0;end
        if(observer.resolve_atomic_control(2,0,csr_input_sample)||csr_input_sample!=0)
            begin detail="原子sample不足2发生下溢或错误授权";return 0;end
        observer.atomic_owner.uid=14;
        observer.atomic_owner.rob=9'h24;
        tr=batch(39);
        event_item.kind=CMO_PATH_ATOMIC_RESPONSE;
        event_item.state=3;
        event_item.rob=9'h24;
        tr.events.push_back(event_item);
        observer.observe_batch(tr);
        if(!observer.query(14,3,9'h24,0,39,1,raw,control_sample,proven,query_detail)||proven) begin
            detail="原子miss/replay被当成最终响应"; return 0;
        end
        // Load direct denied与MSHR corrupt-only应同时给出AF、HE；同拍新S2不能覆盖旧S3。
        old_owner='0;
        old_owner.valid=1;
        old_owner.uid=11;
        old_owner.epoch=3;
        old_owner.rob=9'h21;
        old_owner.lq=8'h12;
        new_owner=old_owner;
        new_owner.uid=12;
        new_owner.rob=9'h22;
        new_owner.lq=8'h13;
        observer.forward_pending[0].valid=1;
        observer.forward_pending[0].owner=old_owner;
        observer.forward_pending[0].direct=observer.clean_origin(39);
        observer.forward_pending[0].direct.raw=2'b11;
        observer.forward_pending[0].mshr=observer.clean_origin(39);
        observer.forward_pending[0].mshr.raw=2'b01;
        tr=batch(40);
        event_item='0;
        event_item.valid=1;
        event_item.kind=CMO_PATH_LOAD_S2;
        event_item.rob=old_owner.rob;
        event_item.lq=old_owner.lq;
        event_item.qual=16'h003f;
        event_item.raw=2'b11;
        event_item.raw_extra=2'b01;
        tr.events.push_back(event_item);
        observer.observe_batch(tr);
        observer.forward_pending[0].valid=1;
        observer.forward_pending[0].owner=new_owner;
        tr=batch(41);
        event_item='0;
        event_item.valid=1;
        event_item.kind=CMO_PATH_LOAD_S3;
        event_item.rob=old_owner.rob;
        event_item.lq=old_owner.lq;
        event_item.qual=16'h001f;
        tr.events.push_back(event_item);
        event_item.kind=CMO_PATH_LOAD_S2;
        event_item.rob=new_owner.rob;
        event_item.lq=new_owner.lq;
        event_item.qual=16'h003c;
        tr.events.push_back(event_item);
        observer.observe_batch(tr);
        if(!observer.query_load_exception_bits(11,3,old_owner.rob,old_owner.lq,41,
            expected_af,expected_he,control_sample,proven,query_detail)||!proven||
            !expected_af||!expected_he||control_sample!=41) begin
            detail="Load独立AF/HE或同拍旧S3/新S2身份失败"; return 0;
        end
        // 首段mabuf返回独立归档，错误PA或错误响应sample不能借同owner授权。
        observer.load_s2_pending[0].valid=1;
        observer.load_s2_pending[0].proven=1;
        observer.load_s2_pending[0].owner=old_owner;
        observer.load_s2_pending[0].expected_load_af=1;
        observer.load_s2_pending[0].data_sample=41;
        observer.load_s2_pending[0].raw=2'b11;
        tr=batch(42);
        event_item='0;
        event_item.valid=1;
        event_item.kind=CMO_PATH_LOAD_S3;
        event_item.rob=old_owner.rob;
        event_item.lq=old_owner.lq;
        event_item.addr=64'(line)+16'hff8;
        event_item.mask=64'hff00;
        event_item.qual=16'h03c7;
        tr.events.push_back(event_item);
        observer.observe_batch(tr);
        if(!observer.query_first_split_error(11,3,old_owner.rob,old_owner.lq,
            64'(line)+16'hffc,4,41,42,44,expected_af,expected_he,control_sample,proven,query_detail)) begin
            // 调用上界44尚未到达水位，属于合法等待；先推进空批次。
            tr=batch(44); observer.observe_batch(tr);
        end
        if(!observer.query_first_split_error(11,3,old_owner.rob,old_owner.lq,
            64'(line)+16'hffc,4,41,42,44,expected_af,expected_he,control_sample,proven,query_detail)||
            !proven||!expected_af) begin detail="首段真实mabuf错误证据遗漏";return 0;end
        if(!observer.query_first_split_error(11,3,old_owner.rob,old_owner.lq,
            64'(line)+16'hfc0,4,41,42,44,expected_af,expected_he,control_sample,proven,query_detail)||
            proven) begin detail="首段未覆盖PA字节被错误授权";return 0;end
        if(!observer.query_first_split_error(11,3,old_owner.rob,old_owner.lq,
            64'(line)+16'hffc,4,40,41,44,expected_af,expected_he,control_sample,proven,query_detail)||
            proven) begin detail="首段错误sample被错误授权";return 0;end
        // 正常两段也必须经mabuf实际返回累积，最终段才形成正常完成来源事实。
        old_owner.uid=15;
        old_owner.rob=9'h25;
        old_owner.lq=8'h15;
        observer.load_s2_pending[0]='0;
        observer.load_s2_pending[0].valid=1;
        observer.load_s2_pending[0].proven=1;
        observer.load_s2_pending[0].owner=old_owner;
        observer.load_s2_pending[0].data_sample=44;
        tr=batch(45);
        event_item='0;
        event_item.valid=1;
        event_item.kind=CMO_PATH_LOAD_S3;
        event_item.rob=old_owner.rob;
        event_item.lq=old_owner.lq;
        event_item.addr=64'(line)+16'hff8;
        event_item.mask=64'hff00;
        event_item.qual=16'h03c7;
        tr.events.push_back(event_item);
        observer.observe_batch(tr);
        if(!observer.query(15,3,old_owner.rob,old_owner.lq,45,0,raw,control_sample,proven,query_detail)||proven) begin
            detail="正常split首段提前产生最终完成事实";return 0;
        end
        observer.load_s2_pending[0].data_sample=45;
        tr=batch(46);
        event_item.addr=64'(line)+16'h1000;
        event_item.mask=64'h000f;
        event_item.qual=16'h07c7;
        tr.events.push_back(event_item);
        observer.observe_batch(tr);
        if(!observer.query(15,3,old_owner.rob,old_owner.lq,46,0,raw,control_sample,proven,query_detail)||
            !proven||raw!=0||control_sample!=46) begin detail="正常split两段来源没有闭环";return 0;end
        if(observer.query(99,3,0,0,47,0,raw,control_sample,proven,query_detail)) begin
            detail="未到水位被误报为证据完备"; return 0;
        end
        if(!observer.query(99,3,0,0,46,0,raw,control_sample,proven,query_detail)||proven) begin
            detail="水位覆盖后的缺证据未明确失败"; return 0;
        end
        if(!test_atomic_registered_transport(query_detail)) begin detail=query_detail;return 0;end
        if(!test_prefetch_error_suppression(query_detail)) begin detail=query_detail;return 0;end
        detail="direct offer/source-generation、已fire边界、逐way旁路、Main首S1保持、原子真实寄存拍/CSR/双贡献及重放、Load独立贡献及首段/正常split通过";
        return 1;
    endfunction
endclass
`endif
