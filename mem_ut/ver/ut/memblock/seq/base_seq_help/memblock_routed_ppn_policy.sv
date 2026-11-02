`ifndef MEMBLOCK_ROUTED_PPN_POLICY__SV
`define MEMBLOCK_ROUTED_PPN_POLICY__SV

class memblock_routed_ppn_policy extends uvm_object;
    `uvm_object_utils(memblock_routed_ppn_policy)
    `include "memblock_routed_ppn_profile_v2.svh"

    typedef struct packed {
        bit [48:0] first;
        bit [48:0] limit;
        bit c;
    } c_span_t;
    typedef struct packed {
        bit [63:0] first;
        bit [63:0] slots;
        int signed region_id;
    } candidate_span_t;

    // 中文注释：每个策略实例仅保有configure指定版本的值型属性副本。
    // configure前清空；无static快照缓存，不跨reset复用旧model状态。
    local c_span_t c_spans[$];
    local bit configured;
    longint unsigned pma_generation;

    function new(string name = "memblock_routed_ppn_policy");
        super.new(name);
        configured = 1'b0;
        pma_generation = 0;
    endfunction:new

    // 抽象职责：冻结一次地址构造所需的PMA属性，合并相邻同c区间，
    // 使完整shape可跨越权限不同而c相同的边界，不修改原PMA表。
    function bit configure(input memblock_pma_pmp_model model,
                           input longint unsigned generation,
                           input bit debug);
        memblock_pma_attribute_span_t attributes[$];
        c_span_t span;
        int last_index;
        configured = 1'b0;
        c_spans.delete();
        pma_generation = 0;
        if (model == null ||
            !model.read_pma_attribute_spans(generation, debug, attributes))
            return 1'b0;
        foreach (attributes[idx]) begin
            span.first = attributes[idx].first;
            span.limit = attributes[idx].limit;
            span.c = attributes[idx].c;
            last_index = c_spans.size()-1;
            if (last_index >= 0 && c_spans[last_index].limit == span.first &&
                c_spans[last_index].c == span.c)
                c_spans[last_index].limit = span.limit;
            else c_spans.push_back(span);
        end
        pma_generation = generation;
        configured = 1'b1;
        return 1'b1;
    endfunction:configure

    static function bit get_external_region(input int unsigned index,
                                            output bit [48:0] first,
                                            output bit [48:0] limit);
        first = 0;
        limit = 0;
        case (index)
            0: begin first = MEMBLOCK_ROUTE_FIRST_0; limit = MEMBLOCK_ROUTE_LIMIT_0; end
            1: begin first = MEMBLOCK_ROUTE_FIRST_1; limit = MEMBLOCK_ROUTE_LIMIT_1; end
            2: begin first = MEMBLOCK_ROUTE_FIRST_2; limit = MEMBLOCK_ROUTE_LIMIT_2; end
            3: begin first = MEMBLOCK_ROUTE_FIRST_3; limit = MEMBLOCK_ROUTE_LIMIT_3; end
            default: return 1'b0;
        endcase
        return 1'b1;
    endfunction:get_external_region

    static function bit get_region(input int signed region_id,
                                   output bit [48:0] first,
                                   output bit [48:0] limit);
        first = 0;
        limit = 0;
        case (region_id)
            0: begin first = 0; limit = 49'h1_0000_0000_0000; end
            1: begin first = MEMBLOCK_REGION_DRAM_FIRST; limit = MEMBLOCK_REGION_DRAM_LIMIT; end
            2: begin first = MEMBLOCK_REGION_APLIC_M_FIRST; limit = MEMBLOCK_REGION_APLIC_M_LIMIT; end
            3: begin first = MEMBLOCK_REGION_APLIC_S_FIRST; limit = MEMBLOCK_REGION_APLIC_S_LIMIT; end
            4: begin first = MEMBLOCK_REGION_UART_FIRST; limit = MEMBLOCK_REGION_UART_LIMIT; end
            5: begin first = MEMBLOCK_REGION_TIMER_FIRST; limit = MEMBLOCK_REGION_TIMER_LIMIT; end
            6: begin first = MEMBLOCK_REGION_PLIC_FIRST; limit = MEMBLOCK_REGION_PLIC_LIMIT; end
            default: return 1'b0;
        endcase
        return 1'b1;
    endfunction:get_region

    // 抽象职责：用宽边界验证完整字节区间能经out1送到TB responder。
    // DCacheCtrl虽可内部响应，具有错误注入副作用，故不属于本集合。
    static function bit is_external_span(input bit [63:0] first,
                                         input bit [63:0] limit);
        bit [48:0] route_first;
        bit [48:0] route_limit;
        if (first >= limit || limit > 64'h0001_0000_0000_0000) return 1'b0;
        for (int unsigned idx = 0; idx < 4; idx++) begin
            if (get_external_region(idx, route_first, route_limit) &&
                first >= {15'b0, route_first} && limit <= {15'b0, route_limit}) return 1'b1;
        end
        return 1'b0;
    endfunction:is_external_span

    function bit query_span_c(input bit [63:0] first,
                              input bit [63:0] limit,
                              output bit cacheable);
        cacheable = 1'b0;
        if (!configured || first >= limit ||
            limit > 64'h0001_0000_0000_0000) return 1'b0;
        foreach (c_spans[idx]) begin
            if (first >= {15'b0, c_spans[idx].first} &&
                limit <= {15'b0, c_spans[idx].limit}) begin
                cacheable = c_spans[idx].c;
                return 1'b1;
            end
        end
        return 1'b0;
    endfunction:query_span_c

    function bit query_c(input bit [63:0] paddr, output bit cacheable);
        cacheable = 1'b0;
        if (paddr >= 64'h0001_0000_0000_0000) return 1'b0;
        return query_span_c(paddr, paddr + 64'd1, cacheable);
    endfunction:query_c

    static function bit valid_shape(input int unsigned shape_shift);
        return shape_shift == 12 || shape_shift == 16 || shape_shift == 21 ||
               shape_shift == 30 || shape_shift == 39;
    endfunction:valid_shape

    // 抽象职责：初选与reuse共用完整shape的路由/属性检查；不把shape
    // 当成scalar访问size。PBMT00保持原c类别，NC/IO不要求PMA同c。
    function bit check_mapping(input bit [43:0] ppn,
                               input int unsigned shape_shift,
                               input bit [1:0] pbmt,
                               input bit target_c);
        bit [63:0] first;
        bit [63:0] limit;
        bit [63:0] shape_bytes;
        bit actual_c;
        if (!configured || !valid_shape(shape_shift) || pbmt == 2'b11)
            return 1'b0;
        shape_bytes = 64'd1 << shape_shift;
        first = {8'b0, ppn, 12'b0};
        // 在宽位检查前不截断PA；清页形状低位只影响页内重组位。
        if (first >= 64'h0001_0000_0000_0000) return 1'b0;
        first &= ~(shape_bytes-1);
        limit = first + shape_bytes;
        if (!is_external_span(first, limit)) return 1'b0;
        if (pbmt == 2'b00)
            return query_span_c(first, limit, actual_c) && actual_c == target_c;
        return 1'b1;
    endfunction:check_mapping

    // 抽象职责：先等权选有候选的命名窗口，再在该窗口合法shape槽中均匀
    // 抽样。窗口0保留全部外部地址；无候选返回false供owner一次4K降级。
    // selected_ppn总是shape对齐canonical，不包含NAPOT标记。
    function bit select_ppn(input bit [43:0] candidate_ppn,
                            input bit [43:0] anchor_resolved_ppn,
                            input int unsigned shape_shift,
                            input bit [1:0] pbmt,
                            input int unsigned random_wt,
                            input int signed fixed_id,
                            output bit [43:0] selected_ppn,
                            output int signed region_id);
        candidate_span_t candidates[$];
        candidate_span_t candidate;
        int signed available_regions[$];
        bit [48:0] region_first;
        bit [48:0] region_limit;
        bit [48:0] route_first;
        bit [48:0] route_limit;
        bit [63:0] first;
        bit [63:0] limit;
        bit [63:0] aligned_first;
        bit [63:0] aligned_limit;
        bit [63:0] shape_bytes;
        bit [63:0] candidate_pa;
        bit [63:0] anchor_pa;
        bit [63:0] total_slots;
        bit [63:0] slot_pick;
        bit [63:0] selected_pa;
        int unsigned random_pick;
        int unsigned region_pick;
        int signed selected_region;
        int unsigned count_before;
        int unsigned span_count;
        bit target_c;
        bit constrain_c;
        bit candidate_ok;

        selected_ppn = '0;
        region_id = -1;
        if (!configured || !valid_shape(shape_shift) || pbmt == 2'b11 ||
            random_wt > 100 || fixed_id < -1 || fixed_id > 6) return 1'b0;
        shape_bytes = 64'd1 << shape_shift;
        candidate_pa = {8'b0, candidate_ppn, 12'b0};
        anchor_pa = {8'b0, anchor_resolved_ppn, 12'b0};
        target_c = 1'b0;
        constrain_c = pbmt == 2'b00 &&
                      query_c(anchor_pa, target_c);
        candidate_ok = check_mapping(candidate_ppn, shape_shift, pbmt,
                                     target_c);
        // 超48bit anchor不能截断来创造c类别，合法性仍由route限制。
        if (pbmt == 2'b00 && !constrain_c) candidate_ok = 1'b0;
        if (fixed_id == -1 && candidate_ok) begin
            if (pbmt == 2'b00 && target_c) begin
                selected_pa = candidate_pa & ~(shape_bytes-1);
                selected_ppn = selected_pa[55:12];
                region_id = 0;
                return 1'b1;
            end
            random_pick = 100;
            if (random_wt != 0 && random_wt != 100 &&
                !std::randomize(random_pick) with { random_pick < 100; })
                return 1'b0;
            if (random_wt == 0 || (random_wt != 100 && random_pick >= random_wt)) begin
                selected_pa = candidate_pa & ~(shape_bytes-1);
                selected_ppn = selected_pa[55:12];
                region_id = 0;
                return 1'b1;
            end
        end
        for (int signed id = 0; id <= 6; id++) begin
            if (fixed_id >= 0 && id != fixed_id) continue;
            if (!get_region(id, region_first, region_limit)) continue;
            count_before = candidates.size();
            for (int unsigned route = 0; route < 4; route++) begin
                if (!get_external_region(route, route_first, route_limit)) continue;
                span_count = constrain_c ? c_spans.size() : 1;
                for (int unsigned idx = 0; idx < span_count; idx++) begin
                    if (constrain_c && c_spans[idx].c != target_c) continue;
                    first = region_first > route_first ? {15'b0, region_first} : {15'b0, route_first};
                    limit = region_limit < route_limit ? {15'b0, region_limit} : {15'b0, route_limit};
                    if (constrain_c) begin
                        if (first < {15'b0, c_spans[idx].first}) first = {15'b0, c_spans[idx].first};
                        if (limit > {15'b0, c_spans[idx].limit}) limit = {15'b0, c_spans[idx].limit};
                    end
                    if (first >= limit) continue;
                    aligned_first = (first + shape_bytes-1) & ~(shape_bytes-1);
                    aligned_limit = limit & ~(shape_bytes-1);
                    if (aligned_first >= aligned_limit) continue;
                    candidate.first = aligned_first;
                    candidate.slots = (aligned_limit-aligned_first) >> shape_shift;
                    candidate.region_id = id;
                    candidates.push_back(candidate);
                end
            end
            if (candidates.size() != count_before) available_regions.push_back(id);
        end
        if (available_regions.size() == 0) return 1'b0;
        region_pick = 0;
        if (available_regions.size() > 1 && !std::randomize(region_pick) with {
                region_pick < available_regions.size();
            }) return 1'b0;
        selected_region = available_regions[region_pick];
        total_slots = 0;
        foreach (candidates[idx]) begin
            if (candidates[idx].region_id == selected_region)
                total_slots += candidates[idx].slots;
        end
        slot_pick = 0;
        if (total_slots > 1 && !std::randomize(slot_pick) with {
                slot_pick < total_slots;
            }) return 1'b0;
        foreach (candidates[idx]) begin
            if (candidates[idx].region_id != selected_region) continue;
            if (slot_pick < candidates[idx].slots) begin
                selected_pa = candidates[idx].first + (slot_pick << shape_shift);
                selected_ppn = selected_pa[55:12];
                region_id = selected_region;
                return check_mapping(selected_ppn, shape_shift,
                                     constrain_c ? pbmt : 2'b01, target_c);
            end
            slot_pick -= candidates[idx].slots;
        end
        return 1'b0;
    endfunction:select_ppn
endclass:memblock_routed_ppn_policy
`endif
