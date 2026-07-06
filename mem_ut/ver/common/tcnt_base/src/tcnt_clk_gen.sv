`ifndef TCNT_CLK_GEN__SV
`define TCNT_CLK_GEN__SV

//======================================================================================
// clk gen
//======================================================================================
`define CLK_GEN(clk,frequence)\
    initial begin \
        int duty_cycle; \
        bit init_clk_value; \
        int init_clk_delay; \
        bit jitter_direction; \
        int jitter_pct; \
        bit jitter_on_high_or_low; \
        realtime high_period; \
        realtime low_period; \
        realtime jitter_current; \
`ifdef DV_CLK_GEN_NO_JITTER \
       duty_cycle = 50; \
`else \
        void'(std::randomize(duty_cycle) with {duty_cycle dist {50:/80, 40:/10, 60:/10};}); \
`endif \
        void'(std::randomize(init_clk_value) with {init_clk_value inside {[1'b0:1'b1]};}); \
        void'(std::randomize(init_clk_delay) with {init_clk_delay inside {[1:10]};}); \
        void'(std::randomize(jitter_direction) with {jitter_direction inside {[1'b0:1'b1]};}); \
`ifdef DV_CLK_GEN_NO_JITTER \
        jitter_pct = 0; \
`else \
        void'(std::randomize(jitter_pct) with {jitter_pct dist {0:/90, 5:/10};}); \
`endif \
        high_period = 1000.0*duty_cycle/(100.0*realtime'(frequence)); \
        low_period  = 1000.0*(100-duty_cycle)/(100.0*realtime'(frequence)); \
        if(jitter_pct != 0)begin \
            jitter_current = (jitter_direction == 1'b1) ? realtime'(jitter_pct)*1000.0/(100.0*realtime'(frequence)) : (-1)*realtime'(jitter_pct)*1000.0/(100.0*realtime'(frequence)); \
        end \
        else begin \
            jitter_current = 0; \
        end \
        clk = init_clk_value; \
        tcnt_realtime::delay_ns(realtime'(init_clk_delay)/10.0); \
        if(init_clk_value == 1'b0)begin \
            clk = ~clk; \
        end \
        forever begin\
            void'(std::randomize(jitter_on_high_or_low)); \
            if(jitter_on_high_or_low == 1'b1)begin \
                tcnt_realtime::delay_ns(high_period + jitter_current); \
                clk = ~clk; \
                tcnt_realtime::delay_ns(low_period); \
                clk = ~clk; \
            end \
            else begin \
                tcnt_realtime::delay_ns(high_period); \
                clk = ~clk; \
                tcnt_realtime::delay_ns(low_period + jitter_current); \
                clk = ~clk; \
            end \
        end\
    end

//======================================================================================
// rst gen
//======================================================================================
`define RST_GEN(rst_n,delay)\
    initial begin \
        bit init_val; \
        int high_delay; \
        void'(std::randomize(init_val)); \
        void'(std::randomize(high_delay) with {high_delay inside {[1:delay]};}); \
        if(init_val == 1'b0)begin \
            rst_n = 0;\
            tcnt_realtime::delay_ns(delay); \
            rst_n = 1;\
        end \
        else begin \
            rst_n = 1;\
            tcnt_realtime::delay_ns(high_delay); \
            rst_n = 0;\
            tcnt_realtime::delay_ns(delay); \
            rst_n = 1;\
        end \
    end

`endif

