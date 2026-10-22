from toffee import Bundle, Signals


class ControlBundle(Bundle):
    """Logical top-level control interface exposed to Toffee agents."""

    reset, backend_reset, l2_flush_enable, l2_flush_done = Signals(4)
    outer_l2_flush_enable, backend_l2_flush_done = Signals(2)


class IdleBundle(Bundle):
    """The externally visible producer valids required to remain idle at reset."""

    uncache_a_valid = Signals(1)
    frontend_instr_uncache_in_d_valid = Signals(1)
    frontend_instr_uncache_out_a_valid = Signals(1)
    frontend_icachectrl_in_d_valid = Signals(1)
    frontend_icachectrl_out_a_valid = Signals(1)
    frontend_icache_in_d_valid = Signals(1)
    frontend_icache_out_a_valid = Signals(1)
    ptw_to_l2_a_valid = Signals(1)
    dcache_a_valid = Signals(1)
    dcache_c_valid = Signals(1)
    dcache_e_valid = Signals(1)
    scalar_load_writeback_0_valid = Signals(1)
    scalar_load_writeback_1_valid = Signals(1)
    scalar_load_writeback_2_valid = Signals(1)
    scalar_store_address_0_valid = Signals(1)
    scalar_store_address_1_valid = Signals(1)
    scalar_store_data_0_valid = Signals(1)
    scalar_store_data_1_valid = Signals(1)
    vector_writeback_0_valid = Signals(1)
    vector_writeback_1_valid = Signals(1)
