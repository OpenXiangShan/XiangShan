`ifndef SOFT_TEST_PMA_PMP_CSR_FIFO_SEQUENCE__SV
`define SOFT_TEST_PMA_PMP_CSR_FIFO_SEQUENCE__SV

// Software-only test: constructs immutable monitor facts, never drives the DUT.
class soft_test_pma_pmp_csr_fifo_sequence extends uvm_sequence;
    `uvm_object_utils(soft_test_pma_pmp_csr_fifo_sequence)
    common_data_transaction data;

    typedef struct {
        longint unsigned produced_count;
        longint unsigned applied_count;
        longint unsigned raw_revision;
        longint unsigned service_revision;
        longint unsigned model_revision;
        longint unsigned pmp_generation;
        longint unsigned pma_generation;
        longint unsigned last_applied_sample;
        int unsigned raw_size;
        int unsigned pmp_generation_history_size;
        int unsigned pma_generation_history_size;
        int unsigned pmp_snapshot_count;
        int unsigned pma_snapshot_count;
        pma_pmp_entry_t pmp_entries[32];
        pma_pmp_entry_t pma_entries[32];
    } replay_state_t;

    function new(string name="soft_test_pma_pmp_csr_fifo_sequence"); super.new(name); endfunction

    function void check(input bit ok, input string message);
        if (!ok) `uvm_fatal("PMA_PMP_FIFO_TEST", message)
    endfunction

    function void push(input longint unsigned sample, input bit [11:0] addr,
                       input bit [63:0] value);
        memblock_sync_pkg::dispatch_raw_pma_pmp_csr_write_t item;
        make_item(sample, addr, value, item);
        memblock_sync_pkg::push_raw_pma_pmp_csr_write(item);
    endfunction

    function void make_item(
        input longint unsigned sample,
        input bit [11:0] addr,
        input bit [63:0] value,
        output memblock_sync_pkg::dispatch_raw_pma_pmp_csr_write_t item
    );
        item = memblock_sync_pkg::make_empty_raw_pma_pmp_csr_write();
        item.valid = 1'b1;
        item.sample_seq = sample;
        item.sample_time = 1;
        item.addr = addr;
        item.data = value;
        item.reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
    endfunction

    function void service_at(input longint unsigned sample);
        memblock_sync_pkg::lifecycle_event_published_seq = sample;
        data.service_csr_pmp_pma_write_observations();
    endfunction

    function void reset_replay_baseline();
        data.reset_all_tables(1);
        data.main_trans_num = 0;
    endfunction

    function void capture_replay_state(output replay_state_t state);
        state = '{default:'0};
        state.produced_count = memblock_sync_pkg::raw_pma_pmp_csr_write_produced_count;
        state.applied_count = data.pma_pmp_csr_applied_count;
        state.raw_revision = memblock_sync_pkg::get_raw_pma_pmp_csr_write_revision();
        state.service_revision = data.pma_pmp_csr_replay_service_revision;
        state.model_revision = data.pma_pmp_model.get_csr_replay_revision();
        state.pmp_generation = data.pma_pmp_model.pmp_generation;
        state.pma_generation = data.pma_pmp_model.pma_generation;
        state.last_applied_sample = data.pma_pmp_last_applied_csr_sample;
        state.raw_size = memblock_sync_pkg::raw_pma_pmp_csr_write_size();
        state.pmp_generation_history_size = data.pma_pmp_model.pmp_generation_history.size();
        state.pma_generation_history_size = data.pma_pmp_model.pma_generation_history.size();
        state.pmp_snapshot_count = data.pma_pmp_model.pmp_history.num();
        state.pma_snapshot_count = data.pma_pmp_model.pma_history.num();
        for (int unsigned idx = 0; idx < 32; idx++) begin
            check(data.pma_pmp_model.read_entry(1'b0, idx, state.pmp_entries[idx]),
                  $sformatf("failed to snapshot PMP%0d", idx));
            check(data.pma_pmp_model.read_entry(1'b1, idx, state.pma_entries[idx]),
                  $sformatf("failed to snapshot PMA%0d", idx));
        end
    endfunction

    function bit replay_state_equal(input replay_state_t lhs,
                                    input replay_state_t rhs);
        bit equal;
        equal = lhs.produced_count == rhs.produced_count &&
               lhs.applied_count == rhs.applied_count &&
               lhs.raw_revision == rhs.raw_revision &&
               lhs.service_revision == rhs.service_revision &&
               lhs.model_revision == rhs.model_revision &&
               lhs.pmp_generation == rhs.pmp_generation &&
               lhs.pma_generation == rhs.pma_generation &&
               lhs.last_applied_sample == rhs.last_applied_sample &&
               lhs.raw_size == rhs.raw_size &&
               lhs.pmp_generation_history_size == rhs.pmp_generation_history_size &&
               lhs.pma_generation_history_size == rhs.pma_generation_history_size &&
               lhs.pmp_snapshot_count == rhs.pmp_snapshot_count &&
               lhs.pma_snapshot_count == rhs.pma_snapshot_count;
        for (int unsigned idx = 0; idx < 32; idx++) begin
            equal &= lhs.pmp_entries[idx] == rhs.pmp_entries[idx] &&
                     lhs.pma_entries[idx] == rhs.pma_entries[idx];
        end
        return equal;
    endfunction

    function void check_no_shared_mutation(input replay_state_t prior,
                                           input string owner);
        replay_state_t after_state;
        capture_replay_state(after_state);
        check(replay_state_equal(prior, after_state),
              $sformatf("%s partially mutated replay state", owner));
    endfunction

    function void inject_raw_without_producer_report(
        input longint unsigned sample,
        input bit [11:0] addr,
        input bit [63:0] value
    );
        memblock_sync_pkg::dispatch_raw_pma_pmp_csr_write_t item;
        item = memblock_sync_pkg::make_empty_raw_pma_pmp_csr_write();
        item.valid = 1'b1;
        item.sample_seq = sample;
        item.sample_time = 1;
        item.addr = addr;
        item.data = value;
        item.reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
        memblock_sync_pkg::raw_pma_pmp_csr_write_q.push_back(item);
        memblock_sync_pkg::raw_pma_pmp_csr_write_produced_count++;
        memblock_sync_pkg::raw_pma_pmp_csr_write_revision++;
    endfunction

    function void expect_outcome(
        input longint unsigned sample,
        input bit [11:0] addr,
        input bit [63:0] value,
        input pma_pmp_csr_replay_outcome_e expected_outcome
    );
        memblock_pma_pmp_replay_ticket ticket;
        push(sample, addr, value);
        check(data.prepare_pma_pmp_csr_replay(sample + 2, ticket),
              $sformatf("prepare failed addr=0x%0h", addr));
        check(ticket != null && ticket.model_ticket != null &&
              ticket.model_ticket.audit_rows.size() == 1 &&
              ticket.model_ticket.audit_rows[0].outcome == expected_outcome,
              $sformatf("outcome mismatch addr=0x%0h expected=%0d", addr, expected_outcome));
        check(data.commit_pma_pmp_csr_replay(ticket),
              $sformatf("commit failed addr=0x%0h", addr));
    endfunction

    function void check_outcome_matrix();
        pma_pmp_entry_t entry;
        pma_pmp_entry_t locked_entry;
        pma_pmp_entry_t unlocked_entry;
        replay_state_t prior_state;
        replay_state_t after_state;
        longint unsigned pmp_gen;
        longint unsigned pma_gen;

        reset_replay_baseline();
        expect_outcome(20, 12'h3b0, 64'h0000_0000_2000_0000,
                       PMA_PMP_CSR_ACCEPTED_CHANGED);
        check(data.pma_pmp_model.read_entry(1'b0, 0, entry), "failed to read PMPADDR0");
        expect_outcome(23, 12'h3b0, entry.addr_raw,
                       PMA_PMP_CSR_ACCEPTED_SAME_VALUE);
        expect_outcome(26, 12'h3a0, 64'h0000_0000_0000_000f,
                       PMA_PMP_CSR_ACCEPTED_CHANGED);
        expect_outcome(29, 12'h3a0, 64'h0000_0000_0000_000f,
                       PMA_PMP_CSR_ACCEPTED_SAME_VALUE);

        expect_outcome(32, 12'h7c8, 64'h0000_0000_0100_0000,
                       PMA_PMP_CSR_ACCEPTED_CHANGED);
        check(data.pma_pmp_model.read_entry(1'b1, 0, entry), "failed to read PMAADDR0");
        expect_outcome(35, 12'h7c8, entry.addr_raw,
                       PMA_PMP_CSR_ACCEPTED_SAME_VALUE);
        expect_outcome(38, 12'h7c0, 64'h0000_0000_0000_006f,
                       PMA_PMP_CSR_ACCEPTED_CHANGED);
        expect_outcome(41, 12'h7c0, 64'h0000_0000_0000_006f,
                       PMA_PMP_CSR_ACCEPTED_SAME_VALUE);

        data.pma_pmp_model.get_current_generations(pmp_gen, pma_gen);
        check(pmp_gen == 4 && pma_gen == 4,
              "PMP/PMA accepted outcome generation count mismatch");

        expect_outcome(44, 12'h3a4, 64'h8080_8080_8080_8080,
                       PMA_PMP_CSR_ACCEPTED_CHANGED);
        expect_outcome(47, 12'h3a4, 64'h8080_8080_8080_8080,
                       PMA_PMP_CSR_IGNORED_LOCKED);

        // Mixed-lock CFG: slot 8 is locked by the setup write while slot 9
        // remains writable.  A changed unlocked slot and a later same-value
        // write must each create exactly one group generation/history event.
        reset_replay_baseline();
        expect_outcome(50, 12'h3a2, 64'h0000_0000_0000_0f80,
                       PMA_PMP_CSR_ACCEPTED_CHANGED);
        check(data.pma_pmp_model.read_entry(1'b0, 8, locked_entry) &&
              data.pma_pmp_model.read_entry(1'b0, 9, unlocked_entry) &&
              locked_entry.lock && unlocked_entry.a == PMA_PMP_A_TOR,
              "mixed-lock setup did not establish locked/unlocked slots");

        capture_replay_state(prior_state);
        expect_outcome(53, 12'h3a2, 64'h0000_0000_0000_1f0f,
                       PMA_PMP_CSR_ACCEPTED_CHANGED);
        capture_replay_state(after_state);
        check(data.pma_pmp_model.read_entry(1'b0, 8, entry) &&
              entry == locked_entry,
              "mixed-lock changed write modified locked slot");
        check(data.pma_pmp_model.read_entry(1'b0, 9, entry) &&
              entry.a == PMA_PMP_A_NAPOT && !entry.lock,
              "mixed-lock changed write did not update unlocked slot");
        check(after_state.pmp_generation == prior_state.pmp_generation + 1 &&
              after_state.pmp_generation_history_size ==
                  prior_state.pmp_generation_history_size + 1 &&
              after_state.pmp_snapshot_count == prior_state.pmp_snapshot_count + 1 &&
              after_state.produced_count == prior_state.produced_count + 1 &&
              after_state.applied_count == prior_state.applied_count + 1 &&
              after_state.raw_size == 0,
              "mixed-lock changed write did not create exactly one conserved event");

        locked_entry = entry;
        check(data.pma_pmp_model.read_entry(1'b0, 8, locked_entry),
              "failed to resnapshot mixed-lock locked slot");
        check(data.pma_pmp_model.read_entry(1'b0, 9, unlocked_entry),
              "failed to resnapshot mixed-lock unlocked slot");
        capture_replay_state(prior_state);
        expect_outcome(56, 12'h3a2, 64'h0000_0000_0000_1f0f,
                       PMA_PMP_CSR_ACCEPTED_SAME_VALUE);
        capture_replay_state(after_state);
        check(data.pma_pmp_model.read_entry(1'b0, 8, entry) &&
              entry == locked_entry,
              "mixed-lock same-value write modified locked slot");
        check(data.pma_pmp_model.read_entry(1'b0, 9, entry) &&
              entry == unlocked_entry,
              "mixed-lock same-value write modified unlocked slot");
        check(after_state.pmp_generation == prior_state.pmp_generation + 1 &&
              after_state.pmp_generation_history_size ==
                  prior_state.pmp_generation_history_size + 1 &&
              after_state.pmp_snapshot_count == prior_state.pmp_snapshot_count + 1 &&
              after_state.produced_count == prior_state.produced_count + 1 &&
              after_state.applied_count == prior_state.applied_count + 1 &&
              after_state.raw_size == 0,
              "mixed-lock same-value write did not create exactly one conserved event");

        expect_outcome(59, 12'h3d0, 64'h1,
                       PMA_PMP_CSR_IGNORED_UNSUPPORTED);
        expect_outcome(62, 12'h123, 64'h1,
                       PMA_PMP_CSR_IGNORED_UNKNOWN);
    endfunction

    function void check_model_off_replay_contract();
        bit saved_model_en;
        longint unsigned pmp_before;
        longint unsigned pma_before;
        longint unsigned pmp_after;
        longint unsigned pma_after;

        reset_replay_baseline();
        saved_model_en = seq_csr_common::pma_pmp_model_en;
        seq_csr_common::pma_pmp_model_en = 1'b0;
        data.pma_pmp_model.get_current_generations(pmp_before, pma_before);
        push(60, 12'h3a0, 64'h0000_0000_0000_000f);
        service_at(62);
        data.pma_pmp_model.get_current_generations(pmp_after, pma_after);
        check(memblock_sync_pkg::raw_pma_pmp_csr_write_size() == 0 &&
              data.pma_pmp_csr_applied_count == 1 &&
              memblock_sync_pkg::raw_pma_pmp_csr_write_produced_count == 1 &&
              pmp_after == pmp_before + 1 && pma_after == pma_before,
              "model-off replay did not maintain FIFO/count/generation history");
        check(data.pma_pmp_model.get_generations_before_sample(62, pmp_after, pma_after) &&
              pmp_after == pmp_before + 1,
              "model-off replay did not publish C+2 history");
        seq_csr_common::pma_pmp_model_en = saved_model_en;
    endfunction

    function void check_commit_counter_boundaries();
        memblock_pma_pmp_replay_ticket ticket;
        replay_state_t prior;
        longint unsigned saved_raw_revision;
        longint unsigned saved_service_revision;
        longint unsigned saved_model_revision;

        // Applied count: MAX-1 may commit to MAX; MAX must reject with zero mutation.
        reset_replay_baseline();
        push(70, 12'h3a0, 64'hf);
        check(data.prepare_pma_pmp_csr_replay(72, ticket), "applied legal-bound prepare failed");
        data.pma_pmp_csr_applied_count = memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1;
        check(data.commit_pma_pmp_csr_replay(ticket) &&
              data.pma_pmp_csr_applied_count == memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64,
              "applied count rejected legal terminal value");

        reset_replay_baseline();
        push(73, 12'h3a0, 64'hf);
        check(data.prepare_pma_pmp_csr_replay(75, ticket), "applied overflow prepare failed");
        data.pma_pmp_csr_applied_count = memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64;
        capture_replay_state(prior);
        check(!data.commit_pma_pmp_csr_replay(ticket), "applied overflow commit was accepted");
        check_no_shared_mutation(prior, "applied overflow");

        // Raw revision: MAX-2 may advance to MAX-1; MAX-1 must reject.
        reset_replay_baseline();
        push(76, 12'h3a0, 64'hf);
        check(data.prepare_pma_pmp_csr_replay(78, ticket), "raw legal-bound prepare failed");
        saved_raw_revision = memblock_sync_pkg::raw_pma_pmp_csr_write_revision;
        memblock_sync_pkg::raw_pma_pmp_csr_write_revision =
            memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 2;
        ticket.raw_revision = memblock_sync_pkg::raw_pma_pmp_csr_write_revision;
        check(data.commit_pma_pmp_csr_replay(ticket) &&
              memblock_sync_pkg::raw_pma_pmp_csr_write_revision ==
                  memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1,
              "raw revision rejected legal terminal live value");
        memblock_sync_pkg::raw_pma_pmp_csr_write_revision = saved_raw_revision;

        reset_replay_baseline();
        push(79, 12'h3a0, 64'hf);
        check(data.prepare_pma_pmp_csr_replay(81, ticket), "raw overflow prepare failed");
        saved_raw_revision = memblock_sync_pkg::raw_pma_pmp_csr_write_revision;
        memblock_sync_pkg::raw_pma_pmp_csr_write_revision =
            memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1;
        ticket.raw_revision = memblock_sync_pkg::raw_pma_pmp_csr_write_revision;
        capture_replay_state(prior);
        check(!data.commit_pma_pmp_csr_replay(ticket), "raw revision overflow was accepted");
        check_no_shared_mutation(prior, "raw revision overflow");
        memblock_sync_pkg::raw_pma_pmp_csr_write_revision = saved_raw_revision;

        // Service revision: MAX-2 may advance to MAX-1; MAX-1 must reject.
        reset_replay_baseline();
        push(82, 12'h3a0, 64'hf);
        check(data.prepare_pma_pmp_csr_replay(84, ticket), "service legal-bound prepare failed");
        saved_service_revision = data.pma_pmp_csr_replay_service_revision;
        data.pma_pmp_csr_replay_service_revision =
            memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 2;
        ticket.service_revision = data.pma_pmp_csr_replay_service_revision;
        check(data.commit_pma_pmp_csr_replay(ticket) &&
              data.pma_pmp_csr_replay_service_revision ==
                  memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1,
              "service revision rejected legal terminal live value");
        data.pma_pmp_csr_replay_service_revision = saved_service_revision;

        reset_replay_baseline();
        push(85, 12'h3a0, 64'hf);
        check(data.prepare_pma_pmp_csr_replay(87, ticket), "service overflow prepare failed");
        saved_service_revision = data.pma_pmp_csr_replay_service_revision;
        data.pma_pmp_csr_replay_service_revision =
            memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1;
        ticket.service_revision = data.pma_pmp_csr_replay_service_revision;
        capture_replay_state(prior);
        check(!data.commit_pma_pmp_csr_replay(ticket), "service revision overflow was accepted");
        check_no_shared_mutation(prior, "service revision overflow");
        data.pma_pmp_csr_replay_service_revision = saved_service_revision;

        // Model revision: MAX-2 may advance to MAX-1; MAX-1 must reject.
        reset_replay_baseline();
        push(88, 12'h3a0, 64'hf);
        saved_model_revision = data.pma_pmp_model.csr_replay_revision;
        data.pma_pmp_model.csr_replay_revision =
            memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 2;
        check(data.prepare_pma_pmp_csr_replay(90, ticket), "model legal-bound prepare failed");
        check(data.commit_pma_pmp_csr_replay(ticket) &&
              data.pma_pmp_model.csr_replay_revision ==
                  memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1,
              "model revision rejected legal terminal live value");
        data.pma_pmp_model.csr_replay_revision = saved_model_revision;

        reset_replay_baseline();
        push(91, 12'h3a0, 64'hf);
        check(data.prepare_pma_pmp_csr_replay(93, ticket), "model overflow prepare failed");
        saved_model_revision = data.pma_pmp_model.csr_replay_revision;
        data.pma_pmp_model.csr_replay_revision =
            memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1;
        ticket.model_ticket.expected_model_revision =
            data.pma_pmp_model.csr_replay_revision;
        capture_replay_state(prior);
        check(!data.commit_pma_pmp_csr_replay(ticket), "model revision overflow was accepted");
        check_no_shared_mutation(prior, "model revision overflow");
        data.pma_pmp_model.csr_replay_revision = saved_model_revision;
    endfunction

    function void check_generation_boundaries();
        memblock_pma_pmp_replay_ticket ticket;
        replay_state_t prior;
        longint unsigned saved_generation;

        reset_replay_baseline();
        saved_generation = data.pma_pmp_model.pmp_generation;
        data.pma_pmp_model.pmp_generation = memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 2;
        push(100, 12'h3a0, 64'hf);
        check(data.prepare_pma_pmp_csr_replay(102, ticket) &&
              ticket.model_ticket.post_pmp_generation ==
                  memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1,
              "PMP generation rejected legal terminal live value");
        data.pma_pmp_model.pmp_generation = saved_generation;

        reset_replay_baseline();
        saved_generation = data.pma_pmp_model.pmp_generation;
        data.pma_pmp_model.pmp_generation = memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1;
        push(103, 12'h3a0, 64'hf);
        capture_replay_state(prior);
        check(!data.prepare_pma_pmp_csr_replay(105, ticket), "PMP generation overflow was accepted");
        check_no_shared_mutation(prior, "PMP generation overflow");
        data.pma_pmp_model.pmp_generation = saved_generation;

        reset_replay_baseline();
        saved_generation = data.pma_pmp_model.pma_generation;
        data.pma_pmp_model.pma_generation = memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 2;
        push(106, 12'h7c0, 64'h6f);
        check(data.prepare_pma_pmp_csr_replay(108, ticket) &&
              ticket.model_ticket.post_pma_generation ==
                  memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1,
              "PMA generation rejected legal terminal live value");
        data.pma_pmp_model.pma_generation = saved_generation;

        reset_replay_baseline();
        saved_generation = data.pma_pmp_model.pma_generation;
        data.pma_pmp_model.pma_generation = memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1;
        push(109, 12'h7c0, 64'h6f);
        capture_replay_state(prior);
        check(!data.prepare_pma_pmp_csr_replay(111, ticket), "PMA generation overflow was accepted");
        check_no_shared_mutation(prior, "PMA generation overflow");
        data.pma_pmp_model.pma_generation = saved_generation;
    endfunction

    function void check_capture_boundaries();
        memblock_pma_pmp_replay_ticket ticket;
        replay_state_t prior;
        longint unsigned max_u64;

        max_u64 = memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64;
        reset_replay_baseline();
        push(max_u64 - 2, 12'h3a0, 64'hf);
        check(data.prepare_pma_pmp_csr_replay(max_u64, ticket) &&
              ticket.model_ticket.audit_rows.size() == 1 &&
              ticket.model_ticket.audit_rows[0].write_sample == max_u64,
              "capture MAX-2 did not produce effective sample MAX");

        reset_replay_baseline();
        inject_raw_without_producer_report(max_u64 - 1, 12'h3a0, 64'hf);
        capture_replay_state(prior);
        check(!data.prepare_pma_pmp_csr_replay(max_u64, ticket),
              "capture MAX-1 was accepted by replay prepare");
        check_no_shared_mutation(prior, "capture MAX-1 rejection");
    endfunction

    function void check_runtime_epoch_reset();
        bit saved_reset_active;
        longint unsigned saved_epoch;
        longint unsigned runtime_epoch;
        longint unsigned raw_revision;
        longint unsigned service_revision;
        longint unsigned model_revision;
        replay_state_t prior;

        reset_replay_baseline();
        push(120, 12'h3a0, 64'hf);
        saved_reset_active = memblock_sync_pkg::l2tlb_runtime_reset_active;
        saved_epoch = memblock_sync_pkg::l2tlb_current_reset_epoch;
        check(!saved_reset_active && saved_epoch != memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64,
              "runtime reset directed precondition failed");
        runtime_epoch = saved_epoch + 1;
        raw_revision = memblock_sync_pkg::get_raw_pma_pmp_csr_write_revision();
        service_revision = data.pma_pmp_csr_replay_service_revision;
        model_revision = data.pma_pmp_model.get_csr_replay_revision();
        memblock_sync_pkg::l2tlb_current_reset_epoch = runtime_epoch;
        memblock_sync_pkg::l2tlb_runtime_reset_active = 1'b1;
        data.reset_pma_pmp_csr_replay_for_runtime_epoch();
        check(data.pma_pmp_csr_replay_reset_epoch == runtime_epoch &&
              data.pma_pmp_csr_applied_count == 0 &&
              data.pma_pmp_last_applied_csr_sample == 0 &&
              memblock_sync_pkg::raw_pma_pmp_csr_write_size() == 0 &&
              memblock_sync_pkg::raw_pma_pmp_csr_write_produced_count == 0 &&
              memblock_sync_pkg::get_raw_pma_pmp_csr_write_revision() == raw_revision + 1 &&
              data.pma_pmp_csr_replay_service_revision == service_revision + 1 &&
              data.pma_pmp_model.get_csr_replay_revision() == model_revision + 1,
              "runtime nonzero-epoch replay reset did not establish one new baseline");
        memblock_sync_pkg::l2tlb_runtime_reset_active = saved_reset_active;
        memblock_sync_pkg::l2tlb_current_reset_epoch = saved_epoch;
        reset_replay_baseline();

        // Under a valid active nonzero runtime epoch, exhaustion of any reset
        // participant must be rejected by the aggregate precheck with no
        // FIFO/model/history/counter/revision mutation.
        push(123, 12'h3a0, 64'hf);
        runtime_epoch = saved_epoch + 1;
        memblock_sync_pkg::l2tlb_current_reset_epoch = runtime_epoch;
        memblock_sync_pkg::l2tlb_runtime_reset_active = 1'b1;

        model_revision = data.pma_pmp_model.csr_replay_revision;
        data.pma_pmp_model.csr_replay_revision =
            memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1;
        capture_replay_state(prior);
        check(!data.precheck_pma_pmp_csr_replay_reset(1'b1, runtime_epoch),
              "runtime model revision reset precheck was accepted");
        check_no_shared_mutation(prior, "runtime model revision reset precheck");
        data.pma_pmp_model.csr_replay_revision = model_revision;

        raw_revision = memblock_sync_pkg::raw_pma_pmp_csr_write_revision;
        memblock_sync_pkg::raw_pma_pmp_csr_write_revision =
            memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1;
        capture_replay_state(prior);
        check(!data.precheck_pma_pmp_csr_replay_reset(1'b1, runtime_epoch),
              "runtime raw revision reset precheck was accepted");
        check_no_shared_mutation(prior, "runtime raw revision reset precheck");
        memblock_sync_pkg::raw_pma_pmp_csr_write_revision = raw_revision;

        service_revision = data.pma_pmp_csr_replay_service_revision;
        data.pma_pmp_csr_replay_service_revision =
            memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1;
        capture_replay_state(prior);
        check(!data.precheck_pma_pmp_csr_replay_reset(1'b1, runtime_epoch),
              "runtime service revision reset precheck was accepted");
        check_no_shared_mutation(prior, "runtime service revision reset precheck");
        data.pma_pmp_csr_replay_service_revision = service_revision;

        memblock_sync_pkg::l2tlb_runtime_reset_active = saved_reset_active;
        memblock_sync_pkg::l2tlb_current_reset_epoch = saved_epoch;
        reset_replay_baseline();
    endfunction

    function void check_producer_and_reset_observability();
        memblock_sync_pkg::dispatch_raw_pma_pmp_csr_write_t item;
        replay_state_t prior;
        longint unsigned raw_revision_before;
        longint unsigned saved_raw_revision;
        longint unsigned saved_service_revision;
        longint unsigned saved_model_revision;
        longint unsigned sum;

        reset_replay_baseline();
        capture_replay_state(prior);
        check(!memblock_sync_pkg::checked_pma_pmp_csr_write_count_add(
                  memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64, 1, sum),
              "expected-count overflow was accepted");
        check_no_shared_mutation(prior, "expected-count overflow precheck");

        // Legal producer terminal count is observable without a report path.
        reset_replay_baseline();
        memblock_sync_pkg::raw_pma_pmp_csr_write_produced_count =
            memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1;
        push(130, 12'h123, 64'h1);
        check(memblock_sync_pkg::raw_pma_pmp_csr_write_produced_count ==
                  memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 &&
              memblock_sync_pkg::peek_raw_pma_pmp_csr_write(item),
              "producer rejected legal terminal count");

        // Producer overflow, raw revision exhaustion, and C+2 sample overflow
        // must be rejected before any shared state changes.
        reset_replay_baseline();
        make_item(133, 12'h123, 64'h1, item);
        memblock_sync_pkg::raw_pma_pmp_csr_write_produced_count =
            memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64;
        capture_replay_state(prior);
        check(!memblock_sync_pkg::precheck_raw_pma_pmp_csr_write_push(item),
              "producer overflow precheck was accepted");
        check_no_shared_mutation(prior, "producer overflow precheck");

        reset_replay_baseline();
        make_item(136, 12'h123, 64'h1, item);
        saved_raw_revision = memblock_sync_pkg::raw_pma_pmp_csr_write_revision;
        memblock_sync_pkg::raw_pma_pmp_csr_write_revision =
            memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1;
        capture_replay_state(prior);
        check(!memblock_sync_pkg::precheck_raw_pma_pmp_csr_write_push(item),
              "producer raw revision overflow precheck was accepted");
        check_no_shared_mutation(prior, "producer raw revision overflow precheck");
        memblock_sync_pkg::raw_pma_pmp_csr_write_revision = saved_raw_revision;

        reset_replay_baseline();
        make_item(memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1,
                  12'h123, 64'h1, item);
        capture_replay_state(prior);
        check(!memblock_sync_pkg::precheck_raw_pma_pmp_csr_write_push(item),
              "producer capture MAX-1 precheck was accepted");
        check_no_shared_mutation(prior, "producer capture MAX-1 precheck");

        // Every replay reset participant must be able to advance before the
        // wrapper mutates any participant.
        reset_replay_baseline();
        saved_model_revision = data.pma_pmp_model.csr_replay_revision;
        data.pma_pmp_model.csr_replay_revision =
            memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1;
        capture_replay_state(prior);
        check(!data.precheck_pma_pmp_csr_replay_reset(
                  1'b0, memblock_sync_pkg::get_l2tlb_current_reset_epoch()),
              "model revision reset precheck was accepted");
        check_no_shared_mutation(prior, "model revision reset precheck");
        data.pma_pmp_model.csr_replay_revision = saved_model_revision;

        reset_replay_baseline();
        saved_raw_revision = memblock_sync_pkg::raw_pma_pmp_csr_write_revision;
        memblock_sync_pkg::raw_pma_pmp_csr_write_revision =
            memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1;
        capture_replay_state(prior);
        check(!data.precheck_pma_pmp_csr_replay_reset(
                  1'b0, memblock_sync_pkg::get_l2tlb_current_reset_epoch()),
              "raw revision reset precheck was accepted");
        check_no_shared_mutation(prior, "raw revision reset precheck");
        memblock_sync_pkg::raw_pma_pmp_csr_write_revision = saved_raw_revision;

        reset_replay_baseline();
        saved_service_revision = data.pma_pmp_csr_replay_service_revision;
        data.pma_pmp_csr_replay_service_revision =
            memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 1;
        capture_replay_state(prior);
        check(!data.precheck_pma_pmp_csr_replay_reset(
                  1'b0, memblock_sync_pkg::get_l2tlb_current_reset_epoch()),
              "service revision reset precheck was accepted");
        check_no_shared_mutation(prior, "service revision reset precheck");
        data.pma_pmp_csr_replay_service_revision = saved_service_revision;

        // One logical testcase reset must advance the FIFO revision exactly once.
        reset_replay_baseline();
        raw_revision_before = memblock_sync_pkg::get_raw_pma_pmp_csr_write_revision();
        reset_replay_baseline();
        check(memblock_sync_pkg::get_raw_pma_pmp_csr_write_revision() ==
                  raw_revision_before + 1,
              "testcase reset advanced raw FIFO revision more than once");
    endfunction

    virtual task body();
        memblock_pma_pmp_replay_ticket ticket, stale;
        pma_pmp_entry_t entry;
        longint unsigned pmp_gen, pma_gen, revision, saved_sample, sum;
        longint unsigned current_epoch;
        longint unsigned produced_count, applied_count, pending_count;

        seq_csr_common::init();
        data = common_data_transaction::get();
        check(data != null, "common_data singleton is null");
        check(memblock_sync_pkg::l2tlb_testcase_lifecycle_initialized,
              "testcase lifecycle was not initialized before directed sequence");
        check(memblock_sync_pkg::pma_pmp_csr_epoch_state_is_current(
                  1'b1, 1'b0, 0, 0), "legal startup epoch zero was rejected");
        check(!memblock_sync_pkg::pma_pmp_csr_epoch_state_is_current(
                  1'b0, 1'b0, 0, 0), "uninitialized startup epoch zero was accepted");
        check(!memblock_sync_pkg::pma_pmp_csr_epoch_state_is_current(
                  1'b1, 1'b1, 0, 0), "active reset accepted epoch zero");
        check(memblock_sync_pkg::pma_pmp_csr_epoch_state_is_current(
                  1'b1, 1'b1, 1, 1), "current runtime epoch was rejected");
        check(memblock_sync_pkg::checked_pma_pmp_csr_write_count_add(
                  64'hffff_ffff_ffff_fffe, 1, sum) &&
              sum == 64'hffff_ffff_ffff_ffff,
              "checked count add rejected legal terminal count");
        check(!memblock_sync_pkg::checked_pma_pmp_csr_write_count_add(
                  64'hffff_ffff_ffff_ffff, 1, sum),
              "checked count add accepted overflow");
        check(memblock_sync_pkg::pma_pmp_csr_revision_can_advance(
                  64'hffff_ffff_ffff_fffd) &&
              !memblock_sync_pkg::pma_pmp_csr_revision_can_advance(
                  64'hffff_ffff_ffff_fffe),
              "revision sentinel boundary mismatch");

        saved_sample = memblock_sync_pkg::lifecycle_event_published_seq;
        current_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
        check(memblock_sync_pkg::pma_pmp_csr_epoch_is_current(current_epoch),
              "live testcase PMA/PMP reset domain is invalid");
        reset_replay_baseline();

        push(10, 12'h3b0, 64'h2000_0000);
        push(11, 12'h3a0, 64'h0000_0000_0000_000f);
        revision = memblock_sync_pkg::get_raw_pma_pmp_csr_write_revision();
        service_at(10);
        service_at(11);
        check(data.pma_pmp_csr_applied_count == 0 &&
              memblock_sync_pkg::raw_pma_pmp_csr_write_size() == 2 &&
              memblock_sync_pkg::get_raw_pma_pmp_csr_write_revision() == revision,
              "C/C+1 no-work mutated replay state");
        service_at(12);
        check(data.pma_pmp_csr_applied_count == 1 &&
              memblock_sync_pkg::raw_pma_pmp_csr_write_size() == 1,
              "C+2 did not apply exactly one ordered write");
        check(data.pma_pmp_model.get_generations_before_sample(11, pmp_gen, pma_gen) &&
              pmp_gen == 0, "history became visible before effective sample");
        check(data.pma_pmp_model.get_generations_before_sample(12, pmp_gen, pma_gen) &&
              pmp_gen == 1, "history is missing at inclusive effective sample");
        service_at(13);
        service_at(13);
        check(data.csr_pmp_pma_writes_observed(2) &&
              data.pma_pmp_csr_applied_count == 2,
              "no-request FIFO did not drain exactly once");
        check(data.pma_pmp_model.read_entry(1'b0, 0, entry) &&
              entry.r && entry.w && entry.x && entry.a == PMA_PMP_A_TOR &&
              entry.addr_raw == 64'h2000_0000,
              "ordered PMPADDR/PMPCFG table mismatch");

        push(140, 12'h3b1, 64'h2100_0000);
        check(data.prepare_pma_pmp_csr_replay(142, ticket), "stale-prefix prepare failed");
        revision = data.pma_pmp_model.get_csr_replay_revision();
        push(141, 12'h3b2, 64'h2200_0000);
        check(!data.commit_pma_pmp_csr_replay(ticket) &&
              data.pma_pmp_model.get_csr_replay_revision() == revision &&
              memblock_sync_pkg::raw_pma_pmp_csr_write_size() == 2,
              "stale prefix commit partially mutated state");
        check(data.prepare_pma_pmp_csr_replay(143, ticket) &&
              data.commit_pma_pmp_csr_replay(ticket), "ordered batch commit failed");
        check(!data.commit_pma_pmp_csr_replay(ticket),
              "duplicate ticket committed");

        push(150, 12'h3b3, 64'h2300_0000);
        check(data.prepare_pma_pmp_csr_replay(152, stale), "end-audit prepare failed");
        check(!data.precheck_pma_pmp_csr_end_state(
                  produced_count, applied_count, pending_count) &&
              produced_count == applied_count + 1 && pending_count == 1 &&
              memblock_sync_pkg::raw_pma_pmp_csr_write_size() == 1,
              "end-state precheck consumed or failed to expose residue");
        service_at(152);
        check(!data.commit_pma_pmp_csr_replay(stale),
              "service followed by duplicate ticket was accepted");
        check(data.precheck_pma_pmp_csr_end_state(
                  produced_count, applied_count, pending_count) &&
              produced_count == applied_count && pending_count == 0,
              "clean end-state precheck rejected drained replay state");
        data.end_test_check();

        push(160, 12'h3b4, 64'h2400_0000);
        check(data.prepare_pma_pmp_csr_replay(162, stale), "pre-reset prepare failed");
        reset_replay_baseline();
        check(!data.commit_pma_pmp_csr_replay(stale) &&
              data.pma_pmp_csr_applied_count == 0 &&
              memblock_sync_pkg::raw_pma_pmp_csr_write_size() == 0,
              "testcase reset revived stale replay ticket");

        check_outcome_matrix();
        check_model_off_replay_contract();
        check_commit_counter_boundaries();
        check_generation_boundaries();
        check_capture_boundaries();
        check_runtime_epoch_reset();
        check_producer_and_reset_observability();

        memblock_sync_pkg::lifecycle_event_published_seq = saved_sample;
        `uvm_info("PMA_PMP_FIFO_TEST", "PMA/PMP FIFO directed checks passed", UVM_LOW)
    endtask
endclass
`endif
