#pragma once
#include "state.hpp"
#include "scalar_dependency.hpp"
#include "unresolved_dependency.hpp"
#include "vector_indices.hpp"
#include "vector_operands.hpp"
#include "translation_permutation.hpp"

namespace memblock::scenarios::mixed {

class MixedScenario : private BoundaryState, private ProgressState,
                      private IdentityState, private AddressSequenceState,
                      private AddressConstants, private TranslationScheduleState,
                      private TailState, private VectorShapeState {
public:
    MixedScenario(int argc, char **argv, const Options &options,
        const RandomConstraints &constraints,
        std::uint64_t progress_interval);
    int run();

private:
    const Options &options;
    const RandomConstraints &constraints;
    const std::uint64_t progress_interval;
    memblock::Environment environment;
    std::mt19937_64 random;
    OrdinaryPageMappings ordinary_pages{options.seed, permuted_physical_base};
    MixedCoverage coverage;
    ConstraintCoverage constraint_coverage;
    const std::uint64_t bank_conflict_line_offset =
        (options.seed * 0x9e3779b97f4a7c15ULL +
            0xd1b54a32d192ed03ULL) % bank_conflict_lines;
    const std::uint64_t bank_conflict_line_stride =
        ((options.seed * 0xbf58476d1ce4e5b9ULL) ^
            0x94d049bb133111ebULL) % bank_conflict_lines | 1U;
    const unsigned constrained_completion_timeout =
        constraints.response_latency.dcache == memblock::ResponseLatencyProfile::spec ||
            constraints.response_latency.ptw == memblock::ResponseLatencyProfile::spec ||
            constraints.response_latency.uncache == memblock::ResponseLatencyProfile::spec
        ? 16384 : 2048;

    std::uint64_t gstage_boundary_loads();
    std::uint64_t canonical_boundary_loads();
    std::string canonical_boundary_summary();
    std::string gstage_boundary_summary();
    void report_progress(bool force = false);
    unsigned long long hypervisor_virtual_base(unsigned spvp, unsigned pbmt,
        unsigned topology);
    unsigned long long hypervisor_guest_base(unsigned spvp, unsigned pbmt,
        unsigned topology);
    unsigned long long hypervisor_physical_base(unsigned pbmt,
        unsigned topology);
    unsigned long long hypervisor_pma_device_virtual_base(unsigned spvp,
        unsigned topology);
    unsigned long long hypervisor_pma_device_guest_base(unsigned spvp,
        unsigned topology);
    unsigned long long hypervisor_pma_device_physical_base(unsigned spvp,
        unsigned topology);
    unsigned long long hypervisor_pmp_virtual_base(unsigned spvp,
        unsigned topology);
    unsigned long long hypervisor_pmp_guest_base(unsigned spvp,
        unsigned topology);
    unsigned long long hypervisor_pmp_physical_base(unsigned spvp,
        unsigned topology);
    std::string load_feedback_summary(const memblock::Environment::ScalarLoadFeedbackStats &feedback);
    std::string raw_load_feedback_summary();
    unsigned long total_ifetch_prefetches();
    memblock::LoadTransaction make_load(std::uint64_t address, memblock::LoadOp op,
        unsigned lane, bool fp = false);
    memblock::StoreTransaction make_store(std::uint64_t address, std::uint64_t data,
        memblock::StoreOp op, unsigned address_lane);
    memblock::PrefetchTransaction make_prefetch(std::uint64_t address, memblock::PrefetchOp op,
        unsigned lane);
    memblock::VectorMemoryTransaction make_vector(bool store, std::uint64_t address, unsigned eew,
        unsigned lane,
        memblock::VectorAddressingMode addressing =
        memblock::VectorAddressingMode::unit_stride);
    memblock::AtomicTransaction make_atomic(std::uint64_t address, memblock::AtomicOp op,
        std::uint64_t data);
    unsigned int random_delay(unsigned minimum, unsigned maximum);
    std::uint64_t random_bare_address(std::uint64_t region, unsigned alignment,
        unsigned span);
    std::uint64_t constrained_locality_line();
    std::uint64_t constrained_cacheable_address(
        unsigned alignment, const TranslationContext &translation,
        const TranslationLeafTopology &leaf,
        std::optional<bool> force_misaligned = std::nullopt,
        bool allow_permuted = true);
    std::uint64_t constrained_hypervisor_address(
        unsigned alignment, const TranslationLeafTopology &leaf,
        unsigned spvp, unsigned pbmt, bool pma_device, bool misaligned,
        unsigned pmp_relation);
    unsigned long constrained_window_address(std::uint64_t base,
        unsigned alignment);
    bool issue_load(const memblock::LoadTransaction &transaction,
        std::optional<std::uint64_t> expected = std::nullopt,
        bool count_action = true);
    bool issue_vector(
        const memblock::VectorMemoryTransaction &transaction,
        std::optional<std::array<unsigned char, 16>> expected =
        std::nullopt);
    bool issue_store(const memblock::StoreTransaction &transaction,
        bool data_first,
        bool hold_pending_store = false);
    std::uint64_t scalar_forward_value(std::uint64_t data, unsigned size,
        bool is_unsigned);
    std::uint64_t atomic_writeback(std::uint64_t old_value, bool word);
    unsigned long atomic_result(memblock::AtomicOp op, std::uint64_t old_value,
        std::uint64_t operand);
    std::array<unsigned char, 16> overlay_vector_store(const memblock::VectorMemoryTransaction &store,
        const memblock::VectorMemoryTransaction &load);

    memblock::ReferencePageMode page_mode(unsigned index);
    bool map_stage_page(unsigned mode, std::uint64_t root,
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        bool noncacheable = false,
        bool io = false,
        bool executable = false,
        bool user = false);
    bool map_g_page(unsigned mode, std::uint64_t root,
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        bool executable = false,
        bool noncacheable = false,
        bool io = false);
    bool map_stage_2m(unsigned mode, std::uint64_t root,
        std::uint64_t address);
    bool map_g_2m(unsigned mode, std::uint64_t root,
        std::uint64_t address);
    bool map_stage_napot(
        unsigned mode, std::uint64_t root,
        std::uint64_t virtual_base,
        std::optional<std::uint64_t> physical_base =
        std::nullopt,
        bool user = false,
        bool noncacheable = false,
        bool io = false);
    bool map_g_napot(
        unsigned mode, std::uint64_t root,
        std::uint64_t guest_physical_base,
        std::optional<std::uint64_t> host_physical_base =
        std::nullopt,
        bool noncacheable = false,
        bool io = false);
    bool prepare_random_translation_contexts();
    bool prepare_permuted_translation_pages();
    bool prepare_user_translation_pages();
    unsigned choose_stream_access();
    std::shared_ptr<const memblock::ReferencePageMap> reference_pages_for(
        std::uint64_t address) const;
    bool validate_translation_byte(std::uint64_t virtual_address,
        std::uint64_t physical_address, bool store,
        const TranslationContext &context);
    bool validate_vector_translation(const memblock::VectorMemoryTransaction &uop);
    bool activate_translation_context(const TranslationContext &context);
    unsigned int first_enabled_mode(const std::array<unsigned, 2> &weights);
    TranslationContext random_translation_context();
    TranslationContext dominant_translation_context();
    TranslationContext translated_context();
    bool binary_class_enabled(unsigned true_per_mille,
        bool value);
    TranslationLeafTopology random_translation_leaf(const TranslationContext &context);
    std::optional<TranslationLeafTopology> required_translation_leaf(
        const TranslationContext &context);
    memblock::scenarios::TranslationLeafTopology choose_translation_leaf(const TranslationContext &context);
    std::optional<TranslationContext> required_translation_context();
    memblock::scenarios::TranslationContext choose_translation_context();
    bool flush_pending_translation_fences();
    bool enter_translation_context(const TranslationContext &context);
    bool issue_constrained_fence(unsigned operation_kind);
    std::uint64_t mmio_access_address();
    bool run_concurrent_windows();
    bool run_concurrent_stream(unsigned instruction_limit);
    unsigned int vector_shape_uop_count(
        const RandomVectorShape &shape);
    unsigned int vector_shape_flow_num(
        const RandomVectorShape &shape);
    unsigned int vector_shape_total_flows(
        const RandomVectorShape &shape);
    unsigned int vector_shape_deficit(const RandomVectorShape &shape,
        unsigned direction);
    const RandomVectorShape & choose_vector_shape(
        unsigned direction, std::optional<unsigned> maximum_flows,
        bool concurrent_slot);
    unsigned int vector_segment_shape_deficit(
        const RandomVectorSegmentShape &shape);
    const RandomVectorSegmentShape & choose_vector_segment_shape(bool require_misaligned = false);
    const RandomVectorSegmentShape & choose_vector_segment_fof_shape();
    bool choose_binary_class(
        unsigned true_per_mille,
        const std::array<std::uint64_t, 2> &counts);
    std::vector<memblock::VectorMemoryTransaction> make_random_vector_uops(
        bool store, std::uint64_t address, const RandomVectorShape &shape,
        bool vector_corner);
    void sample_constrained_vector_instruction(
        bool store, const RandomVectorShape &shape,
        const std::vector<memblock::VectorMemoryTransaction> &uops);
    bool issue_random_vector_instruction(
        const std::vector<memblock::VectorMemoryTransaction> &uops);
    bool issue_random_vector_segment_fof(
        bool first_fault, unsigned stage1_mode,
        const RandomVectorSegmentShape &shape);
    bool issue_random_cmo(
        unsigned operation, bool dirty, bool younger_overlap,
        std::optional<bool> error_denied, unsigned probe_depth_class);
    bool recover_random_precise_error(
        std::uint8_t rob, bool rob_flag, bool store,
        std::uint64_t lq_dequeued_before,
        std::uint64_t sq_dequeued_before,
        std::uint64_t lq_cancels_before,
        std::uint64_t sq_cancels_before);
    bool issue_canonical_boundary(unsigned cross);
    bool issue_vector_canonical_boundary(unsigned cross);
    bool issue_gstage_boundary(unsigned cross, bool vector);
    bool retire_random_nc_store_error(
        std::uint64_t lq_dequeued_before,
        std::uint64_t sq_dequeued_before,
        std::uint64_t lq_cancels_before,
        std::uint64_t sq_cancels_before);
    bool ptw_error_outcome_enabled(unsigned outcome);
    bool ptw_error_mode_slot_enabled(
        unsigned site, unsigned slot);
    bool ptw_error_target_level_enabled(
        unsigned site, unsigned target_level);
    bool initialize_run();
    void initialize_memory();
    bool prepare_tail();
    ActionDisposition select_action(ActionWindow &window);
    void prepare_scalar_policy(ActionWindow &window);
    void prepare_vector_policy(ActionWindow &window);
    void prepare_special_policy(ActionWindow &window);
    bool prepare_action_translation(ActionWindow &window);
    void snapshot_action(ActionWindow &window);
    bool observe_action(ActionWindow &window);
    std::uint64_t action_cacheable_address(ActionWindow &window,
        unsigned alignment,
        std::optional<bool> force_misaligned = std::nullopt,
        bool allow_permuted = true);
    bool execute_scalar_load(ActionWindow &window);
    bool execute_load_merge(ActionWindow &window);
    bool execute_set_pressure(ActionWindow &window);
    bool execute_miss_burst(ActionWindow &window);
    bool execute_scalar_store(ActionWindow &window);
    bool execute_scalar_dependency(ActionWindow &window);
    bool execute_scalar_unresolved(ActionWindow &window);
    bool execute_ptw_error(ActionWindow &window);
    bool execute_vector_memory(ActionWindow &window);
    bool execute_vector_segment(ActionWindow &window);
    bool execute_prefetch(ActionWindow &window);
    bool execute_atomic(ActionWindow &window);
    bool execute_hypervisor(ActionWindow &window);
    bool execute_cmo(ActionWindow &window);
    bool execute_noncacheable(ActionWindow &window);
    bool execute_mmio(ActionWindow &window);
    bool execute_action(ActionWindow &window);
    bool run_serial_action();
    bool observe_final_writes();
    bool finish();
    bool execute();
};

} // namespace memblock::scenarios::mixed
