#pragma once
#include "../shared.hpp"

namespace memblock::scenarios::mixed {

struct BoundaryState {
    std::array<std::uint64_t, 24> canonical_boundary_crosses{};
    std::uint64_t canonical_boundary_controls = 0;
    std::uint64_t canonical_boundary_recoveries = 0;
    std::uint64_t canonical_boundary_store_checks = 0;
    std::array<std::uint64_t, 36> vector_canonical_boundary_crosses{};
    std::uint64_t vector_canonical_boundary_controls = 0;
    std::uint64_t vector_canonical_boundary_recoveries = 0;
    std::uint64_t vector_canonical_boundary_store_checks = 0;
    std::uint64_t vector_canonical_boundary_flows = 0;
    std::array<std::uint64_t, 12> gstage_boundary_crosses{};
    std::array<std::uint64_t, 18> vector_gstage_boundary_crosses{};
    std::uint64_t gstage_boundary_controls = 0;
    std::uint64_t gstage_boundary_recoveries = 0;
    std::uint64_t gstage_boundary_store_checks = 0;
    std::uint64_t vector_gstage_boundary_controls = 0;
    std::uint64_t vector_gstage_boundary_recoveries = 0;
    std::uint64_t vector_gstage_boundary_store_checks = 0;
    std::uint64_t vector_gstage_boundary_flows = 0;
};

struct ProgressState {
    std::string phase = "reset";
    unsigned actions = 0;
    std::uint64_t next_progress_action = 0;
};

struct IdentityState {
    std::uint64_t rob_offset = 0;
    std::uint64_t lq_offset = 0;
    std::uint64_t sq_offset = 0;
};

struct AddressSequenceState {
    std::uint64_t constrained_cold_line = 0;
    std::array<std::uint64_t, 3> cmo_locality_lines{};
    std::uint64_t cmo_overlap_line = 0;
    std::uint64_t cmo_auxiliary_probe_group = 0;
    std::uint64_t atomic_probe_target_line = 0;
    std::uint64_t atomic_auxiliary_probe_group = 0;
    std::uint64_t probe_overlap_line = 0;
    std::uint64_t dcache_error_line = 0;
    std::uint64_t atomic_error_line = 0;
    std::uint64_t ptw_error_line = 0;
    std::uint64_t load_merge_line = 0;
    std::array<std::uint64_t, 4> set_pressure_sequences{};
    std::uint64_t miss_burst_line = 0;
    std::uint64_t bank_conflict_line = 0;
};

struct AddressConstants {
    static constexpr std::uint64_t bare_base = memblock::kDefaultMemoryBase + 0x100000;
    static constexpr std::uint64_t cache0_base = memblock::kDefaultMemoryBase + 0x200000;
    static constexpr std::uint64_t cache1_base =
        memblock::kDefaultMemoryBase + (std::uint64_t{1} << 30) + 0x200000;
    static constexpr std::uint64_t nc_base =
        memblock::kDefaultMemoryBase + (std::uint64_t{2} << 30) + 0x200000;
    static constexpr std::uint64_t mmio_virtual = 0x50038000ULL;
    static constexpr std::uint64_t mmio_physical = 0x90038000ULL;
    static constexpr std::uint64_t hypervisor_span = 0x80000;
    static constexpr std::uint64_t hypervisor_pmp_region_offset = 0x1000;
    static constexpr std::uint64_t hypervisor_pmp_region_size = 0x1000;
    struct HypervisorPbmtMapping {
        bool vs_nc;
        bool vs_io;
        bool g_nc;
        bool g_io;
    };
    static constexpr std::array<
        HypervisorPbmtMapping, RandomConstraints::hypervisor_pbmt_pair_count>
        hypervisor_pbmt_mappings{{
            {false, false, false, false}, // PMA/PMA
            {false, false, true, false},  // PMA/NC
            {false, false, false, true},  // PMA/IO
            {true, false, false, true},   // NC/IO
            {false, true, true, false},   // IO/NC
        }};
    static constexpr std::uint64_t sv39_root = 0x90000000ULL;
    static constexpr std::uint64_t atomic_base = cache0_base + 0xa0000;
    static constexpr std::uint64_t stage1_napot_base = cache0_base + 0xb0000;
    static constexpr std::uint64_t nested_vs_napot_base = cache0_base + 0xc0000;
    static constexpr std::uint64_t nested_g_napot_base = cache0_base + 0xd0000;
    static constexpr std::uint64_t nested_both_napot_base = cache0_base + 0xe0000;
    static constexpr std::uint64_t vector_fof_virtual_base = 0x58000000ULL;
    static constexpr std::uint64_t vector_fof_physical_base = cache0_base + 0x100000;
    static constexpr std::uint64_t cmo_base = cache0_base + 0x200000;
    static constexpr std::uint64_t cmo_span = 0x1000000;
    static constexpr std::uint64_t probe_overlap_base = cmo_base + cmo_span;
    static constexpr std::uint64_t probe_overlap_span = 0x4000000;
    static constexpr std::uint64_t cmo_auxiliary_probe_base =
        probe_overlap_base + probe_overlap_span / 2;
    static constexpr std::uint64_t cmo_auxiliary_probe_span =
        probe_overlap_span / 4;
    static constexpr std::uint64_t atomic_auxiliary_probe_base =
        probe_overlap_base + 3 * probe_overlap_span / 4;
    static constexpr std::uint64_t atomic_auxiliary_probe_span =
        probe_overlap_span / 4;
    static constexpr std::uint64_t dcache_error_base =
        probe_overlap_base + probe_overlap_span;
    static constexpr std::uint64_t dcache_error_span = 0x4000000;
    static constexpr std::uint64_t atomic_error_base =
        dcache_error_base + dcache_error_span;
    static constexpr std::uint64_t atomic_error_span = 0x4000000;
    static constexpr std::uint64_t atomic_probe_target_base =
        atomic_error_base + atomic_error_span / 2;
    static constexpr std::uint64_t atomic_probe_target_span =
        atomic_error_span / 2;
    static constexpr std::uint64_t ptw_error_base =
        atomic_error_base + atomic_error_span;
    static constexpr std::uint64_t ptw_error_span = 0x100000;
    static constexpr std::uint64_t load_merge_base = cache1_base + 0xe00000;
    static constexpr std::uint64_t load_merge_span = 0x4000000;
    static constexpr std::uint64_t set_pressure_base =
        load_merge_base + load_merge_span;
    static constexpr std::uint64_t set_pressure_span = 0x100000000ULL;
    static constexpr std::uint64_t miss_burst_base =
        set_pressure_base + set_pressure_span;
    static constexpr std::uint64_t miss_burst_span = 0x40000000ULL;
    // A seed-specific full-period permutation of this dedicated 512-MiB pool
    // supports more than one million three-way conflict actions without
    // reusing a line. Keep it clear of the lower hypervisor physical pools.
    static constexpr std::uint64_t bank_conflict_base = 0x400000000ULL;
    static constexpr std::uint64_t bank_conflict_span = 0x20000000ULL;
    static constexpr std::uint64_t bank_conflict_line_bytes = 64;
    static constexpr std::uint64_t bank_conflict_lines =
        bank_conflict_span / bank_conflict_line_bytes;
    static_assert((bank_conflict_lines & (bank_conflict_lines - 1U)) == 0);
    // Keep the nested final G-stage walk in a different high-level subtree
    // from the implicit G-stage walks used to fetch VS page-table entries.
    static constexpr std::uint64_t ptw_error_guest_base = 0x18000000000ULL;
    static constexpr std::uint64_t ptw_error_stage1_root_base = 0xf0000000ULL;
    static constexpr std::uint64_t ptw_error_vs_root_base = 0xf4000000ULL;
    static constexpr std::uint64_t ptw_error_g_root_base = 0xf8000000ULL;
    static constexpr std::uint64_t ptw_error_root_stride = 0x20000;
    static constexpr std::uint64_t ptw_error_root_slots = 256;
    // Separate physical pages keep canonical-limit aliases clear of ordinary
    // translated pools. Every byte and post-fault readback uses this model.
    static constexpr std::uint64_t canonical_boundary_physical = 0x500000000ULL;
    static constexpr std::uint64_t gstage_boundary_physical = 0x510000000ULL;
    static constexpr std::array<std::uint64_t, 2> gstage_boundary_roots{{
        0xe6000000ULL, 0xe7000000ULL}};
    static constexpr std::uint64_t guest_virtual = 0x60000000ULL;
    static constexpr std::uint64_t guest_fault_virtual = 0xa0000000ULL;
    static constexpr std::uint64_t guest_physical = 0xb0000000ULL;
    static constexpr std::uint64_t host_physical = 0xd0000000ULL;
    static constexpr std::uint64_t vs_root = 0x96000000ULL;
    static constexpr std::uint64_t g_root = 0x97000000ULL;
    static constexpr std::array<std::uint64_t, 2> random_stage1_roots{{
        0xe0000000ULL, 0xe1000000ULL}};
    static constexpr std::array<std::uint64_t, 2> random_vs_roots{{
        0xe2000000ULL, 0xe3000000ULL}};
    static constexpr std::array<std::uint64_t, 2> random_g_roots{{
        0xe4000000ULL, 0xe5000000ULL}};
};

struct RandomVectorShape {
    unsigned addressing;
    unsigned eew;
    unsigned vsew;
    int lmul_log2;
    int emul_log2;
    long double weight;
};

struct RandomVectorSegmentShape {
    unsigned addressing;
    unsigned eew;
    unsigned vsew;
    int lmul_log2;
    int emul_log2;
    unsigned nf_index;
    long double weight;
};

struct TranslationScheduleState {
    TranslationContext current_translation{};
    bool translation_context_valid = false;
    static constexpr unsigned pending_sfence = 1U << 0;
    static constexpr unsigned pending_hfence_vvma = 1U << 1;
    static constexpr unsigned pending_hfence_gvma = 1U << 2;
    unsigned pending_translation_fences = 0;
    bool translation_coverage_closed = false;
};

struct TailState {
    static constexpr unsigned concurrent_vector_load_flow_limit =
        memblock::kVirtualLoadQueueEntries -
        memblock::kLqEnqueueHeadroom - 3U;
    static constexpr unsigned concurrent_vector_store_flow_limit =
        memblock::kStoreQueueEntries -
        memblock::kSqEnqueueHeadroom - 1U;
    static constexpr std::array<memblock::AtomicOp, 9> atomic_operations_d{{
        memblock::AtomicOp::amoadd_d,
        memblock::AtomicOp::amoxor_d,
        memblock::AtomicOp::amoand_d,
        memblock::AtomicOp::amoor_d,
        memblock::AtomicOp::amoswap_d,
        memblock::AtomicOp::amomin_d,
        memblock::AtomicOp::amomax_d,
        memblock::AtomicOp::amominu_d,
        memblock::AtomicOp::amomaxu_d,
    }};
    static constexpr std::array<memblock::AtomicOp, 9> atomic_operations_w{{
        memblock::AtomicOp::amoadd_w,
        memblock::AtomicOp::amoxor_w,
        memblock::AtomicOp::amoand_w,
        memblock::AtomicOp::amoor_w,
        memblock::AtomicOp::amoswap_w,
        memblock::AtomicOp::amomin_w,
        memblock::AtomicOp::amomax_w,
        memblock::AtomicOp::amominu_w,
        memblock::AtomicOp::amomaxu_w,
    }};
    static constexpr std::array<memblock::LoadOp, 7> hypervisor_load_operations{{
        memblock::LoadOp::hlvb,
        memblock::LoadOp::hlvh,
        memblock::LoadOp::hlvw,
        memblock::LoadOp::hlvd,
        memblock::LoadOp::hlvbu,
        memblock::LoadOp::hlvhu,
        memblock::LoadOp::hlvwu,
    }};
    static constexpr std::array<memblock::LoadOp, 5>
        hypervisor_misaligned_load_operations{{
            memblock::LoadOp::hlvh,
            memblock::LoadOp::hlvw,
            memblock::LoadOp::hlvd,
            memblock::LoadOp::hlvhu,
            memblock::LoadOp::hlvwu,
        }};
    static constexpr std::array<memblock::LoadOp, 2> hypervisor_execute_operations{{
        memblock::LoadOp::hlvxhu,
        memblock::LoadOp::hlvxwu,
    }};
    static constexpr std::array<memblock::StoreOp, 4> hypervisor_store_operations{{
        memblock::StoreOp::hsvb,
        memblock::StoreOp::hsvh,
        memblock::StoreOp::hsvw,
        memblock::StoreOp::hsvd,
    }};
    static constexpr std::uint64_t random_stride_base = AddressConstants::cache0_base + 0x50000;
    static constexpr std::uint64_t random_stride_bytes = 128;
    static constexpr unsigned random_stride_slots = 2048;

    unsigned target_before_redirect{};
    unsigned concurrent_action_limit{};
    std::array<std::uint64_t, 16> atomic_values{};
    unsigned stride_stream_loads = 0;
};

struct VectorShapeState {
    std::vector<RandomVectorShape> vector_shapes;
    std::vector<RandomVectorSegmentShape> vector_segment_shapes;
};

struct ActionWindow {
    unsigned int kind{};
    bool nc_store{};
    bool mmio_store{};
    std::optional<bool> dcache_load_error_denied{};
    bool bank_conflict_action{};
    unsigned int bank_conflict_depth_index{};
    unsigned int bank_conflict_bank{};
    std::optional<unsigned int> bank_conflict_forced_regime{};
    unsigned int atomic_family{};
    unsigned int atomic_width{};
    unsigned int atomic_probe_depth{};
    std::optional<bool> atomic_error_denied{};
    std::optional<bool> uncache_error_denied{};
    bool vector_segment_store{};
    bool vector_segment_fof{};
    bool vector_segment_fof_first_fault{};
    unsigned int vector_segment_fof_stage1_mode{};
    const RandomVectorSegmentShape * vector_segment_fof_shape{};
    std::optional<unsigned int> vector_segment_high_va_cross{};
    bool vector_fof{};
    bool vector_fof_first_fault{};
    unsigned int vector_fof_stage1_mode{};
    unsigned int vector_fof_eew{};
    unsigned int cmo_operation{};
    bool cmo_dirty{};
    bool cmo_younger_overlap{};
    unsigned int cmo_probe_depth_class{};
    std::optional<bool> cmo_error_denied{};
    std::optional<unsigned int> hypervisor_family{};
    bool hypervisor_spvp_user{};
    bool hypervisor_misaligned{};
    bool hypervisor_pma_device{};
    unsigned int hypervisor_pmp_relation{};
    unsigned int hypervisor_pbmt_pair{};
    unsigned int ptw_error_site{};
    unsigned int ptw_error_level_class{};
    unsigned int ptw_error_outcome{};
    unsigned int ptw_error_stage1_mode{};
    unsigned int ptw_error_vs_mode{};
    unsigned int ptw_error_g_mode{};
    unsigned int ptw_error_target_level{};
    bool ptw_error_store{};
    std::uint64_t ptw_error_address{};
    std::uint64_t ptw_error_guest_address{};
    std::uint64_t ptw_error_stage1_root{};
    std::uint64_t ptw_error_vs_root{};
    std::uint64_t ptw_error_g_root{};
    unsigned int set_pressure_depth_index{};
    unsigned int set_pressure_width{};
    unsigned int set_pressure_set{};
    bool set_pressure_dirty{};
    bool set_pressure_refill_overlap{};
    bool set_pressure_release_backpressure{};
    unsigned int set_pressure_window_class{};
    unsigned int miss_burst_depth_index{};
    unsigned int miss_burst_issue_width_index{};
    unsigned int miss_burst_composition{};
    TranslationContext translation{};
    TranslationLeafTopology leaf{};
    std::uint64_t ptw_requests_before{};
    std::uint64_t requests_before{};
    bool sample_dcache{};
    bool sample_translation_action{};
    bool leaf_addressed{};
    bool ordinary_leaf_addressed{};
    std::optional<std::uint64_t> probe_candidate{};
};

enum class ActionDisposition { failed, boundary_completed, ordinary };

} // namespace memblock::scenarios::mixed
