# Shared instruction profiling observations

This layer supplies raw observations to instruction profilers without selecting
samples or assigning execution time to instructions. It leaves functional ROB,
rename, dispatch, and commit decisions unchanged.

## Event metadata

`PerfEvent` defines an eight-bit performance signature vector (PSV). The bit
positions are shared with the profiler trace formats:

| Bit | Event | Observation |
| --- | --- | --- |
| 0 | `DR_L1` | Instruction fetch encountered an ICache miss |
| 1 | `DR_TLB` | Instruction fetch encountered an ITLB miss |
| 2 | `DR_SQ` | Store queue drain event, set by the consuming profiler |
| 3 | `FL_MB` | Control-flow misprediction |
| 4 | `FL_EX` | Exception flush |
| 5 | `FL_MO` | Memory-order redirect |
| 6 | `ST_L1` | Data-cache miss |
| 7 | `ST_TLB` | Data-TLB miss |

The shared frontend produces the first two bits. The remaining bits retain their
encoding for consumers that accumulate execution or recovery events. A signature
records whether an event occurred; it is neither an event count nor a latency.

The frontend preserves fetch miss observations through the way lookup, ICache
main pipe, IFU, and IBuffer. `PerfFrontend.bindPacketPsv` attaches a packet's
signature to its earliest valid instruction that is allowed to enqueue. This
also applies when that instruction bypasses IBuffer storage. Scalar and vector
decode paths preserve the `perfPsv` sideband through rename and dispatch. Rename
combines both source signatures in a fused output uop while preserving each
architectural instruction's input signature for member-based consumers.

The sideband is enabled only when an instruction sampler requires it. Cycle
statistics alone do not allocate frontend event metadata. The existing internal
`DebugOptions` fields identify the consumers; their public command-line and Make
options are supplied with the respective profiler implementations.

## Load/store observations

The s1/s2 debug identity uses a complete `RobPtr`: physical index, wrap flag, and
Former/Latter slot. Existing index-based debug arrays explicitly use `.value`.

The any-issue TLB miss signal includes replay attempts and excludes invalid or
killed requests and hardware prefetches without an architectural target. It is
separate from the existing first-issue miss statistic. Stores do not report a
DCache miss through the load-side s2 event.

## ROB observations

`PerfRobInfo` supplies a common cycle number, the number of architectural
instructions retired in the current cycle, and the actual committed slot
pointers. A committed physical entry can contain multiple architectural
instructions; the retirement count and the slot pointer vector represent these
different units explicitly.

Redirects and ROB-originated flushes are reported separately. Each identifies
the cause pointer, whether that instruction itself is flushed, and the full
instruction PC. Redirects also identify control mispredictions and memory-order
violations; ROB flushes identify exceptions and interrupts. Consumers determine
their own priority and attribution rules.

Control redirects carry a fetch-block PC. The observation adds the signed
instruction offset before extending the address according to the current
translation mode. Memory redirects already carry the instruction PC. The delayed
redirect echo of a ROB flush is exposed so consumers can avoid processing it as
a second recovery event.

The cycle counter starts at zero after reset and advances every cycle while this
observation hardware is elaborated. It is independent of sampling enable,
period, and measurement-window boundaries. No shared sampler, target history,
profiler-specific database table, or attribution policy is part of this layer.
