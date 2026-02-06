# XiangShan Prefetcher Architecture

This document summarizes the prefetcher structure in XiangShan, including both L1 and L2 prefetchers.

## Overview

XiangShan implements a sophisticated multi-level prefetching system with:
- **L1 Prefetchers**: Train on L1 cache behavior and generate prefetch requests to L1/L2/L3
- **L2 Prefetchers**: Integrated in the coupledL2 module, receiving requests from L1 and generating independent L2 prefetches

## L1 Prefetchers

XiangShan implements **four main L1 prefetcher types**, each targeting different memory access patterns:

### 1. Stream Prefetcher (`L1StreamPrefetcher.scala`)

**Algorithm**: Region-based spatial pattern detection using bit-vectors

**Key Features**:
- **StreamBitVectorArray**: 16 entries tracking memory access patterns within 1024-byte regions
- **Region Tag Matching**: Monitors current region and adjacent regions (±1)
- **Activity Detection**: Tracks when patterns become "active" (≥12 hits out of 16 blocks threshold)
- **Bidirectional Support**: Detects both increasing and decreasing access directions (`decr_mode`)
- **Multi-level Depth**: Configurable prefetch depths for L1/L2/L3 (default: 64/640/640 bytes)
- **Confidence Mechanism**: Uses activity threshold to determine when to start prefetching

**Pipeline Stages**:
- **S0**: Feature extraction and region matching
- **S1**: Update bit-vector and check activity threshold
- **S2**: Direction determination and prefetch generation
- **S3-S4**: Output formatting and forwarding

**Output**: Generates multi-level prefetch requests (L1, L2, L3) based on detected patterns

---

### 2. Stride Prefetcher (`L1StridePrefetcher.scala`)

**Algorithm**: PC-hash based fixed-stride pattern detection

**Key Features**:
- **StrideMetaArray**: 10 entries per PC hash for tracking stride history
- **Confidence Tracking**: 2-bit confidence counter (0-3 scale)
  - Increments on stride match
  - Decrements on stride mismatch
  - Resets on zero confidence
- **Stride Validation**: Filters out invalid patterns:
  - stride = 0 (redundant)
  - stride = 1 (handled by stream)
  - Negative strides (optional via `ALLOW_NEGATIVE_STRIDE`)
- **Aggressive Lookahead**: Configurable prefetch distance (default: 1 block, up to 2)
- **Stream Collision Avoidance**: Optional lookup to stream component to prevent redundant prefetches

**Activation Criteria**:
- Confidence must reach maximum (3)
- Valid stride pattern detected
- No collision with stream prefetcher

**Use Case**: Effective for regular strided access patterns (e.g., array traversals with constant stride)

---

### 3. SMS Prefetcher (`SMSPrefetcher.scala`)

**Algorithm**: Spatial Memory Streaming

**Reference**: Based on research by Somogyi et al., ISCA 2006

**Key Components**:
- **Active Generation Table (AGT)**: Tracks currently active memory regions
- **Pattern History Table (PHT)**: Stores historical access patterns for regions
- **Stride Entries**: Maintains stride information for pattern prediction

**Features**:
- **Region Size**: 1024 bytes (configurable)
- **Prefetch Filter**: 16-entry deduplication filter to avoid redundant requests
- **Spatial Correlation**: Learns complex spatial access patterns within regions

**Use Case**: Handles irregular but repeating spatial patterns that stride/stream prefetchers may miss

---

### 4. BERTI Prefetcher (`Berti.scala`)

**Algorithm**: History Table + Delta Table based prediction

**Key Parameters**:
- **History Table (HT)**: 64 sets × 6 ways
  - Tracks recent memory access history
  - Uses address tags for matching
- **Delta Table (DT)**: 64 ways × 4 deltas per entry
  - Stores address offset deltas
  - 13-bit delta width
- **Replacement Policy**: Configurable FIFO or PLRU
- **Training Input**: Cache miss refill events (`refillTrain` signal)

**Operation**:
1. On memory access, lookup history table with address
2. If hit, retrieve associated delta patterns from delta table
3. Generate prefetch addresses by applying deltas to current address
4. Update tables on cache refills

**Use Case**: Effective for complex, delta-correlated access patterns

---

## L1 Prefetcher Integration

### PrefetcherWrapper (`PrefetcherWrapper.scala`)

Main orchestrator that manages all L1 prefetchers:

**Responsibilities**:
- Routes training data from load/store units to individual prefetchers
- Manages multiple prefetcher instances simultaneously
- Handles TLB translation for virtual-to-physical address conversion
- Performs PMP (Physical Memory Protection) checks
- Arbitrates between multiple prefetch requests

**Training Signals**:
- Load/Store execution completion signals (PC + Virtual Address)
- Cache refill events (for BERTI training)
- Miss/hit information from cache hierarchy

### Prefetch Sources (`L1PrefetchInterface.scala`)

Each prefetch request carries a 4-bit source identifier:
```scala
L1_HW_PREFETCH_STRIDE = 2.U
L1_HW_PREFETCH_STREAM = 3.U
L1_HW_PREFETCH_BERTI  = 5.U
L1_HW_PREFETCH_SMS    = (other value)
```

### Feedback Directed Prefetching (FDP)

**FDP Module** (`FDP.scala`): Tracks prefetch effectiveness
- **Useful Prefetches**: Prefetched data actually used by core
- **Useless Prefetches**: Prefetched data evicted before use
- **Adaptive Control**: Can dynamically adjust prefetcher aggressiveness based on accuracy

**Monitor** (`PrefetcherMonitor.scala`): Performance counters and debugging support

---

## L2 Prefetchers

L2 prefetchers are integrated in the **coupledL2** submodule and operate at the L2 cache level.

### L2 Prefetcher Types

#### 1. Prefetch Receiver (`PrefetchReceiverParams`)

**Purpose**: Receives and processes prefetch requests from L1 prefetchers

**Features**:
- Accepts prefetch requests forwarded from L1 prefetchers (Stream, Stride, SMS, BERTI)
- Filters and validates requests before inserting into L2 cache
- Prevents redundant prefetches that would conflict with ongoing L2 operations
- Maintains separate queues for L1→L2 and L2→L3 prefetch requests

**Integration Point**: Connected via `PrefetchRecv` interface in `MemBlock.scala`

#### 2. BOP Prefetcher (`BOPParameters`)

**Algorithm**: Best Offset Prefetcher

**Purpose**: Learns optimal prefetch offsets dynamically

**Features**:
- **Physical BOP (PBOP)**: Operates on physical addresses
- **Virtual BOP (VBOP)**: Operates on virtual addresses before translation
- **Offset Learning**: Tracks which address offsets produce useful prefetches
- **Adaptive**: Adjusts offset based on observed memory access patterns
- **Separate from L1**: Generates independent L2 prefetch requests

#### 3. Temporal Prefetcher (TP)

**Purpose**: Captures temporal correlation between memory accesses

**Features**:
- Tracks sequences of memory accesses over time
- Predicts future accesses based on temporal patterns
- Complements spatial prefetchers (Stream, Stride, BOP)

---

## L2 Prefetch Control (CSR Interface)

L2 prefetchers are controlled via custom CSR register **Spfctl** (address `0x5C1`):

| Bit(s) | Field Name | Description | Default |
|--------|-----------|-------------|---------|
| 1 | `L2_PF_ENABLE` | Master enable for L2 prefetcher | ON |
| 17 | `L2_PF_STORE_ONLY` | Restrict to store-only prefetches | OFF |
| 18 | `L2_PF_RECV_ENABLE` | Enable receiving L1 prefetch requests | ON |
| 19 | `L2_PF_PBOP_ENABLE` | Enable Physical BOP prefetcher | ON |
| 20 | `L2_PF_VBOP_ENABLE` | Enable Virtual BOP prefetcher | ON |
| 21 | `L2_PF_TP_ENABLE` | Enable Temporal Prefetcher | ON |
| 31-22 | `L2_PF_DELAY_LATENCY` | Configurable delay latency for training | (varies) |

**Access**:
- Software can read/write this CSR to dynamically enable/disable prefetchers
- Useful for workload-specific tuning and debugging

---

## System Integration

### Data Flow

```
Load/Store Units
      ↓ (training signals: PC, vaddr, refill)
PrefetcherWrapper
      ↓ (prefetch requests)
   ┌──┴──┬──────┬────────┐
   ↓     ↓      ↓        ↓
Stream Stride  SMS    BERTI
   └──┬──┴──────┴────────┘
      ↓ (4-bit source ID)
   L1 Cache
      ↓ (forwarded requests)
   L2 Prefetch Receiver
      ↓
   ┌──┴────┬──────┬────┐
   ↓       ↓      ↓    ↓
  BOP-P  BOP-V   TP   L2
   └───────┴──────┴────┘
         ↓
      L2 Cache
         ↓
      L3 Cache
```

### Address Translation

**TLB Support**:
- Separate DTLB port dedicated for prefetch address translation
- Virtual addresses from prefetchers translated to physical addresses
- PMP checks applied before issuing prefetch requests to cache hierarchy

**Benefits**:
- Prevents illegal memory accesses from speculative prefetches
- Ensures prefetch respects memory protection boundaries

### Configuration Parameters

Key parameters defined in `Parameters.scala` and `Configs.scala`:
```scala
// L1 Prefetcher Configuration
case class L1PrefetcherParameters(
  streamParams: StreamStrideParams,
  bertiParams: BertiParams,
  smsParams: SMSParams,
  fdpParams: FDPParams
)

// L2 Prefetcher Configuration (in coupledL2)
case class L2PrefetcherParameters(
  receiverParams: PrefetchReceiverParams,
  bopParams: BOPParameters,
  tpParams: TemporalPrefetcherParams
)
```

---

## Performance Considerations

### L1 Prefetcher Trade-offs

| Prefetcher | Strengths | Weaknesses | Best Use Case |
|------------|-----------|------------|---------------|
| **Stream** | Simple, low overhead, effective for sequential access | May over-prefetch, limited to spatial patterns | Sequential array scans |
| **Stride** | Handles regular strides, low storage | Misses irregular patterns | Array traversals with constant stride |
| **SMS** | Captures complex spatial patterns | Higher storage/complexity | Irregular but repeating access patterns |
| **BERTI** | Learns delta correlations, handles complex patterns | Requires training, higher latency | Pointer-chasing, linked structures |

### L2 Prefetcher Benefits

- **Reduced L2 Miss Rate**: Independent L2 prefetchers catch patterns missed by L1
- **Better Coverage**: BOP learns optimal offsets for the specific workload
- **Temporal Correlation**: TP captures time-based access patterns
- **Adaptive**: CSR control allows runtime tuning for different workload phases

### Accuracy vs. Coverage

- **L1 Prefetchers**: Higher accuracy, tighter integration with core
- **L2 Prefetchers**: Broader coverage, can prefetch further ahead
- **FDP Feedback**: Monitors effectiveness to prevent cache pollution

---

## References

### Academic Papers

1. **SMS Prefetcher**: Stephen Somogyi et al. "Spatial Memory Streaming." ISCA 2006.
2. **BOP Prefetcher**: Pierre Michaud. "Best-Offset Hardware Prefetching." HPCA 2016.
3. **Stride Prefetching**: Fu and Patel. "Stride Directed Prefetching in Scalar Processors." MICRO 1992.

### Related Files

- **L1 Prefetchers**: `src/main/scala/xiangshan/mem/prefetch/`
  - `L1StreamPrefetcher.scala`
  - `L1StridePrefetcher.scala`
  - `SMSPrefetcher.scala`
  - `Berti.scala`
  - `PrefetcherWrapper.scala`
  - `L1PrefetchComponent.scala`
  - `L1PrefetchInterface.scala`
  - `BasePrefecher.scala`
  - `FDP.scala`
  - `PrefetcherMonitor.scala`

- **L2 Integration**: 
  - `src/main/scala/xiangshan/L2Top.scala`
  - `src/main/scala/xiangshan/mem/MemBlock.scala`
  - `coupledL2/` submodule (contains L2 prefetcher implementations)

- **Configuration**:
  - `src/main/scala/xiangshan/Parameters.scala`
  - `src/main/scala/top/Configs.scala`

- **Control/Monitoring**:
  - `src/main/scala/xiangshan/backend/fu/CSR.scala`
  - `src/main/scala/xiangshan/backend/fu/NewCSR/CSRCustom.scala`

---

## Debugging and Tuning

### Performance Counters

Prefetcher performance can be monitored via:
- **Prefetch requests issued**: Per-prefetcher counters
- **Prefetch hits**: Prefetched data actually used
- **Prefetch misses**: Prefetched data evicted before use
- **Coverage**: Percentage of misses covered by prefetches
- **Accuracy**: Percentage of prefetches that are useful

### Runtime Control

Use the `Spfctl` CSR (0x5C1) to:
- Enable/disable individual prefetchers dynamically
- Adjust prefetch aggressiveness
- Debug prefetcher behavior
- Optimize for specific workload characteristics

### Common Issues

1. **Over-prefetching**: Too many useless prefetches → Enable FDP feedback
2. **Under-prefetching**: Low coverage → Increase prefetch depth/aggressiveness
3. **Cache Pollution**: Prefetches evict useful data → Tune accuracy thresholds
4. **TLB Pressure**: Too many prefetch translations → Reduce prefetch rate

---

## Summary

XiangShan's prefetcher architecture provides comprehensive coverage through:
- **Multiple L1 prefetchers** targeting different access patterns (Stream, Stride, SMS, BERTI)
- **L2 prefetchers** providing independent learning and broader coverage (BOP, TP)
- **Feedback mechanisms** (FDP) to maintain accuracy and prevent cache pollution
- **Runtime control** via CSR for workload-specific tuning
- **Tight integration** with TLB and PMP for safe, effective prefetching

This multi-level, multi-pattern approach ensures effective prefetching across diverse workloads while maintaining hardware efficiency.
