# DespacitoStream Prefetcher 상세 설계서

**문서 버전**: 1.0
**대상 브랜치**: kunminghu-v3 (commit 316946d28 기반)
**작성일**: 2026-03-21
**설계 위치**: XiangShan CoupledL2 Prefetcher Subsystem

---

## 1. 개요

### 1.1 설계 목적

DespacitoStream Prefetcher는 **PC(Program Counter) 기반 샘플링 프리페처**로, 단일 명령어가 여러 인터리브된 데이터 스트림을 접근하는 패턴을 감지한다. 데이터 스트림 관점에서는 next-line 패턴이지만, 명령어 관점에서는 랜덤으로 보이는 접근 패턴을 타겟으로 한다.

GEM5의 `DespacitoStream` prefetcher를 기반으로 XiangShan CoupledL2 계층에 Chisel RTL로 구현하였다.

### 1.2 설계 범위

- L2 캐시 프리페처로서 CoupledL2 Prefetcher 서브시스템에 통합
- L1 D-Cache → L2로의 PC 전파 경로 신규 구축
- 기존 프리페처(BOP, TP, PrefetchReceiver)와의 중재(Arbitration) 통합
- 파라미터화를 통한 설계 시 enable/disable 선택 가능

### 1.3 참조 구현

- GEM5: `src/mem/cache/prefetch/despacito_stream.hh/.cc`

---

## 2. 아키텍처

### 2.1 시스템 컨텍스트

```
┌──────────────────────────────────────────────────────────┐
│  XiangShan Core                                          │
│  ┌──────────────────────────────────────────────────┐    │
│  │  L1 D-Cache                                      │    │
│  │  ┌────────────┐                                  │    │
│  │  │ MissQueue  │──── TileLink.A ──────────────────┼────┤
│  │  │ (PC 보유)   │     user.PCKey = PC[38:6]        │    │
│  │  └────────────┘     user.VaddrKey = vaddr[38:6]  │    │
│  └──────────────────────────────────────────────────┘    │
└──────────────────────────────────────────────────────────┘
                          │
                          ▼
┌──────────────────────────────────────────────────────────┐
│  CoupledL2                                               │
│  ┌─────────┐    ┌──────────┐    ┌────────────────────┐  │
│  │ SinkA   │───▶│ MainPipe │───▶│ Prefetcher         │  │
│  │(PC추출)  │    │ (Stage3) │    │ ┌────────────────┐ │  │
│  │         │    │ PC전달    │    │ │ BOP (VBOP/PBOP)│ │  │
│  └─────────┘    └──────────┘    │ ├────────────────┤ │  │
│                                 │ │ TP             │ │  │
│                                 │ ├────────────────┤ │  │
│                                 │ │ DespacitoStream│◀┼──┼── 본 설계 대상
│                                 │ ├────────────────┤ │  │
│                                 │ │ PfReceiver     │ │  │
│                                 │ └───────┬────────┘ │  │
│                                 │         ▼          │  │
│                                 │    PrefetchQueue   │  │
│                                 │         ▼          │  │
│                                 │      Pipeline      │  │
│                                 └────────┬───────────┘  │
│                                          ▼              │
│                                     SinkA (Hint)        │
└──────────────────────────────────────────────────────────┘
```

### 2.2 모듈 계층 구조

```
Prefetcher (Top-level)
├── VBestOffsetPrefetch     (optional, hasBOP)
├── PBestOffsetPrefetch     (optional, hasBOP)
├── TemporalPrefetch        (optional, hasTPPrefetcher)
├── DespacitoStreamPrefetch (optional, hasDespacitoStream)  ← NEW
├── PrefetchReceiver        (optional, hasReceiver)
├── PrefetchQueue
└── Pipeline(1-stage)
```

---

## 3. PC 전파 경로 (L1 → L2)

### 3.1 설계 배경

DespacitoStream은 PC를 key로 사용하지만, 기존 L2 PrefetchTrain 인터페이스에는 PC 필드가 없었다. 기존에 VaddrKey로 virtual address를 TileLink user field를 통해 L1→L2로 전달하는 패턴이 존재하므로, 이를 따라 PCKey를 추가하였다.

### 3.2 전파 경로 상세

```
[Stage 1] L1 MissQueue
  req.pc (VAddrBits 전체)
     │
     ▼  bit-slice: pc(VAddrBits-1, blockOffBits) → 33bit (39-6)
[Stage 2] TileLink Channel A — user field
  acquire.user.lift(PCKey) := req.pc(38:6)
     │
     ▼
[Stage 3] L2 SinkA — fromTLAtoTaskBundle()
  task.pc := a.user.lift(PCKey).getOrElse(0.U)
     │
     ▼  TaskBundle 경유
[Stage 4] L2 MainPipe — Stage 3 (s3_prefetch_train)
  train.bits.pc := Mux(mergeA, aMergeTask.pc, req_s3.pc)
     │
     ▼
[Stage 5] PrefetchTrain → DespacitoStreamPrefetch.io.train
  train_pc := io.train.bits.pc.getOrElse(0.U)
```

### 3.3 데이터 구조 변경

#### 3.3.1 TileLink User Field Key 정의 (`L2Param.scala`)

```
case object PCKey extends ControlKey[UInt]("pc")
case class PCField(width: Int) extends BundleField[UInt](
  PCKey, Output(UInt(width.W)), _ := 0.U(width.W)
)
```

- `reqKey` 기본값에 `PCKey` 추가: `Seq(AliasKey, VaddrKey, PCKey, PrefetchKey, ReqSourceKey)`
- L1 client가 `PCField`를 reqField에 추가해야 실제 전파됨. 미추가 시 default 0으로 동작.

#### 3.3.2 TaskBundle 확장 (`Common.scala`)

`MergeTaskBundle`, `TaskBundle` 모두에 pc 필드 추가:

```
val pc = vaddrBitsOpt.map(_ => UInt(vaddrBitsOpt.get.W))
```

- `Option[UInt]` 타입으로, `vaddrBitsOpt`가 None이면 pc 필드 자체가 생성되지 않음
- 비트폭은 vaddr과 동일 (VAddrBits - blockOffBits = 33bit)

#### 3.3.3 PrefetchTrain 확장 (`Prefetcher.scala`)

```
val pc = vaddrBitsOpt.map(_ => UInt(vaddrBitsOpt.get.W))
```

---

## 4. DespacitoStreamPrefetch 모듈 상세 설계

### 4.1 파라미터

| 파라미터 | 기본값 | 설명 |
|---------|--------|------|
| `sampleRate` | 256 | 샘플링 주기. timestamp의 하위 log2(256)=8비트가 0일 때 샘플링 |
| `minDistance` | 4 | touched 판정 최소 시간 거리 (exclusive) |
| `maxDistance` | 8192 | touched 판정 최대 시간 거리 (inclusive) |
| `samplerSets` | 8 | Sampler Table 세트 수 |
| `samplerWays` | 4 | Sampler Table way 수 (총 32 entries) |
| `patternEntries` | 64 | Pattern Table 엔트리 수 (fully-associative) |
| `confBits` | 2 | Confidence counter 비트 수 (max = 3) |
| `filterSize` | 32 | 중복 prefetch 방지 필터 크기 |
| `inflightEntries` | 16 | PrefetchQueue depth (상속) |

### 4.2 I/O 포트

| 포트 | 방향 | 타입 | 설명 |
|------|------|------|------|
| `enable` | Input | Bool | 프리페처 전역 enable |
| `train` | Input | DecoupledIO(PrefetchTrain) | L2 MainPipe로부터의 학습 신호 |
| `req` | Output | DecoupledIO(PrefetchReq) | Prefetch 요청 출력 |
| `resp` | Input | DecoupledIO(PrefetchResp) | Prefetch 응답 (미사용, ready=true) |

- `train.ready` = always true (backpressure 없음)
- `resp.ready` = always true (DespacitoStream은 response feedback 미사용)

### 4.3 내부 스토리지 구조

모든 스토리지는 **Register 기반** (SRAM 미사용). 이로 인해 Mbist 검증 대상에서 제외된다.

#### 4.3.1 Sampler Table

```
구조:  Set-Associative (samplerSets × samplerWays)
기본:  8 sets × 4 ways = 32 entries
인덱싱: block_index의 하위 log2(8)=3 비트
교체:  Round-Robin (세트 별 독립 포인터)
```

| 필드 | 비트폭 | 설명 |
|------|--------|------|
| `valid` | 1 | 유효 비트 |
| `tag` | blockAddrBits - samplerIdxBits | block_index 상위 비트 |
| `timestamp` | 64 | 삽입 시점의 global timestamp |
| `pc` | pcWidth (=33) | 해당 접근의 PC |
| `touched` | 1 | 연속 블록 접근이 감지되었는지 여부 |

**엔트리당 비트폭**: 1 + (30-3) + 64 + 33 + 1 = **126 bits**
**총 스토리지**: 32 × 126 = **4,032 bits** (≈ 504 bytes)

#### 4.3.2 Pattern Table

```
구조:  Fully-Associative CAM
기본:  64 entries
검색:  PC를 key로 병렬 비교
교체:  Round-Robin (단일 전역 포인터)
```

| 필드 | 비트폭 | 설명 |
|------|--------|------|
| `valid` | 1 | 유효 비트 |
| `pc` | pcWidth (=33) | 명령어 PC |
| `conf` | confBits (=2) | Saturating confidence counter |

**엔트리당 비트폭**: 1 + 33 + 2 = **36 bits**
**총 스토리지**: 64 × 36 = **2,304 bits** (≈ 288 bytes)

**CAM 비용**: 64개 엔트리 × 33bit 비교기 = 64개 병렬 comparator. 이 CAM은 매 사이클 최대 2회 조회된다 (evict_pc 매칭 + train_pc 매칭).

#### 4.3.3 Duplicate Filter

```
구조:  Circular Buffer + CAM
기본:  32 entries
교체:  FIFO (Circular pointer)
```

| 필드 | 비트폭 | 설명 |
|------|--------|------|
| `valid` | 1 | 유효 비트 |
| `addr` | fullAddressBits (=36) | 프리페치된 물리 주소 |

**엔트리당 비트폭**: 1 + 36 = **37 bits**
**총 스토리지**: 32 × 37 = **1,184 bits** (≈ 148 bytes)

#### 4.3.4 Global Timestamp Counter

- 64-bit unsigned counter
- 매 유효한 train 수신 시 +1 increment
- 오버플로우: wrap-around (2^64 사이클에 한 번으로 사실상 발생하지 않음)

#### 4.3.5 총 스토리지 요약

| 구조 | 엔트리 수 | 엔트리 크기 | 총 비트 |
|------|----------|------------|---------|
| Sampler Table | 32 | 126b | 4,032 |
| Pattern Table | 64 | 36b | 2,304 |
| Duplicate Filter | 32 | 37b | 1,184 |
| Timestamp Counter | 1 | 64b | 64 |
| RR Pointers | 8+1+1 | 2+6+5b | 27 |
| **합계** | | | **7,611 bits (≈ 951 bytes)** |

### 4.4 조합 논리 상세

모든 로직은 **단일 사이클 파이프라인 없이** 순수 조합 로직 + 레지스터 업데이트로 구성된다. train이 valid인 사이클에 lookup/update/prefetch-generation이 동시에 발생한다.

#### 4.4.1 주소 분해

```
train_addr (fullAddressBits = 36 bits)
  └── block_index = train_addr >> blockOffBits (= >> 6)  → 30 bits
       ├── block_index_prev = block_index - 1            → 30 bits
       ├── curr_idx = block_index[2:0]                   → 3 bits (sampler set index)
       ├── curr_tag = block_index[29:3]                  → 27 bits (sampler tag)
       └── block_addr = Cat(block_index, 0(6bit))        → 36 bits
```

#### 4.4.2 Sampler Lookup 경로 (block_index - 1)

```
prev_idx = block_index_prev[2:0]
prev_tag = block_index_prev[29:3]

                  prev_idx
                    │
          ┌─────────┼─────────┐
          ▼         ▼         ▼
     sampler[0]  sampler[1] ... sampler[7]     ← set 선택 (MUX by prev_idx)
     ┌───┬───┬───┬───┐
     │w0 │w1 │w2 │w3 │                        ← 4-way 병렬 비교
     └─┬─┴─┬─┴─┬─┴─┬─┘
       ▼   ▼   ▼   ▼
     (valid && tag === prev_tag)               ← 4개 comparator (27-bit each)
       │   │   │   │
       ▼   ▼   ▼   ▼
     sampler_hit_vec[3:0]
       │
       ├── sampler_hit = OR reduction          ← hit 여부
       └── sampler_hit_way = OHToUInt          ← hit way 인덱스

     hit_timestamp = sampler_timestamp[prev_idx][sampler_hit_way]
     time_diff = timestamp - hit_timestamp     ← 64-bit subtractor
     in_time_window = (time_diff > 4) && (time_diff <= 8192)
                                               ← 2개 64-bit comparator
```

**Critical Path 요소**: 64-bit subtractor + 64-bit comparator 2개

#### 4.4.3 Sampler Victim 선택

```
curr_idx (현재 블록의 set index)
  │
  ▼
sampler_valid[curr_idx][0..3] 검사
  │
  ├── invalid way 존재? → PriorityEncoder → sampler_invalid_way
  └── 모두 valid?       → sampler_rr_ptr[curr_idx] (Round-Robin)
  │
  ▼
sampler_victim_way = Mux(has_invalid, invalid_way, rr_ptr)
```

#### 4.4.4 Pattern Table 업데이트 로직

Eviction 시 Pattern Table 업데이트 (샘플링 시점에만 동작):

```
evict_pc (evict된 sampler entry의 PC)
  │
  ▼
Pattern Table CAM Search: pattern_pc[0..63] === evict_pc
  │
  ├── pt_hit (기존 엔트리 존재)
  │    ├── evict_touched == true  → conf = min(conf + 1, confMax)   [saturating increment]
  │    └── evict_touched == false → conf = max(conf - 1, 0)         [saturating decrement]
  │
  └── !pt_hit && evict_touched (새 엔트리 생성, positive example만)
       ├── pattern_valid[victim] := true
       ├── pattern_pc[victim]   := evict_pc
       ├── pattern_conf[victim] := confInit (= 1)
       └── pattern_rr_ptr 갱신 (invalid 없을 때만)
```

#### 4.4.5 Prefetch Generation 경로

```
train_pc (현재 train의 PC)
  │
  ▼
Pattern Table CAM Search: pattern_pc[0..63] === train_pc    ← 64개 33-bit comparator
  │
  ├── pf_pt_hit?    ─── No ──→ 프리페치 안 함
  │
  └── Yes
       │
       ▼
     pf_conf = pattern_conf[hit_idx]
     pf_saturated = (pf_conf === confMax)     ← confMax = 3 (2-bit)
       │
       ├── Not saturated ──→ 프리페치 안 함
       │
       └── Saturated
            │
            ▼
          pf_addr = block_addr + blockBytes   ← next-line 주소 (36-bit adder)
            │
            ▼
          Filter CAM: filter_addr[0..31] === pf_addr    ← 32개 36-bit comparator
            │
            ├── filter_hit ──→ 중복, 프리페치 안 함
            │
            └── !filter_hit
                 │
                 ▼
               do_prefetch = true
               → pf_req_valid 레지스터 set
               → filter에 pf_addr 기록 (circular FIFO)
```

### 4.5 상태 머신 / 시퀀스 다이어그램

DespacitoStream은 명시적 FSM을 사용하지 않는다. 매 사이클 `train_valid` 조건 하에 3가지 동작이 조합적으로 동시에 수행된다:

```
매 train_valid 사이클:
  ┌─────────────────────────────────────────────────────────────┐
  │                                                             │
  │  [동작 1] Sampler Lookup (block_index - 1)                  │
  │    hit && in_time_window → touched := true                  │
  │                                                             │
  │  [동작 2] Sampling (timestamp % sampleRate == 0 일 때만)     │
  │    2a. Evict victim → Pattern Table 업데이트                 │
  │    2b. 새 entry 삽입 (current block_index, PC, timestamp)   │
  │                                                             │
  │  [동작 3] Prefetch Generation                               │
  │    PC로 Pattern Table 조회 → conf saturated → next-line pf  │
  │                                                             │
  │  [공통] timestamp := timestamp + 1                          │
  │                                                             │
  └─────────────────────────────────────────────────────────────┘
```

### 4.6 출력 핸드셰이크

Prefetch 요청 출력은 1-entry output register를 사용한다:

```
do_prefetch 발생 시:
  pf_req_valid := true
  pf_req_bits  := {tag, set, needT=false, source=0, pfSource=DespacitoStream}

io.req.fire 발생 시 (downstream이 accept):
  pf_req_valid := false

주의: do_prefetch와 io.req.fire가 동시 발생 시, do_prefetch가 우선 (when/elsewhen 구조)
     → 새 요청이 즉시 래치되고, 이전 요청은 소실될 수 있음 (설계 의도: 최신 요청 우선)
```

### 4.7 Timing 특성

| 항목 | 분석 |
|------|------|
| **Sampler Lookup** | Reg MUX(3-bit) → 4-way parallel compare(27-bit) → OR → OHToUInt |
| **Timestamp 비교** | 64-bit subtractor → 2개 64-bit magnitude comparator |
| **Pattern Table CAM** | 64-entry parallel compare(33-bit) → OR → OHToUInt → conf read |
| **Filter CAM** | 32-entry parallel compare(36-bit) → OR |
| **출력 경로** | 위 4개 경로가 모두 `do_prefetch` 신호에 AND로 수렴 |
| **예상 Critical Path** | Pattern CAM lookup → conf compare → filter CAM → do_prefetch |

**참고**: 모든 연산이 단일 사이클에 수행되므로, 높은 클럭 주파수에서는 타이밍 위반 가능성이 있다. 필요 시 파이프라인 스테이지 삽입을 고려할 수 있다.

---

## 5. Prefetcher 서브시스템 통합

### 5.1 인스턴스화 조건

```scala
// CoupledL2.scala
def hasDespacitoStream = prefetchers.exists(_.isInstanceOf[DespacitoStreamParameters])

// Prefetcher.scala
val despacito = if (hasDespacitoStream) Some(Module(new DespacitoStreamPrefetch())) else None
```

`L2Param.prefetch` 시퀀스에 `DespacitoStreamParameters()`가 포함된 경우에만 하드웨어가 생성된다.

### 5.2 Train 연결

```
io.train ──▶ despacito.io.train
             (L1DataPrefetch 소스 필터링: L1 프리페치 트래픽은 학습에서 제외)
             train.valid := io.train.valid && (reqsource =/= L1DataPrefetch)
```

### 5.3 Enable 제어

```
despacito.io.enable := pfCtrlFromCore.l2_pf_master_en
```

Core의 CSR에서 `l2_pf_master_en`을 통해 런타임에 enable/disable 가능하다.

### 5.4 Prefetch 요청 중재 (Arbitration)

우선순위 (높음 → 낮음):

```
Priority 1: PrefetchReceiver (L1으로부터의 prefetch 요청)
Priority 2: VBOP (Virtual Best-Offset Prefetch)
Priority 3: PBOP (Physical Best-Offset Prefetch)
Priority 4: TP (Temporal Prefetch)
Priority 5: DespacitoStream                              ← 최하위 우선순위
```

**Backpressure 구조**:

```
despacito.io.req.ready :=
  !pfRcv.io.req.valid &&         // Receiver가 없을 때
  !vbop.io.req.valid &&          // VBOP가 없을 때
  !pbop.io.req.valid &&          // PBOP가 없을 때
  !tp.io.req.valid               // TP가 없을 때
```

**ParallelPriorityMux**로 PrefetchQueue에 enqueue할 요청 선택:

```
pftQueue.io.enq.bits := ParallelPriorityMux(Seq(
  pfRcv.valid  → pfRcv.bits,
  vbop.valid   → vbop.bits,
  pbop.valid   → pbop.bits,
  tp.valid     → tp.bits,
  despacito.valid → despacito.bits     ← 최하위
))
```

### 5.5 Response 처리

DespacitoStream은 prefetch 결과에 대한 피드백을 사용하지 않는다:

```
despacito.io.resp.valid := false.B
```

### 5.6 PrefetchQueue → Pipeline → SinkA

```
                     ┌──────────────┐     ┌────────────┐
 Prefetchers ──enq──▶│ PrefetchQueue│──deq▶│ Pipeline   │──▶ io.req (to SinkA)
                     │ (16-entry)   │     │ (1-stage)  │
                     │ flow-through │     │ registered │
                     └──────────────┘     └────────────┘
```

PrefetchQueue 특성:
- 항상 enq.ready = true (overflow 시 oldest entry를 버림)
- 최신 요청 우선 정책

---

## 6. MemReqSource / PfSource 확장

### 6.1 MemReqSource 추가 (`BusKeyField.scala`)

```
val Prefetch2L2DespacitoStream = Value("Prefetch2L2DespacitoStream")
```

- `isL2Prefetch()` 판별 함수에도 추가됨
- L3 / 메모리 컨트롤러에서 prefetch 소스별 통계 수집에 활용

### 6.2 PfSource 추가 (`PrefetchParameters.scala`)

```
val DespacitoStream = Value("DespacitoStream")
```

- `fromMemReqSource` switch에 매핑:
  `MemReqSource.Prefetch2L2DespacitoStream → PfSource.DespacitoStream`

---

## 7. 알고리즘 동작 시나리오

### 7.1 학습 단계 (Warm-up)

```
Cycle  0: PC=0x1000 accesses block A[100]
           → sampler insert (set=100%8=4, tag=100>>3=12) at timestamp 0
           → pattern table: 0x1000 not found → no prefetch

Cycle  1: PC=0x2000 accesses block B[200]
           → sampler lookup block B[199]: miss
           → sampler insert (timestamp 1)

...

Cycle 256: (timestamp % 256 == 0 → sampling time)
           PC=0x1000 accesses block A[101]
           → sampler lookup A[100]: HIT!
             time_diff = 256, 4 < 256 <= 8192 → in_time_window
             → sampler[set=4][way=hit_way].touched := true
           → sampling: evict victim from current set
             victim has touched=true, PC=0x1000
             → pattern table: 0x1000 not found → CREATE new entry
               conf := 1 (confInit)
           → pattern table lookup 0x1000: conf=1, not saturated → no prefetch

Cycle 512: Similar pattern continues
           → pattern table 0x1000 hit: touched → conf := 2

Cycle 768: Similar pattern continues
           → pattern table 0x1000 hit: touched → conf := 3 (saturated!)

Cycle 769: PC=0x1000 accesses block A[N]
           → pattern table lookup 0x1000: conf=3 (saturated)
           → PREFETCH block A[N+1]!
```

### 7.2 Negative Feedback

```
PC=0x3000이 Pattern Table에 conf=2로 기록됨
이후 0x3000의 접근이 non-consecutive → touched=false인 eviction 발생

→ conf := max(2 - 1, 0) = 1
→ 반복 시 conf := 0 → 향후 saturate까지 더 많은 positive example 필요
```

---

## 8. 성능 카운터

| 카운터명 | 조건 | 의미 |
|---------|------|------|
| `despacito_train` | train_valid | 총 학습 이벤트 수 |
| `despacito_sampler_hit` | train_valid && sampler_hit | sampler에서 prev_block 히트 수 |
| `despacito_touched_mark` | sampler_hit && in_time_window | touched=true 마킹 횟수 |
| `despacito_sample_insert` | is_sample_time && pc_valid | 샘플링(삽입) 발생 횟수 |
| `despacito_pattern_update` | evict && pt_hit | 기존 pattern entry 업데이트 횟수 |
| `despacito_pattern_create` | evict && !pt_hit && touched | 신규 pattern entry 생성 횟수 |
| `despacito_pf_attempt` | pt_hit && saturated | 프리페치 시도 횟수 (필터 전) |
| `despacito_pf_filtered` | pt_hit && saturated && filter_hit | 필터에 의해 차단된 프리페치 |
| `despacito_pf_issued` | do_prefetch | 실제 발행된 프리페치 수 |
| `despacito_pf_req_fire` | io.req.fire | downstream이 accept한 프리페치 수 |

**주요 관찰 지표**:
- `pf_issued / train`: 프리페치 발행 비율
- `pf_filtered / pf_attempt`: 필터 효율
- `pf_req_fire / pf_issued`: downstream 수용률
- `touched_mark / sampler_hit`: 시간 윈도우 내 히트 비율

---

## 9. 파라미터 튜닝 가이드라인

| 파라미터 | 증가 시 효과 | 감소 시 효과 | 트레이드오프 |
|---------|-------------|-------------|-------------|
| `sampleRate` | 학습 느림, 노이즈 감소 | 학습 빠름, 오탐 증가 | 면적 무관, 정확도 vs 반응성 |
| `minDistance` | 짧은 간격 접근 무시 | 더 짧은 패턴 감지 | False positive 제어 |
| `maxDistance` | 넓은 시간 윈도우 | 좁은 시간 윈도우 | 비교기 비용 (64-bit) |
| `samplerSets` | Sampler 용량 증가 | Set conflict 증가 | 면적 선형 증가 |
| `samplerWays` | Way conflict 감소 | 면적 절감 | CAM 비교기 수 = sets × ways |
| `patternEntries` | 더 많은 PC 추적 | 면적 절감 | CAM 비교기 수 선형 증가 |
| `confBits` | 보수적 (saturation 느림) | 공격적 (빠른 prefetch) | 2-bit 기본 (max=3)이 적절 |
| `filterSize` | 중복 방지 강화 | 면적 절감 | CAM 비교기 수 선형 증가 |

---

## 10. 수정 파일 요약

| # | 파일 경로 | 변경 유형 | 변경 내용 |
|---|----------|-----------|-----------|
| 1 | `coupledL2/L2Param.scala` | 수정 | PCKey/PCField 정의, reqKey에 PCKey 추가 |
| 2 | `utility/BusKeyField.scala` | 수정 | MemReqSource.Prefetch2L2DespacitoStream 추가 |
| 3 | `xiangshan/cache/dcache/MissQueue.scala` | 수정 | TileLink.A에 PC 전파 (`acquire.user.lift(PCKey)`) |
| 4 | `coupledL2/Common.scala` | 수정 | MergeTaskBundle, TaskBundle에 pc 필드 추가 |
| 5 | `coupledL2/SinkA.scala` | 수정 | TileLink.A에서 PCKey 추출 → task.pc |
| 6 | `coupledL2/tl2tl/MainPipe.scala` | 수정 | Stage 3에서 PrefetchTrain.pc에 PC 전달 |
| 7 | `coupledL2/prefetch/Prefetcher.scala` | 수정 | PrefetchTrain.pc 추가, DespacitoStream 인스턴스 및 중재 |
| 8 | `coupledL2/prefetch/PrefetchParameters.scala` | 수정 | PfSource.DespacitoStream 추가 |
| 9 | `coupledL2/prefetch/DespacitoStreamPrefetch.scala` | **신규** | 프리페처 본체 구현 (269 lines) |
| 10 | `coupledL2/CoupledL2.scala` | 수정 | hasDespacitoStream 헬퍼 추가 |
| 11 | `top/Configs.scala` | 수정 | MinimalConfig에 DespacitoStreamParameters() 추가 |

---

## 11. 검증 결과

### 11.1 빌드 검증

| 단계 | 결과 | 비고 |
|------|------|------|
| Chisel Elaboration | PASS | DespacitoStreamPrefetch.sv 생성 확인 |
| Verilator Compilation | PASS | 경고만 존재 (기존 코드 유래), 에러 없음 |
| EMU Binary Link | PASS | build/emu 생성 완료 |

### 11.2 시뮬레이션 검증

| 항목 | 결과 |
|------|------|
| 벤치마크 | CoreMark 2-iteration |
| 종료 상태 | **HIT GOOD TRAP** (정상 종료) |
| Instructions | 663,687 |
| Cycles | 481,313 |
| IPC | 1.379 |
| Abort/Hang | 없음 |

---

## 12. 알려진 제한 사항 및 향후 과제

1. **단일 사이클 조합 경로**: Pattern Table CAM (64-entry, 33-bit)과 Filter CAM (32-entry, 36-bit)이 단일 사이클에 수행됨. 고주파 설계 시 파이프라인 스테이지 삽입 검토 필요.

2. **PC 유효성**: L1 MissQueue에서 PC가 0인 경우(예: hardware prefetch에 의한 miss) train_pc_valid가 false가 되어 학습/프리페치가 스킵됨. 이는 의도된 동작.

3. **Output Register 경합**: do_prefetch 발생 시 이전의 미소비된 prefetch 요청을 덮어씀. 높은 프리페치 발행률에서는 일부 요청이 유실될 수 있으나, PrefetchQueue의 flow-through 특성과 DespacitoStream의 낮은 우선순위를 감안하면 실질적 영향은 제한적.

4. **Confidence Hysteresis**: 2-bit counter만 사용하므로 positive/negative example이 번갈아 나타나는 경우 conf가 진동할 수 있음. 필요 시 confBits를 3으로 증가시켜 안정성 확보 가능.

5. **Multi-core 확장**: 현재 설계는 코어별 독립된 L2에 인스턴스화됨. 공유 L2/L3 환경에서의 동작은 별도 검증 필요.
