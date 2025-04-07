/***************************************************************************************
* Copyright (c) 2021-2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
* Copyright (c) 2024-2025 Institute of Information Engineering, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.cache.mmu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import utility._
import coupledL2.utils.SplittedSRAM
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import utility.mbist.MbistPipeline

/* ptw cache caches the page table of all the three layers
 * ptw cache resp at next cycle
 * the cache should not be blocked
 * when miss queue if full, just block req outside
 */

class PageCachePerPespBundle(implicit p: Parameters) extends PtwBundle {
  val hit = Bool()
  val pre = Bool()
  val ppn = UInt(gvpnLen.W)
  val pbmt = UInt(ptePbmtLen.W)
  val perm = new PtePermBundle()
  val n = UInt(pteNLen.W)
  val ecc = Bool()
  val level = UInt(2.W)
  val v = Bool()
  val bitmapCheck = Option.when(HasBitmapCheck)(new Bundle {
    val jmp_bitmap_check = Bool()
    val pte = UInt(XLEN.W) // Page Table Entry
  })

  def apply(hit: Bool, pre: Bool, ppn: UInt, pbmt: UInt = 0.U, n: UInt = 0.U,
            perm: PtePermBundle = 0.U.asTypeOf(new PtePermBundle()),
            ecc: Bool = false.B, level: UInt = 0.U, valid: Bool = true.B, jmp_bitmap_check: Bool = false.B,
            pte: UInt = 0.U): Unit = {
    this.hit := hit && !ecc
    this.pre := pre
    this.ppn := ppn
    this.n := n
    this.pbmt := pbmt
    this.perm := perm
    this.ecc := ecc && hit
    this.level := level
    this.v := valid
    if (HasBitmapCheck) {
      this.bitmapCheck.get.jmp_bitmap_check := jmp_bitmap_check
      this.bitmapCheck.get.pte := pte
    }
  }
}

class PageCacheMergePespBundle(implicit p: Parameters) extends PtwBundle {
  assert(tlbcontiguous == 8, "Only support tlbcontiguous = 8!")
  val hit = Bool()
  val pre = Bool()
  val ppn = Vec(tlbcontiguous, UInt(gvpnLen.W))
  val pbmt = Vec(tlbcontiguous, UInt(ptePbmtLen.W))
  val perm = Vec(tlbcontiguous, new PtePermBundle())
  val ecc = Bool()
  val level = UInt(2.W)
  val v = Vec(tlbcontiguous, Bool())
  val bitmapCheck = Option.when(HasBitmapCheck)(new Bundle {
    val jmp_bitmap_check = Bool()
    val hitway = UInt(l2tlbParams.l0nWays.W)
    val ptes = Vec(tlbcontiguous, UInt(XLEN.W)) // Page Table Entry Vector
    val cfs = Vec(tlbcontiguous, Bool()) // Bitmap Check Failed Vector
  })

  def apply(hit: Bool, pre: Bool, ppn: Vec[UInt], pbmt: Vec[UInt] = Vec(tlbcontiguous, 0.U),
            perm: Vec[PtePermBundle] = Vec(tlbcontiguous, 0.U.asTypeOf(new PtePermBundle())),
            ecc: Bool = false.B, level: UInt = 0.U, valid: Vec[Bool] = Vec(tlbcontiguous, true.B),
            jmp_bitmap_check: Bool = false.B,
            hitway: UInt = 0.U, ptes: Vec[UInt] , cfs: Vec[Bool]): Unit = {
    this.hit := hit && !ecc
    this.pre := pre
    this.ppn := ppn
    this.pbmt := pbmt
    this.perm := perm
    this.ecc := ecc && hit
    this.level := level
    this.v := valid
    if (HasBitmapCheck) {
      this.bitmapCheck.get.jmp_bitmap_check := jmp_bitmap_check
      this.bitmapCheck.get.hitway := hitway
      this.bitmapCheck.get.ptes := ptes
      this.bitmapCheck.get.cfs := cfs
    }
  }
}

class PageCacheRespBundle(implicit p: Parameters) extends PtwBundle {
  val l3 = if (EnableSv48) Some(new PageCachePerPespBundle) else None
  val l2 = new PageCachePerPespBundle
  val l1 = new PageCachePerPespBundle
  val l0 = new PageCacheMergePespBundle
  val sp = new PageCachePerPespBundle
}

class PtwCacheReq(implicit p: Parameters) extends PtwBundle {
  val req_info = new L2TlbInnerBundle()
  val isFirst = Bool()
  val bypassed = if (EnableSv48) Vec(4, Bool()) else Vec(3, Bool())
  val isHptwReq = Bool()
  val hptwId = UInt(log2Up(l2tlbParams.llptwsize).W)
}

class PtwCacheIO()(implicit p: Parameters) extends MMUIOBaseBundle with HasPtwConst {
  val req = Flipped(DecoupledIO(new PtwCacheReq()))
  val resp = DecoupledIO(new Bundle {
    val req_info = new L2TlbInnerBundle()
    val isFirst = Bool()
    val hit = Bool()
    val prefetch = Bool() // is the entry fetched by prefetch
    val bypassed = Bool()
    val toFsm = new Bundle {
      val l3Hit = if (EnableSv48) Some(Bool()) else None
      val l2Hit = Bool()
      val l1Hit = Bool()
      val ppn = UInt(gvpnLen.W)
      val stage1Hit = Bool() // find stage 1 pte in cache, but need to search stage 2 pte in cache at PTW
      val bitmapCheck = Option.when(HasBitmapCheck)(new Bundle {
        val jmp_bitmap_check = Bool() // find pte in l0 or sp, but need bitmap check
        val toLLPTW = Bool()
        val hitway = UInt(l2tlbParams.l0nWays.W)
        val pte = UInt(XLEN.W) // Page Table Entry
        val ptes = Vec(tlbcontiguous, UInt(XLEN.W)) // Page Table Entry Vector
        val cfs = Vec(tlbcontiguous, Bool()) // Bitmap Check Failed Vector
        val SPlevel = UInt(log2Up(Level).W)
      })
    }
    val stage1 = new PtwMergeResp()
    val isHptwReq = Bool()
    val toHptw = new Bundle {
      val l3Hit = if (EnableSv48) Some(Bool()) else None
      val l2Hit = Bool()
      val l1Hit = Bool()
      val ppn = UInt(ppnLen.W)
      val id = UInt(log2Up(l2tlbParams.llptwsize).W)
      val resp = new HptwResp() // used if hit
      val bypassed = Bool()
      val bitmapCheck = Option.when(HasBitmapCheck)(new Bundle {
        val jmp_bitmap_check = Bool() // find pte in l0 or sp, but need bitmap check
        val hitway = UInt(l2tlbParams.l0nWays.W)
        val pte = UInt(XLEN.W) // Page Table Entry
        val ptes = Vec(tlbcontiguous, UInt(XLEN.W)) // Page Table Entry Vector
        val cfs = Vec(tlbcontiguous, Bool()) // Bitmap Check Failed Vector
        val fromSP = Bool()
        val SPlevel = UInt(log2Up(Level).W)
      })
    }
  })
  val refill = Flipped(ValidIO(new Bundle {
    val ptes = UInt(blockBits.W)
    val levelOH = new Bundle {
      // NOTE: levelOH has (Level+1) bits, each stands for page cache entries
      val sp = Bool()
      val l0 = Bool()
      val l1 = Bool()
      val l2 = Bool()
      val l3 = if (EnableSv48) Some(Bool()) else None
      def apply(levelUInt: UInt, valid: Bool) = {
        sp := GatedValidRegNext((levelUInt === 1.U || levelUInt === 2.U || levelUInt === 3.U) && valid, false.B)
        l0 := GatedValidRegNext((levelUInt === 0.U) & valid, false.B)
        l1 := GatedValidRegNext((levelUInt === 1.U) & valid, false.B)
        l2 := GatedValidRegNext((levelUInt === 2.U) & valid, false.B)
        l3.map(_ := GatedValidRegNext((levelUInt === 3.U) & valid, false.B))
      }
    }
    // duplicate level and sel_pte for each page caches, for better fanout
    val req_info_dup = Vec(3, new L2TlbInnerBundle())
    val level_dup = Vec(3, UInt(log2Up(Level + 1).W))
    val sel_pte_dup = Vec(3, UInt(XLEN.W))
  }))
  // when refill l0,save way info for late bitmap wakeup convenient
  // valid at same cycle of refill.levelOH.l0
  val l0_way_info = Option.when(HasBitmapCheck)(Output(UInt(l2tlbParams.l0nWays.W)))
  val sfence_dup = Vec(4, Input(new SfenceBundle()))
  val csr_dup = Vec(3, Input(new TlbCsrBundle()))
  val bitmap_wakeup = Option.when(HasBitmapCheck)(Flipped(ValidIO(new Bundle {
    val setIndex = Input(UInt(PtwL0SetIdxLen.W))
    val tag = Input(UInt(SPTagLen.W))
    val isSp = Input(Bool())
    val way_info = UInt(l2tlbParams.l0nWays.W)
    val pte_index = UInt(sectortlbwidth.W)
    val check_success = Bool()
  })))
}

class PtwCache()(implicit p: Parameters) extends XSModule with HasPtwConst with HasPerfEvents {
  val io = IO(new PtwCacheIO)
  val ecc = Code.fromString(l2tlbParams.ecc)
  val l1EntryType = new PTWEntriesWithEcc(ecc, num = PtwL1SectorSize, tagLen = PtwL1TagLen, level = 1, hasPerm = false, ReservedBits = l2tlbParams.l1ReservedBits)
  val l0EntryType = new PTWEntriesWithEcc(ecc, num = PtwL0SectorSize, tagLen = PtwL0TagLen, level = 0, hasPerm = true, ReservedBits = l2tlbParams.l0ReservedBits)

  // use two additional regs to record corresponding cache entry whether via bitmap check
  // 32（l0nSets）* 8 (l0nWays) * 8 (tlbcontiguous)
  val l0BitmapReg = RegInit(VecInit(Seq.fill(l2tlbParams.l0nSets)(VecInit(Seq.fill(l2tlbParams.l0nWays)(VecInit(Seq.fill(tlbcontiguous)(0.U(1.W))))))))
  val spBitmapReg = RegInit(VecInit(Seq.fill(l2tlbParams.spSize)(0.U(1.W))))

  val bitmapEnable = io.csr_dup(0).mbmc.BME === 1.U && io.csr_dup(0).mbmc.CMODE === 0.U
  // TODO: four caches make the codes dirty, think about how to deal with it

  val sfence_dup = io.sfence_dup
  val refill = io.refill.bits
  val refill_prefetch_dup = io.refill.bits.req_info_dup.map(a => from_pre(a.source))
  val refill_h = io.refill.bits.req_info_dup.map(a => Mux(a.s2xlate === allStage, onlyStage1, a.s2xlate))
  val flush_dup = sfence_dup.zip(io.csr_dup).map(f => f._1.valid || f._2.satp.changed || f._2.vsatp.changed || f._2.hgatp.changed)
  val flush = flush_dup(0)

  // when refill, refuce to accept new req
  val rwHarzad = if (sramSinglePort) io.refill.valid else false.B

  // handle hand signal and req_info
  // TODO: replace with FlushableQueue
  val stageReq = Wire(Decoupled(new PtwCacheReq()))         // enq stage & read page cache valid
  val stageDelay = Wire(Vec(2, Decoupled(new PtwCacheReq()))) // page cache resp
  val stageCheck = Wire(Vec(2, Decoupled(new PtwCacheReq()))) // check hit & check ecc
  val stageResp = Wire(Decoupled(new PtwCacheReq()))         // deq stage

  val stageDelay_valid_1cycle = OneCycleValid(stageReq.fire, flush)      // catch ram data
  val stageCheck_valid_1cycle = OneCycleValid(stageDelay(1).fire, flush) // replace & perf counter
  val stageResp_valid_1cycle_dup = Wire(Vec(2, Bool()))
  stageResp_valid_1cycle_dup.map(_ := OneCycleValid(stageCheck(1).fire, flush))  // ecc flush

  stageReq <> io.req
  PipelineConnect(stageReq, stageDelay(0), stageDelay(1).ready, flush, rwHarzad)
  InsideStageConnect(stageDelay(0), stageDelay(1), stageDelay_valid_1cycle)
  PipelineConnect(stageDelay(1), stageCheck(0), stageCheck(1).ready, flush)
  InsideStageConnect(stageCheck(0), stageCheck(1), stageCheck_valid_1cycle)
  PipelineConnect(stageCheck(1), stageResp, io.resp.ready, flush)
  stageResp.ready := !stageResp.valid || io.resp.ready

  // l3: level 3 non-leaf pte
  val l3 = if (EnableSv48) Some(Reg(Vec(l2tlbParams.l3Size, new PtwEntry(tagLen = PtwL3TagLen)))) else None
  val l3v = if (EnableSv48) Some(RegInit(0.U(l2tlbParams.l3Size.W))) else None
  val l3g = if (EnableSv48) Some(Reg(UInt(l2tlbParams.l3Size.W))) else None
  val l3asids = if (EnableSv48) Some(l3.get.map(_.asid)) else None
  val l3vmids = if (EnableSv48) Some(l3.get.map(_.vmid)) else None
  val l3h = if (EnableSv48) Some(Reg(Vec(l2tlbParams.l3Size, UInt(2.W)))) else None

  // l2: level 2 non-leaf pte
  val l2 = Reg(Vec(l2tlbParams.l2Size, new PtwEntry(tagLen = PtwL2TagLen)))
  val l2v = RegInit(0.U(l2tlbParams.l2Size.W))
  val l2g = Reg(UInt(l2tlbParams.l2Size.W))
  val l2asids = l2.map(_.asid)
  val l2vmids = l2.map(_.vmid)
  val l2h = Reg(Vec(l2tlbParams.l2Size, UInt(2.W)))

  // l1: level 1 non-leaf pte
  val l1 = Module(new SplittedSRAM(
    l1EntryType,
    set = l2tlbParams.l1nSets,
    way = l2tlbParams.l1nWays,
    waySplit = 1,
    dataSplit = 4,
    singlePort = sramSinglePort,
    readMCP2 = false,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl
  ))
  val mbistPlL1 = MbistPipeline.PlaceMbistPipeline(1, s"MbistPipePtwL1", hasMbist)
  val l1v = RegInit(0.U((l2tlbParams.l1nSets * l2tlbParams.l1nWays).W))
  val l1g = Reg(UInt((l2tlbParams.l1nSets * l2tlbParams.l1nWays).W))
  val l1h = Reg(Vec(l2tlbParams.l1nSets, Vec(l2tlbParams.l1nWays, UInt(2.W))))
  val l1asids = Reg(Vec(l2tlbParams.l1nSets, Vec(l2tlbParams.l1nWays, UInt(l2tlbParams.hashAsidWidth.W))))
  val l1vmids = Reg(Vec(l2tlbParams.l1nSets, Vec(l2tlbParams.l1nWays, UInt(l2tlbParams.hashAsidWidth.W))))
  def getl1vSet(vpn: UInt) = {
    require(log2Up(l2tlbParams.l1nWays) == log2Down(l2tlbParams.l1nWays))
    val set = genPtwL1SetIdx(vpn)
    require(set.getWidth == log2Up(l2tlbParams.l1nSets))
    val l1vVec = l1v.asTypeOf(Vec(l2tlbParams.l1nSets, UInt(l2tlbParams.l1nWays.W)))
    l1vVec(set)
  }
  def getl1hSet(vpn: UInt) = {
    require(log2Up(l2tlbParams.l1nWays) == log2Down(l2tlbParams.l1nWays))
    val set = genPtwL1SetIdx(vpn)
    require(set.getWidth == log2Up(l2tlbParams.l1nSets))
    l1h(set)
  }

  // l0: level 0 leaf pte of 4KB pages
  val l0 = Module(new SplittedSRAM(
    l0EntryType,
    set = l2tlbParams.l0nSets,
    way = l2tlbParams.l0nWays,
    waySplit = 2,
    dataSplit = 4,
    singlePort = sramSinglePort,
    readMCP2 = false,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl
  ))
  val mbistPlL0 = MbistPipeline.PlaceMbistPipeline(1, s"MbistPipePtwL0", hasMbist)
  val l0v = RegInit(0.U((l2tlbParams.l0nSets * l2tlbParams.l0nWays).W))
  val l0g = Reg(UInt((l2tlbParams.l0nSets * l2tlbParams.l0nWays).W))
  val l0h = Reg(Vec(l2tlbParams.l0nSets, Vec(l2tlbParams.l0nWays, UInt(2.W))))
  val l0asids = Reg(Vec(l2tlbParams.l0nSets, Vec(l2tlbParams.l0nWays, UInt(l2tlbParams.hashAsidWidth.W))))
  val l0vmids = Reg(Vec(l2tlbParams.l0nSets, Vec(l2tlbParams.l0nWays, UInt(l2tlbParams.hashAsidWidth.W))))
  val l0vpns = Reg(Vec(l2tlbParams.l0nSets, Vec(l2tlbParams.l0nWays, UInt(l2tlbParams.hashVpnWidth.W))))
  def getl0vSet(vpn: UInt) = {
    require(log2Up(l2tlbParams.l0nWays) == log2Down(l2tlbParams.l0nWays))
    val set = genPtwL0SetIdx(vpn)
    require(set.getWidth == log2Up(l2tlbParams.l0nSets))
    val l0vVec = l0v.asTypeOf(Vec(l2tlbParams.l0nSets, UInt(l2tlbParams.l0nWays.W)))
    l0vVec(set)
  }
  def getl0hSet(vpn: UInt) = {
    require(log2Up(l2tlbParams.l0nWays) == log2Down(l2tlbParams.l0nWays))
    val set = genPtwL0SetIdx(vpn)
    require(set.getWidth == log2Up(l2tlbParams.l0nSets))
    l0h(set)
  }

  // sp: level 1/2/3 leaf pte of 512GB/1GB/2MB super pages
  val sp = Reg(Vec(l2tlbParams.spSize, new PtwEntry(tagLen = SPTagLen, hasPerm = true, hasLevel = true, hasNapot = true)))
  val spv = RegInit(0.U(l2tlbParams.spSize.W))
  val spg = Reg(UInt(l2tlbParams.spSize.W))
  val spasids = sp.map(_.asid)
  val spvmids = sp.map(_.vmid)
  val sph = Reg(Vec(l2tlbParams.spSize, UInt(2.W)))

  if (HasBitmapCheck) {
    // wakeup corresponding entry
    when (io.bitmap_wakeup.get.valid) {
      when (io.bitmap_wakeup.get.bits.isSp) {
        for (i <- 0 until l2tlbParams.spSize) {
          when (sp(i).tag === io.bitmap_wakeup.get.bits.tag && spv(i) === 1.U) {
            spBitmapReg(i) := io.bitmap_wakeup.get.bits.check_success
          }
        }
      } .otherwise {
        val wakeup_setindex = io.bitmap_wakeup.get.bits.setIndex
        l0BitmapReg(wakeup_setindex)(OHToUInt(io.bitmap_wakeup.get.bits.way_info))(io.bitmap_wakeup.get.bits.pte_index) := io.bitmap_wakeup.get.bits.check_success
        assert(l0v(wakeup_setindex * l2tlbParams.l0nWays.U + OHToUInt(io.bitmap_wakeup.get.bits.way_info)) === 1.U,
          "Wakeuped entry must be valid!")
      }
    }
  }

  // Access Perf
  val l3AccessPerf = if(EnableSv48) Some(Wire(Vec(l2tlbParams.l3Size, Bool()))) else None
  val l2AccessPerf = Wire(Vec(l2tlbParams.l2Size, Bool()))
  val l1AccessPerf = Wire(Vec(l2tlbParams.l1nWays, Bool()))
  val l0AccessPerf = Wire(Vec(l2tlbParams.l0nWays, Bool()))
  val spAccessPerf = Wire(Vec(l2tlbParams.spSize, Bool()))
  if (EnableSv48) l3AccessPerf.map(_.map(_ := false.B))
  l2AccessPerf.map(_ := false.B)
  l1AccessPerf.map(_ := false.B)
  l0AccessPerf.map(_ := false.B)
  spAccessPerf.map(_ := false.B)



  def vpn_match(vpn1: UInt, vpn2: UInt, level: Int) = {
    (vpn1(vpnLen-1, vpnnLen*level+3) === vpn2(vpnLen-1, vpnnLen*level+3))
  }
  // NOTE: not actually bypassed, just check if hit, re-access the page cache
  def refill_bypass(vpn: UInt, level: Int, h_search: UInt) = {
    val change_h = MuxLookup(h_search, noS2xlate)(Seq(
      allStage -> onlyStage1,
      onlyStage1 -> onlyStage1,
      onlyStage2 -> onlyStage2
    ))
    val change_refill_h = MuxLookup(io.refill.bits.req_info_dup(0).s2xlate, noS2xlate)(Seq(
      allStage -> onlyStage1,
      onlyStage1 -> onlyStage1,
      onlyStage2 -> onlyStage2
    ))
    val refill_vpn = io.refill.bits.req_info_dup(0).vpn
    io.refill.valid && (level.U === io.refill.bits.level_dup(0)) && vpn_match(refill_vpn, vpn, level) && change_h === change_refill_h
  }

  val vpn_search = stageReq.bits.req_info.vpn
  val h_search = MuxLookup(stageReq.bits.req_info.s2xlate, noS2xlate)(Seq(
    allStage -> onlyStage1,
    onlyStage1 -> onlyStage1,
    onlyStage2 -> onlyStage2
  ))

  // l3
  val l3Hit = if(EnableSv48) Some(Wire(Bool())) else None
  val l3HitPPN = if(EnableSv48) Some(Wire(UInt(gvpnLen.W))) else None
  val l3HitPbmt = if(EnableSv48) Some(Wire(UInt(ptePbmtLen.W))) else None
  val l3Pre = if(EnableSv48) Some(Wire(Bool())) else None
  val ptwl3replace = if(EnableSv48) Some(ReplacementPolicy.fromString(l2tlbParams.l3Replacer, l2tlbParams.l3Size)) else None
  if (EnableSv48) {
    val hitVecT = l3.get.zipWithIndex.map {
        case (e, i) => (e.hit(vpn_search, io.csr_dup(2).satp.asid, io.csr_dup(2).vsatp.asid, io.csr_dup(2).hgatp.vmid, s2xlate = h_search =/= noS2xlate)
          && l3v.get(i) && h_search === l3h.get(i))
    }
    val hitVec = hitVecT.map(RegEnable(_, stageReq.fire))

    // stageDelay, but check for l3
    val hitPPN = DataHoldBypass(ParallelPriorityMux(hitVec zip l3.get.map(_.ppn)), stageDelay_valid_1cycle)
    val hitPbmt = DataHoldBypass(ParallelPriorityMux(hitVec zip l3.get.map(_.pbmt)), stageDelay_valid_1cycle)
    val hitPre = DataHoldBypass(ParallelPriorityMux(hitVec zip l3.get.map(_.prefetch)), stageDelay_valid_1cycle)
    val hit = DataHoldBypass(ParallelOR(hitVec), stageDelay_valid_1cycle)

    when (hit && stageDelay_valid_1cycle) { ptwl3replace.get.access(OHToUInt(hitVec)) }

    l3AccessPerf.get.zip(hitVec).map{ case (l, h) => l := h && stageDelay_valid_1cycle}
    for (i <- 0 until l2tlbParams.l3Size) {
      XSDebug(stageReq.fire, p"[l3] l3(${i.U}) ${l3.get(i)} hit:${l3.get(i).hit(vpn_search, io.csr_dup(2).satp.asid, io.csr_dup(2).vsatp.asid, io.csr_dup(2).hgatp.vmid, s2xlate = h_search =/= noS2xlate)}\n")
    }
    XSDebug(stageReq.fire, p"[l3] l3v:${Binary(l3v.get)} hitVecT:${Binary(VecInit(hitVecT).asUInt)}\n")
    XSDebug(stageDelay(0).valid, p"[l3] l3Hit:${hit} l3HitPPN:0x${Hexadecimal(hitPPN)} hitVec:${VecInit(hitVec).asUInt}\n")

    VecInit(hitVecT).suggestName(s"l3_hitVecT")
    VecInit(hitVec).suggestName(s"l3_hitVec")

    // synchronize with other entries with RegEnable
    l3Hit.map(_ := RegEnable(hit, stageDelay(1).fire))
    l3HitPPN.map(_ := RegEnable(hitPPN, stageDelay(1).fire))
    l3HitPbmt.map(_ := RegEnable(hitPbmt, stageDelay(1).fire))
    l3Pre.map(_ := RegEnable(hitPre, stageDelay(1).fire))
  }

  // l2
  val ptwl2replace = ReplacementPolicy.fromString(l2tlbParams.l2Replacer, l2tlbParams.l2Size)
  val (l2Hit, l2HitPPN, l2HitPbmt, l2Pre) = {
    val hitVecT = l2.zipWithIndex.map {
      case (e, i) => (e.hit(vpn_search, io.csr_dup(2).satp.asid, io.csr_dup(2).vsatp.asid, io.csr_dup(2).hgatp.vmid, s2xlate = h_search =/= noS2xlate)
        && l2v(i) && h_search === l2h(i))
    }
    val hitVec = hitVecT.map(RegEnable(_, stageReq.fire))

    // stageDelay, but check for l2
    val hitPPN = DataHoldBypass(ParallelPriorityMux(hitVec zip l2.map(_.ppn)), stageDelay_valid_1cycle)
    val hitPbmt = DataHoldBypass(ParallelPriorityMux(hitVec zip l2.map(_.pbmt)), stageDelay_valid_1cycle)
    val hitPre = DataHoldBypass(ParallelPriorityMux(hitVec zip l2.map(_.prefetch)), stageDelay_valid_1cycle)
    val hit = DataHoldBypass(ParallelOR(hitVec), stageDelay_valid_1cycle)

    when (hit && stageDelay_valid_1cycle) { ptwl2replace.access(OHToUInt(hitVec)) }

    l2AccessPerf.zip(hitVec).map{ case (l, h) => l := h && stageDelay_valid_1cycle}
    for (i <- 0 until l2tlbParams.l2Size) {
      XSDebug(stageReq.fire, p"[l2] l2(${i.U}) ${l2(i)} hit:${l2(i).hit(vpn_search, io.csr_dup(2).satp.asid, io.csr_dup(2).vsatp.asid, io.csr_dup(2).hgatp.vmid, s2xlate = h_search =/= noS2xlate)}\n")
    }
    XSDebug(stageReq.fire, p"[l2] l2v:${Binary(l2v)} hitVecT:${Binary(VecInit(hitVecT).asUInt)}\n")
    XSDebug(stageDelay(0).valid, p"[l2] l2Hit:${hit} l2HitPPN:0x${Hexadecimal(hitPPN)} hitVec:${VecInit(hitVec).asUInt}\n")

    VecInit(hitVecT).suggestName(s"l2_hitVecT")
    VecInit(hitVec).suggestName(s"l2_hitVec")

    // synchronize with other entries with RegEnable
    (RegEnable(hit, stageDelay(1).fire),
     RegEnable(hitPPN, stageDelay(1).fire),
     RegEnable(hitPbmt, stageDelay(1).fire),
     RegEnable(hitPre, stageDelay(1).fire))
  }

  // l1
  val ptwl1replace = ReplacementPolicy.fromString(l2tlbParams.l1Replacer,l2tlbParams.l1nWays,l2tlbParams.l1nSets)
  val (l1Hit, l1HitPPN, l1HitPbmt, l1Pre, l1eccError) = {
    val ridx = genPtwL1SetIdx(vpn_search)
    l1.io.r.req.valid := stageReq.fire
    l1.io.r.req.bits.apply(setIdx = ridx)
    val vVec_req = getl1vSet(vpn_search)
    val hVec_req = getl1hSet(vpn_search)

    // delay one cycle after sram read
    val delay_vpn = stageDelay(0).bits.req_info.vpn
    val delay_h = MuxLookup(stageDelay(0).bits.req_info.s2xlate, noS2xlate)(Seq(
      allStage -> onlyStage1,
      onlyStage1 -> onlyStage1,
      onlyStage2 -> onlyStage2
    ))
    val data_resp = DataHoldBypass(l1.io.r.resp.data, stageDelay_valid_1cycle)
    val vVec_delay = RegEnable(vVec_req, stageReq.fire)
    val hVec_delay = RegEnable(hVec_req, stageReq.fire)
    val hitVec_delay = VecInit(data_resp.zip(vVec_delay.asBools).zip(hVec_delay).map { case ((wayData, v), h) =>
      wayData.entries.hit(delay_vpn, io.csr_dup(1).satp.asid, io.csr_dup(1).vsatp.asid, io.csr_dup(1).hgatp.vmid, s2xlate = delay_h =/= noS2xlate) && v && (delay_h === h)})

    // check hit and ecc
    val check_vpn = stageCheck(0).bits.req_info.vpn
    val ramDatas = RegEnable(data_resp, stageDelay(1).fire)
    val vVec = RegEnable(vVec_delay, stageDelay(1).fire).asBools

    val hitVec = RegEnable(hitVec_delay, stageDelay(1).fire)
    val hitWayEntry = ParallelPriorityMux(hitVec zip ramDatas)
    val hitWayData = hitWayEntry.entries
    val hit = ParallelOR(hitVec)
    val hitWay = ParallelPriorityMux(hitVec zip (0 until l2tlbParams.l1nWays).map(_.U(log2Up(l2tlbParams.l1nWays).W)))
    val eccError = WireInit(false.B)
    if (l2tlbParams.enablePTWECC) {
      eccError := hitWayEntry.decode()
    } else {
      eccError := false.B
    }

    ridx.suggestName(s"l1_ridx")
    ramDatas.suggestName(s"l1_ramDatas")
    hitVec.suggestName(s"l1_hitVec")
    hitWayData.suggestName(s"l1_hitWayData")
    hitWay.suggestName(s"l1_hitWay")

    when (hit && stageCheck_valid_1cycle) { ptwl1replace.access(genPtwL1SetIdx(check_vpn), hitWay) }

    l1AccessPerf.zip(hitVec).map{ case (l, h) => l := h && stageCheck_valid_1cycle }
    XSDebug(stageDelay_valid_1cycle, p"[l1] ridx:0x${Hexadecimal(ridx)}\n")
    for (i <- 0 until l2tlbParams.l1nWays) {
      XSDebug(stageCheck_valid_1cycle, p"[l1] ramDatas(${i.U}) ${ramDatas(i)}  l1v:${vVec(i)}  hit:${hit}\n")
    }
    XSDebug(stageCheck_valid_1cycle, p"[l1] l1Hit:${hit} l1HitPPN:0x${Hexadecimal(hitWayData.ppns(genPtwL1SectorIdx(check_vpn)))} hitVec:${Binary(hitVec.asUInt)} hitWay:${hitWay} vidx:${vVec}\n")

    (hit, hitWayData.ppns(genPtwL1SectorIdx(check_vpn)), hitWayData.pbmts(genPtwL1SectorIdx(check_vpn)), hitWayData.prefetch, eccError)
  }
  val te = ClockGate.genTeSink
  val l0_masked_clock = ClockGate(te.cgen, stageReq.fire | (!flush_dup(0) && refill.levelOH.l0) | mbistPlL0.map(_.mbist.req).getOrElse(false.B), clock)
  val l1_masked_clock = ClockGate(te.cgen, stageReq.fire | (!flush_dup(1) && refill.levelOH.l1) | mbistPlL1.map(_.mbist.req).getOrElse(false.B), clock)
  l0.clock := l0_masked_clock
  l1.clock := l1_masked_clock
  // l0
  val ptwl0replace = ReplacementPolicy.fromString(l2tlbParams.l0Replacer,l2tlbParams.l0nWays,l2tlbParams.l0nSets)
  val (l0Hit, l0HitData, l0Pre, l0eccError, l0HitWay, l0BitmapCheckResult, l0JmpBitmapCheck) = {
    val ridx = genPtwL0SetIdx(vpn_search)
    l0.io.r.req.valid := stageReq.fire
    l0.io.r.req.bits.apply(setIdx = ridx)
    val vVec_req = getl0vSet(vpn_search)
    val hVec_req = getl0hSet(vpn_search)

    // delay one cycle after sram read
    val delay_vpn = stageDelay(0).bits.req_info.vpn
    val delay_h = MuxLookup(stageDelay(0).bits.req_info.s2xlate, noS2xlate)(Seq(
      allStage -> onlyStage1,
      onlyStage1 -> onlyStage1,
      onlyStage2 -> onlyStage2
    ))
    val data_resp = DataHoldBypass(l0.io.r.resp.data, stageDelay_valid_1cycle)
    val vVec_delay = RegEnable(vVec_req, stageReq.fire)
    val hVec_delay = RegEnable(hVec_req, stageReq.fire)
    val hitVec_delay = VecInit(data_resp.zip(vVec_delay.asBools).zip(hVec_delay).map { case ((wayData, v), h) =>
      wayData.entries.hit(delay_vpn, io.csr_dup(0).satp.asid, io.csr_dup(0).vsatp.asid, io.csr_dup(0).hgatp.vmid, s2xlate = delay_h =/= noS2xlate) && v && (delay_h === h)})

    // check hit and ecc
    val check_vpn = stageCheck(0).bits.req_info.vpn
    val ramDatas = RegEnable(data_resp, stageDelay(1).fire)
    val vVec = RegEnable(vVec_delay, stageDelay(1).fire).asBools

    val hitVec = RegEnable(hitVec_delay, stageDelay(1).fire)
    val hitWayEntry = ParallelPriorityMux(hitVec zip ramDatas)
    val hitWayData = hitWayEntry.entries
    val hitWayEcc = hitWayEntry.ecc
    val hitWay = ParallelPriorityMux(hitVec zip (0 until l2tlbParams.l0nWays).map(_.U(log2Up(l2tlbParams.l0nWays).W)))

    val ishptw = RegEnable(stageDelay(0).bits.isHptwReq,stageDelay(1).fire)
    val s2x_info = RegEnable(stageDelay(0).bits.req_info.s2xlate,stageDelay(1).fire)
    val pte_index = RegEnable(stageDelay(0).bits.req_info.vpn(sectortlbwidth - 1, 0),stageDelay(1).fire)
    val jmp_bitmap_check  = WireInit(false.B)
    val hit = WireInit(false.B)
    val l0bitmapreg = WireInit((VecInit(Seq.fill(l2tlbParams.l0nWays)(VecInit(Seq.fill(tlbcontiguous)(0.U(1.W)))))))
    if (HasBitmapCheck) {
      l0bitmapreg := RegEnable(RegNext(l0BitmapReg(ridx)), stageDelay(1).fire)
      // cause llptw will trigger bitmapcheck
      // add a coniditonal logic
      // (s2x_info =/= allStage || ishptw)
      hit := Mux(bitmapEnable && (s2x_info =/= allStage || ishptw), ParallelOR(hitVec) && l0bitmapreg(hitWay)(pte_index) === 1.U, ParallelOR(hitVec))
      when (bitmapEnable && (s2x_info =/= allStage || ishptw) && ParallelOR(hitVec) && l0bitmapreg(hitWay)(pte_index) === 0.U) {
        jmp_bitmap_check := true.B
      }
    } else {
      hit := ParallelOR(hitVec)
    }
    val eccError = WireInit(false.B)
    if (l2tlbParams.enablePTWECC) {
      eccError := hitWayEntry.decode()
    } else {
      eccError := false.B
    }

    when (hit && stageCheck_valid_1cycle) { ptwl0replace.access(genPtwL0SetIdx(check_vpn), hitWay) }

    l0AccessPerf.zip(hitVec).map{ case (l, h) => l := h && stageCheck_valid_1cycle }
    XSDebug(stageReq.fire, p"[l0] ridx:0x${Hexadecimal(ridx)}\n")
    for (i <- 0 until l2tlbParams.l0nWays) {
      XSDebug(stageCheck_valid_1cycle, p"[l0] ramDatas(${i.U}) ${ramDatas(i)}  l0v:${vVec(i)}  hit:${hitVec(i)}\n")
    }
    XSDebug(stageCheck_valid_1cycle, p"[l0] l0Hit:${hit} l0HitData:${hitWayData} hitVec:${Binary(hitVec.asUInt)} hitWay:${hitWay} v:${vVec}\n")

    ridx.suggestName(s"l0_ridx")
    ramDatas.suggestName(s"l0_ramDatas")
    hitVec.suggestName(s"l0_hitVec")
    hitWay.suggestName(s"l0_hitWay")

    (hit, hitWayData, hitWayData.prefetch, eccError, UIntToOH(hitWay), l0bitmapreg(hitWay), jmp_bitmap_check)
  }
  val l0HitPPN = l0HitData.ppns
  val l0HitPbmt = l0HitData.pbmts
  val l0HitPerm = l0HitData.perms.getOrElse(0.U.asTypeOf(Vec(PtwL0SectorSize, new PtePermBundle)))
  val l0HitValid = VecInit(l0HitData.onlypf.map(!_))
  val l0Ptes = WireInit(VecInit(Seq.fill(tlbcontiguous)(0.U(XLEN.W)))) // L0 lavel Page Table Entry Vector
  val l0cfs = WireInit(VecInit(Seq.fill(tlbcontiguous)(false.B))) // L0 lavel Bitmap Check Failed Vector
  if (HasBitmapCheck) {
    for (i <- 0 until tlbcontiguous) {
      l0Ptes(i) := Cat(l0HitData.pbmts(i).asUInt,l0HitPPN(i), 0.U(2.W),l0HitPerm(i).asUInt,l0HitValid(i).asUInt)
      l0cfs(i) := !l0BitmapCheckResult(i)
    }
  }

  // super page
  val spreplace = ReplacementPolicy.fromString(l2tlbParams.spReplacer, l2tlbParams.spSize)
  val (spHit, spHitData, spPre, spValid, spJmpBitmapCheck) = {
    val hitVecT = sp.zipWithIndex.map { case (e, i) => e.hit(vpn_search, io.csr_dup(0).satp.asid, io.csr_dup(0).vsatp.asid, io.csr_dup(0).hgatp.vmid, allType = true, s2xlate = h_search =/= noS2xlate) && spv(i) && (sph(i) === h_search) }
    val hitVec = hitVecT.map(RegEnable(_, stageReq.fire))
    val hitData = ParallelPriorityMux(hitVec zip sp)
    val ishptw = RegEnable(stageReq.bits.isHptwReq, stageReq.fire)
    val s2x_info = RegEnable(stageReq.bits.req_info.s2xlate, stageReq.fire)
    val jmp_bitmap_check  = WireInit(false.B)
    val hit = WireInit(false.B)
    if (HasBitmapCheck) {
      hit := Mux(bitmapEnable && (s2x_info =/= allStage || ishptw), ParallelOR(hitVec) && spBitmapReg(OHToUInt(hitVec)) === 1.U, ParallelOR(hitVec))
      when (bitmapEnable && (s2x_info =/= allStage || ishptw) && ParallelOR(hitVec) && spBitmapReg(OHToUInt(hitVec)) === 0.U) {
        jmp_bitmap_check := true.B
      }
    } else {
      hit := ParallelOR(hitVec)
    }

    when (hit && stageDelay_valid_1cycle) { spreplace.access(OHToUInt(hitVec)) }

    spAccessPerf.zip(hitVec).map{ case (s, h) => s := h && stageDelay_valid_1cycle }
    for (i <- 0 until l2tlbParams.spSize) {
      XSDebug(stageReq.fire, p"[sp] sp(${i.U}) ${sp(i)} hit:${sp(i).hit(vpn_search, io.csr_dup(0).satp.asid, io.csr_dup(0).vsatp.asid, io.csr_dup(0).hgatp.vmid, s2xlate = h_search =/= noS2xlate)} spv:${spv(i)}\n")
    }
    XSDebug(stageDelay_valid_1cycle, p"[sp] spHit:${hit} spHitData:${hitData} hitVec:${Binary(VecInit(hitVec).asUInt)}\n")

    VecInit(hitVecT).suggestName(s"sp_hitVecT")
    VecInit(hitVec).suggestName(s"sp_hitVec")

    (RegEnable(hit, stageDelay(1).fire),
     RegEnable(hitData, stageDelay(1).fire),
     RegEnable(hitData.prefetch, stageDelay(1).fire),
     RegEnable(hitData.v, stageDelay(1).fire),
     RegEnable(jmp_bitmap_check, stageDelay(1).fire))
  }
  val spHitPerm = spHitData.perm.getOrElse(0.U.asTypeOf(new PtePermBundle))
  val spHitLevel = spHitData.level.getOrElse(0.U)
  val spPte = Cat(spHitData.pbmt.asUInt,spHitData.ppn, 0.U(2.W), spHitPerm.asUInt,spHitData.v.asUInt) // Super-page Page Table Entry

  val check_res = Wire(new PageCacheRespBundle)
  check_res.l3.map(_.apply(l3Hit.get, l3Pre.get, l3HitPPN.get, l3HitPbmt.get))
  check_res.l2.apply(l2Hit, l2Pre, l2HitPPN, l2HitPbmt)
  check_res.l1.apply(l1Hit, l1Pre, l1HitPPN, l1HitPbmt, ecc = l1eccError)
  check_res.l0.apply(l0Hit, l0Pre, l0HitPPN, l0HitPbmt, l0HitPerm, l0eccError, valid = l0HitValid, jmp_bitmap_check = l0JmpBitmapCheck, hitway = l0HitWay, ptes = l0Ptes, cfs = l0cfs)
  check_res.sp.apply(spHit, spPre, spHitData.ppn, spHitData.pbmt, spHitData.n.getOrElse(0.U), spHitPerm, false.B, spHitLevel, spValid, spJmpBitmapCheck, spPte)

  val resp_res = Reg(new PageCacheRespBundle)
  when (stageCheck(1).fire) { resp_res := check_res }

  // stageResp bypass
  val bypassed = if (EnableSv48) Wire(Vec(4, Bool())) else Wire(Vec(3, Bool()))
  bypassed.indices.foreach(i =>
    bypassed(i) := stageResp.bits.bypassed(i) ||
      ValidHoldBypass(refill_bypass(stageResp.bits.req_info.vpn, i, stageResp.bits.req_info.s2xlate),
        OneCycleValid(stageCheck(1).fire, false.B) || io.refill.valid)
  )

  // stageResp bypass to hptw
  val hptw_bypassed = if (EnableSv48) Wire(Vec(4, Bool())) else Wire(Vec(3, Bool()))
  hptw_bypassed.indices.foreach(i =>
    hptw_bypassed(i) := stageResp.bits.bypassed(i) ||
      ValidHoldBypass(refill_bypass(stageResp.bits.req_info.vpn, i, stageResp.bits.req_info.s2xlate),
        io.resp.fire)
  )

  val isAllStage = stageResp.bits.req_info.s2xlate === allStage
  val isOnlyStage2 = stageResp.bits.req_info.s2xlate === onlyStage2
  val stage1Hit = (resp_res.l0.hit || resp_res.sp.hit) && isAllStage
  val idx = stageResp.bits.req_info.vpn(2, 0)
  val stage1Pf = !Mux(resp_res.l0.hit, resp_res.l0.v(idx), resp_res.sp.v)
  io.resp.bits.req_info   := stageResp.bits.req_info
  io.resp.bits.isFirst  := stageResp.bits.isFirst
  io.resp.bits.hit      := (resp_res.l0.hit || resp_res.sp.hit) && (!isAllStage || isAllStage && stage1Pf)
  if (EnableSv48) {
    io.resp.bits.bypassed := ((bypassed(0) && !resp_res.l0.hit) || (bypassed(1) && !resp_res.l1.hit) || (bypassed(2) && !resp_res.l2.hit) || (bypassed(3) && !resp_res.l3.get.hit)) && !isAllStage
  } else {
    io.resp.bits.bypassed := ((bypassed(0) && !resp_res.l0.hit) || (bypassed(1) && !resp_res.l1.hit) || (bypassed(2) && !resp_res.l2.hit)) && !isAllStage
  }
  io.resp.bits.prefetch := resp_res.l0.pre && resp_res.l0.hit || resp_res.sp.pre && resp_res.sp.hit
  io.resp.bits.toFsm.l3Hit.map(_ := resp_res.l3.get.hit && !stage1Hit && !isOnlyStage2 && !stageResp.bits.isHptwReq)
  io.resp.bits.toFsm.l2Hit := resp_res.l2.hit && !stage1Hit && !isOnlyStage2 && !stageResp.bits.isHptwReq
  io.resp.bits.toFsm.l1Hit := resp_res.l1.hit && !stage1Hit && !isOnlyStage2 && !stageResp.bits.isHptwReq
  io.resp.bits.toFsm.ppn   := Mux(resp_res.l1.hit, resp_res.l1.ppn, Mux(resp_res.l2.hit, resp_res.l2.ppn, resp_res.l3.getOrElse(0.U.asTypeOf(new PageCachePerPespBundle)).ppn))
  io.resp.bits.toFsm.stage1Hit := stage1Hit
  if (HasBitmapCheck) {
    io.resp.bits.toFsm.bitmapCheck.get.jmp_bitmap_check := resp_res.l0.bitmapCheck.get.jmp_bitmap_check || resp_res.sp.bitmapCheck.get.jmp_bitmap_check
    io.resp.bits.toFsm.bitmapCheck.get.toLLPTW := resp_res.l0.bitmapCheck.get.jmp_bitmap_check && (stageResp.bits.req_info.s2xlate === noS2xlate || stageResp.bits.req_info.s2xlate === onlyStage1)
    io.resp.bits.toFsm.bitmapCheck.get.hitway := resp_res.l0.bitmapCheck.get.hitway
    io.resp.bits.toFsm.bitmapCheck.get.pte := resp_res.sp.bitmapCheck.get.pte
    io.resp.bits.toFsm.bitmapCheck.get.ptes := resp_res.l0.bitmapCheck.get.ptes
    io.resp.bits.toFsm.bitmapCheck.get.cfs := resp_res.l0.bitmapCheck.get.cfs
    io.resp.bits.toFsm.bitmapCheck.get.SPlevel := resp_res.sp.level
  }

  io.resp.bits.isHptwReq := stageResp.bits.isHptwReq
  if (EnableSv48) {
    io.resp.bits.toHptw.bypassed := ((hptw_bypassed(0) && !resp_res.l0.hit) || (hptw_bypassed(1) && !resp_res.l1.hit) || (hptw_bypassed(2) && !resp_res.l2.hit) || (hptw_bypassed(3) && !resp_res.l3.get.hit)) && stageResp.bits.isHptwReq
  } else {
    io.resp.bits.toHptw.bypassed := ((hptw_bypassed(0) && !resp_res.l0.hit) || (hptw_bypassed(1) && !resp_res.l1.hit) || (hptw_bypassed(2) && !resp_res.l2.hit)) && stageResp.bits.isHptwReq
  }
  io.resp.bits.toHptw.id := stageResp.bits.hptwId
  io.resp.bits.toHptw.l3Hit.map(_ := resp_res.l3.get.hit && stageResp.bits.isHptwReq)
  io.resp.bits.toHptw.l2Hit := resp_res.l2.hit && stageResp.bits.isHptwReq
  io.resp.bits.toHptw.l1Hit := resp_res.l1.hit && stageResp.bits.isHptwReq
  io.resp.bits.toHptw.ppn := Mux(resp_res.l1.hit, resp_res.l1.ppn, Mux(resp_res.l2.hit, resp_res.l2.ppn, resp_res.l3.getOrElse(0.U.asTypeOf(new PageCachePerPespBundle)).ppn))(ppnLen - 1, 0)
  io.resp.bits.toHptw.resp.entry.tag := stageResp.bits.req_info.vpn
  io.resp.bits.toHptw.resp.entry.asid := DontCare
  io.resp.bits.toHptw.resp.entry.vmid.map(_ := io.csr_dup(0).hgatp.vmid)
  io.resp.bits.toHptw.resp.entry.level.map(_ := Mux(resp_res.l0.hit, 0.U, resp_res.sp.level))
  io.resp.bits.toHptw.resp.entry.prefetch := from_pre(stageResp.bits.req_info.source)
  io.resp.bits.toHptw.resp.entry.ppn := Mux(resp_res.l0.hit, resp_res.l0.ppn(idx), resp_res.sp.ppn)(ppnLen - 1, 0)
  io.resp.bits.toHptw.resp.entry.pbmt := Mux(resp_res.l0.hit, resp_res.l0.pbmt(idx), resp_res.sp.pbmt)
  io.resp.bits.toHptw.resp.entry.n.map(_ := Mux(resp_res.sp.hit, resp_res.sp.n, 0.U))
  io.resp.bits.toHptw.resp.entry.perm.map(_ := Mux(resp_res.l0.hit, resp_res.l0.perm(idx), resp_res.sp.perm))
  io.resp.bits.toHptw.resp.entry.v := Mux(resp_res.l0.hit, resp_res.l0.v(idx), resp_res.sp.v)
  io.resp.bits.toHptw.resp.gpf := !io.resp.bits.toHptw.resp.entry.v
  io.resp.bits.toHptw.resp.gaf := false.B
  if (HasBitmapCheck) {
    io.resp.bits.toHptw.bitmapCheck.get.jmp_bitmap_check := resp_res.l0.bitmapCheck.get.jmp_bitmap_check || resp_res.sp.bitmapCheck.get.jmp_bitmap_check
    io.resp.bits.toHptw.bitmapCheck.get.hitway := resp_res.l0.bitmapCheck.get.hitway
    io.resp.bits.toHptw.bitmapCheck.get.pte := resp_res.sp.bitmapCheck.get.pte
    io.resp.bits.toHptw.bitmapCheck.get.ptes := resp_res.l0.bitmapCheck.get.ptes
    io.resp.bits.toHptw.bitmapCheck.get.cfs := resp_res.l0.bitmapCheck.get.cfs
    io.resp.bits.toHptw.bitmapCheck.get.fromSP := resp_res.sp.bitmapCheck.get.jmp_bitmap_check
    io.resp.bits.toHptw.bitmapCheck.get.SPlevel := resp_res.sp.level
  }

  io.resp.bits.stage1.entry.map(_.tag := stageResp.bits.req_info.vpn(vpnLen - 1, 3))
  io.resp.bits.stage1.entry.map(_.asid := Mux(stageResp.bits.req_info.hasS2xlate(), io.csr_dup(0).vsatp.asid, io.csr_dup(0).satp.asid)) // DontCare
  io.resp.bits.stage1.entry.map(_.vmid.map(_ := io.csr_dup(0).hgatp.vmid))
  if (EnableSv48) {
    io.resp.bits.stage1.entry.map(_.level.map(_ := Mux(resp_res.l0.hit, 0.U,
      Mux(resp_res.sp.hit, resp_res.sp.level,
        Mux(resp_res.l1.hit, 1.U,
          Mux(resp_res.l2.hit, 2.U, 3.U))))))
  } else {
    io.resp.bits.stage1.entry.map(_.level.map(_ := Mux(resp_res.l0.hit, 0.U,
      Mux(resp_res.sp.hit, resp_res.sp.level,
        Mux(resp_res.l1.hit, 1.U, 2.U)))))
  }
  io.resp.bits.stage1.entry.map(_.prefetch := from_pre(stageResp.bits.req_info.source))
  for (i <- 0 until tlbcontiguous) {
    if (EnableSv48) {
      io.resp.bits.stage1.entry(i).ppn := Mux(resp_res.l0.hit, resp_res.l0.ppn(i)(gvpnLen - 1, sectortlbwidth),
        Mux(resp_res.sp.hit, resp_res.sp.ppn(gvpnLen - 1, sectortlbwidth),
          Mux(resp_res.l1.hit, resp_res.l1.ppn(gvpnLen - 1, sectortlbwidth),
            Mux(resp_res.l2.hit, resp_res.l2.ppn(gvpnLen - 1, sectortlbwidth),
              resp_res.l3.get.ppn(gvpnLen - 1, sectortlbwidth)))))
      io.resp.bits.stage1.entry(i).ppn_low := Mux(resp_res.l0.hit, resp_res.l0.ppn(i)(sectortlbwidth - 1, 0),
        Mux(resp_res.sp.hit, resp_res.sp.ppn(sectortlbwidth - 1, 0),
          Mux(resp_res.l1.hit, resp_res.l1.ppn(sectortlbwidth - 1, 0),
            Mux(resp_res.l2.hit, resp_res.l2.ppn(sectortlbwidth - 1, 0),
              resp_res.l3.get.ppn(sectortlbwidth - 1, 0)))))
      io.resp.bits.stage1.entry(i).v := Mux(resp_res.l0.hit, resp_res.l0.v(i),
        Mux(resp_res.sp.hit, resp_res.sp.v,
          Mux(resp_res.l1.hit, resp_res.l1.v,
            Mux(resp_res.l2.hit, resp_res.l2.v,
              resp_res.l3.get.v))))
    } else {
      io.resp.bits.stage1.entry(i).ppn := Mux(resp_res.l0.hit, resp_res.l0.ppn(i)(gvpnLen - 1, sectortlbwidth),
        Mux(resp_res.sp.hit, resp_res.sp.ppn(gvpnLen - 1, sectortlbwidth),
          Mux(resp_res.l1.hit, resp_res.l1.ppn(gvpnLen - 1, sectortlbwidth),
            resp_res.l2.ppn(gvpnLen - 1, sectortlbwidth))))
      io.resp.bits.stage1.entry(i).ppn_low := Mux(resp_res.l0.hit, resp_res.l0.ppn(i)(sectortlbwidth - 1, 0),
        Mux(resp_res.sp.hit, resp_res.sp.ppn(sectortlbwidth - 1, 0),
          Mux(resp_res.l1.hit, resp_res.l1.ppn(sectortlbwidth - 1, 0),
            resp_res.l2.ppn(sectortlbwidth - 1, 0))))
      io.resp.bits.stage1.entry(i).v := Mux(resp_res.l0.hit, resp_res.l0.v(i),
        Mux(resp_res.sp.hit, resp_res.sp.v,
          Mux(resp_res.l1.hit, resp_res.l1.v,
            resp_res.l2.v)))
    }
    io.resp.bits.stage1.entry(i).pbmt := Mux(resp_res.l0.hit, resp_res.l0.pbmt(i),
      Mux(resp_res.sp.hit, resp_res.sp.pbmt,
        Mux(resp_res.l1.hit, resp_res.l1.pbmt,
          resp_res.l2.pbmt)))
    io.resp.bits.stage1.entry(i).n.map(_ := Mux(resp_res.sp.hit, resp_res.sp.n, 0.U))
    io.resp.bits.stage1.entry(i).perm.map(_ := Mux(resp_res.l0.hit, resp_res.l0.perm(i),  Mux(resp_res.sp.hit, resp_res.sp.perm, 0.U.asTypeOf(new PtePermBundle))))
    io.resp.bits.stage1.entry(i).pf := !io.resp.bits.stage1.entry(i).v
    io.resp.bits.stage1.entry(i).af := false.B
    io.resp.bits.stage1.entry(i).cf := l0cfs(i) // L0 lavel Bitmap Check Failed Vector
  }
  io.resp.bits.stage1.pteidx := UIntToOH(idx).asBools
  io.resp.bits.stage1.not_super := Mux(resp_res.l0.hit, true.B, false.B)
  io.resp.bits.stage1.not_merge := false.B
  io.resp.valid := stageResp.valid
  XSError(stageResp.valid && resp_res.l0.hit && resp_res.sp.hit, "normal page and super page both hit")

  // refill Perf
  val l3RefillPerf = if (EnableSv48) Some(Wire(Vec(l2tlbParams.l3Size, Bool()))) else None
  val l2RefillPerf = Wire(Vec(l2tlbParams.l2Size, Bool()))
  val l1RefillPerf = Wire(Vec(l2tlbParams.l1nWays, Bool()))
  val l0RefillPerf = Wire(Vec(l2tlbParams.l0nWays, Bool()))
  val spRefillPerf = Wire(Vec(l2tlbParams.spSize, Bool()))
  l3RefillPerf.map(_.map(_ := false.B))
  l2RefillPerf.map(_ := false.B)
  l1RefillPerf.map(_ := false.B)
  l0RefillPerf.map(_ := false.B)
  spRefillPerf.map(_ := false.B)

  // refill
  l1.io.w.req <> DontCare
  l0.io.w.req <> DontCare
  l1.io.w.req.valid := false.B
  l0.io.w.req.valid := false.B

  val memRdata = refill.ptes
  val memPtes = (0 until (l2tlbParams.blockBytes/(XLEN/8))).map(i => memRdata((i+1)*XLEN-1, i*XLEN).asTypeOf(new PteBundle))
  val memSelData = io.refill.bits.sel_pte_dup
  val memPte = memSelData.map(a => a.asTypeOf(new PteBundle))
  val mPBMTE = io.csr.mPBMTE
  val hPBMTE = io.csr.hPBMTE
  val pbmte = Mux(refill.req_info_dup(0).s2xlate === onlyStage1 || refill.req_info_dup(0).s2xlate === allStage, hPBMTE, mPBMTE)

  def Tran2D(flushMask: UInt): Vec[UInt] = {
    val tran2D = Wire(Vec(l2tlbParams.l0nSets,UInt(l2tlbParams.l0nWays.W)))
    for (i <- 0 until l2tlbParams.l0nSets) {
      tran2D(i) := flushMask((i + 1) * l2tlbParams.l0nWays - 1, i * l2tlbParams.l0nWays)
    }
    tran2D
  }
  def updateL0BitmapReg(l0BitmapReg: Vec[Vec[Vec[UInt]]], tran2D: Vec[UInt]) = {
    for (i <- 0 until l2tlbParams.l0nSets) {
      for (j <- 0 until l2tlbParams.l0nWays) {
        when (tran2D(i)(j) === 0.U) {
          for (k <- 0 until tlbcontiguous) {
            l0BitmapReg(i)(j)(k) := 0.U
          }
        }
      }
    }
  }
  def TranVec(flushMask: UInt): Vec[UInt] = {
    val vec = Wire(Vec(l2tlbParams.spSize,UInt(1.W)))
    for (i <- 0 until l2tlbParams.spSize) {
      vec(i) := flushMask(i)
    }
    vec
  }
  def updateSpBitmapReg(spBitmapReg: Vec[UInt], vec : Vec[UInt]) = {
    for (i <- 0 until l2tlbParams.spSize) {
      spBitmapReg(i) := spBitmapReg(i) & vec(i)
    }
  }

  // TODO: handle sfenceLatch outsize
  if (EnableSv48) {
    val l3Refill =
      !flush_dup(2) &&
      refill.levelOH.l3.get &&
      !memPte(2).isLeaf() &&
      memPte(2).canRefill(refill.level_dup(2), refill.req_info_dup(2).s2xlate, pbmte, io.csr_dup(2).hgatp.mode)
    val l3RefillIdx = replaceWrapper(l3v.get, ptwl3replace.get.way).suggestName(s"l3_refillIdx")
    val l3RfOH = UIntToOH(l3RefillIdx).asUInt.suggestName(s"l3_rfOH")
    when (l3Refill) {
      l3.get(l3RefillIdx).refill(
        refill.req_info_dup(2).vpn,
        Mux(refill.req_info_dup(2).s2xlate =/= noS2xlate, io.csr_dup(2).vsatp.asid, io.csr_dup(2).satp.asid),
        io.csr_dup(2).hgatp.vmid,
        memSelData(2),
        3.U,
        refill_prefetch_dup(2)
      )
      ptwl2replace.access(l3RefillIdx)
      l3v.get := l3v.get | l3RfOH
      l3g.get := (l3g.get & ~l3RfOH) | Mux(memPte(2).perm.g, l3RfOH, 0.U)
      l3h.get(l3RefillIdx) := refill_h(2)

      for (i <- 0 until l2tlbParams.l3Size) {
        l3RefillPerf.get(i) := i.U === l3RefillIdx
      }
    }
    XSDebug(l3Refill, p"[l3 refill] refillIdx:${l3RefillIdx} refillEntry:${l3.get(l3RefillIdx).genPtwEntry(refill.req_info_dup(2).vpn, Mux(refill.req_info_dup(2).s2xlate =/= noS2xlate, io.csr_dup(2).vsatp.asid, io.csr_dup(2).satp.asid), memSelData(2), 0.U, prefetch = refill_prefetch_dup(2))}\n")
    XSDebug(l3Refill, p"[l3 refill] l3v:${Binary(l3v.get)}->${Binary(l3v.get | l3RfOH)} l3g:${Binary(l3g.get)}->${Binary((l3g.get & ~l3RfOH) | Mux(memPte(2).perm.g, l3RfOH, 0.U))}\n")
  }

  // L2 refill
  val l2Refill =
    !flush_dup(2) &&
    refill.levelOH.l2 &&
    !memPte(2).isLeaf() &&
    memPte(2).canRefill(refill.level_dup(2), refill.req_info_dup(2).s2xlate, pbmte, io.csr_dup(2).hgatp.mode)
  val l2RefillIdx = replaceWrapper(l2v, ptwl2replace.way).suggestName(s"l2_refillIdx")
  val l2RfOH = UIntToOH(l2RefillIdx).asUInt.suggestName(s"l2_rfOH")
  when (
    l2Refill
  ) {
    l2(l2RefillIdx).refill(
      refill.req_info_dup(2).vpn,
      Mux(refill.req_info_dup(2).s2xlate =/= noS2xlate, io.csr_dup(2).vsatp.asid, io.csr_dup(2).satp.asid),
      io.csr_dup(2).hgatp.vmid,
      memSelData(2),
      2.U,
      refill_prefetch_dup(2)
    )
    ptwl2replace.access(l2RefillIdx)
    l2v := l2v | l2RfOH
    l2g := (l2g & ~l2RfOH) | Mux(memPte(2).perm.g, l2RfOH, 0.U)
    l2h(l2RefillIdx) := refill_h(2)

    for (i <- 0 until l2tlbParams.l2Size) {
      l2RefillPerf(i) := i.U === l2RefillIdx
    }
  }
  XSDebug(l2Refill, p"[l2 refill] refillIdx:${l2RefillIdx} refillEntry:${l2(l2RefillIdx).genPtwEntry(refill.req_info_dup(2).vpn, Mux(refill.req_info_dup(2).s2xlate =/= noS2xlate, io.csr_dup(2).vsatp.asid, io.csr_dup(2).satp.asid), memSelData(2), 0.U, prefetch = refill_prefetch_dup(2))}\n")
  XSDebug(l2Refill, p"[l2 refill] l2v:${Binary(l2v)}->${Binary(l2v | l2RfOH)} l2g:${Binary(l2g)}->${Binary((l2g & ~l2RfOH) | Mux(memPte(2).perm.g, l2RfOH, 0.U))}\n")

  // L1 refill
  val l1Refill = !flush_dup(1) && refill.levelOH.l1
  val l1RefillIdx = genPtwL1SetIdx(refill.req_info_dup(1).vpn).suggestName(s"l1_refillIdx")
  val l1VictimWay = replaceWrapper(getl1vSet(refill.req_info_dup(1).vpn), ptwl1replace.way(l1RefillIdx)).suggestName(s"l1_victimWay")
  val l1VictimWayOH = UIntToOH(l1VictimWay).suggestName(s"l1_victimWayOH")
  val l1RfvOH = UIntToOH(Cat(l1RefillIdx, l1VictimWay)).asUInt.suggestName(s"l1_rfvOH")
  val l1Wdata = Wire(l1EntryType)
  val l1Wvpn = refill.req_info_dup(1).vpn
  val l1Wasid = Mux(refill.req_info_dup(1).s2xlate =/= noS2xlate, io.csr_dup(1).vsatp.asid, io.csr_dup(1).satp.asid)
  l1Wdata.gen(
    vpn = l1Wvpn,
    asid = l1Wasid,
    vmid = io.csr_dup(1).hgatp.vmid,
    data = memRdata,
    levelUInt = 1.U,
    refill_prefetch_dup(1),
    refill.req_info_dup(1).s2xlate,
    pbmte,
    io.csr_dup(1).hgatp.mode
  )
  when (l1Refill) {
    l1.io.w.apply(
      valid = true.B,
      setIdx = l1RefillIdx,
      data = l1Wdata,
      waymask = l1VictimWayOH
    )
    ptwl1replace.access(l1RefillIdx, l1VictimWay)
    l1v := l1v | l1RfvOH
    l1g := l1g & ~l1RfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, l1RfvOH, 0.U)
    l1h(l1RefillIdx)(l1VictimWay) := refill_h(1)
    l1asids(l1RefillIdx)(l1VictimWay) := XORFold(l1Wasid, l2tlbParams.hashAsidWidth)
    l1vmids(l1RefillIdx)(l1VictimWay) := XORFold(io.csr_dup(1).hgatp.vmid, l2tlbParams.hashAsidWidth)

    for (i <- 0 until l2tlbParams.l1nWays) {
      l1RefillPerf(i) := i.U === l1VictimWay
    }
  }
  XSDebug(l1Refill, p"[l1 refill] refillIdx:0x${Hexadecimal(l1RefillIdx)} victimWay:${l1VictimWay} victimWayOH:${Binary(l1VictimWayOH)} rfvOH(in UInt):${Cat(l1RefillIdx, l1VictimWay)}\n")
  XSDebug(l1Refill, p"[l1 refill] refilldata:0x${l1Wdata}\n")
  XSDebug(l1Refill, p"[l1 refill] l1v:${Binary(l1v)} -> ${Binary(l1v | l1RfvOH)}\n")
  XSDebug(l1Refill, p"[l1 refill] l1g:${Binary(l1g)} -> ${Binary(l1g & ~l1RfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, l1RfvOH, 0.U))}\n")

  // L0 refill
  val l0Refill = !flush_dup(0) && refill.levelOH.l0 && !memPte(0).isNapot(refill.level_dup(0))
  val l0RefillIdx = genPtwL0SetIdx(refill.req_info_dup(0).vpn).suggestName(s"l0_refillIdx")
  val l0VictimWay = replaceWrapper(getl0vSet(refill.req_info_dup(0).vpn), ptwl0replace.way(l0RefillIdx)).suggestName(s"l0_victimWay")
  val l0VictimWayOH = UIntToOH(l0VictimWay).asUInt.suggestName(s"l0_victimWayOH")
  val l0RfvOH = UIntToOH(Cat(l0RefillIdx, l0VictimWay)).suggestName(s"l0_rfvOH")
  val l0Wdata = Wire(l0EntryType)
  // trans the l0 way info, for late wakeup logic
  if (HasBitmapCheck) {
    io.l0_way_info.get := l0VictimWayOH
  }
  val l0Wvpn = refill.req_info_dup(0).vpn
  val l0Wasid = Mux(refill.req_info_dup(0).s2xlate =/= noS2xlate, io.csr_dup(0).vsatp.asid, io.csr_dup(0).satp.asid)
  l0Wdata.gen(
    vpn = l0Wvpn,
    asid = l0Wasid,
    vmid = io.csr_dup(0).hgatp.vmid,
    data = memRdata,
    levelUInt = 0.U,
    refill_prefetch_dup(0),
    refill.req_info_dup(0).s2xlate,
    pbmte,
    io.csr_dup(0).hgatp.mode
  )
  when (l0Refill) {
    l0.io.w.apply(
      valid = true.B,
      setIdx = l0RefillIdx,
      data = l0Wdata,
      waymask = l0VictimWayOH
    )
    ptwl0replace.access(l0RefillIdx, l0VictimWay)
    l0v := l0v | l0RfvOH
    l0g := l0g & ~l0RfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, l0RfvOH, 0.U)
    l0h(l0RefillIdx)(l0VictimWay) := refill_h(0)
    if (HasBitmapCheck) {updateL0BitmapReg(l0BitmapReg, Tran2D(~l0RfvOH))}
    l0asids(l0RefillIdx)(l0VictimWay) := XORFold(l0Wasid, l2tlbParams.hashAsidWidth)
    l0vmids(l0RefillIdx)(l0VictimWay) := XORFold(io.csr_dup(0).hgatp.vmid, l2tlbParams.hashAsidWidth)
    l0vpns(l0RefillIdx)(l0VictimWay) := XORFold(l0Wvpn(vpnLen - 1, vpnLen - PtwL0TagLen), l2tlbParams.hashVpnWidth)

    for (i <- 0 until l2tlbParams.l0nWays) {
      l0RefillPerf(i) := i.U === l0VictimWay
    }
  }
  XSDebug(l0Refill, p"[l0 refill] refillIdx:0x${Hexadecimal(l0RefillIdx)} victimWay:${l0VictimWay} victimWayOH:${Binary(l0VictimWayOH)} rfvOH(in UInt):${Cat(l0RefillIdx, l0VictimWay)}\n")
  XSDebug(l0Refill, p"[l0 refill] refilldata:0x${l0Wdata}\n")
  XSDebug(l0Refill, p"[l0 refill] l0v:${Binary(l0v)} -> ${Binary(l0v | l0RfvOH)}\n")
  XSDebug(l0Refill, p"[l0 refill] l0g:${Binary(l0g)} -> ${Binary(l0g & ~l0RfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, l0RfvOH, 0.U))}\n")


  // misc entries: super & invalid
  val spRefill =
    !flush_dup(0) &&
    (refill.levelOH.sp || (refill.levelOH.l0 && memPte(0).isNapot(refill.level_dup(0)))) &&
    ((memPte(0).isLeaf() && memPte(0).canRefill(refill.level_dup(0), refill.req_info_dup(0).s2xlate, pbmte, io.csr_dup(0).hgatp.mode)) ||
    memPte(0).onlyPf(refill.level_dup(0), refill.req_info_dup(0).s2xlate, pbmte))
  val spRefillIdx = spreplace.way.suggestName(s"sp_refillIdx") // LFSR64()(log2Up(l2tlbParams.spSize)-1,0) // TODO: may be LRU
  val spRfOH = UIntToOH(spRefillIdx).asUInt.suggestName(s"sp_rfOH")
  when (spRefill) {
    sp(spRefillIdx).refill(
      refill.req_info_dup(0).vpn,
      Mux(refill.req_info_dup(0).s2xlate =/= noS2xlate, io.csr_dup(0).vsatp.asid, io.csr_dup(0).satp.asid),
      io.csr_dup(0).hgatp.vmid,
      memSelData(0),
      refill.level_dup(0),
      refill_prefetch_dup(0),
      !memPte(0).onlyPf(refill.level_dup(0), refill.req_info_dup(0).s2xlate, pbmte)
    )
    spreplace.access(spRefillIdx)
    spv := spv | spRfOH
    spg := spg & ~spRfOH | Mux(memPte(0).perm.g, spRfOH, 0.U)
    sph(spRefillIdx) := refill_h(0)
    if (HasBitmapCheck) {updateSpBitmapReg(spBitmapReg, TranVec(~spRfOH))}

    for (i <- 0 until l2tlbParams.spSize) {
      spRefillPerf(i) := i.U === spRefillIdx
    }
  }
  XSDebug(spRefill, p"[sp refill] refillIdx:${spRefillIdx} refillEntry:${sp(spRefillIdx).genPtwEntry(refill.req_info_dup(0).vpn, Mux(refill.req_info_dup(0).s2xlate =/= noS2xlate, io.csr_dup(0).vsatp.asid, io.csr_dup(0).satp.asid), memSelData(0), refill.level_dup(0), refill_prefetch_dup(0))}\n")
  XSDebug(spRefill, p"[sp refill] spv:${Binary(spv)}->${Binary(spv | spRfOH)} spg:${Binary(spg)}->${Binary(spg & ~spRfOH | Mux(memPte(0).perm.g, spRfOH, 0.U))}\n")

  val l1eccFlush = resp_res.l1.ecc && stageResp_valid_1cycle_dup(0) // RegNext(l1eccError, init = false.B)
  val l0eccFlush = resp_res.l0.ecc && stageResp_valid_1cycle_dup(1) // RegNext(l0eccError, init = false.B)
  val eccVpn = stageResp.bits.req_info.vpn

  XSError(l1eccFlush, "l2tlb.cache.l1 ecc error. Should not happen at sim stage")
  XSError(l0eccFlush, "l2tlb.cache.l0 ecc error. Should not happen at sim stage")
  when (l1eccFlush) {
    val flushSetIdxOH = UIntToOH(genPtwL1SetIdx(eccVpn))
    val flushMask = VecInit(flushSetIdxOH.asBools.map { a => Fill(l2tlbParams.l1nWays, a.asUInt) }).asUInt
    l1v := l1v & ~flushMask
    l1g := l1g & ~flushMask
  }

  when (l0eccFlush) {
    val flushSetIdxOH = UIntToOH(genPtwL0SetIdx(eccVpn))
    val flushMask = VecInit(flushSetIdxOH.asBools.map { a => Fill(l2tlbParams.l0nWays, a.asUInt) }).asUInt
    l0v := l0v & ~flushMask
    l0g := l0g & ~flushMask
  }

  // sfence logic
  val l0hashAsid = XORFold(sfence_dup(0).bits.id, l2tlbParams.hashAsidWidth)
  val l1hashAsid = XORFold(sfence_dup(1).bits.id, l2tlbParams.hashAsidWidth)
  val l0asidhit = VecInit(l0asids.flatMap(_.map(_ === l0hashAsid))).asUInt
  val l1asidhit = VecInit(l1asids.flatMap(_.map(_ === l1hashAsid))).asUInt
  val l2asidhit = VecInit(l2asids.map(_ === sfence_dup(2).bits.id)).asUInt
  val spasidhit = VecInit(spasids.map(_ === sfence_dup(0).bits.id)).asUInt

  val sfence_valid = sfence_dup(0).valid && !sfence_dup(0).bits.hg && !sfence_dup(0).bits.hv
  when (sfence_valid) {
    val l0hashVmid = XORFold(io.csr_dup(0).hgatp.vmid, l2tlbParams.hashAsidWidth)
    val l1hashVmid = XORFold(io.csr_dup(1).hgatp.vmid, l2tlbParams.hashAsidWidth)
    val l0vmidhit = VecInit(l0vmids.flatMap(_.map(_ === l0hashVmid))).asUInt
    val l1vmidhit = VecInit(l1vmids.flatMap(_.map(_ === l1hashVmid))).asUInt
    val l2vmidhit = VecInit(l2vmids.map(_.getOrElse(0.U) === io.csr_dup(2).hgatp.vmid)).asUInt
    val spvmidhit = VecInit(spvmids.map(_.getOrElse(0.U) === io.csr_dup(0).hgatp.vmid)).asUInt

    val l0hhit = VecInit(l0h.flatMap(_.map{a => io.csr_dup(0).priv.virt && a === onlyStage1 || !io.csr_dup(0).priv.virt && a === noS2xlate})).asUInt
    val l1hhit = VecInit(l1h.flatMap(_.map{a => io.csr_dup(1).priv.virt && a === onlyStage1 || !io.csr_dup(1).priv.virt && a === noS2xlate})).asUInt
    val l2hhit = VecInit(l2h.map{a => io.csr_dup(2).priv.virt && a === onlyStage1 || !io.csr_dup(2).priv.virt && a === noS2xlate}).asUInt
    val sphhit = VecInit(sph.map{a => io.csr_dup(0).priv.virt && a === onlyStage1 || !io.csr_dup(0).priv.virt && a === noS2xlate}).asUInt
    val l0virthit = l0hhit & VecInit(l0vmidhit.asBools.map{a => io.csr_dup(0).priv.virt && a || !io.csr_dup(0).priv.virt}).asUInt
    val l1virthit = l1hhit & VecInit(l1vmidhit.asBools.map{a => io.csr_dup(1).priv.virt && a || !io.csr_dup(1).priv.virt}).asUInt
    val l2virthit = l2hhit & VecInit(l2vmidhit.asBools.map{a => io.csr_dup(2).priv.virt && a || !io.csr_dup(2).priv.virt}).asUInt
    val spvirthit = sphhit & VecInit(spvmidhit.asBools.map{a => io.csr_dup(0).priv.virt && a || !io.csr_dup(0).priv.virt}).asUInt

    val sfence_vpn = sfence_dup(0).bits.addr(sfence_dup(0).bits.addr.getWidth-1, offLen)
    val l0hashVpn = XORFold(sfence_vpn(vpnLen - 1, vpnLen - PtwL0TagLen), l2tlbParams.hashVpnWidth)
    val l0vpnhit = VecInit(l0vpns.flatMap(_.map(_ === l0hashVpn))).asUInt
    val l0flushSetIdx = UIntToOH(genPtwL0SetIdx(sfence_vpn))
    val l0flushMask = VecInit(l0flushSetIdx.asBools.map{a => Fill(l2tlbParams.l0nWays, a.asUInt)}).asUInt

    when (sfence_dup(0).bits.rs1/*va*/) {
      when (sfence_dup(0).bits.rs2) {
        // all va && all asid
        l0v := l0v & ~l0virthit
        l1v := l1v & ~l1virthit
        l2v := l2v & ~l2virthit
        spv := spv & ~spvirthit
      } .otherwise {
        // all va && specific asid except global
        l0v := l0v & ~(l0virthit & ~l0g & l0asidhit)
        l1v := l1v & ~(l1virthit & ~l1g & l1asidhit)
        l2v := l2v & ~(l2virthit & ~l2g & l2asidhit)
        spv := spv & ~(spvirthit & ~spg & spasidhit)
      }
    } .otherwise {
      when (sfence_dup(0).bits.rs2) {
        // specific leaf of addr && all asid
        l0v := l0v & ~(l0virthit & l0vpnhit & l0flushMask)
        spv := spv & ~(sphhit & VecInit(sp.map(_.hit(sfence_vpn, sfence_dup(0).bits.id, sfence_dup(0).bits.id, io.csr_dup(0).hgatp.vmid, ignoreAsid = true, s2xlate = io.csr_dup(0).priv.virt))).asUInt)
      } .otherwise {
        // specific leaf of addr && specific asid
        l0v := l0v & ~(l0virthit & ~l0g & l0asidhit & l0vpnhit & l0flushMask)
        spv := spv & ~(~spg & sphhit & VecInit(sp.map(_.hit(sfence_vpn, sfence_dup(0).bits.id, sfence_dup(0).bits.id, io.csr_dup(0).hgatp.vmid, s2xlate = io.csr_dup(0).priv.virt))).asUInt)
      }
    }
  }

  val hfencev_valid = sfence_dup(0).valid && sfence_dup(0).bits.hv
  when (hfencev_valid) {
    val l0hashVmid = XORFold(io.csr_dup(0).hgatp.vmid, l2tlbParams.hashAsidWidth)
    val l1hashVmid = XORFold(io.csr_dup(1).hgatp.vmid, l2tlbParams.hashAsidWidth)
    val l0vmidhit = VecInit(l0vmids.flatMap(_.map(_ === l0hashVmid))).asUInt
    val l1vmidhit = VecInit(l1vmids.flatMap(_.map(_ === l1hashVmid))).asUInt
    val l2vmidhit = VecInit(l2vmids.map(_.getOrElse(0.U) === io.csr_dup(2).hgatp.vmid)).asUInt
    val spvmidhit = VecInit(spvmids.map(_.getOrElse(0.U) === io.csr_dup(0).hgatp.vmid)).asUInt

    val l0hhit = VecInit(l0h.flatMap(_.map(_ === onlyStage1))).asUInt
    val l1hhit = VecInit(l1h.flatMap(_.map(_ === onlyStage1))).asUInt
    val l2hhit = VecInit(l2h.map(_ === onlyStage1)).asUInt
    val sphhit = VecInit(sph.map(_ === onlyStage1)).asUInt

    val hfencev_vpn = sfence_dup(0).bits.addr(sfence_dup(0).bits.addr.getWidth-1, offLen)
    val l0hashVpn = XORFold(hfencev_vpn(vpnLen - 1, vpnLen - PtwL0TagLen), l2tlbParams.hashVpnWidth)
    val l0vpnhit = VecInit(l0vpns.flatMap(_.map(_ === l0hashVpn))).asUInt
    val l0flushSetIdx = UIntToOH(genPtwL0SetIdx(hfencev_vpn))
    val l0flushMask = VecInit(l0flushSetIdx.asBools.map{a => Fill(l2tlbParams.l0nWays, a.asUInt)}).asUInt

    when(sfence_dup(0).bits.rs1) {
      when(sfence_dup(0).bits.rs2) {
        l0v := l0v & ~(l0hhit & l0vmidhit)
        l1v := l1v & ~(l1hhit & l1vmidhit)
        l2v := l2v & ~(l2hhit & l2vmidhit)
        spv := spv & ~(sphhit & spvmidhit)
      }.otherwise {
        l0v := l0v & ~(l0hhit & l0vmidhit & ~l0g & l0asidhit)
        l1v := l1v & ~(l1hhit & l1vmidhit & ~l1g & l1asidhit)
        l2v := l2v & ~(l2hhit & l2vmidhit & ~l2g & l2asidhit)
        spv := spv & ~(sphhit & spvmidhit & ~spg & spasidhit)
      }
    }.otherwise {
      when(sfence_dup(0).bits.rs2) {
        l0v := l0v & ~(l0hhit & l0vmidhit & l0vpnhit & l0flushMask)
        spv := spv & ~(sphhit & VecInit(sp.map(_.hit(hfencev_vpn, sfence_dup(0).bits.id, sfence_dup(0).bits.id, io.csr_dup(0).hgatp.vmid, ignoreAsid = true, s2xlate = true.B))).asUInt)
      }.otherwise {
        l0v := l0v & ~(l0hhit & l0vmidhit & ~l0g & l0asidhit & l0vpnhit & l0flushMask)
        spv := spv & ~(~spg & sphhit & VecInit(sp.map(_.hit(hfencev_vpn, sfence_dup(0).bits.id, sfence_dup(0).bits.id, io.csr_dup(0).hgatp.vmid, s2xlate = true.B))).asUInt)
      }
    }
  }


  val hfenceg_valid = sfence_dup(0).valid && sfence_dup(0).bits.hg
  when(hfenceg_valid) {
    val l0hashVmid = XORFold(sfence_dup(0).bits.id, l2tlbParams.hashAsidWidth)
    val l1hashVmid = XORFold(sfence_dup(1).bits.id, l2tlbParams.hashAsidWidth)
    val l0vmidhit = VecInit(l0vmids.flatMap(_.map(_ === l0hashVmid))).asUInt
    val l1vmidhit = VecInit(l1vmids.flatMap(_.map(_ === l1hashVmid))).asUInt
    val l2vmidhit = VecInit(l2vmids.map(_.getOrElse(0.U) === sfence_dup(2).bits.id)).asUInt
    val spvmidhit = VecInit(spvmids.map(_.getOrElse(0.U) === sfence_dup(0).bits.id)).asUInt

    val l0hhit = VecInit(l0h.flatMap(_.map(_ === onlyStage2))).asUInt
    val l1hhit = VecInit(l1h.flatMap(_.map(_ === onlyStage2))).asUInt
    val l2hhit = VecInit(l2h.map(_ === onlyStage2)).asUInt
    val sphhit = VecInit(sph.map(_ === onlyStage2)).asUInt

    val hfenceg_gvpn = (sfence_dup(0).bits.addr << 2)(sfence_dup(0).bits.addr.getWidth - 1, offLen)
    val l0hashVpn = XORFold(hfenceg_gvpn(vpnLen - 1, vpnLen - PtwL0TagLen), l2tlbParams.hashVpnWidth)
    val l0vpnhit = VecInit(l0vpns.flatMap(_.map(_ === l0hashVpn))).asUInt
    val l0flushSetIdx = UIntToOH(genPtwL0SetIdx(hfenceg_gvpn))
    val l0flushMask = VecInit(l0flushSetIdx.asBools.map{a => Fill(l2tlbParams.l0nWays, a.asUInt)}).asUInt

    when(sfence_dup(0).bits.rs1) {
      when(sfence_dup(0).bits.rs2) {
        l0v := l0v & ~l0hhit
        l1v := l1v & ~l1hhit
        l2v := l2v & ~l2hhit
        spv := spv & ~sphhit
      }.otherwise {
        l0v := l0v & ~(l0hhit & l0vmidhit)
        l1v := l1v & ~(l1hhit & l1vmidhit)
        l2v := l2v & ~(l2hhit & l2vmidhit)
        spv := spv & ~(sphhit & spvmidhit)
      }
    }.otherwise {
      when(sfence_dup(0).bits.rs2) {
        l0v := l0v & ~(l0hhit & l0vpnhit & l0flushMask)
        spv := spv & ~(sphhit & VecInit(sp.map(_.hit(hfenceg_gvpn, 0.U, 0.U, sfence_dup(0).bits.id, ignoreAsid = true, s2xlate = false.B))).asUInt)
      }.otherwise {
        l0v := l0v & ~(l0hhit & l0vmidhit & l0vpnhit & l0flushMask)
        spv := spv & ~(~spg & sphhit & VecInit(sp.map(_.hit(hfenceg_gvpn, 0.U, 0.U, sfence_dup(0).bits.id, ignoreAsid = true, s2xlate = true.B))).asUInt)
      }
    }
  }

  if (EnableSv48) {
    val l3asidhit = VecInit(l3asids.get.map(_ === sfence_dup(2).bits.id)).asUInt
    val l3vmidhit = VecInit(l3vmids.get.map(_.getOrElse(0.U) === io.csr_dup(2).hgatp.vmid)).asUInt
    val l3hhit = VecInit(l3h.get.map{a => io.csr_dup(2).priv.virt && a === onlyStage1 || !io.csr_dup(2).priv.virt && a === noS2xlate}).asUInt

    when (sfence_valid) {
      val l3vmidhit = VecInit(l3vmids.get.map(_.getOrElse(0.U) === io.csr_dup(2).hgatp.vmid)).asUInt
      val l3hhit = VecInit(l3h.get.map{a => io.csr_dup(2).priv.virt && a === onlyStage1 || !io.csr_dup(2).priv.virt && a === noS2xlate}).asUInt
      val sfence_vpn = sfence_dup(2).bits.addr(sfence_dup(2).bits.addr.getWidth-1, offLen)

      when (sfence_dup(2).bits.rs1/*va*/) {
        when (sfence_dup(2).bits.rs2) {
          // all va && all asid
          l3v.map(_ := l3v.get & ~(l3hhit & VecInit(l3vmidhit.asBools.map{a => io.csr_dup(2).priv.virt && a || !io.csr_dup(2).priv.virt}).asUInt))
        } .otherwise {
          // all va && specific asid except global
          l3v.map(_ := l3v.get & ~(~l3g.get & l3hhit & l3asidhit & VecInit(l3vmidhit.asBools.map{a => io.csr_dup(2).priv.virt && a || !io.csr_dup(2).priv.virt}).asUInt))
        }
      }
    }

    when (hfencev_valid) {
      val l3vmidhit = VecInit(l3vmids.get.map(_.getOrElse(0.U) === io.csr_dup(2).hgatp.vmid)).asUInt
      val l3hhit = VecInit(l3h.get.map(_ === onlyStage1)).asUInt
      val hfencev_vpn = sfence_dup(2).bits.addr(sfence_dup(2).bits.addr.getWidth-1, offLen)
      when(sfence_dup(2).bits.rs1) {
        when(sfence_dup(2).bits.rs2) {
          l3v.map(_ := l3v.get & ~(l3hhit & l3vmidhit))
        }.otherwise {
          l3v.map(_ := l3v.get & ~(~l3g.get & l3hhit & l3asidhit & l3vmidhit))
        }
      }
    }

    when (hfenceg_valid) {
      val l3vmidhit = VecInit(l3vmids.get.map(_.getOrElse(0.U) === sfence_dup(2).bits.id)).asUInt
      val l3hhit = VecInit(l3h.get.map(_ === onlyStage2)).asUInt
      val hfenceg_gvpn = (sfence_dup(2).bits.addr << 2)(sfence_dup(2).bits.addr.getWidth - 1, offLen)
      when(sfence_dup(2).bits.rs1) {
        when(sfence_dup(2).bits.rs2) {
          l3v.map(_ := l3v.get & ~l3hhit)
        }.otherwise {
          l3v.map(_ := l3v.get & ~(l3hhit & l3vmidhit))
        }
      }
    }
  }

  def InsideStageConnect(in: DecoupledIO[PtwCacheReq], out: DecoupledIO[PtwCacheReq], inFire: Bool): Unit = {
    in.ready := !in.valid || out.ready
    out.valid := in.valid
    out.bits := in.bits
    out.bits.bypassed.zip(in.bits.bypassed).zipWithIndex.map{ case (b, i) =>
      val bypassed_reg = Reg(Bool())
      val bypassed_wire = refill_bypass(in.bits.req_info.vpn, i, in.bits.req_info.s2xlate) && io.refill.valid
      when (inFire) { bypassed_reg := bypassed_wire }
      .elsewhen (io.refill.valid) { bypassed_reg := bypassed_reg || bypassed_wire }

      b._1 := b._2 || (bypassed_wire || (bypassed_reg && !inFire))
    }
  }

  // Perf Count
  val resp_l0 = resp_res.l0.hit
  val resp_sp = resp_res.sp.hit
  val resp_l3_pre = if (EnableSv48) Some(resp_res.l3.get.pre) else None
  val resp_l2_pre = resp_res.l2.pre
  val resp_l1_pre = resp_res.l1.pre
  val resp_l0_pre = resp_res.l0.pre
  val resp_sp_pre = resp_res.sp.pre
  val base_valid_access_0 = !from_pre(io.resp.bits.req_info.source) && io.resp.fire
  XSPerfAccumulate("access", base_valid_access_0)
  if (EnableSv48) {
    XSPerfAccumulate("l3_hit", base_valid_access_0 && io.resp.bits.toFsm.l3Hit.get && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  }
  XSPerfAccumulate("l2_hit", base_valid_access_0 && io.resp.bits.toFsm.l2Hit && !io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  XSPerfAccumulate("l1_hit", base_valid_access_0 && io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  XSPerfAccumulate("l0_hit", base_valid_access_0 && resp_l0)
  XSPerfAccumulate("sp_hit", base_valid_access_0 && resp_sp)
  XSPerfAccumulate("pte_hit",base_valid_access_0 && io.resp.bits.hit)

  if (EnableSv48) {
    XSPerfAccumulate("l3_hit_pre", base_valid_access_0 && resp_l3_pre.get && io.resp.bits.toFsm.l3Hit.get && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  }
  XSPerfAccumulate("l2_hit_pre", base_valid_access_0 && resp_l2_pre && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  XSPerfAccumulate("l1_hit_pre", base_valid_access_0 && resp_l1_pre && io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  XSPerfAccumulate("l0_hit_pre", base_valid_access_0 && resp_l0_pre && resp_l0)
  XSPerfAccumulate("sp_hit_pre", base_valid_access_0 && resp_sp_pre && resp_sp)
  XSPerfAccumulate("pte_hit_pre",base_valid_access_0 && (resp_l0_pre && resp_l0 || resp_sp_pre && resp_sp) && io.resp.bits.hit)

  val base_valid_access_1 = from_pre(io.resp.bits.req_info.source) && io.resp.fire
  XSPerfAccumulate("pre_access", base_valid_access_1)
  if (EnableSv48) {
    XSPerfAccumulate("pre_l3_hit", base_valid_access_1 && io.resp.bits.toFsm.l3Hit.get && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  }
  XSPerfAccumulate("pre_l2_hit", base_valid_access_1 && io.resp.bits.toFsm.l2Hit && !io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  XSPerfAccumulate("pre_l1_hit", base_valid_access_1 && io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  XSPerfAccumulate("pre_l0_hit", base_valid_access_1 && resp_l0)
  XSPerfAccumulate("pre_sp_hit", base_valid_access_1 && resp_sp)
  XSPerfAccumulate("pre_pte_hit",base_valid_access_1 && io.resp.bits.hit)

  if (EnableSv48) {
    XSPerfAccumulate("pre_l3_hit_pre", base_valid_access_1 && resp_l3_pre.get && io.resp.bits.toFsm.l3Hit.get && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  }
  XSPerfAccumulate("pre_l2_hit_pre", base_valid_access_1 && resp_l2_pre && io.resp.bits.toFsm.l2Hit && !io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  XSPerfAccumulate("pre_l1_hit_pre", base_valid_access_1 && resp_l1_pre && io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  XSPerfAccumulate("pre_l0_hit_pre", base_valid_access_1 && resp_l0_pre && resp_l0)
  XSPerfAccumulate("pre_sp_hit_pre", base_valid_access_1 && resp_sp_pre && resp_sp)
  XSPerfAccumulate("pre_pte_hit_pre",base_valid_access_1 && (resp_l0_pre && resp_l0 || resp_sp_pre && resp_sp) && io.resp.bits.hit)

  val base_valid_access_2 = stageResp.bits.isFirst && !from_pre(io.resp.bits.req_info.source) && io.resp.fire
  XSPerfAccumulate("access_first", base_valid_access_2)
  if (EnableSv48) {
    XSPerfAccumulate("l3_hit_first", base_valid_access_2 && io.resp.bits.toFsm.l3Hit.get && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  }
  XSPerfAccumulate("l2_hit_first", base_valid_access_2 && io.resp.bits.toFsm.l2Hit && !io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  XSPerfAccumulate("l1_hit_first", base_valid_access_2 && io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  XSPerfAccumulate("l0_hit_first", base_valid_access_2 && resp_l0)
  XSPerfAccumulate("sp_hit_first", base_valid_access_2 && resp_sp)
  XSPerfAccumulate("pte_hit_first",base_valid_access_2 && io.resp.bits.hit)

  if (EnableSv48) {
    XSPerfAccumulate("l3_hit_pre_first", base_valid_access_2 && resp_l3_pre.get && io.resp.bits.toFsm.l3Hit.get && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  }
  XSPerfAccumulate("l2_hit_pre_first", base_valid_access_2 && resp_l2_pre && io.resp.bits.toFsm.l2Hit && !io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  XSPerfAccumulate("l1_hit_pre_first", base_valid_access_2 && resp_l1_pre && io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  XSPerfAccumulate("l0_hit_pre_first", base_valid_access_2 && resp_l0_pre && resp_l0)
  XSPerfAccumulate("sp_hit_pre_first", base_valid_access_2 && resp_sp_pre && resp_sp)
  XSPerfAccumulate("pte_hit_pre_first",base_valid_access_2 && (resp_l0_pre && resp_l0 || resp_sp_pre && resp_sp) && io.resp.bits.hit)

  val base_valid_access_3 = stageResp.bits.isFirst && from_pre(io.resp.bits.req_info.source) && io.resp.fire
  XSPerfAccumulate("pre_access_first", base_valid_access_3)
  if (EnableSv48) {
    XSPerfAccumulate("pre_l3_hit_first", base_valid_access_3 && io.resp.bits.toFsm.l3Hit.get && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  }
  XSPerfAccumulate("pre_l2_hit_first", base_valid_access_3 && io.resp.bits.toFsm.l2Hit && !io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  XSPerfAccumulate("pre_l1_hit_first", base_valid_access_3 && io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  XSPerfAccumulate("pre_l0_hit_first", base_valid_access_3 && resp_l0)
  XSPerfAccumulate("pre_sp_hit_first", base_valid_access_3 && resp_sp)
  XSPerfAccumulate("pre_pte_hit_first", base_valid_access_3 && io.resp.bits.hit)

  if (EnableSv48) {
    XSPerfAccumulate("pre_l3_hit_pre_first", base_valid_access_3 && resp_l3_pre.get && io.resp.bits.toFsm.l3Hit.get && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  }
  XSPerfAccumulate("pre_l2_hit_pre_first", base_valid_access_3 && resp_l2_pre && io.resp.bits.toFsm.l2Hit && !io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  XSPerfAccumulate("pre_l1_hit_pre_first", base_valid_access_3 && resp_l1_pre && io.resp.bits.toFsm.l1Hit && !io.resp.bits.hit)
  XSPerfAccumulate("pre_l0_hit_pre_first", base_valid_access_3 && resp_l0_pre && resp_l0)
  XSPerfAccumulate("pre_sp_hit_pre_first", base_valid_access_3 && resp_sp_pre && resp_sp)
  XSPerfAccumulate("pre_pte_hit_pre_first",base_valid_access_3 && (resp_l0_pre && resp_l0 || resp_sp_pre && resp_sp) && io.resp.bits.hit)

  XSPerfAccumulate("rwHarzad", io.req.valid && !io.req.ready)
  XSPerfAccumulate("out_blocked", io.resp.valid && !io.resp.ready)
  if (EnableSv48) {
    l3AccessPerf.get.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"l3AccessIndex${i}", l) }
  }
  l2AccessPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"l2AccessIndex${i}", l) }
  l1AccessPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"l1AccessIndex${i}", l) }
  l0AccessPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"l0AccessIndex${i}", l) }
  spAccessPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"SPAccessIndex${i}", l) }
  if (EnableSv48) {
    l3RefillPerf.get.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"l3RefillIndex${i}", l) }
  }
  l2RefillPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"l2RefillIndex${i}", l) }
  l1RefillPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"l1RefillIndex${i}", l) }
  l0RefillPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"l0RefillIndex${i}", l) }
  spRefillPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"SPRefillIndex${i}", l) }

  if (EnableSv48) {
    XSPerfAccumulate("l3Refill", Cat(l3RefillPerf.get).orR)
  }
  XSPerfAccumulate("l2Refill", Cat(l2RefillPerf).orR)
  XSPerfAccumulate("l1Refill", Cat(l1RefillPerf).orR)
  XSPerfAccumulate("l0Refill", Cat(l0RefillPerf).orR)
  XSPerfAccumulate("spRefill", Cat(spRefillPerf).orR)
  if (EnableSv48) {
    XSPerfAccumulate("l3Refill_pre", Cat(l3RefillPerf.get).orR && refill_prefetch_dup(0))
  }
  XSPerfAccumulate("l2Refill_pre", Cat(l2RefillPerf).orR && refill_prefetch_dup(0))
  XSPerfAccumulate("l1Refill_pre", Cat(l1RefillPerf).orR && refill_prefetch_dup(0))
  XSPerfAccumulate("l0Refill_pre", Cat(l0RefillPerf).orR && refill_prefetch_dup(0))
  XSPerfAccumulate("spRefill_pre", Cat(spRefillPerf).orR && refill_prefetch_dup(0))

  // debug
  XSDebug(sfence_dup(0).valid, p"[sfence] original v and g vector:\n")
  if (EnableSv48) {
    XSDebug(sfence_dup(0).valid, p"[sfence] l3v:${Binary(l3v.get)}\n")
  }
  XSDebug(sfence_dup(0).valid, p"[sfence] l2v:${Binary(l2v)}\n")
  XSDebug(sfence_dup(0).valid, p"[sfence] l1v:${Binary(l1v)}\n")
  XSDebug(sfence_dup(0).valid, p"[sfence] l0v:${Binary(l0v)}\n")
  XSDebug(sfence_dup(0).valid, p"[sfence] l0g:${Binary(l0g)}\n")
  XSDebug(sfence_dup(0).valid, p"[sfence] spv:${Binary(spv)}\n")
  XSDebug(RegNext(sfence_dup(0).valid), p"[sfence] new v and g vector:\n")
  if (EnableSv48) {
    XSDebug(RegNext(sfence_dup(0).valid), p"[sfence] l3v:${Binary(l3v.get)}\n")
  }
  XSDebug(RegNext(sfence_dup(0).valid), p"[sfence] l2v:${Binary(l2v)}\n")
  XSDebug(RegNext(sfence_dup(0).valid), p"[sfence] l1v:${Binary(l1v)}\n")
  XSDebug(RegNext(sfence_dup(0).valid), p"[sfence] l0v:${Binary(l0v)}\n")
  XSDebug(RegNext(sfence_dup(0).valid), p"[sfence] l0g:${Binary(l0g)}\n")
  XSDebug(RegNext(sfence_dup(0).valid), p"[sfence] spv:${Binary(spv)}\n")

  val perfEvents = Seq(
    ("access           ", base_valid_access_0             ),
    ("l2_hit           ", l2Hit                           ),
    ("l1_hit           ", l1Hit                           ),
    ("l0_hit           ", l0Hit                           ),
    ("sp_hit           ", spHit                           ),
    ("pte_hit          ", l0Hit || spHit                  ),
    ("rwHarzad         ", io.req.valid && !io.req.ready   ),
    ("out_blocked      ", io.resp.valid && !io.resp.ready ),
  )
  generatePerfEvent()
}
