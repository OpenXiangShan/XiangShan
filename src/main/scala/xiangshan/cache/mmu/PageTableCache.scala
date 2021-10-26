/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.internal.naming.chiselName
import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._

/* ptw cache caches the page table of all the three layers
 * ptw cache resp at next cycle
 * the cache should not be blocked
 * when miss queue if full, just block req outside
 */

class PageCachePerPespBundle(implicit p: Parameters) extends PtwBundle {
  val hit = Bool()
  val pre = Bool()
  val ppn = UInt(ppnLen.W)
  val perm = new PtePermBundle()
  val ecc = Bool()
  val level = UInt(2.W)

  def apply(hit: Bool, pre: Bool, ppn: UInt, perm: PtePermBundle = 0.U.asTypeOf(new PtePermBundle()),
            ecc: Bool = false.B, level: UInt = 0.U) {
    this.hit := hit && !ecc
    this.pre := pre
    this.ppn := ppn
    this.perm := perm
    this.ecc := ecc && hit
    this.level := level
  }
}

class PageCacheRespBundle(implicit p: Parameters) extends PtwBundle {
  val l1 = new PageCachePerPespBundle
  val l2 = new PageCachePerPespBundle
  val l3 = new PageCachePerPespBundle
  val sp = new PageCachePerPespBundle
}

class PtwCacheReq(implicit p: Parameters) extends PtwBundle {
  val req_info = new L2TlbInnerBundle()
  val isFirst = Bool()
}

class PtwCacheIO()(implicit p: Parameters) extends MMUIOBaseBundle with HasPtwConst {
  val req = Flipped(DecoupledIO(new PtwCacheReq()))
  val resp = DecoupledIO(new Bundle {
    val req_info = new L2TlbInnerBundle()
    val hit = Bool()
    val prefetch = Bool() // is the entry fetched by prefetch
    val toFsm = new Bundle {
      val l1Hit = Bool()
      val l2Hit = Bool()
      val ppn = UInt(ppnLen.W)
    }
    val toTlb = new PtwEntry(tagLen = vpnLen, hasPerm = true, hasLevel = true)
  })
  val refill = Flipped(ValidIO(new Bundle {
    val ptes = UInt(blockBits.W)
    val req_info = new L2TlbInnerBundle()
    val level = UInt(log2Up(Level).W)
    val addr_low = UInt((log2Up(l2tlbParams.blockBytes) - log2Up(XLEN/8)).W)
  }))
}

@chiselName
class PtwCache()(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new PtwCacheIO)

  val ecc = Code.fromString(l2tlbParams.ecc)
  val l2EntryType = new PTWEntriesWithEcc(ecc, num = PtwL2SectorSize, tagLen = PtwL2TagLen, level = 1, hasPerm = false)
  val l3EntryType = new PTWEntriesWithEcc(ecc, num = PtwL3SectorSize, tagLen = PtwL3TagLen, level = 2, hasPerm = true)

  // TODO: four caches make the codes dirty, think about how to deal with it

  val sfence = io.sfence
  val refill = io.refill.bits
  val refill_prefetch = from_pre(io.refill.bits.req_info.source)
  val flush = sfence.valid || io.csr.satp.changed

  // when refill, refuce to accept new req
  val rwHarzad = if (sramSinglePort) io.refill.valid else false.B

  // handle hand signal and req_info
  val stage1 = Wire(Decoupled(new PtwCacheReq()))
  val stage2 = Wire(Decoupled(new PtwCacheReq()))
  val stage3 = Wire(Decoupled(new PtwCacheReq()))
  stage1 <> io.req
  PipelineConnect(stage1, stage2, stage3.ready, flush, rwHarzad)
  PipelineConnect(stage2, stage3, io.resp.ready, flush)
  stage3.ready := io.resp.ready

  // l1: level 0 non-leaf pte
  val l1 = Reg(Vec(l2tlbParams.l1Size, new PtwEntry(tagLen = PtwL1TagLen)))
  val l1v = RegInit(0.U(l2tlbParams.l1Size.W))
  val l1g = Reg(UInt(l2tlbParams.l1Size.W))
  val l1asids = Reg(Vec(l2tlbParams.l1Size, UInt(AsidLength.W)))

  // l2: level 1 non-leaf pte
  val l2 = Module(new SRAMTemplate(
    l2EntryType,
    set = l2tlbParams.l2nSets,
    way = l2tlbParams.l2nWays,
    singlePort = sramSinglePort
  ))
  val l2v = RegInit(0.U((l2tlbParams.l2nSets * l2tlbParams.l2nWays).W))
  val l2g = Reg(UInt((l2tlbParams.l2nSets * l2tlbParams.l2nWays).W))
  val l2asids = Reg(Vec(l2tlbParams.l2nSets, Vec(l2tlbParams.l2nWays, UInt(AsidLength.W))))
  def getl2vSet(vpn: UInt) = {
    require(log2Up(l2tlbParams.l2nWays) == log2Down(l2tlbParams.l2nWays))
    val set = genPtwL2SetIdx(vpn)
    require(set.getWidth == log2Up(l2tlbParams.l2nSets))
    val l2vVec = l2v.asTypeOf(Vec(l2tlbParams.l2nSets, UInt(l2tlbParams.l2nWays.W)))
    l2vVec(set)
  }
  def getl2asidSet(vpn: UInt) = {
    require(log2Up(l2tlbParams.l2nWays) == log2Down(l2tlbParams.l2nWays))
    val set = genPtwL2SetIdx(vpn)
    require(set.getWidth == log2Up(l2tlbParams.l2nSets))
    l2asids(set)
  }

  // l3: level 2 leaf pte of 4KB pages
  val l3 = Module(new SRAMTemplate(
    l3EntryType,
    set = l2tlbParams.l3nSets,
    way = l2tlbParams.l3nWays,
    singlePort = sramSinglePort
  ))
  val l3v = RegInit(0.U((l2tlbParams.l3nSets * l2tlbParams.l3nWays).W))
  val l3g = Reg(UInt((l2tlbParams.l3nSets * l2tlbParams.l3nWays).W))
  val l3asids = Reg(Vec(l2tlbParams.l3nSets, Vec(l2tlbParams.l3nWays, UInt(AsidLength.W))))
  def getl3vSet(vpn: UInt) = {
    require(log2Up(l2tlbParams.l3nWays) == log2Down(l2tlbParams.l3nWays))
    val set = genPtwL3SetIdx(vpn)
    require(set.getWidth == log2Up(l2tlbParams.l3nSets))
    val l3vVec = l3v.asTypeOf(Vec(l2tlbParams.l3nSets, UInt(l2tlbParams.l3nWays.W)))
    l3vVec(set)
  }
  def getl3asidSet(vpn: UInt) = {
    require(log2Up(l2tlbParams.l3nWays) == log2Down(l2tlbParams.l3nWays))
    val set = genPtwL3SetIdx(vpn)
    require(set.getWidth == log2Up(l2tlbParams.l3nSets))
    l3asids(set)
  }

  // sp: level 0/1 leaf pte of 1GB/2MB super pages
  val sp = Reg(Vec(l2tlbParams.spSize, new PtwEntry(tagLen = SPTagLen, hasPerm = true, hasLevel = true)))
  val spv = RegInit(0.U(l2tlbParams.spSize.W))
  val spg = Reg(UInt(l2tlbParams.spSize.W))
  val spasids = Reg(Vec(l2tlbParams.spSize, UInt(AsidLength.W)))

  // Access Perf
  val l1AccessPerf = Wire(Vec(l2tlbParams.l1Size, Bool()))
  val l2AccessPerf = Wire(Vec(l2tlbParams.l2nWays, Bool()))
  val l3AccessPerf = Wire(Vec(l2tlbParams.l3nWays, Bool()))
  val spAccessPerf = Wire(Vec(l2tlbParams.spSize, Bool()))
  l1AccessPerf.map(_ := false.B)
  l2AccessPerf.map(_ := false.B)
  l3AccessPerf.map(_ := false.B)
  spAccessPerf.map(_ := false.B)

  // stage1 & stage2, read page cache and data resp

  val cache_read_valid = OneCycleValid(stage1.fire, flush)
  // l1
  val ptwl1replace = ReplacementPolicy.fromString(l2tlbParams.l1Replacer, l2tlbParams.l1Size)
  val (l1Hit, l1HitPPN, l1Pre) = {
    val hitVecT = l1.zipWithIndex.map { case (e, i) => e.hit(stage1.bits.req_info.vpn, io.csr.satp.asid) && l1v(i) }
    val hitVec = hitVecT.map(RegEnable(_, stage1.fire))
    val hitPPN = ParallelPriorityMux(hitVec zip l1.map(_.ppn))
    val hitPre = ParallelPriorityMux(hitVec zip l1.map(_.prefetch))
    val hit = ParallelOR(hitVec) && cache_read_valid

    when (hit) { ptwl1replace.access(OHToUInt(hitVec)) }

    l1AccessPerf.zip(hitVec).map{ case (l, h) => l := h && RegNext(stage1.fire)}
    for (i <- 0 until l2tlbParams.l1Size) {
      XSDebug(stage1.fire, p"[l1] l1(${i.U}) ${l1(i)} hit:${l1(i).hit(stage1.bits.req_info.vpn, io.csr.satp.asid)}\n")
    }
    XSDebug(stage1.fire, p"[l1] l1v:${Binary(l1v)} hitVecT:${Binary(VecInit(hitVecT).asUInt)}\n")
    XSDebug(stage2.valid, p"[l1] l1Hit:${hit} l1HitPPN:0x${Hexadecimal(hitPPN)} hitVec:${VecInit(hitVec).asUInt}\n")

    VecInit(hitVecT).suggestName(s"l1_hitVecT")
    VecInit(hitVec).suggestName(s"l1_hitVec")

    (hit, hitPPN, hitPre)
  }

  // l2
  val ptwl2replace = ReplacementPolicy.fromString(l2tlbParams.l2Replacer,l2tlbParams.l2nWays,l2tlbParams.l2nSets)
  val (l2Hit, l2HitPPN, l2Pre, l2eccError) = {
    val ridx = genPtwL2SetIdx(stage1.bits.req_info.vpn)
    val vidx = RegEnable(VecInit(getl2vSet(stage1.bits.req_info.vpn).asBools), stage1.fire)
    val asids_idx = RegEnable(getl2asidSet(stage1.bits.req_info.vpn), stage1.fire)
    l2.io.r.req.valid := stage1.fire
    l2.io.r.req.bits.apply(setIdx = ridx)
    val ramDatas = l2.io.r.resp.data
    val hitVec = VecInit(ramDatas.zip(vidx).map { case (wayData, v) => wayData.entries.hit(stage2.bits.req_info.vpn, io.csr.satp.asid) && v })
    val hitWayEntry = ParallelPriorityMux(hitVec zip ramDatas)
    val hitWayData = hitWayEntry.entries
    val hit = ParallelOR(hitVec) && cache_read_valid && RegNext(l2.io.r.req.ready, init = false.B)
    val hitWay = ParallelPriorityMux(hitVec zip (0 until l2tlbParams.l2nWays).map(_.U))
    val eccError = hitWayEntry.decode()

    ridx.suggestName(s"l2_ridx")
    vidx.suggestName(s"l2_vidx")
    ramDatas.suggestName(s"l2_ramDatas")
    hitVec.suggestName(s"l2_hitVec")
    hitWayData.suggestName(s"l2_hitWayData")
    hitWay.suggestName(s"l2_hitWay")

    when (hit) { ptwl2replace.access(genPtwL2SetIdx(stage2.bits.req_info.vpn), hitWay) }

    l2AccessPerf.zip(hitVec).map{ case (l, h) => l := h && RegNext(stage1.fire) }
    XSDebug(stage1.fire, p"[l2] ridx:0x${Hexadecimal(ridx)}\n")
    for (i <- 0 until l2tlbParams.l2nWays) {
      XSDebug(RegNext(stage1.fire), p"[l2] ramDatas(${i.U}) ${ramDatas(i)}  l2v:${vidx(i)}  hit:${ramDatas(i).entries.hit(stage2.bits.req_info.vpn, io.csr.satp.asid)}\n")
    }
    XSDebug(stage2.valid, p"[l2] l2Hit:${hit} l2HitPPN:0x${Hexadecimal(hitWayData.ppns(genPtwL2SectorIdx(stage2.bits.req_info.vpn)))} hitVec:${Binary(hitVec.asUInt)} hitWay:${hitWay} vidx:${Binary(vidx.asUInt)}\n")

    (hit, hitWayData.ppns(genPtwL2SectorIdx(stage2.bits.req_info.vpn)), hitWayData.prefetch, eccError)
  }

  // l3
  val ptwl3replace = ReplacementPolicy.fromString(l2tlbParams.l3Replacer,l2tlbParams.l3nWays,l2tlbParams.l3nSets)
  val (l3Hit, l3HitData, l3Pre, l3eccError) = {
    val ridx = genPtwL3SetIdx(stage1.bits.req_info.vpn)
    val vidx = RegEnable(VecInit(getl3vSet(stage1.bits.req_info.vpn).asBools), stage1.fire)
    val asids_idx = RegEnable(getl3asidSet(stage1.bits.req_info.vpn), stage1.fire)
    l3.io.r.req.valid := stage1.fire
    l3.io.r.req.bits.apply(setIdx = ridx)
    val ramDatas = l3.io.r.resp.data
    val hitVec = VecInit(ramDatas.zip(vidx).map{ case (wayData, v) => wayData.entries.hit(stage2.bits.req_info.vpn, io.csr.satp.asid) && v })
    val hitWayEntry = ParallelPriorityMux(hitVec zip ramDatas)
    val hitWayData = hitWayEntry.entries
    val hitWayEcc = hitWayEntry.ecc
    val hit = ParallelOR(hitVec) && cache_read_valid && RegNext(l3.io.r.req.ready, init = false.B)
    val hitWay = ParallelPriorityMux(hitVec zip (0 until l2tlbParams.l3nWays).map(_.U))
    val eccError = hitWayEntry.decode()

    when (hit) { ptwl3replace.access(genPtwL3SetIdx(stage2.bits.req_info.vpn), hitWay) }

    l3AccessPerf.zip(hitVec).map{ case (l, h) => l := h && RegNext(stage1.fire) }
    XSDebug(stage1.fire, p"[l3] ridx:0x${Hexadecimal(ridx)}\n")
    for (i <- 0 until l2tlbParams.l3nWays) {
      XSDebug(RegNext(stage1.fire), p"[l3] ramDatas(${i.U}) ${ramDatas(i)}  l3v:${vidx(i)}  hit:${ramDatas(i).entries.hit(stage2.bits.req_info.vpn, io.csr.satp.asid)}\n")
    }
    XSDebug(stage2.valid, p"[l3] l3Hit:${hit} l3HitData:${hitWayData} hitVec:${Binary(hitVec.asUInt)} hitWay:${hitWay} vidx:${Binary(vidx.asUInt)}\n")

    ridx.suggestName(s"l3_ridx")
    vidx.suggestName(s"l3_vidx")
    ramDatas.suggestName(s"l3_ramDatas")
    hitVec.suggestName(s"l3_hitVec")
    hitWay.suggestName(s"l3_hitWay")

    (hit, hitWayData, hitWayData.prefetch, eccError)
  }
  val l3HitPPN = l3HitData.ppns(genPtwL3SectorIdx(stage2.bits.req_info.vpn))
  val l3HitPerm = l3HitData.perms.getOrElse(0.U.asTypeOf(Vec(PtwL3SectorSize, new PtePermBundle)))(genPtwL3SectorIdx(stage2.bits.req_info.vpn))

  // super page
  val spreplace = ReplacementPolicy.fromString(l2tlbParams.spReplacer, l2tlbParams.spSize)
  val (spHit, spHitData, spPre) = {
    val hitVecT = sp.zipWithIndex.map { case (e, i) => e.hit(stage1.bits.req_info.vpn, io.csr.satp.asid) && spv(i) }
    val hitVec = hitVecT.map(RegEnable(_, stage1.fire))
    val hitData = ParallelPriorityMux(hitVec zip sp)
    val hit = ParallelOR(hitVec) && cache_read_valid

    when (hit) { spreplace.access(OHToUInt(hitVec)) }

    spAccessPerf.zip(hitVec).map{ case (s, h) => s := h && RegNext(stage1.fire) }
    for (i <- 0 until l2tlbParams.spSize) {
      XSDebug(stage1.fire, p"[sp] sp(${i.U}) ${sp(i)} hit:${sp(i).hit(stage1.bits.req_info.vpn, io.csr.satp.asid)} spv:${spv(i)}\n")
    }
    XSDebug(stage2.valid, p"[sp] spHit:${hit} spHitData:${hitData} hitVec:${Binary(VecInit(hitVec).asUInt)}\n")

    VecInit(hitVecT).suggestName(s"sp_hitVecT")
    VecInit(hitVec).suggestName(s"sp_hitVec")

    (hit, hitData, hitData.prefetch)
  }
  val spHitPerm = spHitData.perm.getOrElse(0.U.asTypeOf(new PtePermBundle))
  val spHitLevel = spHitData.level.getOrElse(0.U)

  val s2_res = Wire(new PageCacheRespBundle)
  s2_res.l1.apply(l1Hit, l1Pre, l1HitPPN)
  s2_res.l2.apply(l2Hit, l2Pre, l2HitPPN, ecc = l2eccError)
  s2_res.l3.apply(l3Hit, l3Pre, l3HitPPN, l3HitPerm, l3eccError)
  s2_res.sp.apply(spHit, spPre, spHitData.ppn, spHitPerm, false.B, spHitLevel)
  val s2_res_reg = DataHoldBypass(s2_res, RegNext(stage1.fire()))

  // stage3, add stage 3 for ecc check...
  val s3_res = Reg(new PageCacheRespBundle)
  when (stage2.fire()) {
    s3_res := s2_res_reg
  }

  io.resp.bits.req_info   := stage3.bits.req_info
  io.resp.bits.hit      := s3_res.l3.hit || s3_res.sp.hit
  io.resp.bits.prefetch := s3_res.l3.pre && s3_res.l3.hit || s3_res.sp.pre && s3_res.sp.hit
  io.resp.bits.toFsm.l1Hit := s3_res.l1.hit
  io.resp.bits.toFsm.l2Hit := s3_res.l2.hit
  io.resp.bits.toFsm.ppn   := Mux(s3_res.l2.hit, s3_res.l2.ppn, s3_res.l1.ppn)
  io.resp.bits.toTlb.tag   := stage3.bits.req_info.vpn
  io.resp.bits.toTlb.asid  := io.csr.satp.asid // DontCare
  io.resp.bits.toTlb.ppn   := Mux(s3_res.l3.hit, s3_res.l3.ppn, s3_res.sp.ppn)
  io.resp.bits.toTlb.perm.map(_ := Mux(s3_res.l3.hit, s3_res.l3.perm, s3_res.sp.perm))
  io.resp.bits.toTlb.level.map(_ := Mux(s3_res.l3.hit, 2.U, s3_res.sp.level))
  io.resp.bits.toTlb.prefetch := from_pre(stage3.bits.req_info.source)
  io.resp.valid := stage3.valid
  assert(!(l3Hit && spHit), "normal page and super page both hit")

  // refill Perf
  val l1RefillPerf = Wire(Vec(l2tlbParams.l1Size, Bool()))
  val l2RefillPerf = Wire(Vec(l2tlbParams.l2nWays, Bool()))
  val l3RefillPerf = Wire(Vec(l2tlbParams.l3nWays, Bool()))
  val spRefillPerf = Wire(Vec(l2tlbParams.spSize, Bool()))
  l1RefillPerf.map(_ := false.B)
  l2RefillPerf.map(_ := false.B)
  l3RefillPerf.map(_ := false.B)
  spRefillPerf.map(_ := false.B)

  // refill
  l2.io.w.req <> DontCare
  l3.io.w.req <> DontCare
  l2.io.w.req.valid := false.B
  l3.io.w.req.valid := false.B

  def get_part(data: UInt, index: UInt): UInt = {
    val inner_data = data.asTypeOf(Vec(data.getWidth / XLEN, UInt(XLEN.W)))
    inner_data(index)
  }

  val memRdata = refill.ptes
  val memSelData = get_part(memRdata, refill.addr_low)
  val memPtes = (0 until (l2tlbParams.blockBytes/(XLEN/8))).map(i => memRdata((i+1)*XLEN-1, i*XLEN).asTypeOf(new PteBundle))
  val memPte = memSelData.asTypeOf(new PteBundle)

  memPte.suggestName("memPte")

  // TODO: handle sfenceLatch outsize
  when (io.refill.valid && !memPte.isPf(refill.level) && !flush ) {
    when (refill.level === 0.U && !memPte.isLeaf()) {
      // val refillIdx = LFSR64()(log2Up(l2tlbParams.l1Size)-1,0) // TODO: may be LRU
      val refillIdx = replaceWrapper(l1v, ptwl1replace.way)
      refillIdx.suggestName(s"PtwL1RefillIdx")
      val rfOH = UIntToOH(refillIdx)
      l1(refillIdx).refill(
        refill.req_info.vpn,
        io.csr.satp.asid,
        memSelData,
        0.U,
        refill_prefetch
      )
      ptwl1replace.access(refillIdx)
      l1v := l1v | rfOH
      l1g := (l1g & ~rfOH) | Mux(memPte.perm.g, rfOH, 0.U)

      for (i <- 0 until l2tlbParams.l1Size) {
        l1RefillPerf(i) := i.U === refillIdx
      }

      XSDebug(p"[l1 refill] refillIdx:${refillIdx} refillEntry:${l1(refillIdx).genPtwEntry(refill.req_info.vpn, io.csr.satp.asid, memSelData, 0.U, refill_prefetch)}\n")
      XSDebug(p"[l1 refill] l1v:${Binary(l1v)}->${Binary(l1v | rfOH)} l1g:${Binary(l1g)}->${Binary((l1g & ~rfOH) | Mux(memPte.perm.g, rfOH, 0.U))}\n")

      refillIdx.suggestName(s"l1_refillIdx")
      rfOH.suggestName(s"l1_rfOH")
    }

    when (refill.level === 1.U && !memPte.isLeaf()) {
      val refillIdx = genPtwL2SetIdx(refill.req_info.vpn)
      val victimWay = replaceWrapper(RegEnable(VecInit(getl2vSet(refill.req_info.vpn).asBools).asUInt, stage1.fire), ptwl2replace.way(refillIdx))
      val victimWayOH = UIntToOH(victimWay)
      val rfvOH = UIntToOH(Cat(refillIdx, victimWay))
      val wdata = Wire(l2EntryType)
      wdata.gen(
        vpn = refill.req_info.vpn,
        asid = io.csr.satp.asid,
        data = memRdata,
        levelUInt = 1.U,
        refill_prefetch
      )
      l2.io.w.apply(
        valid = true.B,
        setIdx = refillIdx,
        data = wdata,
        waymask = victimWayOH
      )
      ptwl2replace.access(refillIdx, victimWay)
      l2v := l2v | rfvOH
      l2g := l2g & ~rfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, rfvOH, 0.U)

      for (i <- 0 until l2tlbParams.l2nWays) {
        l2RefillPerf(i) := i.U === victimWay
      }

      XSDebug(p"[l2 refill] refillIdx:0x${Hexadecimal(refillIdx)} victimWay:${victimWay} victimWayOH:${Binary(victimWayOH)} rfvOH(in UInt):${Cat(refillIdx, victimWay)}\n")
      XSDebug(p"[l2 refill] refilldata:0x${wdata}\n")
      XSDebug(p"[l2 refill] l2v:${Binary(l2v)} -> ${Binary(l2v | rfvOH)}\n")
      XSDebug(p"[l2 refill] l2g:${Binary(l2g)} -> ${Binary(l2g & ~rfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, rfvOH, 0.U))}\n")

      refillIdx.suggestName(s"l2_refillIdx")
      victimWay.suggestName(s"l2_victimWay")
      victimWayOH.suggestName(s"l2_victimWayOH")
      rfvOH.suggestName(s"l2_rfvOH")
    }

    when (refill.level === 2.U && memPte.isLeaf()) {
      val refillIdx = genPtwL3SetIdx(refill.req_info.vpn)
      val victimWay = replaceWrapper(RegEnable(VecInit(getl3vSet(refill.req_info.vpn).asBools).asUInt, stage1.fire), ptwl3replace.way(refillIdx))
      val victimWayOH = UIntToOH(victimWay)
      val rfvOH = UIntToOH(Cat(refillIdx, victimWay))
      val wdata = Wire(l3EntryType)
      wdata.gen(
        vpn = refill.req_info.vpn,
        asid = io.csr.satp.asid,
        data = memRdata,
        levelUInt = 2.U,
        refill_prefetch
      )
      l3.io.w.apply(
        valid = true.B,
        setIdx = refillIdx,
        data = wdata,
        waymask = victimWayOH
      )
      ptwl3replace.access(refillIdx, victimWay)
      l3v := l3v | rfvOH
      l3g := l3g & ~rfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, rfvOH, 0.U)

      for (i <- 0 until l2tlbParams.l3nWays) {
        l3RefillPerf(i) := i.U === victimWay
      }

      XSDebug(p"[l3 refill] refillIdx:0x${Hexadecimal(refillIdx)} victimWay:${victimWay} victimWayOH:${Binary(victimWayOH)} rfvOH(in UInt):${Cat(refillIdx, victimWay)}\n")
      XSDebug(p"[l3 refill] refilldata:0x${wdata}\n")
      XSDebug(p"[l3 refill] l3v:${Binary(l3v)} -> ${Binary(l3v | rfvOH)}\n")
      XSDebug(p"[l3 refill] l3g:${Binary(l3g)} -> ${Binary(l3g & ~rfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, rfvOH, 0.U))}\n")

      refillIdx.suggestName(s"l3_refillIdx")
      victimWay.suggestName(s"l3_victimWay")
      victimWayOH.suggestName(s"l3_victimWayOH")
      rfvOH.suggestName(s"l3_rfvOH")
    }
    when ((refill.level === 0.U || refill.level === 1.U) && memPte.isLeaf()) {
      val refillIdx = spreplace.way// LFSR64()(log2Up(l2tlbParams.spSize)-1,0) // TODO: may be LRU
      val rfOH = UIntToOH(refillIdx)
      sp(refillIdx).refill(
        refill.req_info.vpn,
        io.csr.satp.asid,
        memSelData,
        refill.level,
        refill_prefetch
      )
      spreplace.access(refillIdx)
      spv := spv | rfOH
      spg := spg & ~rfOH | Mux(memPte.perm.g, rfOH, 0.U)

      for (i <- 0 until l2tlbParams.spSize) {
        spRefillPerf(i) := i.U === refillIdx
      }

      XSDebug(p"[sp refill] refillIdx:${refillIdx} refillEntry:${sp(refillIdx).genPtwEntry(refill.req_info.vpn, io.csr.satp.asid, memSelData, refill.level, refill_prefetch)}\n")
      XSDebug(p"[sp refill] spv:${Binary(spv)}->${Binary(spv | rfOH)} spg:${Binary(spg)}->${Binary(spg & ~rfOH | Mux(memPte.perm.g, rfOH, 0.U))}\n")

      refillIdx.suggestName(s"sp_refillIdx")
      rfOH.suggestName(s"sp_rfOH")
    }
  }

  val l2eccFlush = s3_res.l2.ecc && stage3.fire() // RegNext(l2eccError, init = false.B)
  val l3eccFlush = s3_res.l3.ecc && stage3.fire() // RegNext(l3eccError, init = false.B)
  val eccVpn = stage3.bits.req_info.vpn

  assert(!l2eccFlush)
  assert(!l3eccFlush)
  when (l2eccFlush) {
    val flushSetIdxOH = UIntToOH(genPtwL2SetIdx(eccVpn))
    val flushMask = VecInit(flushSetIdxOH.asBools.map { a => Fill(l2tlbParams.l2nWays, a.asUInt) }).asUInt
    l2v := l2v & ~flushMask
    l2g := l2g & ~flushMask
  }

  when (l3eccFlush) {
    val flushSetIdxOH = UIntToOH(genPtwL3SetIdx(eccVpn))
    val flushMask = VecInit(flushSetIdxOH.asBools.map { a => Fill(l2tlbParams.l3nWays, a.asUInt) }).asUInt
    l3v := l3v & ~flushMask
    l3g := l3g & ~flushMask
  }

  // sfence
  when (sfence.valid) {
    val l1asidhit = VecInit(l1asids.map(_ === sfence.bits.asid)).asUInt
    val spasidhit = VecInit(spasids.map(_ === sfence.bits.asid)).asUInt
    val sfence_vpn = sfence.bits.addr(sfence.bits.addr.getWidth-1, offLen)

    when (sfence.bits.rs1/*va*/) {
      when (sfence.bits.rs2) {
        // all va && all asid
        l1v := 0.U
        l2v := 0.U
        l3v := 0.U
        spv := 0.U
      } .otherwise {
        // all va && specific asid except global

        l1v := l1v & (~l1asidhit | l1g)
        l2v := l2v & l2g
        l3v := l3v & l3g
        spv := spv & (~spasidhit | spg)
      }
    } .otherwise {
      // val flushMask = UIntToOH(genTlbL2Idx(sfence.bits.addr(sfence.bits.addr.getWidth-1, offLen)))
      val flushSetIdxOH = UIntToOH(genPtwL3SetIdx(sfence_vpn))
      // val flushMask = VecInit(flushSetIdxOH.asBools.map(Fill(l2tlbParams.l3nWays, _.asUInt))).asUInt
      val flushMask = VecInit(flushSetIdxOH.asBools.map { a => Fill(l2tlbParams.l3nWays, a.asUInt) }).asUInt
      flushSetIdxOH.suggestName(s"sfence_nrs1_flushSetIdxOH")
      flushMask.suggestName(s"sfence_nrs1_flushMask")

      when (sfence.bits.rs2) {
        // specific leaf of addr && all asid
        l3v := l3v & ~flushMask
        spv := spv & (~VecInit(sp.map(_.hit(sfence_vpn, sfence.bits.asid, ignoreAsid = true))).asUInt | spg)
      } .otherwise {
        // specific leaf of addr && specific asid
        l3v := l3v & (~flushMask | l3g)
        spv := spv & (~VecInit(sp.map(_.hit(sfence_vpn, sfence.bits.asid))).asUInt | spg)
      }
    }
  }

  // Perf Count
  val resp_l3 = s3_res.l3.hit
  val resp_sp = s3_res.sp.hit
  val resp_l1_pre = s3_res.l1.pre
  val resp_l2_pre = s3_res.l2.pre
  val resp_l3_pre = s3_res.l3.pre
  val resp_sp_pre = s3_res.sp.pre
  val base_valid_access_0 = !from_pre(io.resp.bits.req_info.source) && io.resp.fire()
  XSPerfAccumulate("access", base_valid_access_0)
  XSPerfAccumulate("l1_hit", base_valid_access_0 && io.resp.bits.toFsm.l1Hit && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.hit)
  XSPerfAccumulate("l2_hit", base_valid_access_0 && io.resp.bits.toFsm.l2Hit && !io.resp.bits.hit)
  XSPerfAccumulate("l3_hit", base_valid_access_0 && resp_l3)
  XSPerfAccumulate("sp_hit", base_valid_access_0 && resp_sp)
  XSPerfAccumulate("pte_hit",base_valid_access_0 && io.resp.bits.hit)

  XSPerfAccumulate("l1_hit_pre", base_valid_access_0 && resp_l1_pre && io.resp.bits.toFsm.l1Hit && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.hit)
  XSPerfAccumulate("l2_hit_pre", base_valid_access_0 && resp_l2_pre && io.resp.bits.toFsm.l2Hit && !io.resp.bits.hit)
  XSPerfAccumulate("l3_hit_pre", base_valid_access_0 && resp_l3_pre && resp_l3)
  XSPerfAccumulate("sp_hit_pre", base_valid_access_0 && resp_sp_pre && resp_sp)
  XSPerfAccumulate("pte_hit_pre",base_valid_access_0 && (resp_l3_pre && resp_l3 || resp_sp_pre && resp_sp) && io.resp.bits.hit)

  val base_valid_access_1 = from_pre(io.resp.bits.req_info.source) && io.resp.fire()
  XSPerfAccumulate("pre_access", base_valid_access_1)
  XSPerfAccumulate("pre_l1_hit", base_valid_access_1 && io.resp.bits.toFsm.l1Hit && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.hit)
  XSPerfAccumulate("pre_l2_hit", base_valid_access_1 && io.resp.bits.toFsm.l2Hit && !io.resp.bits.hit)
  XSPerfAccumulate("pre_l3_hit", base_valid_access_1 && resp_l3)
  XSPerfAccumulate("pre_sp_hit", base_valid_access_1 && resp_sp)
  XSPerfAccumulate("pre_pte_hit",base_valid_access_1 && io.resp.bits.hit)

  XSPerfAccumulate("pre_l1_hit_pre", base_valid_access_1 && resp_l1_pre && io.resp.bits.toFsm.l1Hit && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.hit)
  XSPerfAccumulate("pre_l2_hit_pre", base_valid_access_1 && resp_l2_pre && io.resp.bits.toFsm.l2Hit && !io.resp.bits.hit)
  XSPerfAccumulate("pre_l3_hit_pre", base_valid_access_1 && resp_l3_pre && resp_l3)
  XSPerfAccumulate("pre_sp_hit_pre", base_valid_access_1 && resp_sp_pre && resp_sp)
  XSPerfAccumulate("pre_pte_hit_pre",base_valid_access_1 && (resp_l3_pre && resp_l3 || resp_sp_pre && resp_sp) && io.resp.bits.hit)

  val base_valid_access_2 = stage3.bits.isFirst && !from_pre(io.resp.bits.req_info.source) && io.resp.fire()
  XSPerfAccumulate("access_first", base_valid_access_2)
  XSPerfAccumulate("l1_hit_first", base_valid_access_2 && io.resp.bits.toFsm.l1Hit && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.hit)
  XSPerfAccumulate("l2_hit_first", base_valid_access_2 && io.resp.bits.toFsm.l2Hit && !io.resp.bits.hit)
  XSPerfAccumulate("l3_hit_first", base_valid_access_2 && resp_l3)
  XSPerfAccumulate("sp_hit_first", base_valid_access_2 && resp_sp)
  XSPerfAccumulate("pte_hit_first",base_valid_access_2 && io.resp.bits.hit)

  XSPerfAccumulate("l1_hit_pre_first", base_valid_access_2 && resp_l1_pre && io.resp.bits.toFsm.l1Hit && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.hit)
  XSPerfAccumulate("l2_hit_pre_first", base_valid_access_2 && resp_l2_pre && io.resp.bits.toFsm.l2Hit && !io.resp.bits.hit)
  XSPerfAccumulate("l3_hit_pre_first", base_valid_access_2 && resp_l3_pre && resp_l3)
  XSPerfAccumulate("sp_hit_pre_first", base_valid_access_2 && resp_sp_pre && resp_sp)
  XSPerfAccumulate("pte_hit_pre_first",base_valid_access_2 && (resp_l3_pre && resp_l3 || resp_sp_pre && resp_sp) && io.resp.bits.hit)

  val base_valid_access_3 = stage3.bits.isFirst && from_pre(io.resp.bits.req_info.source) && io.resp.fire()
  XSPerfAccumulate("pre_access_first", base_valid_access_3)
  XSPerfAccumulate("pre_l1_hit_first", base_valid_access_3 && io.resp.bits.toFsm.l1Hit && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.hit)
  XSPerfAccumulate("pre_l2_hit_first", base_valid_access_3 && io.resp.bits.toFsm.l2Hit && !io.resp.bits.hit)
  XSPerfAccumulate("pre_l3_hit_first", base_valid_access_3 && resp_l3)
  XSPerfAccumulate("pre_sp_hit_first", base_valid_access_3 && resp_sp)
  XSPerfAccumulate("pre_pte_hit_first", base_valid_access_3 && io.resp.bits.hit)

  XSPerfAccumulate("pre_l1_hit_pre_first", base_valid_access_3 && resp_l1_pre && io.resp.bits.toFsm.l1Hit && !io.resp.bits.toFsm.l2Hit && !io.resp.bits.hit)
  XSPerfAccumulate("pre_l2_hit_pre_first", base_valid_access_3 && resp_l2_pre && io.resp.bits.toFsm.l2Hit && !io.resp.bits.hit)
  XSPerfAccumulate("pre_l3_hit_pre_first", base_valid_access_3 && resp_l3_pre && resp_l3)
  XSPerfAccumulate("pre_sp_hit_pre_first", base_valid_access_3 && resp_sp_pre && resp_sp)
  XSPerfAccumulate("pre_pte_hit_pre_first",base_valid_access_3 && (resp_l3_pre && resp_l3 || resp_sp_pre && resp_sp) && io.resp.bits.hit)

  XSPerfAccumulate("rwHarzad", io.req.valid && !io.req.ready)
  XSPerfAccumulate("out_blocked", io.resp.valid && !io.resp.ready)
  l1AccessPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"L1AccessIndex${i}", l) }
  l2AccessPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"L2AccessIndex${i}", l) }
  l3AccessPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"L3AccessIndex${i}", l) }
  spAccessPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"SPAccessIndex${i}", l) }
  l1RefillPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"L1RefillIndex${i}", l) }
  l2RefillPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"L2RefillIndex${i}", l) }
  l3RefillPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"L3RefillIndex${i}", l) }
  spRefillPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"SPRefillIndex${i}", l) }

  XSPerfAccumulate("l1Refill", Cat(l1RefillPerf).orR)
  XSPerfAccumulate("l2Refill", Cat(l2RefillPerf).orR)
  XSPerfAccumulate("l3Refill", Cat(l3RefillPerf).orR)
  XSPerfAccumulate("spRefill", Cat(spRefillPerf).orR)
  XSPerfAccumulate("l1Refill_pre", Cat(l1RefillPerf).orR && refill_prefetch)
  XSPerfAccumulate("l2Refill_pre", Cat(l2RefillPerf).orR && refill_prefetch)
  XSPerfAccumulate("l3Refill_pre", Cat(l3RefillPerf).orR && refill_prefetch)
  XSPerfAccumulate("spRefill_pre", Cat(spRefillPerf).orR && refill_prefetch)

  // debug
  XSDebug(sfence.valid, p"[sfence] original v and g vector:\n")
  XSDebug(sfence.valid, p"[sfence] l1v:${Binary(l1v)}\n")
  XSDebug(sfence.valid, p"[sfence] l2v:${Binary(l2v)}\n")
  XSDebug(sfence.valid, p"[sfence] l3v:${Binary(l3v)}\n")
  XSDebug(sfence.valid, p"[sfence] l3g:${Binary(l3g)}\n")
  XSDebug(sfence.valid, p"[sfence] spv:${Binary(spv)}\n")
  XSDebug(RegNext(sfence.valid), p"[sfence] new v and g vector:\n")
  XSDebug(RegNext(sfence.valid), p"[sfence] l1v:${Binary(l1v)}\n")
  XSDebug(RegNext(sfence.valid), p"[sfence] l2v:${Binary(l2v)}\n")
  XSDebug(RegNext(sfence.valid), p"[sfence] l3v:${Binary(l3v)}\n")
  XSDebug(RegNext(sfence.valid), p"[sfence] l3g:${Binary(l3g)}\n")
  XSDebug(RegNext(sfence.valid), p"[sfence] spv:${Binary(spv)}\n")

  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(8))
  })
  val perfEvents = Seq(
    ("access           ", base_valid_access_0             ),
    ("l1_hit           ", l1Hit                           ),
    ("l2_hit           ", l2Hit                           ),
    ("l3_hit           ", l3Hit                           ),
    ("sp_hit           ", spHit                           ),
    ("pte_hit          ", l3Hit || spHit                  ),
    ("rwHarzad         ",  io.req.valid && !io.req.ready  ),
    ("out_blocked      ",  io.resp.valid && !io.resp.ready),
  )

  for (((perf_out,(perf_name,perf)),i) <- perfinfo.perfEvents.perf_events.zip(perfEvents).zipWithIndex) {
    perf_out.incr_step := RegNext(perf)
  }
}
