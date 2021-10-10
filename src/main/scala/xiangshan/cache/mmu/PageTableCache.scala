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
class PtwCacheIO()(implicit p: Parameters) extends PtwBundle {
  val req = Flipped(DecoupledIO(new Bundle {
    val vpn = UInt(vpnLen.W)
    val source = UInt(bPtwWidth.W)
    val isReplay = Bool()
  }))
  val resp = DecoupledIO(new Bundle {
    val source = UInt(bPtwWidth.W)
    val vpn = UInt(vpnLen.W)
    val isReplay = Bool()
    val hit = Bool()
    val toFsm = new Bundle {
      val l1Hit = Bool()
      val l2Hit = Bool()
      val ppn = UInt(ppnLen.W)
    }
    val toTlb = new PtwEntry(tagLen = vpnLen, hasPerm = true, hasLevel = true)
  })
  val refill = Flipped(ValidIO(new Bundle {
    val ptes = UInt(blockBits.W)
    val vpn = UInt(vpnLen.W)
    val level = UInt(log2Up(Level).W)
    val addr_low = UInt((log2Up(l2tlbParams.blockBytes) - log2Up(XLEN/8)).W)
  }))
  val sfence = Input(new SfenceBundle)
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

  val first_valid = io.req.valid
  val first_fire = first_valid && io.req.ready
  val first_req = io.req.bits
  val second_ready = Wire(Bool())
  val second_valid = ValidHold(first_fire, io.resp.fire(), sfence.valid)
  val second_req = RegEnable(first_req, first_fire)
  // NOTE: if ptw cache resp may be blocked, hard to handle refill
  // when miss queue is full, please to block itlb and dtlb input

  // when refill, refuce to accept new req
  val rwHarzad = if (sramSinglePort) io.refill.valid else false.B
  io.req.ready := !rwHarzad && (second_ready || io.req.bits.isReplay)
  // NOTE: when write, don't ready, whe
  //       when replay, just come in, out make sure resp.fire()

  // l1: level 0 non-leaf pte
  val l1 = Reg(Vec(l2tlbParams.l1Size, new PtwEntry(tagLen = PtwL1TagLen)))
  val l1v = RegInit(0.U(l2tlbParams.l1Size.W))
  val l1g = Reg(UInt(l2tlbParams.l1Size.W))

  // l2: level 1 non-leaf pte
  val l2 = Module(new SRAMTemplate(
    l2EntryType,
    set = l2tlbParams.l2nSets,
    way = l2tlbParams.l2nWays,
    singlePort = sramSinglePort
  ))
  val l2v = RegInit(0.U((l2tlbParams.l2nSets * l2tlbParams.l2nWays).W))
  val l2g = Reg(UInt((l2tlbParams.l2nSets * l2tlbParams.l2nWays).W))
  def getl2vSet(vpn: UInt) = {
    require(log2Up(l2tlbParams.l2nWays) == log2Down(l2tlbParams.l2nWays))
    val set = genPtwL2SetIdx(vpn)
    require(set.getWidth == log2Up(l2tlbParams.l2nSets))
    val l2vVec = l2v.asTypeOf(Vec(l2tlbParams.l2nSets, UInt(l2tlbParams.l2nWays.W)))
    l2vVec(set)
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
  def getl3vSet(vpn: UInt) = {
    require(log2Up(l2tlbParams.l3nWays) == log2Down(l2tlbParams.l3nWays))
    val set = genPtwL3SetIdx(vpn)
    require(set.getWidth == log2Up(l2tlbParams.l3nSets))
    val l3vVec = l3v.asTypeOf(Vec(l2tlbParams.l3nSets, UInt(l2tlbParams.l3nWays.W)))
    l3vVec(set)
  }

  // sp: level 0/1 leaf pte of 1GB/2MB super pages
  val sp = Reg(Vec(l2tlbParams.spSize, new PtwEntry(tagLen = SPTagLen, hasPerm = true, hasLevel = true)))
  val spv = RegInit(0.U(l2tlbParams.spSize.W))
  val spg = Reg(UInt(l2tlbParams.spSize.W))

  // Access Perf
  val l1AccessPerf = Wire(Vec(l2tlbParams.l1Size, Bool()))
  val l2AccessPerf = Wire(Vec(l2tlbParams.l2nWays, Bool()))
  val l3AccessPerf = Wire(Vec(l2tlbParams.l3nWays, Bool()))
  val spAccessPerf = Wire(Vec(l2tlbParams.spSize, Bool()))
  l1AccessPerf.map(_ := false.B)
  l2AccessPerf.map(_ := false.B)
  l3AccessPerf.map(_ := false.B)
  spAccessPerf.map(_ := false.B)

  // l1
  val ptwl1replace = ReplacementPolicy.fromString(l2tlbParams.l1Replacer, l2tlbParams.l1Size)
  val (l1Hit, l1HitPPN) = {
    val hitVecT = l1.zipWithIndex.map { case (e, i) => e.hit(first_req.vpn) && l1v(i) }
    val hitVec = hitVecT.map(RegEnable(_, first_fire))
    val hitPPN = ParallelPriorityMux(hitVec zip l1.map(_.ppn))
    val hit = ParallelOR(hitVec) && second_valid

    when (hit) { ptwl1replace.access(OHToUInt(hitVec)) }

    l1AccessPerf.zip(hitVec).map{ case (l, h) => l := h && RegNext(first_fire)}
    for (i <- 0 until l2tlbParams.l1Size) {
      XSDebug(first_fire, p"[l1] l1(${i.U}) ${l1(i)} hit:${l1(i).hit(first_req.vpn)}\n")
    }
    XSDebug(first_fire, p"[l1] l1v:${Binary(l1v)} hitVecT:${Binary(VecInit(hitVecT).asUInt)}\n")
    XSDebug(second_valid, p"[l1] l1Hit:${hit} l1HitPPN:0x${Hexadecimal(hitPPN)} hitVec:${VecInit(hitVec).asUInt}\n")

    VecInit(hitVecT).suggestName(s"l1_hitVecT")
    VecInit(hitVec).suggestName(s"l1_hitVec")

    (hit, hitPPN)
  }

  // l2
  val ptwl2replace = ReplacementPolicy.fromString(l2tlbParams.l2Replacer,l2tlbParams.l2nWays,l2tlbParams.l2nSets)
  val (l2Hit, l2HitPPN, l2eccError) = {
    val ridx = genPtwL2SetIdx(first_req.vpn)
    val vidx = RegEnable(VecInit(getl2vSet(first_req.vpn).asBools), first_fire)
    l2.io.r.req.valid := first_fire
    l2.io.r.req.bits.apply(setIdx = ridx)
    val ramDatas = l2.io.r.resp.data
    // val hitVec = VecInit(ramDatas.map{wayData => wayData.hit(first_req.vpn) })
    val hitVec = VecInit(ramDatas.zip(vidx).map { case (wayData, v) => wayData.entries.hit(second_req.vpn) && v })
    val hitWayEntry = ParallelPriorityMux(hitVec zip ramDatas)
    val hitWayData = hitWayEntry.entries
    val hitWayEcc = hitWayEntry.ecc
    val hit = ParallelOR(hitVec) && second_valid
    val hitWay = ParallelPriorityMux(hitVec zip (0 until l2tlbParams.l2nWays).map(_.U))

    val eccError = ecc.decode(Cat(hitWayEcc, hitWayData.asUInt())).error

    ridx.suggestName(s"l2_ridx")
    vidx.suggestName(s"l2_vidx")
    ramDatas.suggestName(s"l2_ramDatas")
    hitVec.suggestName(s"l2_hitVec")
    hitWayData.suggestName(s"l2_hitWayData")
    hitWay.suggestName(s"l2_hitWay")

    when (hit) { ptwl2replace.access(genPtwL2SetIdx(second_req.vpn), hitWay) }

    l2AccessPerf.zip(hitVec).map{ case (l, h) => l := h && RegNext(first_fire) }
    XSDebug(first_fire, p"[l2] ridx:0x${Hexadecimal(ridx)}\n")
    for (i <- 0 until l2tlbParams.l2nWays) {
      XSDebug(RegNext(first_fire), p"[l2] ramDatas(${i.U}) ${ramDatas(i)}  l2v:${vidx(i)}  hit:${ramDatas(i).entries.hit(second_req.vpn)}\n")
    }
    XSDebug(second_valid, p"[l2] l2Hit:${hit} l2HitPPN:0x${Hexadecimal(hitWayData.ppns(genPtwL2SectorIdx(second_req.vpn)))} hitVec:${Binary(hitVec.asUInt)} hitWay:${hitWay} vidx:${Binary(vidx.asUInt)}\n")

    (hit && !eccError, hitWayData.ppns(genPtwL2SectorIdx(second_req.vpn)), hit && eccError)
  }

  // l3
  val ptwl3replace = ReplacementPolicy.fromString(l2tlbParams.l3Replacer,l2tlbParams.l3nWays,l2tlbParams.l3nSets)
  val (l3Hit, l3HitData, l3eccError) = {
    val ridx = genPtwL3SetIdx(first_req.vpn)
    val vidx = RegEnable(VecInit(getl3vSet(first_req.vpn).asBools), first_fire)
    l3.io.r.req.valid := first_fire
    l3.io.r.req.bits.apply(setIdx = ridx)
    val ramDatas = l3.io.r.resp.data
    val hitVec = VecInit(ramDatas.zip(vidx).map{ case (wayData, v) => wayData.entries.hit(second_req.vpn) && v })
    val hitWayEntry = ParallelPriorityMux(hitVec zip ramDatas)
    val hitWayData = hitWayEntry.entries
    val hitWayEcc = hitWayEntry.ecc
    val hit = ParallelOR(hitVec) && second_valid
    val hitWay = ParallelPriorityMux(hitVec zip (0 until l2tlbParams.l3nWays).map(_.U))

    val eccError = ecc.decode(Cat(hitWayEcc, hitWayData.asUInt())).error

    when (hit) { ptwl3replace.access(genPtwL3SetIdx(second_req.vpn), hitWay) }

    l3AccessPerf.zip(hitVec).map{ case (l, h) => l := h && RegNext(first_fire) }
    XSDebug(first_fire, p"[l3] ridx:0x${Hexadecimal(ridx)}\n")
    for (i <- 0 until l2tlbParams.l3nWays) {
      XSDebug(RegNext(first_fire), p"[l3] ramDatas(${i.U}) ${ramDatas(i)}  l3v:${vidx(i)}  hit:${ramDatas(i).entries.hit(second_req.vpn)}\n")
    }
    XSDebug(second_valid, p"[l3] l3Hit:${hit} l3HitData:${hitWayData} hitVec:${Binary(hitVec.asUInt)} hitWay:${hitWay} vidx:${Binary(vidx.asUInt)}\n")

    ridx.suggestName(s"l3_ridx")
    vidx.suggestName(s"l3_vidx")
    ramDatas.suggestName(s"l3_ramDatas")
    hitVec.suggestName(s"l3_hitVec")
    hitWay.suggestName(s"l3_hitWay")

    (hit && !eccError, hitWayData, hit && eccError)
  }
  val l3HitPPN = l3HitData.ppns(genPtwL3SectorIdx(second_req.vpn))
  val l3HitPerm = l3HitData.perms.getOrElse(0.U.asTypeOf(Vec(PtwL3SectorSize, new PtePermBundle)))(genPtwL3SectorIdx(second_req.vpn))

  // super page
  val spreplace = ReplacementPolicy.fromString(l2tlbParams.spReplacer, l2tlbParams.spSize)
  val (spHit, spHitData) = {
    val hitVecT = sp.zipWithIndex.map { case (e, i) => e.hit(first_req.vpn) && spv(i) }
    val hitVec = hitVecT.map(RegEnable(_, first_fire))
    val hitData = ParallelPriorityMux(hitVec zip sp)
    val hit = ParallelOR(hitVec) && second_valid

    when (hit) { spreplace.access(OHToUInt(hitVec)) }

    spAccessPerf.zip(hitVec).map{ case (s, h) => s := h && RegNext(first_fire) }
    for (i <- 0 until l2tlbParams.spSize) {
      XSDebug(first_fire, p"[sp] sp(${i.U}) ${sp(i)} hit:${sp(i).hit(first_req.vpn)} spv:${spv(i)}\n")
    }
    XSDebug(second_valid, p"[sp] spHit:${hit} spHitData:${hitData} hitVec:${Binary(VecInit(hitVec).asUInt)}\n")

    VecInit(hitVecT).suggestName(s"sp_hitVecT")
    VecInit(hitVec).suggestName(s"sp_hitVec")

    (hit, hitData)
  }
  val spHitPerm = spHitData.perm.getOrElse(0.U.asTypeOf(new PtePermBundle))
  val spHitLevel = spHitData.level.getOrElse(0.U)

  val resp = Wire(io.resp.bits.cloneType)
  val resp_latch = RegEnable(resp, io.resp.valid && !io.resp.ready)
  val resp_latch_valid = ValidHold(io.resp.valid && !io.resp.ready, io.resp.ready, sfence.valid)
  second_ready := !(second_valid || resp_latch_valid) || io.resp.fire()
  resp.source   := second_req.source
  resp.vpn      := second_req.vpn
  resp.isReplay := second_req.isReplay
  resp.hit      := l3Hit || spHit
  resp.toFsm.l1Hit := l1Hit
  resp.toFsm.l2Hit := l2Hit
  resp.toFsm.ppn   := Mux(l2Hit, l2HitPPN, l1HitPPN)
  resp.toTlb.tag   := second_req.vpn
  resp.toTlb.ppn   := Mux(l3Hit, l3HitPPN, spHitData.ppn)
  resp.toTlb.perm.map(_ := Mux(l3Hit, l3HitPerm, spHitPerm))
  resp.toTlb.level.map(_ := Mux(l3Hit, 2.U, spHitLevel))

  io.resp.valid := second_valid
  io.resp.bits := Mux(resp_latch_valid, resp_latch, resp)
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
  when (io.refill.valid && !memPte.isPf(refill.level) && !sfence.valid ) {
    when (refill.level === 0.U && !memPte.isLeaf()) {
      // val refillIdx = LFSR64()(log2Up(l2tlbParams.l1Size)-1,0) // TODO: may be LRU
      val refillIdx = replaceWrapper(l1v, ptwl1replace.way)
      refillIdx.suggestName(s"PtwL1RefillIdx")
      val rfOH = UIntToOH(refillIdx)
      l1(refillIdx).refill(refill.vpn, memSelData)
      ptwl1replace.access(refillIdx)
      l1v := l1v | rfOH
      l1g := (l1g & ~rfOH) | Mux(memPte.perm.g, rfOH, 0.U)

      for (i <- 0 until l2tlbParams.l1Size) {
        l1RefillPerf(i) := i.U === refillIdx
      }

      XSDebug(p"[l1 refill] refillIdx:${refillIdx} refillEntry:${l1(refillIdx).genPtwEntry(refill.vpn, memSelData)}\n")
      XSDebug(p"[l1 refill] l1v:${Binary(l1v)}->${Binary(l1v | rfOH)} l1g:${Binary(l1g)}->${Binary((l1g & ~rfOH) | Mux(memPte.perm.g, rfOH, 0.U))}\n")

      refillIdx.suggestName(s"l1_refillIdx")
      rfOH.suggestName(s"l1_rfOH")
    }

    when (refill.level === 1.U && !memPte.isLeaf()) {
      val refillIdx = genPtwL2SetIdx(refill.vpn)
      val victimWay = replaceWrapper(RegEnable(VecInit(getl2vSet(refill.vpn).asBools).asUInt, first_fire), ptwl2replace.way(refillIdx))
      val victimWayOH = UIntToOH(victimWay)
      val rfvOH = UIntToOH(Cat(refillIdx, victimWay))
      val wdata = Wire(l2EntryType)
      wdata.entries := wdata.entries.genEntries(vpn = refill.vpn, data = memRdata, levelUInt = 1.U)
      wdata.ecc := ecc.encode(wdata.entries.asUInt()) >> wdata.entries.getWidth
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
      XSDebug(p"[l2 refill] refilldata:0x${
        (new PtwEntries(num = PtwL2SectorSize, tagLen = PtwL2TagLen, level = 1, hasPerm = false)).genEntries(
          vpn = refill.vpn, data = memRdata, levelUInt = 1.U)
      }\n")
      XSDebug(p"[l2 refill] l2v:${Binary(l2v)} -> ${Binary(l2v | rfvOH)}\n")
      XSDebug(p"[l2 refill] l2g:${Binary(l2g)} -> ${Binary(l2g & ~rfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, rfvOH, 0.U))}\n")

      refillIdx.suggestName(s"l2_refillIdx")
      victimWay.suggestName(s"l2_victimWay")
      victimWayOH.suggestName(s"l2_victimWayOH")
      rfvOH.suggestName(s"l2_rfvOH")
    }

    when (refill.level === 2.U && memPte.isLeaf()) {
      val refillIdx = genPtwL3SetIdx(refill.vpn)
      val victimWay = replaceWrapper(RegEnable(VecInit(getl3vSet(refill.vpn).asBools).asUInt, first_fire), ptwl3replace.way(refillIdx))
      val victimWayOH = UIntToOH(victimWay)
      val rfvOH = UIntToOH(Cat(refillIdx, victimWay))
      val wdata = Wire(l3EntryType)
      wdata.entries := wdata.entries.genEntries(vpn = refill.vpn, data = memRdata, levelUInt = 2.U)
      wdata.ecc := ecc.encode(wdata.entries.asUInt()) >> wdata.entries.getWidth
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
      XSDebug(p"[l3 refill] refilldata:0x${
        (new PtwEntries(num = PtwL3SectorSize, tagLen = PtwL3TagLen, level = 2, hasPerm = true)).genEntries(
          vpn = refill.vpn, data = memRdata, levelUInt = 2.U)
      }\n")
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
      sp(refillIdx).refill(refill.vpn, memSelData, refill.level)
      spreplace.access(refillIdx)
      spv := spv | rfOH
      spg := spg & ~rfOH | Mux(memPte.perm.g, rfOH, 0.U)

      for (i <- 0 until l2tlbParams.spSize) {
        spRefillPerf(i) := i.U === refillIdx
      }

      XSDebug(p"[sp refill] refillIdx:${refillIdx} refillEntry:${sp(refillIdx).genPtwEntry(refill.vpn, memSelData, refill.level)}\n")
      XSDebug(p"[sp refill] spv:${Binary(spv)}->${Binary(spv | rfOH)} spg:${Binary(spg)}->${Binary(spg & ~rfOH | Mux(memPte.perm.g, rfOH, 0.U))}\n")

      refillIdx.suggestName(s"sp_refillIdx")
      rfOH.suggestName(s"sp_rfOH")
    }
  }

  val l2eccFlush = RegNext(l2eccError, init = false.B)
  val l3eccFlush = RegNext(l3eccError, init = false.B)
  val eccVpn = RegNext(second_req.vpn)

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
    when (sfence.bits.rs1/*va*/) {
      when (sfence.bits.rs2) {
        // all va && all asid
        l1v := 0.U
        l2v := 0.U
        l3v := 0.U
        spv := 0.U
      } .otherwise {
        // all va && specific asid except global
        l1v := l1v & l1g
        l2v := l2v & l2g
        l3v := l3v & l3g
        spv := spv & spg
      }
    } .otherwise {
      // val flushMask = UIntToOH(genTlbL2Idx(sfence.bits.addr(sfence.bits.addr.getWidth-1, offLen)))
      val flushSetIdxOH = UIntToOH(genPtwL3SetIdx(sfence.bits.addr(sfence.bits.addr.getWidth-1, offLen)))
      // val flushMask = VecInit(flushSetIdxOH.asBools.map(Fill(l2tlbParams.l3nWays, _.asUInt))).asUInt
      val flushMask = VecInit(flushSetIdxOH.asBools.map { a => Fill(l2tlbParams.l3nWays, a.asUInt) }).asUInt
      flushSetIdxOH.suggestName(s"sfence_nrs1_flushSetIdxOH")
      flushMask.suggestName(s"sfence_nrs1_flushMask")
      when (sfence.bits.rs2) {
        // specific leaf of addr && all asid
        l3v := l3v & ~flushMask
        l3g := l3g & ~flushMask
      } .otherwise {
        // specific leaf of addr && specific asid
        l3v := l3v & (~flushMask | l3g)
      }
      spv := 0.U
    }
  }

  // Perf Count
  XSPerfAccumulate("access", second_valid)
  XSPerfAccumulate("l1_hit", l1Hit)
  XSPerfAccumulate("l2_hit", l2Hit)
  XSPerfAccumulate("l3_hit", l3Hit)
  XSPerfAccumulate("sp_hit", spHit)
  XSPerfAccumulate("pte_hit", l3Hit || spHit)
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
}
