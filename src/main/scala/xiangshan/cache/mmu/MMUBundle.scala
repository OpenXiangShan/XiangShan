/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.fu.util.HasCSRConst
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import xiangshan.backend.fu.{PMPReqBundle, PMPConfig}
import xiangshan.backend.fu.PMPBundle


abstract class TlbBundle(implicit p: Parameters) extends XSBundle with HasTlbConst
abstract class TlbModule(implicit p: Parameters) extends XSModule with HasTlbConst


class PtePermBundle(implicit p: Parameters) extends TlbBundle {
  val d = Bool()
  val a = Bool()
  val g = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()

  override def toPrintable: Printable = {
    p"d:${d} a:${a} g:${g} u:${u} x:${x} w:${w} r:${r}"// +
    //(if(hasV) (p"v:${v}") else p"")
  }
}

class TlbPMBundle(implicit p: Parameters) extends TlbBundle {
  val r = Bool()
  val w = Bool()
  val x = Bool()
  val c = Bool()
  val atomic = Bool()

  def assign_ap(pm: PMPConfig) = {
    r := pm.r
    w := pm.w
    x := pm.x
    c := pm.c
    atomic := pm.atomic
  }
}

class TlbPermBundle(implicit p: Parameters) extends TlbBundle {
  val pf = Bool() // NOTE: if this is true, just raise pf
  val af = Bool() // NOTE: if this is true, just raise af
  // pagetable perm (software defined)
  val d = Bool()
  val a = Bool()
  val g = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()

  def apply(item: PtwSectorResp) = {
    val ptePerm = item.entry.perm.get.asTypeOf(new PtePermBundle().cloneType)
    this.pf := item.pf
    this.af := item.af
    this.d := ptePerm.d
    this.a := ptePerm.a
    this.g := ptePerm.g
    this.u := ptePerm.u
    this.x := ptePerm.x
    this.w := ptePerm.w
    this.r := ptePerm.r

    this
  }

  def applyS2(item: HptwResp) = {
    val ptePerm = item.entry.perm.get.asTypeOf(new PtePermBundle().cloneType)
    this.pf := item.gpf
    this.af := item.gaf
    this.d := ptePerm.d
    this.a := ptePerm.a
    this.g := ptePerm.g
    this.u := ptePerm.u
    this.x := ptePerm.x
    this.w := ptePerm.w
    this.r := ptePerm.r

    this
  }

  override def toPrintable: Printable = {
    p"pf:${pf} af:${af} d:${d} a:${a} g:${g} u:${u} x:${x} w:${w} r:${r} "
  }
}

class TlbSectorPermBundle(implicit p: Parameters) extends TlbBundle {
  val pf = Bool() // NOTE: if this is true, just raise pf
  val af = Bool() // NOTE: if this is true, just raise af
  // pagetable perm (software defined)
  val d = Bool()
  val a = Bool()
  val g = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()

  def apply(item: PtwSectorResp) = {
    val ptePerm = item.entry.perm.get.asTypeOf(new PtePermBundle().cloneType)
    this.pf := item.pf
    this.af := item.af
    this.d := ptePerm.d
    this.a := ptePerm.a
    this.g := ptePerm.g
    this.u := ptePerm.u
    this.x := ptePerm.x
    this.w := ptePerm.w
    this.r := ptePerm.r

    this
  }
  override def toPrintable: Printable = {
    p"pf:${pf} af:${af} d:${d} a:${a} g:${g} u:${u} x:${x} w:${w} r:${r} "
  }
}

// multi-read && single-write
// input is data, output is hot-code(not one-hot)
class CAMTemplate[T <: Data](val gen: T, val set: Int, val readWidth: Int)(implicit p: Parameters) extends TlbModule {
  val io = IO(new Bundle {
    val r = new Bundle {
      val req = Input(Vec(readWidth, gen))
      val resp = Output(Vec(readWidth, Vec(set, Bool())))
    }
    val w = Input(new Bundle {
      val valid = Bool()
      val bits = new Bundle {
        val index = UInt(log2Up(set).W)
        val data = gen
      }
    })
  })

  val wordType = UInt(gen.getWidth.W)
  val array = Reg(Vec(set, wordType))

  io.r.resp.zipWithIndex.map{ case (a,i) =>
    a := array.map(io.r.req(i).asUInt === _)
  }

  when (io.w.valid) {
    array(io.w.bits.index) := io.w.bits.data.asUInt
  }
}

class TlbSectorEntry(pageNormal: Boolean, pageSuper: Boolean)(implicit p: Parameters) extends TlbBundle {
  require(pageNormal && pageSuper)

  val tag = UInt(sectorvpnLen.W)
  val asid = UInt(asidLen.W)
  /* level, 11: 512GB size page(only for sv48)
            10: 1GB size page
            01: 2MB size page
            00: 4KB size page
     future sv57 extension should change level width
  */
  val level = Some(UInt(2.W))
  val ppn = UInt(sectorppnLen.W)
  val pbmt = UInt(ptePbmtLen.W)
  val g_pbmt = UInt(ptePbmtLen.W)
  val perm = new TlbSectorPermBundle
  val valididx = Vec(tlbcontiguous, Bool())
  val pteidx = Vec(tlbcontiguous, Bool())
  val ppn_low = Vec(tlbcontiguous, UInt(sectortlbwidth.W))

  val g_perm = new TlbPermBundle
  val vmid = UInt(vmidLen.W)
  val s2xlate = UInt(2.W)


  /** level usage:
   *  !PageSuper: page is only normal, level is None, match all the tag
   *  !PageNormal: page is only super, level is a Bool(), match high 9*2 parts
   *  bits0  0: need mid 9bits
   *         1: no need mid 9bits
   *  PageSuper && PageNormal: page hold all the three type,
   *  bits0  0: need low 9bits
   *  bits1  0: need mid 9bits
   */

  def hit(vpn: UInt, asid: UInt, nSets: Int = 1, ignoreAsid: Boolean = false, vmid: UInt, hasS2xlate: Bool, onlyS2: Bool = false.B, onlyS1: Bool = false.B): Bool = {
    val asid_hit = Mux(hasS2xlate && onlyS2, true.B, if (ignoreAsid) true.B else (this.asid === asid))
    val addr_low_hit = valididx(vpn(2, 0))
    val vmid_hit = Mux(hasS2xlate, this.vmid === vmid, true.B)
    val isPageSuper = !(level.getOrElse(0.U) === 0.U)
    val pteidx_hit = Mux(hasS2xlate && !isPageSuper && !onlyS1, pteidx(vpn(2, 0)), true.B)

    val tmp_level = level.get
    val tag_matchs = Wire(Vec(Level + 1, Bool()))
    tag_matchs(0) := tag(vpnnLen - sectortlbwidth - 1, 0) === vpn(vpnnLen - 1, sectortlbwidth)
    for (i <- 1 until Level) {
      tag_matchs(i) := tag(vpnnLen * (i + 1) - sectortlbwidth - 1, vpnnLen * i - sectortlbwidth) === vpn(vpnnLen * (i + 1) - 1, vpnnLen * i)
    }
    tag_matchs(Level) := tag(sectorvpnLen - 1, vpnnLen * Level - sectortlbwidth) === vpn(vpnLen - 1, vpnnLen * Level)
    val level_matchs = Wire(Vec(Level + 1, Bool()))
    for (i <- 0 until Level) {
      level_matchs(i) := tag_matchs(i) || tmp_level >= (i + 1).U
    }
    level_matchs(Level) := tag_matchs(Level)

    asid_hit && level_matchs.asUInt.andR && addr_low_hit && vmid_hit && pteidx_hit
  }

  def wbhit(data: PtwRespS2, asid: UInt, nSets: Int = 1, ignoreAsid: Boolean = false, s2xlate: UInt): Bool = {
    val s1vpn = data.s1.entry.tag
    val s2vpn = data.s2.entry.tag(vpnLen - 1, sectortlbwidth)
    val wb_vpn = Mux(s2xlate === onlyStage2, s2vpn, s1vpn)
    val vpn = Cat(wb_vpn, 0.U(sectortlbwidth.W))
    val asid_hit = if (ignoreAsid) true.B else (this.asid === asid)
    val vpn_hit = Wire(Bool())
    val index_hit = Wire(Vec(tlbcontiguous, Bool()))
    val wb_valididx = Wire(Vec(tlbcontiguous, Bool()))
    val hasS2xlate = this.s2xlate =/= noS2xlate
    val onlyS1 = this.s2xlate === onlyStage1
    val onlyS2 = this.s2xlate === onlyStage2
    val pteidx_hit = MuxCase(true.B, Seq(
      onlyS2 -> (VecInit(UIntToOH(data.s2.entry.tag(sectortlbwidth - 1, 0))).asUInt === pteidx.asUInt),
      hasS2xlate -> (pteidx.asUInt === data.s1.pteidx.asUInt)
    ))
    wb_valididx := Mux(s2xlate === onlyStage2, VecInit(UIntToOH(data.s2.entry.tag(sectortlbwidth - 1, 0)).asBools), data.s1.valididx)
    val s2xlate_hit = s2xlate === this.s2xlate

    val tmp_level = level.get
    val tag_matchs = Wire(Vec(Level + 1, Bool()))
    tag_matchs(0) := tag(vpnnLen - sectortlbwidth - 1, 0) === vpn(vpnnLen - 1, sectortlbwidth)
    for (i <- 1 until Level) {
      tag_matchs(i) := tag(vpnnLen * (i + 1) - sectortlbwidth - 1, vpnnLen * i - sectortlbwidth) === vpn(vpnnLen * (i + 1) - 1, vpnnLen * i)
    }
    tag_matchs(Level) := tag(sectorvpnLen - 1, vpnnLen * Level - sectortlbwidth) === vpn(vpnLen - 1, vpnnLen * Level)
    val level_matchs = Wire(Vec(Level + 1, Bool()))
    for (i <- 0 until Level) {
      level_matchs(i) := tag_matchs(i) || tmp_level >= (i + 1).U
    }
    level_matchs(Level) := tag_matchs(Level)
    vpn_hit := asid_hit && level_matchs.asUInt.andR

    for (i <- 0 until tlbcontiguous) {
      index_hit(i) := wb_valididx(i) && valididx(i)
    }

    // For example, tlb req to page cache with vpn 0x10
    // At this time, 0x13 has not been paged, so page cache only resp 0x10
    // When 0x13 refill to page cache, previous item will be flushed
    // Now 0x10 and 0x13 are both valid in page cache
    // However, when 0x13 refill to tlb, will trigger multi hit
    // So will only trigger multi-hit when PopCount(data.valididx) = 1
    vpn_hit && index_hit.reduce(_ || _) && PopCount(wb_valididx) === 1.U && s2xlate_hit && pteidx_hit
  }

  def apply(item: PtwRespS2): TlbSectorEntry = {
    this.asid := item.s1.entry.asid
    val inner_level = MuxLookup(item.s2xlate, 2.U)(Seq(
      onlyStage1 -> item.s1.entry.level.getOrElse(0.U),
      onlyStage2 -> item.s2.entry.level.getOrElse(0.U),
      allStage -> (item.s1.entry.level.getOrElse(0.U) min item.s2.entry.level.getOrElse(0.U)),
      noS2xlate -> item.s1.entry.level.getOrElse(0.U)
    ))
    this.level.map(_ := inner_level)
    this.perm.apply(item.s1)
    this.pbmt := item.s1.entry.pbmt

    val s1tag = item.s1.entry.tag
    val s2tag = item.s2.entry.tag(gvpnLen - 1, sectortlbwidth)
    // if stage1 page is larger than stage2 page, need to merge s1tag and s2tag.
    val s1tagFix = MuxCase(s1tag, Seq(
      (item.s1.entry.level.getOrElse(0.U) === 3.U && item.s2.entry.level.getOrElse(0.U) === 2.U) -> Cat(item.s1.entry.tag(sectorvpnLen - 1, vpnnLen * 3 - sectortlbwidth), item.s2.entry.tag(vpnnLen * 3 - 1, vpnnLen * 2), 0.U((vpnnLen * 2 - sectortlbwidth).W)),
      (item.s1.entry.level.getOrElse(0.U) === 3.U && item.s2.entry.level.getOrElse(0.U) === 1.U) -> Cat(item.s1.entry.tag(sectorvpnLen - 1, vpnnLen * 3 - sectortlbwidth), item.s2.entry.tag(vpnnLen * 3 - 1, vpnnLen), 0.U((vpnnLen - sectortlbwidth).W)),
      (item.s1.entry.level.getOrElse(0.U) === 3.U && item.s2.entry.level.getOrElse(0.U) === 0.U) -> Cat(item.s1.entry.tag(sectorvpnLen - 1, vpnnLen * 3 - sectortlbwidth), item.s2.entry.tag(vpnnLen * 3 - 1, sectortlbwidth)),
      (item.s1.entry.level.getOrElse(0.U) === 2.U && item.s2.entry.level.getOrElse(0.U) === 1.U) -> Cat(item.s1.entry.tag(sectorvpnLen - 1, vpnnLen * 2 - sectortlbwidth), item.s2.entry.tag(vpnnLen * 2 - 1, vpnnLen), 0.U((vpnnLen - sectortlbwidth).W)),
      (item.s1.entry.level.getOrElse(0.U) === 2.U && item.s2.entry.level.getOrElse(0.U) === 0.U) -> Cat(item.s1.entry.tag(sectorvpnLen - 1, vpnnLen * 2 - sectortlbwidth), item.s2.entry.tag(vpnnLen * 2 - 1, sectortlbwidth)),
      (item.s1.entry.level.getOrElse(0.U) === 1.U && item.s2.entry.level.getOrElse(0.U) === 0.U) -> Cat(item.s1.entry.tag(sectorvpnLen - 1, vpnnLen - sectortlbwidth), item.s2.entry.tag(vpnnLen - 1, sectortlbwidth))
    ))
    this.tag := Mux(item.s2xlate === onlyStage2, s2tag, Mux(item.s2xlate === allStage, s1tagFix, s1tag))
    val s2page_pageSuper = item.s2.entry.level.getOrElse(0.U) =/= 0.U
    this.pteidx := Mux(item.s2xlate === onlyStage2, VecInit(UIntToOH(item.s2.entry.tag(sectortlbwidth - 1, 0)).asBools),  item.s1.pteidx)
    val s2_valid = Mux(s2page_pageSuper, VecInit(Seq.fill(tlbcontiguous)(true.B)), VecInit(UIntToOH(item.s2.entry.tag(sectortlbwidth - 1, 0)).asBools))
    this.valididx := Mux(item.s2xlate === onlyStage2, s2_valid, item.s1.valididx)
    // if stage2 page is larger than stage1 page, need to merge s2tag and s2ppn to get a new s2ppn.
    val s1ppn = item.s1.entry.ppn
    val s1ppn_low = item.s1.ppn_low
    val s2ppn = MuxLookup(item.s2.entry.level.getOrElse(0.U), item.s2.entry.ppn(ppnLen - 1, sectortlbwidth))(Seq(
      3.U -> Cat(item.s2.entry.ppn(ppnLen - 1, vpnnLen * 3), item.s2.entry.tag(vpnnLen * 3 - 1, sectortlbwidth)),
      2.U -> Cat(item.s2.entry.ppn(ppnLen - 1, vpnnLen * 2), item.s2.entry.tag(vpnnLen * 2 - 1, sectortlbwidth)),
      1.U -> Cat(item.s2.entry.ppn(ppnLen - 1, vpnnLen), item.s2.entry.tag(vpnnLen - 1, sectortlbwidth))
    ))
    val s2ppn_tmp = MuxLookup(item.s2.entry.level.getOrElse(0.U), item.s2.entry.ppn(ppnLen - 1, 0))(Seq(
      3.U -> Cat(item.s2.entry.ppn(ppnLen - 1, vpnnLen * 3), item.s2.entry.tag(vpnnLen * 3 - 1, 0)),
      2.U -> Cat(item.s2.entry.ppn(ppnLen - 1, vpnnLen * 2), item.s2.entry.tag(vpnnLen * 2 - 1, 0)),
      1.U -> Cat(item.s2.entry.ppn(ppnLen - 1, vpnnLen), item.s2.entry.tag(vpnnLen - 1, 0))
    ))
    val s2ppn_low = VecInit(Seq.fill(tlbcontiguous)(s2ppn_tmp(sectortlbwidth - 1, 0)))
    this.ppn := Mux(item.s2xlate === noS2xlate || item.s2xlate === onlyStage1, s1ppn, s2ppn)
    this.ppn_low := Mux(item.s2xlate === noS2xlate || item.s2xlate === onlyStage1, s1ppn_low, s2ppn_low)
    this.vmid := item.s1.entry.vmid.getOrElse(0.U)
    this.g_pbmt := item.s2.entry.pbmt
    this.g_perm.applyS2(item.s2)
    this.s2xlate := item.s2xlate
    this
  }

  // 4KB is normal entry, 2MB/1GB is considered as super entry
  def is_normalentry(): Bool = {
    if (!pageSuper) { true.B }
    else if (!pageNormal) { false.B }
    else { level.get === 0.U }
  }


  def genPPN(saveLevel: Boolean = false, valid: Bool = false.B)(vpn: UInt) : UInt = {
    val inner_level = level.getOrElse(0.U)
    val ppn_res = Cat(ppn(sectorppnLen - 1, vpnnLen * 3 - sectortlbwidth),
      Mux(inner_level >= "b11".U , vpn(vpnnLen * 3 - 1, vpnnLen * 2), ppn(vpnnLen * 3 - sectortlbwidth - 1, vpnnLen * 2 - sectortlbwidth)),
      Mux(inner_level >= "b10".U , vpn(vpnnLen * 2 - 1, vpnnLen), ppn(vpnnLen * 2 - sectortlbwidth - 1, vpnnLen - sectortlbwidth)),
      Mux(inner_level >= "b01".U , vpn(vpnnLen - 1, 0), Cat(ppn(vpnnLen - sectortlbwidth - 1, 0), ppn_low(vpn(sectortlbwidth - 1, 0)))))

    if (saveLevel)
      RegEnable(ppn_res, valid)
    else
      ppn_res
  }

  def hasS2xlate(): Bool = {
    this.s2xlate =/= noS2xlate
  }

  override def toPrintable: Printable = {
    val inner_level = level.getOrElse(2.U)
    p"asid: ${asid} level:${inner_level} vpn:${Hexadecimal(tag)} ppn:${Hexadecimal(ppn)} perm:${perm}"
  }

}

object TlbCmd {
  def read  = "b00".U
  def write = "b01".U
  def exec  = "b10".U

  def atom_read  = "b100".U // lr
  def atom_write = "b101".U // sc / amo

  def apply() = UInt(3.W)
  def isRead(a: UInt) = a(1,0)===read
  def isWrite(a: UInt) = a(1,0)===write
  def isExec(a: UInt) = a(1,0)===exec

  def isAtom(a: UInt) = a(2)
  def isAmo(a: UInt) = a===atom_write // NOTE: sc mixed
}

// Svpbmt extension
object Pbmt {
  def pma:  UInt = "b00".U  // None
  def nc:   UInt = "b01".U  // Non-cacheable, idempotent, weakly-ordered (RVWMO), main memory
  def io:   UInt = "b10".U  // Non-cacheable, non-idempotent, strongly-ordered (I/O ordering), I/O
  def rsvd: UInt = "b11".U  // Reserved for future standard use
  def width: Int = 2

  def apply() = UInt(2.W)
  def isUncache(a: UInt) = a===nc || a===io
}

class TlbStorageIO(nSets: Int, nWays: Int, ports: Int, nDups: Int = 1)(implicit p: Parameters) extends MMUIOBaseBundle {
  val r = new Bundle {
    val req = Vec(ports, Flipped(DecoupledIO(new Bundle {
      val vpn = Output(UInt(vpnLen.W))
      val s2xlate = Output(UInt(2.W))
    })))
    val resp = Vec(ports, ValidIO(new Bundle{
      val hit = Output(Bool())
      val ppn = Vec(nDups, Output(UInt(ppnLen.W)))
      val pbmt = Vec(nDups, Output(UInt(ptePbmtLen.W)))
      val g_pbmt = Vec(nDups, Output(UInt(ptePbmtLen.W)))
      val perm = Vec(nDups, Output(new TlbSectorPermBundle()))
      val g_perm = Vec(nDups, Output(new TlbPermBundle()))
      val s2xlate = Vec(nDups, Output(UInt(2.W)))
    }))
  }
  val w = Flipped(ValidIO(new Bundle {
    val wayIdx = Output(UInt(log2Up(nWays).W))
    val data = Output(new PtwRespS2)
  }))
  val access = Vec(ports, new ReplaceAccessBundle(nSets, nWays))

  def r_req_apply(valid: Bool, vpn: UInt, i: Int, s2xlate:UInt): Unit = {
    this.r.req(i).valid := valid
    this.r.req(i).bits.vpn := vpn
    this.r.req(i).bits.s2xlate := s2xlate

  }

  def r_resp_apply(i: Int) = {
    (this.r.resp(i).bits.hit, this.r.resp(i).bits.ppn, this.r.resp(i).bits.perm, this.r.resp(i).bits.g_perm, this.r.resp(i).bits.pbmt, this.r.resp(i).bits.g_pbmt)
  }

  def w_apply(valid: Bool, wayIdx: UInt, data: PtwRespS2): Unit = {
    this.w.valid := valid
    this.w.bits.wayIdx := wayIdx
    this.w.bits.data := data
  }

}

class TlbStorageWrapperIO(ports: Int, q: TLBParameters, nDups: Int = 1)(implicit p: Parameters) extends MMUIOBaseBundle {
  val r = new Bundle {
    val req = Vec(ports, Flipped(DecoupledIO(new Bundle {
      val vpn = Output(UInt(vpnLen.W))
      val s2xlate = Output(UInt(2.W))
    })))
    val resp = Vec(ports, ValidIO(new Bundle{
      val hit = Output(Bool())
      val ppn = Vec(nDups, Output(UInt(ppnLen.W)))
      val pbmt = Vec(nDups, Output(UInt(ptePbmtLen.W)))
      val g_pbmt = Vec(nDups, Output(UInt(ptePbmtLen.W)))
      val perm = Vec(nDups, Output(new TlbPermBundle()))
      val g_perm = Vec(nDups, Output(new TlbPermBundle()))
      val s2xlate = Vec(nDups, Output(UInt(2.W)))
    }))
  }
  val w = Flipped(ValidIO(new Bundle {
    val data = Output(new PtwRespS2)
  }))
  val replace = if (q.outReplace) Flipped(new TlbReplaceIO(ports, q)) else null

  def r_req_apply(valid: Bool, vpn: UInt, i: Int, s2xlate: UInt): Unit = {
    this.r.req(i).valid := valid
    this.r.req(i).bits.vpn := vpn
    this.r.req(i).bits.s2xlate := s2xlate
  }

  def r_resp_apply(i: Int) = {
    (this.r.resp(i).bits.hit, this.r.resp(i).bits.ppn, this.r.resp(i).bits.perm, this.r.resp(i).bits.g_perm, this.r.resp(i).bits.s2xlate, this.r.resp(i).bits.pbmt, this.r.resp(i).bits.g_pbmt)
  }

  def w_apply(valid: Bool, data: PtwRespS2): Unit = {
    this.w.valid := valid
    this.w.bits.data := data
  }
}

class ReplaceAccessBundle(nSets: Int, nWays: Int)(implicit p: Parameters) extends TlbBundle {
  val sets = Output(UInt(log2Up(nSets).W))
  val touch_ways = ValidIO(Output(UInt(log2Up(nWays).W)))
}

class ReplaceIO(Width: Int, nSets: Int, nWays: Int)(implicit p: Parameters) extends TlbBundle {
  val access = Vec(Width, Flipped(new ReplaceAccessBundle(nSets, nWays)))

  val refillIdx = Output(UInt(log2Up(nWays).W))
  val chosen_set = Flipped(Output(UInt(log2Up(nSets).W)))

  def apply_sep(in: Seq[ReplaceIO], vpn: UInt): Unit = {
    for ((ac_rep, ac_tlb) <- access.zip(in.map(a => a.access.map(b => b)).flatten)) {
      ac_rep := ac_tlb
    }
    this.chosen_set := get_set_idx(vpn, nSets)
    in.map(a => a.refillIdx := this.refillIdx)
  }
}

class TlbReplaceIO(Width: Int, q: TLBParameters)(implicit p: Parameters) extends
  TlbBundle {
  val page = new ReplaceIO(Width, q.NSets, q.NWays)

  def apply_sep(in: Seq[TlbReplaceIO], vpn: UInt) = {
    this.page.apply_sep(in.map(_.page), vpn)
  }

}

class MemBlockidxBundle(implicit p: Parameters) extends TlbBundle {
  val is_ld = Bool()
  val is_st = Bool()
  val idx = UInt(log2Ceil(VirtualLoadQueueMaxStoreQueueSize).W)
}

class TlbReq(implicit p: Parameters) extends TlbBundle {
  val vaddr = Output(UInt(VAddrBits.W))
  val cmd = Output(TlbCmd())
  val hyperinst = Output(Bool())
  val hlvx = Output(Bool())
  val size = Output(UInt(log2Ceil(log2Ceil(VLEN/8)+1).W))
  val kill = Output(Bool()) // Use for blocked tlb that need sync with other module like icache
  val memidx = Output(new MemBlockidxBundle)
  // do not translate, but still do pmp/pma check
  val no_translate = Output(Bool())
  val pmp_addr = Output(UInt(PAddrBits.W)) // load s1 send prefetch paddr
  val debug = new Bundle {
    val pc = Output(UInt(XLEN.W))
    val robIdx = Output(new RobPtr)
    val isFirstIssue = Output(Bool())
  }

  // Maybe Block req needs a kill: for itlb, itlb and icache may not sync, itlb should wait icache to go ahead
  override def toPrintable: Printable = {
    p"vaddr:0x${Hexadecimal(vaddr)} cmd:${cmd} kill:${kill} pc:0x${Hexadecimal(debug.pc)} robIdx:${debug.robIdx}"
  }
}

class TlbExceptionBundle(implicit p: Parameters) extends TlbBundle {
  val ld = Output(Bool())
  val st = Output(Bool())
  val instr = Output(Bool())
}

class TlbResp(nDups: Int = 1)(implicit p: Parameters) extends TlbBundle {
  val paddr = Vec(nDups, Output(UInt(PAddrBits.W)))
  val gpaddr = Vec(nDups, Output(UInt(GPAddrBits.W)))
  val pbmt = Vec(nDups, Output(UInt(ptePbmtLen.W)))
  val miss = Output(Bool())
  val fastMiss = Output(Bool())
  val excp = Vec(nDups, new Bundle {
    val gpf = new TlbExceptionBundle()
    val pf = new TlbExceptionBundle()
    val af = new TlbExceptionBundle()
  })
  val ptwBack = Output(Bool()) // when ptw back, wake up replay rs's state
  val memidx = Output(new MemBlockidxBundle)

  val debug = new Bundle {
    val robIdx = Output(new RobPtr)
    val isFirstIssue = Output(Bool())
  }
  override def toPrintable: Printable = {
    p"paddr:0x${Hexadecimal(paddr(0))} miss:${miss} excp.pf: ld:${excp(0).pf.ld} st:${excp(0).pf.st} instr:${excp(0).pf.instr} ptwBack:${ptwBack}"
  }
}

class TlbRequestIO(nRespDups: Int = 1)(implicit p: Parameters) extends TlbBundle {
  val req = DecoupledIO(new TlbReq)
  val req_kill = Output(Bool())
  val resp = Flipped(DecoupledIO(new TlbResp(nRespDups)))
}

class TlbPtwIO(Width: Int = 1)(implicit p: Parameters) extends TlbBundle {
  val req = Vec(Width, DecoupledIO(new PtwReq))
  val resp = Flipped(DecoupledIO(new PtwRespS2))


  override def toPrintable: Printable = {
    p"req(0):${req(0).valid} ${req(0).ready} ${req(0).bits} | resp:${resp.valid} ${resp.ready} ${resp.bits}"
  }
}

class TlbPtwIOwithMemIdx(Width: Int = 1)(implicit p: Parameters) extends TlbBundle {
  val req = Vec(Width, DecoupledIO(new PtwReqwithMemIdx))
  val resp = Flipped(DecoupledIO(new PtwRespS2withMemIdx()))


  override def toPrintable: Printable = {
    p"req(0):${req(0).valid} ${req(0).ready} ${req(0).bits} | resp:${resp.valid} ${resp.ready} ${resp.bits}"
  }
}

class TlbHintReq(implicit p: Parameters) extends TlbBundle {
  val id = Output(UInt(log2Up(loadfiltersize).W))
  val full = Output(Bool())
}

class TLBHintResp(implicit p: Parameters) extends TlbBundle {
  val id = Output(UInt(log2Up(loadfiltersize).W))
  // When there are multiple matching entries for PTW resp in filter
  // e.g. vaddr 0, 0x80000000. vaddr 1, 0x80010000
  // these two vaddrs are not in a same 4K Page, so will send to ptw twice
  // However, when ptw resp, if they are in a 1G or 2M huge page
  // The two entries will both hit, and both need to replay
  val replay_all = Output(Bool())
}

class TlbHintIO(implicit p: Parameters) extends TlbBundle {
  val req = Vec(backendParams.LdExuCnt, new TlbHintReq)
  val resp = ValidIO(new TLBHintResp)
}

class MMUIOBaseBundle(implicit p: Parameters) extends TlbBundle {
  val sfence = Input(new SfenceBundle)
  val csr = Input(new TlbCsrBundle)

  def base_connect(sfence: SfenceBundle, csr: TlbCsrBundle): Unit = {
    this.sfence <> sfence
    this.csr <> csr
  }

  // overwrite satp. write satp will cause flushpipe but csr.priv won't
  // satp will be dealyed several cycles from writing, but csr.priv won't
  // so inside mmu, these two signals should be divided
  def base_connect(sfence: SfenceBundle, csr: TlbCsrBundle, satp: TlbSatpBundle) = {
    this.sfence <> sfence
    this.csr <> csr
    this.csr.satp := satp
  }
}

class TlbRefilltoMemIO()(implicit p: Parameters) extends TlbBundle {
  val valid = Bool()
  val memidx = new MemBlockidxBundle
}

class TlbIO(Width: Int, nRespDups: Int = 1, q: TLBParameters)(implicit p: Parameters) extends
  MMUIOBaseBundle {
  val hartId = Input(UInt(hartIdLen.W))
  val requestor = Vec(Width, Flipped(new TlbRequestIO(nRespDups)))
  val flushPipe = Vec(Width, Input(Bool()))
  val redirect = Flipped(ValidIO(new Redirect)) // flush the signal need_gpa in tlb
  val ptw = new TlbPtwIOwithMemIdx(Width)
  val refill_to_mem = Output(new TlbRefilltoMemIO())
  val replace = if (q.outReplace) Flipped(new TlbReplaceIO(Width, q)) else null
  val pmp = Vec(Width, ValidIO(new PMPReqBundle(q.lgMaxSize)))
  val tlbreplay = Vec(Width, Output(Bool()))
}

class VectorTlbPtwIO(Width: Int)(implicit p: Parameters) extends TlbBundle {
  val req = Vec(Width, DecoupledIO(new PtwReqwithMemIdx()))
  val resp = Flipped(DecoupledIO(new Bundle {
    val data = new PtwRespS2withMemIdx
    val vector = Output(Vec(Width, Bool()))
    val getGpa = Output(Vec(Width, Bool()))
  }))

  def connect(normal: TlbPtwIOwithMemIdx): Unit = {
    req <> normal.req
    resp.ready := normal.resp.ready
    normal.resp.bits := resp.bits.data
    normal.resp.valid := resp.valid
  }
}

/****************************  L2TLB  *************************************/
abstract class PtwBundle(implicit p: Parameters) extends XSBundle with HasPtwConst
abstract class PtwModule(outer: L2TLB) extends LazyModuleImp(outer)
  with HasXSParameter with HasPtwConst

class PteBundle(implicit p: Parameters) extends PtwBundle{
  val n = UInt(pteNLen.W)
  val pbmt = UInt(ptePbmtLen.W)
  val reserved  = UInt(pteResLen.W)
  val ppn_high = UInt(ppnHignLen.W)
  val ppn  = UInt(ppnLen.W)
  val rsw  = UInt(pteRswLen.W)
  val perm = new Bundle {
    val d    = Bool()
    val a    = Bool()
    val g    = Bool()
    val u    = Bool()
    val x    = Bool()
    val w    = Bool()
    val r    = Bool()
    val v    = Bool()
  }

  def unaligned(level: UInt) = {
    isLeaf() &&
      !(level === 0.U ||
        level === 1.U && ppn(vpnnLen-1,   0) === 0.U ||
        level === 2.U && ppn(vpnnLen*2-1, 0) === 0.U ||
        level === 3.U && ppn(vpnnLen*3-1, 0) === 0.U)
  }

  def isLeaf() = {
    (perm.r || perm.x || perm.w) && perm.v
  }

  def isNext() = {
    !(perm.r || perm.x || perm.w) && perm.v
  }

  def isPf(level: UInt) = {
    val pf = WireInit(false.B)
    when (reserved =/= 0.U){
      pf := true.B
    }.elsewhen(pbmt === 3.U){
      pf := true.B
    }.elsewhen (isNext()) {
      pf := (perm.u || perm.a || perm.d || n =/= 0.U || pbmt =/= 0.U)
    }.elsewhen (!perm.v || (!perm.r && perm.w)) {
      pf := true.B
    }.otherwise{
      pf := unaligned(level)
    }
    pf
  }

  def isGpf(level: UInt) = {
    val gpf = WireInit(false.B)
    when (isNext()) {
      gpf := (perm.u || perm.a || perm.d )
    }.elsewhen (!perm.v || (!perm.r && perm.w)) {
      gpf := true.B
    }.elsewhen (!perm.u) {
      gpf := true.B
    }.otherwise{
      gpf := unaligned(level)
    }
    gpf
  }

  // ppn of Xiangshan is 48 - 12 bits but ppn of sv48 is 44 bits
  // access fault will be raised when ppn >> ppnLen is not zero
  def isAf(): Bool = {
    !(ppn_high === 0.U)
  }

  def isStage1Gpf(mode: UInt) = {
    val sv39_high = Cat(ppn_high, ppn) >> (GPAddrBitsSv39x4 - offLen)
    val sv48_high = Cat(ppn_high, ppn) >> (GPAddrBitsSv48x4 - offLen)
    !(Mux(mode === Sv39, sv39_high, Mux(mode === Sv48, sv48_high, 0.U)) === 0.U)
  }

  def getPerm() = {
    val pm = Wire(new PtePermBundle)
    pm.d := perm.d
    pm.a := perm.a
    pm.g := perm.g
    pm.u := perm.u
    pm.x := perm.x
    pm.w := perm.w
    pm.r := perm.r
    pm
  }
  def getPPN() = {
    Cat(ppn_high, ppn)
  }
  override def toPrintable: Printable = {
    p"ppn:0x${Hexadecimal(ppn)} perm:b${Binary(perm.asUInt)}"
  }
}

class PtwEntry(tagLen: Int, hasPerm: Boolean = false, hasLevel: Boolean = false)(implicit p: Parameters) extends PtwBundle {
  val tag = UInt(tagLen.W)
  val asid = UInt(asidLen.W)
  val vmid = if (HasHExtension) Some(UInt(vmidLen.W)) else None
  val pbmt = UInt(ptePbmtLen.W)
  val ppn = UInt(gvpnLen.W)
  val perm = if (hasPerm) Some(new PtePermBundle) else None
  val level = if (hasLevel) Some(UInt(log2Up(Level + 1).W)) else None
  val prefetch = Bool()
  val v = Bool()

  def is_normalentry(): Bool = {
    if (!hasLevel) true.B
    else level.get === 2.U
  }

  def genPPN(vpn: UInt): UInt = {
    if (!hasLevel) {
      ppn
    } else {
      MuxLookup(level.get, 0.U)(Seq(
        3.U -> Cat(ppn(ppn.getWidth-1, vpnnLen*3), vpn(vpnnLen*3-1, 0)),
        2.U -> Cat(ppn(ppn.getWidth-1, vpnnLen*2), vpn(vpnnLen*2-1, 0)),
        1.U -> Cat(ppn(ppn.getWidth-1, vpnnLen), vpn(vpnnLen-1, 0)),
        0.U -> ppn)
      )
    }
  }

  //s2xlate control whether compare vmid or not
  def hit(vpn: UInt, asid: UInt, vasid: UInt, vmid: UInt, allType: Boolean = false, ignoreAsid: Boolean = false, s2xlate: Bool) = {
    require(vpn.getWidth == vpnLen)
//    require(this.asid.getWidth <= asid.getWidth)
    val asid_value = Mux(s2xlate, vasid, asid)
    val asid_hit = if (ignoreAsid) true.B else (this.asid === asid_value)
    val vmid_hit = Mux(s2xlate, (this.vmid.getOrElse(0.U) === vmid), true.B)
    if (allType) {
      require(hasLevel)
      val tag_match = Wire(Vec(4, Bool())) // 512GB, 1GB, 2MB or 4KB, not parameterized here
      for (i <- 0 until 3) {
        tag_match(i) := tag(vpnnLen * (i + 1) - 1, vpnnLen * i) === vpn(vpnnLen * (i + 1) - 1, vpnnLen * i)
      }
      tag_match(3) := tag(tagLen - 1, vpnnLen * 3) === vpn(tagLen - 1, vpnnLen * 3)

      val level_match = MuxLookup(level.getOrElse(0.U), false.B)(Seq(
        3.U -> tag_match(3),
        2.U -> (tag_match(3) && tag_match(2)),
        1.U -> (tag_match(3) && tag_match(2) && tag_match(1)),
        0.U -> (tag_match(3) && tag_match(2) && tag_match(1) && tag_match(0)))
      )

      asid_hit && vmid_hit && level_match
    } else if (hasLevel) {
      val tag_match = Wire(Vec(3, Bool())) // SuperPage, 512GB, 1GB or 2MB
      tag_match(0) := tag(tagLen - 1, tagLen - vpnnLen - extendVpnnBits) === vpn(vpnLen - 1, vpnLen - vpnnLen - extendVpnnBits)
      for (i <- 1 until 3) {
        tag_match(i) := tag(tagLen - vpnnLen * i - extendVpnnBits - 1, tagLen - vpnnLen * (i + 1) - extendVpnnBits) === vpn(vpnLen - vpnnLen * i - extendVpnnBits - 1, vpnLen - vpnnLen * (i + 1) - extendVpnnBits)
      }

      val level_match = MuxLookup(level.getOrElse(0.U), false.B)(Seq(
        3.U -> tag_match(0),
        2.U -> (tag_match(0) && tag_match(1)),
        1.U -> (tag_match(0) && tag_match(1) && tag_match(2)))
      )

      asid_hit && vmid_hit && level_match
    } else {
      asid_hit && vmid_hit && tag === vpn(vpnLen - 1, vpnLen - tagLen)
    }
  }

  def refill(vpn: UInt, asid: UInt, vmid: UInt, pte: UInt, level: UInt = 0.U, prefetch: Bool, valid: Bool = false.B): Unit = {
    require(this.asid.getWidth <= asid.getWidth) // maybe equal is better, but ugly outside

    tag := vpn(vpnLen - 1, vpnLen - tagLen)
    pbmt := pte.asTypeOf(new PteBundle().cloneType).pbmt
    ppn := pte.asTypeOf(new PteBundle().cloneType).ppn
    perm.map(_ := pte.asTypeOf(new PteBundle().cloneType).perm)
    this.asid := asid
    this.vmid.map(_ := vmid)
    this.prefetch := prefetch
    this.v := valid
    this.level.map(_ := level)
  }

  def genPtwEntry(vpn: UInt, asid: UInt, pte: UInt, level: UInt = 0.U, prefetch: Bool, valid: Bool = false.B) = {
    val e = Wire(new PtwEntry(tagLen, hasPerm, hasLevel))
    e.refill(vpn, asid, pte, level, prefetch, valid)
    e
  }



  override def toPrintable: Printable = {
    // p"tag:0x${Hexadecimal(tag)} ppn:0x${Hexadecimal(ppn)} perm:${perm}"
    p"tag:0x${Hexadecimal(tag)} pbmt: ${pbmt} ppn:0x${Hexadecimal(ppn)} " +
      (if (hasPerm) p"perm:${perm.getOrElse(0.U.asTypeOf(new PtePermBundle))} " else p"") +
      (if (hasLevel) p"level:${level.getOrElse(0.U)}" else p"") +
      p"prefetch:${prefetch}"
  }
}

class PtwSectorEntry(tagLen: Int, hasPerm: Boolean = false, hasLevel: Boolean = false)(implicit p: Parameters) extends PtwEntry(tagLen, hasPerm, hasLevel) {
  override val ppn = UInt(sectorptePPNLen.W)
}

class PtwMergeEntry(tagLen: Int, hasPerm: Boolean = false, hasLevel: Boolean = false)(implicit p: Parameters) extends PtwSectorEntry(tagLen, hasPerm, hasLevel) {
  val ppn_low = UInt(sectortlbwidth.W)
  val af = Bool()
  val pf = Bool()
}

class PtwEntries(num: Int, tagLen: Int, level: Int, hasPerm: Boolean, hasReservedBitforMbist: Boolean)(implicit p: Parameters) extends PtwBundle {
  require(log2Up(num)==log2Down(num))
  // NOTE: hasPerm means that is leaf or not.

  val tag  = UInt(tagLen.W)
  val asid = UInt(asidLen.W)
  val vmid = Some(UInt(vmidLen.W))
  val pbmts = Vec(num, UInt(ptePbmtLen.W))
  val ppns = Vec(num, UInt(gvpnLen.W))
  val vs   = Vec(num, Bool())
  val af   = Vec(num, Bool())
  val perms = if (hasPerm) Some(Vec(num, new PtePermBundle)) else None
  val prefetch = Bool()
  val reservedbit = if(hasReservedBitforMbist) Some(Bool()) else None
  // println(s"PtwEntries: tag:1*${tagLen} ppns:${num}*${ppnLen} vs:${num}*1")
  // NOTE: vs is used for different usage:
  // for l3, which store the leaf(leaves), vs is page fault or not.
  // for l2, which shoule not store leaf, vs is valid or not, that will anticipate in hit check
  // Because, l2 should not store leaf(no perm), it doesn't store perm.
  // If l2 hit a leaf, the perm is still unavailble. Should still page walk. Complex but nothing helpful.
  // TODO: divide vs into validVec and pfVec
  // for l2: may valid but pf, so no need for page walk, return random pte with pf.

  def tagClip(vpn: UInt) = {
    require(vpn.getWidth == vpnLen)
    vpn(vpnLen - 1, vpnLen - tagLen)
  }

  def sectorIdxClip(vpn: UInt, level: Int) = {
    getVpnClip(vpn, level)(log2Up(num) - 1, 0)
  }

  def hit(vpn: UInt, asid: UInt, vasid: UInt, vmid:UInt, ignoreAsid: Boolean = false, s2xlate: Bool) = {
    val asid_value = Mux(s2xlate, vasid, asid)
    val asid_hit = if (ignoreAsid) true.B else (this.asid === asid_value)
    val vmid_hit = Mux(s2xlate, this.vmid.getOrElse(0.U) === vmid, true.B)
    asid_hit && vmid_hit && tag === tagClip(vpn) && (if (hasPerm) true.B else vs(sectorIdxClip(vpn, level)))
  }

  def genEntries(vpn: UInt, asid: UInt, vmid: UInt, data: UInt, levelUInt: UInt, prefetch: Bool, s2xlate: UInt) = {
    require((data.getWidth / XLEN) == num,
      s"input data length must be multiple of pte length: data.length:${data.getWidth} num:${num}")

    val ps = Wire(new PtwEntries(num, tagLen, level, hasPerm, hasReservedBitforMbist))
    ps.tag := tagClip(vpn)
    ps.asid := asid
    ps.vmid.map(_ := vmid)
    ps.prefetch := prefetch
    for (i <- 0 until num) {
      val pte = data((i+1)*XLEN-1, i*XLEN).asTypeOf(new PteBundle)
      ps.pbmts(i) := pte.pbmt
      ps.ppns(i) := pte.ppn
      ps.vs(i)   := !pte.isPf(levelUInt) && (if (hasPerm) pte.isLeaf() else !pte.isLeaf())
      ps.af(i)   := Mux(s2xlate === allStage, false.B, pte.isAf()) // if allstage, this refill is from ptw or llptw, so the af is invalid
      ps.perms.map(_(i) := pte.perm)
    }
    ps.reservedbit.map(_ := true.B)
    ps
  }

  override def toPrintable: Printable = {
    // require(num == 4, "if num is not 4, please comment this toPrintable")
    // NOTE: if num is not 4, please comment this toPrintable
    val permsInner = perms.getOrElse(0.U.asTypeOf(Vec(num, new PtePermBundle)))
    p"asid: ${Hexadecimal(asid)} tag:0x${Hexadecimal(tag)} pbmt:${printVec(pbmts)} ppns:${printVec(ppns)} vs:${Binary(vs.asUInt)} " +
      (if (hasPerm) p"perms:${printVec(permsInner)}" else p"")
  }
}

class PTWEntriesWithEcc(eccCode: Code, num: Int, tagLen: Int, level: Int, hasPerm: Boolean, hasReservedBitforMbist: Boolean = false)(implicit p: Parameters) extends PtwBundle {
  val entries = new PtwEntries(num, tagLen, level, hasPerm, hasReservedBitforMbist)

  val ecc_block = XLEN
  val ecc_info = get_ecc_info()
  val ecc = if (l2tlbParams.enablePTWECC) Some(UInt(ecc_info._1.W)) else None

  def get_ecc_info(): (Int, Int, Int, Int) = {
    val eccBits_per = eccCode.width(ecc_block) - ecc_block

    val data_length = entries.getWidth
    val data_align_num = data_length / ecc_block
    val data_not_align = (data_length % ecc_block) != 0 // ugly code
    val data_unalign_length = data_length - data_align_num * ecc_block
    val eccBits_unalign = eccCode.width(data_unalign_length) - data_unalign_length

    val eccBits = eccBits_per * data_align_num + eccBits_unalign
    (eccBits, eccBits_per, data_align_num, data_unalign_length)
  }

  def encode() = {
    val data = entries.asUInt
    val ecc_slices = Wire(Vec(ecc_info._3, UInt(ecc_info._2.W)))
    for (i <- 0 until ecc_info._3) {
      ecc_slices(i) := eccCode.encode(data((i+1)*ecc_block-1, i*ecc_block)) >> ecc_block
    }
    if (ecc_info._4 != 0) {
      val ecc_unaligned = eccCode.encode(data(data.getWidth-1, ecc_info._3*ecc_block)) >> ecc_info._4
      ecc.map(_ := Cat(ecc_unaligned, ecc_slices.asUInt))
    } else { ecc.map(_ := ecc_slices.asUInt)}
  }

  def decode(): Bool = {
    val data = entries.asUInt
    val res = Wire(Vec(ecc_info._3 + 1, Bool()))
    for (i <- 0 until ecc_info._3) {
      res(i) := {if (ecc_info._2 != 0) eccCode.decode(Cat(ecc.get((i+1)*ecc_info._2-1, i*ecc_info._2), data((i+1)*ecc_block-1, i*ecc_block))).error else false.B}
    }
    if (ecc_info._2 != 0 && ecc_info._4 != 0) {
      res(ecc_info._3) := eccCode.decode(
        Cat(ecc.get(ecc_info._1-1, ecc_info._2*ecc_info._3), data(data.getWidth-1, ecc_info._3*ecc_block))).error
    } else { res(ecc_info._3) := false.B }

    Cat(res).orR
  }

  def gen(vpn: UInt, asid: UInt, vmid: UInt, data: UInt, levelUInt: UInt, prefetch: Bool, s2xlate: UInt) = {
    this.entries := entries.genEntries(vpn, asid, vmid, data, levelUInt, prefetch, s2xlate)
    this.encode()
  }
}

class PtwReq(implicit p: Parameters) extends PtwBundle {
  val vpn = UInt(vpnLen.W) //vpn or gvpn
  val s2xlate = UInt(2.W)
  def hasS2xlate(): Bool = {
    this.s2xlate =/= noS2xlate
  }
  def isOnlyStage2: Bool = {
    this.s2xlate === onlyStage2
  }
  override def toPrintable: Printable = {
    p"vpn:0x${Hexadecimal(vpn)}"
  }
}

class PtwReqwithMemIdx(implicit p: Parameters) extends PtwReq {
  val memidx = new MemBlockidxBundle
  val getGpa = Bool() // this req is to get gpa when having guest page fault
}

class PtwResp(implicit p: Parameters) extends PtwBundle {
  val entry = new PtwEntry(tagLen = vpnLen, hasPerm = true, hasLevel = true)
  val pf = Bool()
  val af = Bool()

  def apply(pf: Bool, af: Bool, level: UInt, pte: PteBundle, vpn: UInt, asid: UInt) = {
    this.entry.level.map(_ := level)
    this.entry.tag := vpn
    this.entry.perm.map(_ := pte.getPerm())
    this.entry.ppn := pte.ppn
    this.entry.pbmt := pte.pbmt
    this.entry.prefetch := DontCare
    this.entry.asid := asid
    this.entry.v := !pf
    this.pf := pf
    this.af := af
  }

  override def toPrintable: Printable = {
    p"entry:${entry} pf:${pf} af:${af}"
  }
}

class HptwResp(implicit p: Parameters) extends PtwBundle {
  val entry = new PtwEntry(tagLen = gvpnLen, hasPerm = true, hasLevel = true)
  val gpf = Bool()
  val gaf = Bool()

  def apply(gpf: Bool, gaf: Bool, level: UInt, pte: PteBundle, vpn: UInt, vmid: UInt) = {
    val resp_pte = Mux(gaf, 0.U.asTypeOf(pte), pte)
    this.entry.level.map(_ := level)
    this.entry.tag := vpn
    this.entry.perm.map(_ := resp_pte.getPerm())
    this.entry.ppn := resp_pte.ppn
    this.entry.pbmt := resp_pte.pbmt
    this.entry.prefetch := DontCare
    this.entry.asid := DontCare
    this.entry.vmid.map(_ := vmid)
    this.entry.v := !gpf
    this.gpf := gpf
    this.gaf := gaf
  }

  def genPPNS2(vpn: UInt): UInt = {
    MuxLookup(entry.level.get, 0.U)(Seq(
      3.U -> Cat(entry.ppn(entry.ppn.getWidth - 1, vpnnLen * 3), vpn(vpnnLen * 3 - 1, 0)),
      2.U -> Cat(entry.ppn(entry.ppn.getWidth - 1, vpnnLen * 2), vpn(vpnnLen * 2 - 1, 0)),
      1.U -> Cat(entry.ppn(entry.ppn.getWidth - 1, vpnnLen), vpn(vpnnLen - 1, 0)),
      0.U -> Cat(entry.ppn(entry.ppn.getWidth - 1, 0))
    ))
  }

  def hit(gvpn: UInt, vmid: UInt): Bool = {
    val vmid_hit = this.entry.vmid.getOrElse(0.U) === vmid
    val tag_match = Wire(Vec(4, Bool())) // 512GB, 1GB, 2MB or 4KB, not parameterized here
    for (i <- 0 until 3) {
      tag_match(i) := entry.tag(vpnnLen * (i + 1)  - 1, vpnnLen * i) === gvpn(vpnnLen * (i + 1)  - 1, vpnnLen * i)
    }
    tag_match(3) := entry.tag(gvpnLen - 1, vpnnLen * 3) === gvpn(gvpnLen - 1, vpnnLen * 3)

    val level_match = MuxLookup(entry.level.getOrElse(0.U), false.B)(Seq(
      3.U -> tag_match(3),
      2.U -> (tag_match(3) && tag_match(2)),
      1.U -> (tag_match(3) && tag_match(2) && tag_match(1)),
      0.U -> (tag_match(3) && tag_match(2) && tag_match(1) && tag_match(0)))
    )

    vmid_hit && level_match
  }
}

class PtwSectorResp(implicit p: Parameters) extends PtwBundle {
  val entry = new PtwSectorEntry(tagLen = sectorvpnLen, hasPerm = true, hasLevel = true)
  val addr_low = UInt(sectortlbwidth.W)
  val ppn_low = Vec(tlbcontiguous, UInt(sectortlbwidth.W))
  val valididx = Vec(tlbcontiguous, Bool())
  val pteidx = Vec(tlbcontiguous, Bool())
  val pf = Bool()
  val af = Bool()


  def genPPN(vpn: UInt): UInt = {
    MuxLookup(entry.level.get, 0.U)(Seq(
      3.U -> Cat(entry.ppn(entry.ppn.getWidth - 1, vpnnLen * 3 - sectortlbwidth), vpn(vpnnLen * 3 - 1, 0)),
      2.U -> Cat(entry.ppn(entry.ppn.getWidth - 1, vpnnLen * 2 - sectortlbwidth), vpn(vpnnLen * 2 - 1, 0)),
      1.U -> Cat(entry.ppn(entry.ppn.getWidth - 1, vpnnLen - sectortlbwidth), vpn(vpnnLen - 1, 0)),
      0.U -> Cat(entry.ppn(entry.ppn.getWidth - 1, 0), ppn_low(vpn(sectortlbwidth - 1, 0))))
    )
  }

  def isLeaf() = {
    (entry.perm.get.r || entry.perm.get.x || entry.perm.get.w) && entry.v
  }

  def isFakePte() = {
    !pf && !entry.v
  }

  def hit(vpn: UInt, asid: UInt, vmid: UInt, allType: Boolean = false, ignoreAsid: Boolean = false, s2xlate: Bool): Bool = {
    require(vpn.getWidth == vpnLen)
    //    require(this.asid.getWidth <= asid.getWidth)
    val asid_hit = if (ignoreAsid) true.B else (this.entry.asid === asid)
    val vmid_hit = Mux(s2xlate, this.entry.vmid.getOrElse(0.U) === vmid, true.B)
    if (allType) {
      val addr_low_hit = valididx(vpn(sectortlbwidth - 1, 0))
      val tag_match = Wire(Vec(4, Bool())) // 512GB, 1GB, 2MB or 4KB, not parameterized here
      tag_match(0) := entry.tag(vpnnLen - sectortlbwidth - 1, 0) === vpn(vpnnLen - 1, sectortlbwidth)
      for (i <- 1 until 3) {
        tag_match(i) := entry.tag(vpnnLen * (i + 1) - sectortlbwidth - 1, vpnnLen * i - sectortlbwidth) === vpn(vpnnLen * (i + 1) - 1, vpnnLen * i)
      }
      tag_match(3) := entry.tag(sectorvpnLen - 1, vpnnLen * 3 - sectortlbwidth) === vpn(vpnLen - 1, vpnnLen * 3)

      val level_match = MuxLookup(entry.level.getOrElse(0.U), false.B)(Seq(
        3.U -> tag_match(3),
        2.U -> (tag_match(3) && tag_match(2)),
        1.U -> (tag_match(3) && tag_match(2) && tag_match(1)),
        0.U -> (tag_match(3) && tag_match(2) && tag_match(1) && tag_match(0)))
      )

      asid_hit && vmid_hit && level_match && addr_low_hit
    } else {
      val addr_low_hit = valididx(vpn(sectortlbwidth - 1, 0))
      val tag_match = Wire(Vec(3, Bool())) // SuperPage, 512GB, 1GB or 2MB
      for (i <- 0 until 3) {
        tag_match(i) := entry.tag(sectorvpnLen - vpnnLen * i - 1, sectorvpnLen - vpnnLen * (i + 1)) === vpn(vpnLen - vpnnLen * i - 1, vpnLen - vpnnLen * (i + 1))
      }

      val level_match = MuxLookup(entry.level.getOrElse(0.U), false.B)(Seq(
        3.U -> tag_match(0),
        2.U -> (tag_match(0) && tag_match(1)),
        1.U -> (tag_match(0) && tag_match(1) && tag_match(2)))
      )

      asid_hit && vmid_hit && level_match && addr_low_hit
    }
  }
}

class PtwMergeResp(implicit p: Parameters) extends PtwBundle {
  val entry = Vec(tlbcontiguous, new PtwMergeEntry(tagLen = sectorvpnLen, hasPerm = true, hasLevel = true))
  val pteidx = Vec(tlbcontiguous, Bool())
  val not_super = Bool()

  def apply(pf: Bool, af: Bool, level: UInt, pte: PteBundle, vpn: UInt, asid: UInt, vmid:UInt, addr_low : UInt, not_super : Boolean = true) = {
    assert(tlbcontiguous == 8, "Only support tlbcontiguous = 8!")
    val resp_pte = pte
    val ptw_resp = Wire(new PtwMergeEntry(tagLen = sectorvpnLen, hasPerm = true, hasLevel = true))
    ptw_resp.ppn := resp_pte.getPPN()(ptePPNLen - 1, sectortlbwidth)
    ptw_resp.ppn_low := resp_pte.getPPN()(sectortlbwidth - 1, 0)
    ptw_resp.pbmt := resp_pte.pbmt
    ptw_resp.level.map(_ := level)
    ptw_resp.perm.map(_ := resp_pte.getPerm())
    ptw_resp.tag := vpn(vpnLen - 1, sectortlbwidth)
    ptw_resp.pf := pf
    ptw_resp.af := af
    ptw_resp.v := resp_pte.perm.v
    ptw_resp.prefetch := DontCare
    ptw_resp.asid := asid
    ptw_resp.vmid.map(_ := vmid)
    this.pteidx := UIntToOH(addr_low).asBools
    this.not_super := not_super.B


    for (i <- 0 until tlbcontiguous) {
      this.entry(i) := ptw_resp
    }
  }

  def genPPN(): UInt = {
    val idx = OHToUInt(pteidx)
    val tag = Cat(entry(idx).tag, idx(sectortlbwidth - 1, 0))
    MuxLookup(entry(idx).level.get, 0.U)(Seq(
      3.U -> Cat(entry(idx).ppn(entry(idx).ppn.getWidth - 1, vpnnLen * 3 - sectortlbwidth), tag(vpnnLen * 3 - 1, 0)),
      2.U -> Cat(entry(idx).ppn(entry(idx).ppn.getWidth - 1, vpnnLen * 2 - sectortlbwidth), tag(vpnnLen * 2 - 1, 0)),
      1.U -> Cat(entry(idx).ppn(entry(idx).ppn.getWidth - 1, vpnnLen - sectortlbwidth), tag(vpnnLen - 1, 0)),
      0.U -> Cat(entry(idx).ppn(entry(idx).ppn.getWidth - 1, 0), entry(idx).ppn_low))
    )
  }
}

class PtwRespS2(implicit p: Parameters) extends PtwBundle {
  val s2xlate = UInt(2.W)
  val s1 = new PtwSectorResp()
  val s2 = new HptwResp()

  def hasS2xlate: Bool = {
    this.s2xlate =/= noS2xlate
  }

  def isOnlyStage2: Bool = {
    this.s2xlate === onlyStage2
  }

  def getVpn(vpn: UInt): UInt = {
    val level = s1.entry.level.getOrElse(0.U) min s2.entry.level.getOrElse(0.U)
    val s1tag = Cat(s1.entry.tag, OHToUInt(s1.pteidx))
    val s1tagFix = MuxCase(s1.entry.tag, Seq(
      (s1.entry.level.getOrElse(0.U) === 3.U && s2.entry.level.getOrElse(0.U) === 2.U) -> Cat(s1.entry.tag(sectorvpnLen - 1, vpnnLen * 3 - sectortlbwidth), s2.entry.tag(vpnnLen * 3 - 1, vpnnLen * 2), 0.U((vpnnLen * 2 - sectortlbwidth).W)),
      (s1.entry.level.getOrElse(0.U) === 3.U && s2.entry.level.getOrElse(0.U) === 1.U) -> Cat(s1.entry.tag(sectorvpnLen - 1, vpnnLen * 3 - sectortlbwidth), s2.entry.tag(vpnnLen * 3 - 1, vpnnLen), 0.U((vpnnLen - sectortlbwidth).W)),
      (s1.entry.level.getOrElse(0.U) === 3.U && s2.entry.level.getOrElse(0.U) === 0.U) -> Cat(s1.entry.tag(sectorvpnLen - 1, vpnnLen * 3 - sectortlbwidth), s2.entry.tag(vpnnLen * 3 - 1, sectortlbwidth)),
      (s1.entry.level.getOrElse(0.U) === 2.U && s2.entry.level.getOrElse(0.U) === 1.U) -> Cat(s1.entry.tag(sectorvpnLen - 1, vpnnLen * 2 - sectortlbwidth), s2.entry.tag(vpnnLen * 2 - 1, vpnnLen), 0.U((vpnnLen - sectortlbwidth).W)),
      (s1.entry.level.getOrElse(0.U) === 2.U && s2.entry.level.getOrElse(0.U) === 0.U) -> Cat(s1.entry.tag(sectorvpnLen - 1, vpnnLen * 2 - sectortlbwidth), s2.entry.tag(vpnnLen * 2 - 1, sectortlbwidth)),
      (s1.entry.level.getOrElse(0.U) === 1.U && s2.entry.level.getOrElse(0.U) === 0.U) -> Cat(s1.entry.tag(sectorvpnLen - 1, vpnnLen - sectortlbwidth), s2.entry.tag(vpnnLen - 1, sectortlbwidth))
    ))
    val s1_vpn = MuxLookup(level, s1tag)(Seq(
      3.U -> Cat(s1tagFix(sectorvpnLen - 1, vpnnLen * 3 - sectortlbwidth), vpn(vpnnLen * 3 - 1, 0)),
      2.U -> Cat(s1tagFix(sectorvpnLen - 1, vpnnLen * 2 - sectortlbwidth), vpn(vpnnLen * 2 - 1, 0)),
      1.U -> Cat(s1tagFix(sectorvpnLen - 1, vpnnLen - sectortlbwidth), vpn(vpnnLen - 1, 0)))
    )
    val s2_vpn = s2.entry.tag
    Mux(s2xlate === onlyStage2, s2_vpn, Mux(s2xlate === allStage, s1_vpn, s1tag))
  }

  def hit(vpn: UInt, asid: UInt, vasid: UInt, vmid: UInt, allType: Boolean = false, ignoreAsid: Boolean = false): Bool = {
    val noS2_hit = s1.hit(vpn, Mux(this.hasS2xlate, vasid, asid), vmid, allType, ignoreAsid, this.hasS2xlate)
    val onlyS2_hit = s2.hit(vpn, vmid)
    // allstage and onlys1 hit
    val s1vpn = Cat(s1.entry.tag, s1.addr_low)
    val level = s1.entry.level.getOrElse(0.U) min s2.entry.level.getOrElse(0.U)

    val tag_match = Wire(Vec(4, Bool())) // 512GB, 1GB, 2MB or 4KB, not parameterized here
    for (i <- 0 until 3) {
      tag_match(i) := vpn(vpnnLen * (i + 1) - 1, vpnnLen * i) === s1vpn(vpnnLen * (i + 1) - 1, vpnnLen * i)
    }
    tag_match(3) := vpn(vpnLen - 1, vpnnLen * 3) === s1vpn(vpnLen - 1, vpnnLen * 3)
    val level_match = MuxLookup(level, false.B)(Seq(
      3.U -> tag_match(3),
      2.U -> (tag_match(3) && tag_match(2)),
      1.U -> (tag_match(3) && tag_match(2) && tag_match(1)),
      0.U -> (tag_match(3) && tag_match(2) && tag_match(1) && tag_match(0)))
    )

    val vpn_hit = level_match
    val vmid_hit = Mux(this.s2xlate === allStage, s2.entry.vmid.getOrElse(0.U) === vmid, true.B)
    val vasid_hit = if (ignoreAsid) true.B else (s1.entry.asid === vasid)
    val all_onlyS1_hit = vpn_hit && vmid_hit && vasid_hit
    Mux(this.s2xlate === noS2xlate, noS2_hit,
      Mux(this.s2xlate === onlyStage2, onlyS2_hit, all_onlyS1_hit))
  }
}

class PtwRespS2withMemIdx(implicit p: Parameters) extends PtwRespS2 {
  val memidx = new MemBlockidxBundle()
  val getGpa = Bool() // this req is to get gpa when having guest page fault
}

class L2TLBIO(implicit p: Parameters) extends PtwBundle {
  val hartId = Input(UInt(hartIdLen.W))
  val tlb = Vec(PtwWidth, Flipped(new TlbPtwIO))
  val sfence = Input(new SfenceBundle)
  val csr = new Bundle {
    val tlb = Input(new TlbCsrBundle)
    val distribute_csr = Flipped(new DistributedCSRIO)
  }
}

class L2TlbMemReqBundle(implicit p: Parameters) extends PtwBundle {
  val addr = UInt(PAddrBits.W)
  val id = UInt(bMemID.W)
  val hptw_bypassed = Bool()
}

class L2TlbInnerBundle(implicit p: Parameters) extends PtwReq {
  val source = UInt(bSourceWidth.W)
}

class L2TlbWithHptwIdBundle(implicit p: Parameters) extends PtwBundle {
  val req_info = new L2TlbInnerBundle
  val isHptwReq = Bool()
  val isLLptw = Bool()
  val hptwId = UInt(log2Up(l2tlbParams.llptwsize).W)
}

object ValidHoldBypass{
  def apply(infire: Bool, outfire: Bool, flush: Bool = false.B) = {
    val valid = RegInit(false.B)
    when (infire) { valid := true.B }
    when (outfire) { valid := false.B } // ATTENTION: order different with ValidHold
    when (flush) { valid := false.B } // NOTE: the flush will flush in & out, is that ok?
    valid || infire
  }
}

class L1TlbDB(implicit p: Parameters) extends TlbBundle {
  val vpn = UInt(vpnLen.W)
}

class PageCacheDB(implicit p: Parameters) extends TlbBundle with HasPtwConst {
  val vpn = UInt(vpnLen.W)
  val source = UInt(bSourceWidth.W)
  val bypassed = Bool()
  val is_first = Bool()
  val prefetched = Bool()
  val prefetch = Bool()
  val l2Hit = Bool()
  val l1Hit = Bool()
  val hit = Bool()
}

class PTWDB(implicit p: Parameters) extends TlbBundle with HasPtwConst {
  val vpn = UInt(vpnLen.W)
  val source = UInt(bSourceWidth.W)
}

class L2TlbPrefetchDB(implicit p: Parameters) extends TlbBundle {
  val vpn = UInt(vpnLen.W)
}

class L2TlbMissQueueDB(implicit p: Parameters) extends TlbBundle {
  val vpn = UInt(vpnLen.W)
}
