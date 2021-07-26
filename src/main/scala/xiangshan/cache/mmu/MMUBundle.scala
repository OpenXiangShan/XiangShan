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
import xiangshan._
import utils._
import xiangshan.backend.roq.RoqPtr
import xiangshan.backend.fu.util.HasCSRConst
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._

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

class TlbPermBundle(implicit p: Parameters) extends TlbBundle {
  val pf = Bool() // NOTE: if this is true, just raise pf
  // pagetable perm (software defined)
  val d = Bool()
  val a = Bool()
  val g = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()
  // pma perm (hardwired)
  val pr = Bool() //readable
  val pw = Bool() //writeable
  val pe = Bool() //executable
  val pa = Bool() //atom op permitted
  val pi = Bool() //icacheable
  val pd = Bool() //dcacheable

  override def toPrintable: Printable = {
    p"pf:${pf} d:${d} a:${a} g:${g} u:${u} x:${x} w:${w} r:${r}"
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
    array(io.w.bits.index) := io.w.bits.data
  }
}

class TlbSPMeta(implicit p: Parameters) extends TlbBundle {
  val tag = UInt(vpnLen.W) // tag is vpn
  val level = UInt(1.W) // 1 for 2MB, 0 for 1GB

  def hit(vpn: UInt): Bool = {
    val a = tag(vpnnLen*3-1, vpnnLen*2) === vpn(vpnnLen*3-1, vpnnLen*2)
    val b = tag(vpnnLen*2-1, vpnnLen*1) === vpn(vpnnLen*2-1, vpnnLen*1)
    XSDebug(Mux(level.asBool, a&b, a), p"Hit superpage: hit:${Mux(level.asBool, a&b, a)} tag:${Hexadecimal(tag)} level:${level} a:${a} b:${b} vpn:${Hexadecimal(vpn)}\n")
    Mux(level.asBool, a&b, a)
  }

  def apply(vpn: UInt, level: UInt) = {
    this.tag := vpn
    this.level := level(0)

    this
  }

}

class TlbData(superpage: Boolean = false)(implicit p: Parameters) extends TlbBundle {
  val level = if(superpage) Some(UInt(1.W)) else None // /*2 for 4KB,*/ 1 for 2MB, 0 for 1GB
  val ppn = UInt(ppnLen.W)
  val perm = new TlbPermBundle

  def genPPN(vpn: UInt): UInt = {
    if (superpage) {
      val insideLevel = level.getOrElse(0.U)
      Mux(insideLevel.asBool, Cat(ppn(ppn.getWidth-1, vpnnLen*1), vpn(vpnnLen*1-1, 0)),
                              Cat(ppn(ppn.getWidth-1, vpnnLen*2), vpn(vpnnLen*2-1, 0)))
    } else {
      ppn
    }
  }

  def apply(ppn: UInt, level: UInt, perm: UInt, pf: Bool) = {
    this.level.map(_ := level(0))
    this.ppn := ppn
    // refill pagetable perm
    val ptePerm = perm.asTypeOf(new PtePermBundle)
    this.perm.pf:= pf
    this.perm.d := ptePerm.d
    this.perm.a := ptePerm.a
    this.perm.g := ptePerm.g
    this.perm.u := ptePerm.u
    this.perm.x := ptePerm.x
    this.perm.w := ptePerm.w
    this.perm.r := ptePerm.r

    // get pma perm
    val (pmaMode, accessWidth) = AddressSpace.memmapAddrMatch(Cat(ppn, 0.U(12.W)))
    this.perm.pr := PMAMode.read(pmaMode)
    this.perm.pw := PMAMode.write(pmaMode)
    this.perm.pe := PMAMode.execute(pmaMode)
    this.perm.pa := PMAMode.atomic(pmaMode)
    this.perm.pi := PMAMode.icache(pmaMode)
    this.perm.pd := PMAMode.dcache(pmaMode)

    this
  }

  override def toPrintable: Printable = {
    val insideLevel = level.getOrElse(0.U)
    p"level:${insideLevel} ppn:${Hexadecimal(ppn)} perm:${perm}"
  }

  override def cloneType: this.type = (new TlbData(superpage)).asInstanceOf[this.type]
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
}

class TlbReq(implicit p: Parameters) extends TlbBundle {
  val vaddr = UInt(VAddrBits.W)
  val cmd = TlbCmd()
  val roqIdx = new RoqPtr
  val debug = new Bundle {
    val pc = UInt(XLEN.W)
    val isFirstIssue = Bool()
  }

  override def toPrintable: Printable = {
    p"vaddr:0x${Hexadecimal(vaddr)} cmd:${cmd} pc:0x${Hexadecimal(debug.pc)} roqIdx:${roqIdx}"
  }
}

class TlbResp(implicit p: Parameters) extends TlbBundle {
  val paddr = UInt(PAddrBits.W)
  val miss = Bool()
  val mmio = Bool()
  val excp = new Bundle {
    val pf = new Bundle {
      val ld = Bool()
      val st = Bool()
      val instr = Bool()
    }
    val af = new Bundle {
      val ld = Bool()
      val st = Bool()
      val instr = Bool()
    }
  }
  val ptwBack = Bool() // when ptw back, wake up replay rs's state

  override def toPrintable: Printable = {
    p"paddr:0x${Hexadecimal(paddr)} miss:${miss} excp.pf: ld:${excp.pf.ld} st:${excp.pf.st} instr:${excp.pf.instr} ptwBack:${ptwBack}"
  }
}

class TlbRequestIO()(implicit p: Parameters) extends TlbBundle {
  val req = DecoupledIO(new TlbReq)
  val resp = Flipped(DecoupledIO(new TlbResp))
}

class BlockTlbRequestIO()(implicit p: Parameters) extends TlbBundle {
  val req = DecoupledIO(new TlbReq)
  val resp = Flipped(DecoupledIO(new TlbResp))
}

class TlbPtwIO(Width: Int = 1)(implicit p: Parameters) extends TlbBundle {
  val req = Vec(Width, DecoupledIO(new PtwReq))
  val resp = Flipped(DecoupledIO(new PtwResp))

  override def cloneType: this.type = (new TlbPtwIO(Width)).asInstanceOf[this.type]

  override def toPrintable: Printable = {
    p"req(0):${req(0).valid} ${req(0).ready} ${req(0).bits} | resp:${resp.valid} ${resp.ready} ${resp.bits}"
  }
}

class TlbIO(Width: Int)(implicit p: Parameters) extends TlbBundle {
  val requestor = Vec(Width, Flipped(new TlbRequestIO))
  val ptw = new TlbPtwIO(Width)
  val sfence = Input(new SfenceBundle)
  val csr = Input(new TlbCsrBundle)

  override def cloneType: this.type = (new TlbIO(Width)).asInstanceOf[this.type]
}


/****************************  PTW  *************************************/
abstract class PtwBundle(implicit p: Parameters) extends XSBundle with HasPtwConst
abstract class PtwModule(outer: PTW) extends LazyModuleImp(outer)
  with HasXSParameter with HasPtwConst

class PteBundle(implicit p: Parameters) extends PtwBundle{
  val reserved  = UInt(pteResLen.W)
  val ppn  = UInt(ppnLen.W)
  val rsw  = UInt(2.W)
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
    isLeaf() && !(level === 2.U ||
                  level === 1.U && ppn(vpnnLen-1,   0) === 0.U ||
                  level === 0.U && ppn(vpnnLen*2-1, 0) === 0.U)
  }

  def isPf(level: UInt) = {
    !perm.v || (!perm.r && perm.w) || unaligned(level)
  }

  def isLeaf() = {
    perm.r || perm.x || perm.w
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

  override def toPrintable: Printable = {
    p"ppn:0x${Hexadecimal(ppn)} perm:b${Binary(perm.asUInt)}"
  }
}

class PtwEntry(tagLen: Int, hasPerm: Boolean = false, hasLevel: Boolean = false)(implicit p: Parameters) extends PtwBundle {
  val tag = UInt(tagLen.W)
  val ppn = UInt(ppnLen.W)
  val perm = if (hasPerm) Some(new PtePermBundle) else None
  val level = if (hasLevel) Some(UInt(log2Up(Level).W)) else None

  def hit(vpn: UInt, allType: Boolean = false) = {
    require(vpn.getWidth == vpnLen)
    if (allType) {
      require(hasLevel)
      val hit0 = tag(tagLen - 1,    vpnnLen*2) === vpn(tagLen - 1, vpnnLen*2)
      val hit1 = tag(vpnnLen*2 - 1, vpnnLen)   === vpn(vpnnLen*2 - 1,  vpnnLen)
      val hit2 = tag(vpnnLen - 1,     0)         === vpn(vpnnLen - 1, 0)
      Mux(level.getOrElse(0.U) === 2.U, hit2 && hit1 && hit0, Mux(level.getOrElse(0.U) === 1.U, hit1 && hit0, hit0))
    } else if (hasLevel) {
      val hit0 = tag(tagLen - 1, tagLen - vpnnLen) === vpn(vpnLen - 1, vpnLen - vpnnLen)
      val hit1 = tag(tagLen - vpnnLen - 1, tagLen - vpnnLen * 2) === vpn(vpnLen - vpnnLen - 1, vpnLen - vpnnLen * 2)
      Mux(level.getOrElse(0.U) === 0.U, hit0, hit0 && hit1)
    } else {
      tag === vpn(vpnLen - 1, vpnLen - tagLen)
    }
  }

  def refill(vpn: UInt, pte: UInt, level: UInt = 0.U) {
    tag := vpn(vpnLen - 1, vpnLen - tagLen)
    ppn := pte.asTypeOf(pteBundle).ppn
    perm.map(_ := pte.asTypeOf(pteBundle).perm)
    this.level.map(_ := level)
  }

  def genPtwEntry(vpn: UInt, pte: UInt, level: UInt = 0.U) = {
    val e = Wire(new PtwEntry(tagLen, hasPerm, hasLevel))
    e.refill(vpn, pte, level)
    e
  }

  override def cloneType: this.type = (new PtwEntry(tagLen, hasPerm, hasLevel)).asInstanceOf[this.type]

  override def toPrintable: Printable = {
    // p"tag:0x${Hexadecimal(tag)} ppn:0x${Hexadecimal(ppn)} perm:${perm}"
    p"tag:0x${Hexadecimal(tag)} ppn:0x${Hexadecimal(ppn)} " +
      (if (hasPerm) p"perm:${perm.getOrElse(0.U.asTypeOf(new PtePermBundle))} " else p"") +
      (if (hasLevel) p"level:${level.getOrElse(0.U)}" else p"")
  }
}

class PtwEntries(num: Int, tagLen: Int, level: Int, hasPerm: Boolean)(implicit p: Parameters) extends PtwBundle {
  require(log2Up(num)==log2Down(num))

  val tag  = UInt(tagLen.W)
  val ppns = Vec(num, UInt(ppnLen.W))
  val vs   = Vec(num, Bool())
  val perms = if (hasPerm) Some(Vec(num, new PtePermBundle)) else None
  // println(s"PtwEntries: tag:1*${tagLen} ppns:${num}*${ppnLen} vs:${num}*1")

  def tagClip(vpn: UInt) = {
    require(vpn.getWidth == vpnLen)
    vpn(vpnLen - 1, vpnLen - tagLen)
  }

  def sectorIdxClip(vpn: UInt, level: Int) = {
    getVpnClip(vpn, level)(log2Up(num) - 1, 0)
  }

  def hit(vpn: UInt) = {
    tag === tagClip(vpn) && vs(sectorIdxClip(vpn, level)) // TODO: optimize this. don't need to compare each with tag
  }

  def genEntries(vpn: UInt, data: UInt, levelUInt: UInt) = {
    require((data.getWidth / XLEN) == num,
      "input data length must be multiple of pte length")

    val ps = Wire(new PtwEntries(num, tagLen, level, hasPerm))
    ps.tag := tagClip(vpn)
    for (i <- 0 until num) {
      val pte = data((i+1)*XLEN-1, i*XLEN).asTypeOf(new PteBundle)
      ps.ppns(i) := pte.ppn
      ps.vs(i)   := !pte.isPf(levelUInt) && (if (hasPerm) pte.isLeaf() else !pte.isLeaf())
      ps.perms.map(_(i) := pte.perm)
    }
    ps
  }

  override def cloneType: this.type = (new PtwEntries(num, tagLen, level, hasPerm)).asInstanceOf[this.type]
  override def toPrintable: Printable = {
    // require(num == 4, "if num is not 4, please comment this toPrintable")
    // NOTE: if num is not 4, please comment this toPrintable
    val permsInner = perms.getOrElse(0.U.asTypeOf(Vec(num, new PtePermBundle)))
    p"tag:0x${Hexadecimal(tag)} ppns:${printVec(ppns)} vs:${Binary(vs.asUInt)} " +
      (if (hasPerm) p"perms:${printVec(permsInner)}" else p"")
  }
}

class PtwReq(implicit p: Parameters) extends PtwBundle {
  val vpn = UInt(vpnLen.W)

  override def toPrintable: Printable = {
    p"vpn:0x${Hexadecimal(vpn)}"
  }
}

class PtwResp(implicit p: Parameters) extends PtwBundle {
  val entry = new PtwEntry(tagLen = vpnLen, hasPerm = true, hasLevel = true)
  val pf  = Bool()

  override def toPrintable: Printable = {
    p"entry:${entry} pf:${pf}"
  }
}

class PtwIO(implicit p: Parameters) extends PtwBundle {
  val tlb = Vec(PtwWidth, Flipped(new TlbPtwIO))
  val sfence = Input(new SfenceBundle)
  val csr = Input(new TlbCsrBundle)
}

object ValidHold {
  def apply(infire: Bool, outfire: Bool, flush: Bool = false.B ) = {
    val valid = RegInit(false.B)
    when (outfire) { valid := false.B }
    when (infire) { valid := true.B }
    when (flush) { valid := false.B } // NOTE: the flush will flush in & out, is that ok?
    valid
  }
}

object OneCycleValid {
  def apply(fire: Bool, flush: Bool = false.B) = {
    val valid = RegInit(false.B)
    when (valid) { valid := false.B }
    when (fire) { valid := true.B }
    when (flush) { valid := false.B }
    valid
  }
}