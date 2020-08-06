package xiangshan.mem.cache

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap
import xiangshan.mem._
import xiangshan.mem.pipeline._
import bus.simplebus._
import xiangshan.backend.fu.HasCSRConst

trait HasTlbConst extends HasXSParameter {
  val Level = 3

  val offLen  = 12
  val ppn0Len = 9
  val ppn1Len = 9
  val ppn2Len = PAddrBits - offLen - ppn0Len - ppn1Len
  val ppnnLen = 9
  val ppnLen  = PAddrBits - offLen
  val vpnnLen = 9
  val vpnLen  = VAddrBits - offLen

  val entryLen = XLEN
  val flagLen = 8
  val pteResLen = XLEN - ppnLen - 2 - flagLen
  val asidLen = 16

  def vaBundle = new Bundle {
    val vpn2 = UInt(vpnnLen.W)
    val vpn1 = UInt(vpnnLen.W)
    val vpn0 = UInt(vpnnLen.W)
    val off  = UInt( offLen.W)
  }

  def vaBundle2 = new Bundle {
    val vpn  = UInt(vpnLen.W)
    val off  = UInt(offLen.W)
  }

  def paBundle = new Bundle {
    val ppn2 = UInt(ppn2Len.W)
    val ppn1 = UInt(ppn1Len.W)
    val ppn0 = UInt(ppn0Len.W)
    val off  = UInt( offLen.W)
  }

  def paBundle2 = new Bundle {
    val ppn  = UInt(ppnLen.W)
    val off  = UInt(offLen.W)
  }

  def pteBundle = new Bundle {
    val reserved  = UInt(pteResLen.W)
    val ppn  = UInt(ppnLen.W)
    val rsw  = UInt(2.W)
    val perm = new Bundle {
      val d    = UInt(1.W)
      val a    = UInt(1.W)
      val g    = UInt(1.W)
      val u    = UInt(1.W)
      val x    = UInt(1.W)
      val w    = UInt(1.W)
      val r    = UInt(1.W)
      val v    = UInt(1.W)
    }
  }
}

abstract class TlbBundle extends Bundle with HasTlbConst
abstract class TlbModule extends Module with HasTlbConst

class PermBundle(val hasV: Boolean = true) extends TlbBundle {
  val d = Bool()
  val a = Bool()
  val g = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()
  if (hasV) { val v = Bool() }
}

class TlbEntry extends TlbBundle {
  val vpn = UInt(vpnLen.W) // tag is vpn
  val ppn = UInt(ppnLen.W)
  val level = UInt(log2Up(Level).W) // 0 for 4KB, 1 for 2MB, 2 for 1GB
  // val asid = UInt(asidLen.W), asid maybe expensive to support, but useless
  // val v = Bool() // v&g is special, may need sperate storage?
  val perm = new PermBundle(hasV = false)

  def vpnHit(vpn: UInt):Bool = {
    val fullMask = VecInit((Seq.fill(vpnLen)(true.B))).asUInt
    val maskLevel = VecInit((0 until Level).map{i =>
      VecInit(Seq.fill(vpnLen-i*vpnnLen)(true.B) ++ Seq.fill(i*vpnnLen)(false.B)).asUInt})
    val mask = maskLevel(level)
    (mask&this.vpn) === (mask&vpn)
  }

  // def asidHit(asid: UInt) = {
  //   this.asid === asid
  // }

  def hit(vpn: UInt/*, asid: UInt*/):Bool = {
    vpnHit(vpn)// && asidHit(asid)
  }

  def genTlbEntry(pte: UInt, level: UInt, vpn: UInt/*, asid: UInt*/) = {
    val e = Wire(new TlbEntry)
    e.ppn := pte.asTypeOf(pteBundle).ppn
    e.level := level
    e.vpn := vpn
    e.perm := pte.asTypeOf(pteBundle).perm
    // e.asid := asid
    e
  }
}

object TlbCmd {
  def read  = "b00".U
  def write = "b01".U
  def exec  = "b10".U

  def apply() = UInt(2.W)
  def isRead(a: UInt) = a===read
  def isWrite(a: UInt) = a===write
  def isExec(a: UInt) = a===exec
}

class TlbReq extends TlbBundle {
  val vaddr = UInt(VAddrBits.W)
  val cmd = TlbCmd()
}

class TlbResp extends TlbBundle {
  val paddr = UInt(PAddrBits.W)
  val miss = Bool()
  val excp = new Bundle {
    val pf = new Bundle {
      val ld = Bool()
      val st = Bool()
      val instr = Bool()
    }
    // val ma = new Bundle { // may handle in other module
    //   val ld = Bool()
    //   val st = Bool()
    //   val instr = Bool()
    // }
    // val af = new Bundle {
    //   val ld = Bool()
    //   val st = Bool()
    //   val instr = Bool()
    // }
  }
}

class TlbRequestIO(Width: Int) extends TlbBundle {
  val req = Vec(Width, Valid(new TlbReq))
  val resp = Vec(Width, Flipped(Valid(new TlbResp)))

  override def cloneType: this.type = (new TlbRequestIO(Width)).asInstanceOf[this.type]
}

class TlbPtwIO extends TlbBundle {
  val req = DecoupledIO(new PtwReq)
  val resp = Flipped(DecoupledIO(new PtwResp))
}

class TlbIO(Width: Int) extends TlbBundle {
  val requestor = Flipped(new TlbRequestIO(Width))
  val ptw = new TlbPtwIO

  override def cloneType: this.type = (new TlbIO(Width)).asInstanceOf[this.type]
}

class FakeTlb(Width: Int = 1) extends TlbModule {
  val io = IO(new TlbIO(Width))
  // Tlb has 4 ports: 2 for load, 2 for store
  io <> DontCare
  // fake Tlb
  (0 until LoadPipelineWidth + StorePipelineWidth).map(i => {
    io.requestor.resp(i).valid := io.requestor.req(i).valid
    io.requestor.resp(i).bits.paddr := io.requestor.req(i).bits.vaddr
    io.requestor.resp(i).bits.miss := false.B
  })
}

class TLB(Width: Int, isDtlb: Boolean) extends TlbModule with HasCSRConst{
  val io = IO(new TlbIO(Width))

  val req    = io.requestor.req
  val resp   = io.requestor.resp
  val ptw    = io.ptw

  val sfence = WireInit(0.U.asTypeOf(new SfenceBundle))
  val csr    = WireInit(0.U.asTypeOf(new TlbCsrBundle))
  val satp   = csr.satp
  val priv   = csr.priv
  val ifecth = if (isDtlb) false.B else true.B
  val mode   = if (isDtlb) priv.dmode else priv.imode
  BoringUtils.addSink(sfence, "SfenceBundle")
  BoringUtils.addSink(csr, "TLBCSRIO")

  val reqAddr = req.map(_.bits.vaddr.asTypeOf(vaBundle2))
  val cmd     = req.map(_.bits.cmd)
  val valid   = req.map(_.valid)

  def widthMapSeq[T <: Seq[Data]](f: Int => T) = (0 until Width).map(f)
  def widthMap[T <: Data](f: Int => T) = (0 until Width).map(f)

  val v = RegInit(0.U(TlbEntrySize.W))
  val entry = Reg(Vec(TlbEntrySize, new TlbEntry))
  // val g = entry.map(_.perm.g) // g is not used, for asid is not used
  val hitVec = widthMapSeq{ i => 
    (v.asBools zip VecInit(entry.map(_.hit(reqAddr(i).vpn/*, satp.asid*/)))).map{ case (a,b) => a&b } }
  val hit = widthMap{i => ParallelOR(hitVec(i)).asBool & valid(i) }
  val miss = widthMap{i => !hit(i) && valid(i) }
  val hitppn = widthMap{ i => ParallelMux(hitVec(i) zip entry.map(_.ppn)) }
  val hitPerm = widthMap{ i => ParallelMux(hitVec(i) zip entry.map(_.perm)) }
  val multiHit = {
    val hitSum = widthMap{ i => PopCount(hitVec(i)) }
    ParallelOR(widthMap{ i => !(hitSum(i) === 0.U || hitSum(i) === 1.U) })
  }
  assert(!multiHit) // add multiHit here, later it should be removed (maybe), turn to miss and flush

  // resp
  for(i <- 0 until Width) {
    // req(i).ready := resp(i).ready // true.B // ValidIO
    resp(i).valid := valid(i)
    resp(i).bits.paddr := Cat(hitppn(i), reqAddr(i).off)
    resp(i).bits.miss := miss(i)

    val perm = hitPerm(i) // NOTE: given the excp, the out module choose one to use?
    val modeCheck = !(mode === ModeU && !perm.u || mode === ModeS && perm.u && (!priv.sum || ifecth))
    resp(i).bits.excp.pf.ld    := hit(i) && !(modeCheck && (perm.r || priv.mxr && perm.x)) && (TlbCmd.isRead(cmd(i)) && true.B/*!isAMO*/)
    resp(i).bits.excp.pf.st    := hit(i) && !(modeCheck && perm.w) && (TlbCmd.isWrite(cmd(i)) || false.B/*TODO isAMO. */)
    resp(i).bits.excp.pf.instr := hit(i) && !(modeCheck && perm.x) && TlbCmd.isExec(cmd(i))
  }

  // sfence (flush)
  when (sfence.valid) {
    when (sfence.bits.rs1) { // virtual address *.rs1 <- (rs1===0.U)
      when (sfence.bits.rs2) { // asid, but i do not want to support asid, *.rs2 <- (rs2===0.U)
        v := 0.U // all should be flush
      }.otherwise { // all pte but only spec asid
        v := v & ~VecInit(entry.map(e => /*e.asid === sfence.bits.asid && */!e.perm.g)).asUInt
      }
    }.otherwise { // virtual rs1=/=0.U
      when (sfence.bits.rs2) { // asid
        v := v & ~VecInit(entry.map(_.vpn === sfence.bits.addr.asTypeOf(vaBundle2).vpn)).asUInt
      }.otherwise { // particular va and asid
        v := v & ~VecInit(entry.map(e => e.vpn === sfence.bits.addr.asTypeOf(vaBundle2).vpn && (/*e.asid === sfence.bits.asid && */!e.perm.g))).asUInt
      }
    }
  }

  // ptw
  val state_idle :: state_wait :: Nil = Enum(2)
  val state = RegInit(state_idle)

  ptw <> DontCare
  ptw.req.valid := ParallelOR(miss).asBool
  ptw.resp.ready := state === state_wait
  switch (state) {
    is (state_idle) {
      for(i <- Width-1 to 0 by -1) {
        when (miss(i)) {
          state := state_wait
          ptw.req.bits.vpn := reqAddr(i).vpn
          ptw.req.bits.cmd := cmd(i)
        }
        assert(!ptw.resp.valid)
      }
    }

    is (state_wait) {
      when (ptw.resp.fire()) {
        state := state_idle
      }
    }
  }

  // refill
  val refill = ptw.resp.fire()
  val refillIdx = LFSR64()(log2Up(TlbEntrySize)-1,0)
  when (refill) {
    v := v | (1.U << refillIdx)
    entry(refillIdx) := ptw.resp.bits
  }
}
