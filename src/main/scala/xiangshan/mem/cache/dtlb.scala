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

trait HasTLBConst extends HasXSParameter with HasMEMConst {
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

abstract class TlbBundle extends Bundle with HasTLBConst
abstract class TlbModule extends Module with HasTLBConst

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

class TLBEntry extends TlbBundle {
  val vpn = UInt(vpnLen.W) // tag is vpn
  val ppn = UInt(ppnLen.W)
  val level = UInt(log2Up(Level).W) // 0 for 4KB, 1 for 2MB, 2 for 1GB
  // val asid = UInt(asidLen.W), asid maybe expensive to support, but useless
  // val v = Bool() // v&g is special, may need sperate storage?
  val perm = new PermBundle(hasV = false)

  def vpnHit(vpn: UInt) = {
    val fullMask = VecInit((Seq.fill(vpnLen)(true.B))).asUInt
    val maskLevel = VecInit((0 until Level).map{i => 
      VecInit(Seq.fill(vpnLen-i*vpnnLen)(true.B) ++ Seq.fill(i*vpnnLen)(false.B)).asUInt})
    val mask = maskLevel(level)
    (mask&this.vpn) === (mask&vpn)
  }

  // def asidHit(asid: UInt) = {
  //   this.asid === asid
  // }

  def hit(vpn: UInt, asid: UInt) = {
    vpnHit(vpn)// && asidHit(asid)
  }

  def genTLBEntry(pte: UInt, level: UInt, vpn: UInt/*, asid: UInt*/) = {
    val e = new TLBEntry
    e.ppn := pte.asTypeOf(pteBundle).ppn
    e.level := level
    e.vpn := vpn
    e.perm := pte.asTypeOf(pteBundle).perm
    // e.asid := asid
    e
  }
}

class DtlbReq extends TlbBundle {
  val vaddr = UInt(VAddrBits.W)
  val cmd = SimpleBusCmd() // TODO: turn to Bool
}

class DtlbResp extends TlbBundle {
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

class DtlbToLsuIO extends TlbBundle {
  val req = Vec(TLBWidth, Flipped(Valid(new DtlbReq)))
  val resp = Vec(TLBWidth, Valid(new DtlbResp))
}

class PTWReq extends TlbBundle {
  val vpn = UInt(vpnLen.W)
}

class PTWResp extends TlbBundle {
  val pte = UInt(XLEN.W)
  val level = UInt(log2Up(Level).W)
}

class TlbPtwIO extends TlbBundle {
  val req = DecoupledIO(new PTWReq)
  val resp = Flipped(DecoupledIO(new PTWResp))
}

class TlbIssQueIO extends TlbBundle{
  val miss = Output(Vec(TLBWidth, Bool()))
  val missCanIss = Output(Bool())
}

class SfenceBundle extends TlbBundle{
  val rs1 = Bool()
  val rs2 = Bool()
  val addr = UInt(vpnLen.W)
  // val asid = UInt(asidLen.W)
}

class SfenceIO extends ValidIO(new SfenceBundle)

class TlbCsrBundle extends TlbBundle {
  
}

class TlbCsrIO extends TlbBundle {
  val satp = Output(new Bundle {
    val mode = UInt(4.W) // TODO: may change number to parameter
    val asid = UInt(16.W)
    val ppn  = UInt(44.W)
  })
  val priv = Output(new Bundle {
    val mxr = Bool()
    val sum = Bool()
  })
}

class DtlbIO extends TlbBundle {
  val lsu = new DtlbToLsuIO
  val ptw = new TlbPtwIO
  val issQue = new TlbIssQueIO
  val sfence = new SfenceIO
  val csr = Flipped(new TlbCsrIO)
}

class FakeDtlb extends TlbModule {
  val io = IO(new DtlbIO)
  // Dtlb has 4 ports: 2 for load, 2 fore store 
  io <> DontCare
  // fake dtlb
  (0 until LoadPipelineWidth + StorePipelineWidth).map(i => {
    io.lsu.resp(i).valid := io.lsu.req(i).valid
    io.lsu.resp(i).bits.paddr := io.lsu.req(i).bits.vaddr
    io.lsu.resp(i).bits.miss := false.B
  })
}

class DTLB extends TlbModule {
  val io = IO(new DtlbIO)

  val req = io.lsu.req
  val resp = io.lsu.resp
  val valid = req.map(_.valid)
  val sfence = io.sfence
  val satp = io.csr.satp
  
  val reqAddr = io.lsu.req.map(_.bits.vaddr.asTypeOf(vaBundle2))
  val cmd = io.lsu.req.map(_.bits.cmd)

  val v = RegInit(VecInit(Seq.fill(TLBEntrySize)(false.B)).asUInt)
  val entry = Reg(Vec(TLBEntrySize, new TLBEntry))
  // val g = entry.map(_.perm.g) // g is not used, for asid is not used

  val hitVec = (0 until TLBWidth) map { i => (v & VecInit(entry.map(e => e.hit(reqAddr(i).vpn, satp.asid))).asUInt).asBools }
  val hit = (0 until TLBWidth) map { i => ParallelOR(hitVec(i)).asBool }
  val hitppn = (0 until TLBWidth) map { i => ParallelMux(hitVec(i) zip entry.map(_.ppn)) }
  val multiHit = {
    val hitSum = (0 until TLBWidth) map { i =>  PopCount(hitVec(i)) }
    ParallelOR((0 until TLBWidth) map { i => !(hitSum(i) === 0.U || hitSum(i) === 1.U) })
  }
  assert(!multiHit) // add multiHit here, later it should be removed (maybe), turn to miss and flush

  val excp_tmp = false.B // TODO: add exception check

  // resp
  for(i <- 0 until TLBWidth) {
    // io.lsu.req(i).ready := io.resp(i).ready // true.B // ValidIO
    io.lsu.resp(i).valid := valid(i) && hit(i)
    io.lsu.resp(i).bits.paddr := Cat(hitppn(i), reqAddr(i).off)
    io.lsu.resp(i).bits.miss := ~hit(i)
    io.lsu.resp(i).bits.excp.pf.ld := excp_tmp
    io.lsu.resp(i).bits.excp.pf.st := excp_tmp
    io.lsu.resp(i).bits.excp.pf.instr := excp_tmp
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
  val state_rdy :: state_wait :: Nil = Enum(2)
  val state = RegInit(state_rdy)

  val vpnPtw = RegEnable(io.ptw.req.bits.vpn, io.ptw.req.fire())
  switch (state) {
    is (state_rdy) {
      for(i <- TLBWidth-1 to 0 by -1) {
        when (!hit(i) && io.ptw.req.fire()) {
          state := state_wait
          io.ptw.req.valid := true.B
          io.ptw.req.bits.vpn := reqAddr(i).vpn
        }
        assert(!io.ptw.resp.valid)
      }
    }
    
    is (state_wait) {
      io.ptw.resp.ready := true.B
      when (io.ptw.resp.fire()) {
        state := state_rdy
      }
    }
  }

  // refill
  val ptwResp = io.ptw.resp
  val refill = ptwResp.fire() 
  val refillIdx = LFSR64()(log2Up(TLBEntrySize)-1,0)
  when (refill) {
    v := v | (1.U << refillIdx)
    entry(refillIdx) := new TLBEntry().genTLBEntry(ptwResp.bits.pte, ptwResp.bits.level, vpnPtw)
  }

  // issQue
  io.issQue.miss := (~VecInit(hit).asUInt).asBools
  io.issQue.missCanIss := io.ptw.resp.fire() // one cycle fire
}
