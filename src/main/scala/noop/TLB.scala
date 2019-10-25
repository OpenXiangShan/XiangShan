package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.simplebus._
import bus.axi4._
import utils._

trait Sv39Const{
  val vpnLen = 27
  val ppnLen = 44
  val Level = 3
  val ppn2Len = 26
  val ppn1Len = 9
  val ppn0Len = 9
  val offLen  = 12
  val vpn2Len = 9
  val vpn1Len = 9
  val vpn0Len = 9
  val vaResLen= 25
  val paResLen= 8

  val paddrLen = 64
  val vaddrLen = 64
  val satpLen = 64
  val satpModeLen = 4
  val asidLen = 16
  val flagLen = 8
  val ptEntryLen = 64

  def vaBundle = new Bundle {
    val reserverd = UInt(vaResLen.W)
    val vpn2 = UInt(vpn2Len.W)
    val vpn1 = UInt(vpn1Len.W)
    val vpn0 = UInt(vpn0Len.W)
    val off  = UInt( offLen.W)
  }

  def vaBundle2 = new Bundle {
    val reserverd = UInt(vaResLen.W)
    val vpn  = UInt(vpnLen.W)
    val off  = UInt(offLen.W)
  }

  def vpnBundle = new Bundle {
    val vpn2 = UInt(vpn2Len.W)
    val vpn1 = UInt(vpn1Len.W)
    val vpn0 = UInt(vpn0Len.W)
  }

  def paBundle = new Bundle {
    val reserved = UInt(paResLen.W)
    val ppn2 = UInt(ppn2Len.W)
    val ppn1 = UInt(ppn1Len.W)
    val ppn0 = UInt(ppn0Len.W)
    val off  = UInt( offLen.W)
  }

  def paBundle2 = new Bundle {
    val reserved = UInt(paResLen.W)
    val ppn  = UInt(ppnLen.W)
    val off  = UInt(offLen.W)
  }
  
  def paddrApply(ppn: UInt, off: UInt):UInt = {
    Cat(0.U(paResLen.W), Cat(ppn, off))
  }
  
  def pteBundle = new Bundle {
    val reserved  = UInt(10.W)
    val ppn  = UInt(ppnLen.W)
    val rsw  = UInt(2.W)
    val flag = UInt(flagLen.W)
  }

  def satpBundle = new Bundle {
    val mode = UInt(satpModeLen.W)
    val asid = UInt(asidLen.W)
    val ppn  = UInt(ppnLen.W)
  }

  def flagBundle = new Bundle {
    val D    = UInt(1.W)
    val A    = UInt(1.W)
    val G    = UInt(1.W)
    val U    = UInt(1.W)
    val X    = UInt(1.W)
    val W    = UInt(1.W)
    val R    = UInt(1.W)
    val V    = UInt(1.W)
  }
}

case class TLBConfig (
  name: String = "tlb",
  userBits: Int = 0,

  totalSize: Int = 128, 
  ways: Int = 128 
)

sealed trait HasTlbConst {
  implicit val tlbConfig: TLBConfig

  val AddrBits: Int
  val XLEN: Int

  val tlbName = tlbConfig.name
  val userBits = tlbConfig.userBits

  val TotalSize = tlbConfig.totalSize
  val Ways = tlbConfig.ways
  val Sets = 1

  val debug = true

  def TlbMetaArrayReadBus() = new SRAMReadBus(new TLBMetaBundle, set = Sets, way = Ways)
  def TlbDataArrayReadBus() = new SRAMReadBus(new TLBDataBundle, set = Sets, way = Ways)
  def TlbMetaArrayWriteBus() = new SRAMWriteBus(new TLBMetaBundle, set = Sets, way = Ways)
  def TlbDataArrayWriteBus() = new SRAMWriteBus(new TLBDataBundle, set = Sets, way = Ways)

  def isSameWord(a1: UInt, a2:UInt) = ((a1 >> 2) === (a2 >> 2))
  //def isSetConflict(a1: UInt, a2: UInt) = (a1.asTypeof(addrBundle).index === a2.asTypeof(addrBundle).index)
}

sealed abstract class TlbBundle(implicit tlbConfig: TLBConfig) extends Bundle with HasNOOPParameter with HasTlbConst with Sv39Const
sealed abstract class TlbModule(implicit tlbConfig: TLBConfig) extends Module with HasNOOPParameter with HasTlbConst with Sv39Const

sealed class TLBMetaBundle(implicit val tlbConfig: TLBConfig) extends TlbBundle {
  val vpn = Output(UInt(vpnLen.W))
  val asid = Output(UInt(asidLen.W))
  val flag = Output(UInt(flagLen.W))

  def apply(vpn: UInt, asid: UInt, flag: UInt) = {
    this.vpn := vpn
    this.asid := asid
    this.flag := flag
    this
  }
}

sealed class TLBDataBundle(implicit val tlbConfig: TLBConfig) extends TlbBundle {
  val ppn = Output(UInt(vpnLen.W))

  def apply(ppn: UInt) = {
    this.ppn := ppn
    this
  }
}

class TlbReq(userBits: Int) extends SimpleBusReqBundle(userBits = userBits) with Sv39Const

class TlbResp(userBits: Int) extends SimpleBusReqBundle(userBits = userBits) with Sv39Const

class TLBIO(userBits: Int) extends Bundle {
  val req = Flipped(Decoupled(new TlbReq(userBits = userBits)))
  val resp = Decoupled(new TlbResp(userBits = userBits))
}

class TlbStage1IO(userBits: Int) extends TlbReq(userBits)

class TlbStage1(implicit val tlbConfig: TLBConfig) extends TlbModule{
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new TlbReq(userBits)))
    val out = Decoupled(new TlbStage1IO(userBits))

    val metaReadBus = TlbMetaArrayReadBus()
    val dataReadBus = TlbDataArrayReadBus()

    val s2s3Miss = Input(Bool()) //
  })

  val vpn = io.in.bits.addr.asTypeOf(vaBundle2).vpn
  val readBusValid = io.in.valid && io.out.ready && !io.s2s3Miss //io.s2s3Miss ??
  
  io.metaReadBus.apply(valid = readBusValid, setIdx = 0.U)
  io.dataReadBus.apply(valid = readBusValid, setIdx = 0.U)
  
  io.out.bits := io.in.bits
  io.out.valid := io.in.valid && io.s2s3Miss && io.metaReadBus.req.ready && io.dataReadBus.req.ready //change req.ready to req.fire()??
  io.in.ready := (!io.in.valid || io.out.fire()) && io.metaReadBus.req.ready && io.dataReadBus.req.ready
}

sealed class TlbStage2IO(implicit val tlbConfig: TLBConfig) extends TlbBundle {
  val req = new TlbReq(userBits)
  val metas = Vec(Ways, new TLBMetaBundle)
  val datas = Vec(Ways, new TLBDataBundle)
  val hit = Output(Bool())
  val waymask = Output(UInt(Ways.W))
} 

class TlbStage2(implicit val tlbConfig: TLBConfig) extends TlbModule{
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new TlbStage1IO(userBits)))
    val out = Decoupled(new TlbStage2IO)

    val metaReadResp = Flipped(Vec(Ways, new TLBMetaBundle))
    val dataReadResp = Flipped(Vec(Ways, new TLBDataBundle))
  })
  
  val req = io.in.bits
  val vpn = req.addr.asTypeOf(vaBundle2).vpn

  val hitVec = VecInit(io.metaReadResp.map(m => m.flag.asTypeOf(flagBundle).V.asBool && (m.vpn === vpn) && io.in.valid)).asUInt
  val victimWaymask = (if (Ways > 1) (1.U(log2Up(Ways).W) << LFSR64()(log2Up(Ways)-1,0)) else 1.U(1.W))
  val waymask = Mux(io.out.bits.hit, hitVec, victimWaymask)
  assert(PopCount(waymask) <= 1.U)

  io.out.bits.metas := io.metaReadResp
  io.out.bits.datas := io.dataReadResp
  io.out.bits.hit := io.in.valid && hitVec.orR
  io.out.bits.waymask := waymask
  
  io.out.bits.req <> req
  io.out.valid := io.in.valid
  io.in.ready := !io.in.valid || io.out.fire()
}

sealed class TlbStage3(implicit val tlbConfig: TLBConfig) extends TlbModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new TlbStage2IO))
    val out = Decoupled(new TlbResp(userBits))
    val isFinish = Output(Bool())
    val flush = Input(Bool())
    val satp = Input(UInt(XLEN.W))
    val dataWriteBus = TlbDataArrayWriteBus()
    val metaWriteBus = TlbMetaArrayWriteBus()
    val mem = new SimpleBusUC(userBits = userBits)
  })

  val req = io.in.bits.req
  val satp = io.satp.asTypeOf(satpBundle)
  val vpn = req.addr.asTypeOf(vaBundle2).vpn.asTypeOf(vpnBundle)
  val hit = io.in.valid && io.in.bits.hit
  val miss = io.in.valid && !io.in.bits.hit
  val meta = Mux1H(io.in.bits.waymask, io.in.bits.metas)

  val dataRead = Mux1H(io.in.bits.waymask, io.in.bits.datas).ppn

  val s_idle :: s_memReadReq :: s_memReadResp :: s_wait_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val needFlush = RegInit(false.B)

  when (io.flush && (state =/= s_idle)) { needFlush := true.B}
  when (io.out.fire() && needFlush) { needFlush := false.B}

  val raddr = Reg(UInt(AddrBits.W))
  val cmd = SimpleBusCmd.read
  io.mem.req.bits.apply(addr = raddr, cmd = cmd, size = (if (XLEN == 64) "b11".U else "b10".U), wdata = 0.U, wmask = Fill(DataBytes, 0.U))
  io.mem.req.valid := (state === s_memReadReq)
  io.mem.resp.ready := true.B

  val alreadyOutFire = RegEnable(true.B, init = false.B, io.out.fire())

  val level = RegInit(Level.U(log2Up(Level).W))
  val ptwFinish = (level === 1.U) && io.mem.resp.fire()
  val memRdata = io.mem.resp.bits.rdata.asTypeOf(pteBundle)

  switch (state) {
    is (s_idle) {
      alreadyOutFire := false.B

      when (miss && !io.flush) {
        state := s_memReadReq
        raddr := paddrApply(satp.ppn, vpn.vpn2) //
        level := Level.U
      }
    }

    is (s_memReadReq) { when (io.mem.req.fire()) {
      state := s_memReadResp
    }}

    is (s_memReadResp) { when (io.mem.resp.fire()) {
      when (level === 3.U) {
        state := s_memReadReq
        raddr := paddrApply(memRdata.ppn, vpn.vpn1)
      }
      when (level === 2.U) {
        state := s_memReadReq
        raddr := paddrApply(memRdata.ppn, vpn.vpn0)
      }
      when (level === 1.U) {state := s_wait_resp}
      level := level - 1.U
    }}

    is (s_wait_resp) { when (io.out.fire() || needFlush || alreadyOutFire){
      state := s_idle
      level := Level.U
    }}
  }

  val dataRefill = memRdata.ppn
  val dataRefillWriteBus = Wire(TlbDataArrayWriteBus).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire() && level===1.U, setIdx = 0.U,
    data = Wire(new TLBDataBundle).apply(dataRefill), waymask = io.in.bits.waymask) //need change

  io.dataWriteBus.req <> dataRefillWriteBus.req

  val metaRefillWriteBus = Wire(TlbMetaArrayWriteBus()).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire() && (level === 1.U),
    data = Wire(new TLBMetaBundle).apply(vpn = vpn.asUInt, asid = satp.asid, flag = memRdata.flag), //need change
    setIdx = 0.U, waymask = io.in.bits.waymask)

  io.metaWriteBus.req <> metaRefillWriteBus.req

  io.out.bits.addr := paddrApply(Mux(hit, dataRead, memRdata.ppn), req.addr.asTypeOf(vaBundle2).off)
  io.out.bits.size := req.size
  io.out.bits.cmd := req.cmd
  io.out.bits.wmask := req.wmask
  io.out.bits.wdata := req.wdata
  io.out.bits.user.map(_:=req.user.getOrElse(0.U))
  //io.out.valid := io.in.valid /*???*/ && Mux(hit, true.B, state === s_wait_resp)
  io.out.valid := Mux(hit, true.B, state === s_wait_resp)

  io.isFinish := Mux(hit, io.out.fire(), (state === s_wait_resp) && (io.out.fire() || alreadyOutFire))
  io.in.ready := io.out.ready && (state === s_idle) && !miss
}

class TLB(implicit val tlbConfig: TLBConfig) extends TlbModule {
  val io = IO(new Bundle {
    val in = new TLBIO(userBits = userBits)
    val mem = new SimpleBusUC(userBits = userBits)
    val flush = Input(UInt(2.W)) 
    val exu = Flipped(new TLBExuIO) 
  })

  val s1 = Module(new TlbStage1)
  val s2 = Module(new TlbStage2)
  val s3 = Module(new TlbStage3)
  val metaArray = Module(new SRAMTemplate(new TLBMetaBundle, set = Sets, way = Ways, shouldReset = true, singlePort = true))
  val dataArray = Module(new SRAMTemplate(new TLBDataBundle, set = Sets, way = Ways, singlePort = true))
  metaArray.reset := reset.asBool || io.exu.sfence.valid

  s1.io.in <> io.in.req
  PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire(), io.flush(0))
  PipelineConnect(s2.io.out, s3.io.in, s3.io.isFinish, io.flush(1))
  io.in.resp <> s3.io.out
  s3.io.flush := io.flush(1)
  s3.io.satp := io.exu.satp
  io.mem <> s3.io.mem

  //stalling ??? unknown what means
  s1.io.s2s3Miss := s3.io.in.valid && !s3.io.in.bits.hit

  //meta-data read. for coh is useles so the Arbiter is useless
  metaArray.io.r.req <> s1.io.metaReadBus.req
  s1.io.metaReadBus.resp := metaArray.io.r.resp
  metaArray.io.w <> s3.io.metaWriteBus

  dataArray.io.r.req <> s1.io.dataReadBus.req
  s1.io.dataReadBus.resp := dataArray.io.r.resp
  dataArray.io.w <> s3.io.dataWriteBus

  s2.io.metaReadResp := metaArray.io.r.resp.data
  s2.io.dataReadResp := dataArray.io.r.resp.data
}
/*
object TLB {
  def apply(flush: UInt, exu: TLBExuIO)(implicit tlbConfig: TLBConfig) {
    val tlb = new TLB(userBits = AddrBits*2)
    tlb.in.req <> req
    resp <> tlb.in.resp
    tlb.flush := flush
    tlb.exu <> exu
    tlb
  }
}
*/
class TLBIOTran(userBits: Int = 0) extends NOOPModule {
  val io = IO(new Bundle{
    val in = Flipped(new SimpleBusUC(userBits = userBits))
    val out = new SimpleBusUC(userBits = userBits)
  })

  io.out.req <> io.in.req
  io.in.resp <> io.out.resp
}
/*
object TLBIOTran {
  def apply(resp: Decoupled(new SimpleBusRespBundle)) {
    val tran = new TLBIOTran(userBits = AddrBits*2)
    resp <> tran.in.resp
    tran
  }
}
*/
object TLBOpType {
  def vma = "b0".U
}

class fuTlb extends NOOPModule {
  val io = IO(new Bundle{
    val cfIn = Flipped(new CtrlFlowIO)
    val redirect = new RedirectIO
    val valid = Input(Bool())
  }) // MOUIO is the same as what we need cf and redirect 

  io.redirect.valid := io.valid
  io.redirect.target := io.cfIn.pc + 4.U
}

object fuTlb {
  def apply(cf : CtrlFlowIO, valid : Bool) = {
    val futlb = Module(new fuTlb)
    futlb.io.cfIn <> cf
    futlb.io.valid := valid
    futlb.io.redirect
  }
}

class TLBCacheMemConnect()