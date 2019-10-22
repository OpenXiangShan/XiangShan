package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.simplebus._
import bus.axi4._
import utils._

trait Sv39Const {
  val debug = true

  val vpnLen = 27
  val ppnLen = 44
}

trait pteSv39Const extends Sv39Const{
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
  
  def paddrApply(ppn: UInt, off: UInt) = {
    acquire(ppn.getWidth==ppnLen)
    acquire(off.getWidth==offLen)
    Cat(0.U(paResLen.W), Cat(ppn, off))
  }
  
  def pteBundle = new Bundle {
    val reserved  = UInt(10.W)
    val ppn  = UInt(ppnLen.W)
    val rsw  = UInt(2.W)
    val D    = UInt(1.W)
    val A    = UInt(1.W)
    val G    = UInt(1.W)
    val U    = UInt(1.W)
    val X    = UInt(1.W)
    val W    = UInt(1.W)
    val R    = UInt(1.W)
    val V    = UInt(1.W)
  }

  

  def satpBundle = new Bundle {
    val mode = UInt(satpModeLen.W)
    val asid = UInt(asidLen.W)
    val ppn  = UInt(ppnLen.W)
  }
}

trait pteConst extends pteSv39Const

object TLBOpType {
  def vma = "b0".U
}

trait tlbSv39Const extends Sv39Const{
  val tlbEntryNum = 8
  val tlbEntryLen = 95
  val tlbAsidLen = 16
  val flagLen = 8

  def tlbBundle = new Bundle {
    val vpn  = UInt(vpnLen.W)
    val asid = UInt(tlbAsidLen.W)
    val ppn  = UInt(ppnLen.W)
    val D    = UInt(1.W) 
    val A    = UInt(1.W)
    val G    = UInt(1.W)
    val U    = UInt(1.W)
    val X    = UInt(1.W)
    val W    = UInt(1.W)
    val R    = UInt(1.W)
    val V    = UInt(1.W)
  }

  def tlbEntryApply(vpn: UInt, asid: UInt, ppn: UInt, flag: UInt) {
    acquire(vpn.getWidth==vpnLen)
    acquire(asid.getWidth==tlbAsidLen)
    acquire(ppn.getWidth==ppnLen)
    acquire(ppn.getWidth==flagLen)
    Cat(Cat(vpn, asid), Cat(ppn, flag))
  }
}

trait tlbConst extends tlbSv39Const

case class TLBConfig {
  name: String = "tlb",
  userBits: Int = 0,

  totalSize: Int = 128, //项
  ways: Int = 1 //还不理解way的含义
}

sealed trait HasTlbConst {
  implicit val tlbCOnfig: tlbCOnfig

  val AddrBits: Int
  val XLEN: Int

  val tlbNmae = tlbConfig.name
  val userBits = cacheConfig.userBits

  val TotalSize = tlbConfig.totalSize
  val Ways = tlbConfig.ways
  val LineSize = XLEN //byte => ppnLen
  val LineBeats = LineSize / 8 // DATA WIDTH 64 应该用不到这个值
  val Sets = TotalSize * 1024 / LineSize / Ways
  val OffsetBits = log2Up(LineSize) //?
  val IndexBits = log2Up(Sets) //?
  val WordIndexBits = log2Up(LineBeats) //?
  val TagBits = AddrBits - OffsetBits - IndexBits

  val debug = true

  def addrBundle = new Bundle {
    //,,,
  }

  def TlbMetaArrayReadBus() = new SRAMReadBus(new MetaBundle, set = Sets, way = Ways)
  def TlbDataArrayReadBus() = new SRAMReadBus(new DataBundle, set = Sets*LineBeats, way = Ways)
  def TlbMetaArrayWriteBus() = new SRAMWriteBus(new MetaBundle, set = Sets, way = Ways)
  def TlbDataArrayWriteBus() = new SRAMWriteBus(new DataBundle, set = Sets*LineBeats, way = Ways)

  def isSameWord(a1: UInt, a2:UInt) = ((a1 >> 2) === (a2 >> 2))
  //def isSetConflict(a1: UInt, a2: UInt) = (a1.asTypeof(addrBundle).index === a2.asTypeof(addrBundle).index)
}

sealed abstract class TlbBundle(implicit tlbConfig: TlbConfig) extends Bundle with HasNOOPParameter with HasTlbConst
sealed abstract class TlbModule(implicit tlbConfig: TlbConfig) extends Module with HasNOOPParameter with HasTlbConst

sealed class MetaBundle(implicit val tlbConfig: TlbConfig) extends TlbBundle {
  val vpn = Output(UInt(vpnLen.W))
  val asid = Output(UInt(asidLen.W))
  val flag = Output(UInt(flagLen.W))

  def apply(vpn: UInt, asid: UInt, flag: UInt) = {
    this.vpn = vpn
    this.asid = asid
    this.flag = flag
    this
  }
}

sealed class DataBundle(implicit val tlbConfig: TlbConfig) extends TlbBundle {
  val ppn = Output(UInt(DataBits.W))

  def apply(ppn: UInt) = {
    this.ppn = ppn
    this
  }
}

sealed class PaddrBundle(implicit val tlbConfig: TlbConfig) extends TlbBundle {
  val 
}

trait tlbConst with tlbSv32Const

class TlbReq extends Bundle with tlbConst {
  val vpn = Output(UInt(vpnLen.W))
  val flag = Output(UInt(flagLen.W))
}

class TlbResp extends Bundle with tlbConst {
  val ppn = Output(UInt(ppnLen.W))
  val flag = Output(UInt(flagLen.W))
}

class TLBIO extends Bundle {
  val req = Flipped(Decoupled(new TlbReq))
  val resp = Decoupled(new TlbResp)
}

class TlbStage1IO extends TlbReq

class TlbStage1(implicit val tlbConfig: TlbConfig) extends Module with tlbConst {
  val io = IO(new Bundle {
    val flush = Input(Bool())
    val in = Flipped(Decoupled(new TlbReq))
    val out = Decoupled(new TlbStage1IO)

    val metaReadBus = TlbMetaArrayReadBus()
    val dataReadBus = TlbDataArrayReadBus()

    val s2s3Miss = Input(Bool()) //
  })

  val vpn = io.in.bits.vpn
  val readBusValid = io.in.valid && io.out.ready && !io.s2s3Miss //io.s2s3Miss ??
  
  io.metaReadBus.apply(valid = readBusValid, setIdx = addr.index)
  io.dataReadBus.apply(valid = readBusValid, setIdx = Cat(addr.index, addr.wordIndex))
  
  io.out.bits := io.in.bits
  io.out.valid := io.in.valid && io.s2s3Miss && io.metaReadBus.req.ready && io.dataReadBus.req.ready //change req.ready to req.fire()??
  io.in.ready := (!io.in.valid || io.out.fire()) && io.metaReadBus.req.ready && io.dataReadBus.req.ready
}

sealed class TlbStage2IO(implicit val tlbConfig: TlbConfig) extends TlbBundle {
  val req = new TlbReq
  val metas = Vec(Ways, new MetaBundle)
  val datas = Vec(Ways, new DataBundle)
  val hit = Output(Bool())
  val waymask = Output(UInt(Ways.W))
} 

class TlbStage2(implicit val tlbConfig: TlbConfig) extends Module with tlbConst {
  val io = IO(new Bundle {
    val flush = Input(Bool())
    val in = Flipped(Decoupled(new TlbStage1IO))
    val out = Decoupled(new TlbStage2IO)

    val metaReadResp = Flipped(Vec(Ways, new MetaBundle))
    val dataReadResp = Flipped(Vec(Ways, new DataBundle))
  })
  
  val req = io.in.bits.req
  val vpn = req.vpn

  val hitVec = VecInit(io.metaReadResp.map(m => m.valid && (m.tag === addr.tag) && io.in.valid)).asUInt
  val victimWaymask = if (Ways > 1) (1.U << LFSR64()(log2Up(Ways)-1,0)) else "b1.U"
  val waymask = Mux(io.out.bits.hit, hitVec, victimWaymask)
  assert(PopCount(waymask) <= 1.U)

  io.out.bits.metas := io.metaReadResp
  io.out.bits.datas := io.dataReadResp
  io.out.bits.hit := io.in.valid && hitVec.orR
  io.out.bits.waysmask := waymask
  
  io.out.bits.req <> req
  io.out.valid := io.in.valid
  in.in.ready := !io.in.valid || io.out.fire()
}

sealed class TlbStage3(implicit val tlbConfig: TlbConfig) extends TlbModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new TlbStage2IO))
    val out = Decoupled(new TlbResp)
    val isFinish = Output(Book())
    val flush = Input(Bool())
    val dataWriteBus = TlbDataArrayWriteBus()
    val dataReadBus = TlbDataArrayReadBus()
    val metaWriteBus = TlbMetaArrayWriteBus()
    val mem = new SimpleBusUC
  })

  val req = io.in.bits
  val vpn = req.vpn
  val hit = io.in.valid && io.in.bits.hit
  val miss = io.in.valid && !io.in.bits.hit
  val meta = Mux1H(io.in.bits.waymask, io.in.bits.metas)

  val dataRead = Mux1H(io.in.bits.waysmask, io.in.bits.datas).data
  val wrodMask = 0.U(DataBits.W)

  val s_idle :: s_memReadReq :: s_memReadResp :: s_wait_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val needFlush = RegInit(false.B)

  when (io.flush && (state =/= s_idle)) { needFlush := true.B}
  when (io.out.fire() && needFlush) { needFlush := false.B}

  val readBeatCnt = Counter(LineBeats) //unknown meaning
  val writBeatCnt = Counter(LineBeats)

  val raddr = Reg(UInt(AddrBits.W))
  val cmd = SimpleBusCmd.read
  io.mem.req.bits.apply(addr = raddr, cmd = cmd, size = (if (XLEN == 64) "b11".U else "b10".U), wdata = 0.U, wmask = Fill(DataBytes, 0.U))
  io.mem.resp.ready := true.B
  io.mem.req.valid := (state === s_memReadReq)

  val alreadyOutFire = RegEnable(true.B, init = false.B, io.out.fire())
  val inRdataRegDemand = RegEnable(io.mem.resp.bits.rdata, readingFirst) //??

  val level = RegInit(Level.U(log2Up(Level).W))
  val ptwFinish = (level === 1.U) && io.mem.resp.fire()

  switch (state) {
    is (s_idle) {
      afterFirstRead := false.B
      alreadyOutFire := false.B

      when (miss && !io.flush) {
        state := s_memReadReq
        raddr := paddrApply.apply(satp.ppn, io.in.bits.vpn()) //
        level := (Level-1).U
      }
    }

    is (s_memReadReq) { when (io.mem.req.fire()) {
      state := s_memReadResp
      readBeatCnt.value := addr.wordIndex //unknown meaning??
    }}

    is (s_memReadResp) { when (io.mem.resp.fire()) {
      afterFirstRead := true.B
      readBeatCnt.inc()
      when (level === 2.U) {state := s_memReadReq ; raddr := paddrApply.apply(...)}
      when (level === 1.U) {state := s_wait_resp}
      //need judge the flag: valid,,,
    }}

    is (s_wait_resp) { when (io.out.fire() || needFlush || alreadyOutFire){
      state := s_idle
    }}
  }

  val dataRefill = io.mem.resp.bits.rdata.toTlbBundle
  val dataRefilWriteBus = Wire(TlbDataArrayWriteBus).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire() && level===1.U, setIdx = Cat(addr.index, readBeatCnt.value),
    data = Wire(new DataBundle).apply(dataRefill), waymask = io.in.bits.waymask) //need change

  io.dataWriteBus.req <> dataRefillWriteBus.req

  val metaRefillWriteBus = Wire(TlbMetaArrayWriteBus()).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire() && (level === 1.U),
    data = Wire(new MetaBundle).apply(valid = true.B, tag = addr.tag) //need change
    setIdx = addr.index, waymask = io.in.bits.waymask)

  io.metaWriteBus.req <> metaRefillWriteBus.req

  io.out.bits.vpn := Mux(hit, dataRead, inRdataRegDemand)
  io.out.bits.flag := Mux(hit, dataRead.flag, inRdataRegDemand.flag)
  io.out.valid := io.in.valid /*???*/ && Mux(hit, true.B, afterFirstRead && !alreadyOutFire)

  io.isFinish := Mux(hit, io.out.fire(), (state === s_wait_resp) && (io.out.fire() || alreadyOutFire))
  io.in.ready：= io.out.ready && (state === s_idle) && !miss
}

class TLB(dataBits: Int = 32, userBits: Int = 32, name: String = "default") extends Module with tlbConst {
  val io = IO(new Bundle {
    val flush = Input(UInt(2.W)) //flush for bp fail
    val exu = Input(new TlbFlushBundle) 
    val in = Flipped(Decoupled(TlbIO))
    val out = SimpleBusC
  }

  val s1 = Module(new TlbStage1)
  val s2 = Module(new TlbStage2)
  val s3 = Module(new TlbStage3)

  val metaArray = Module(new SRAMTemplate(new MetaBundle, set = Sets, way = Ways, shouldReset = true, singlePort = true))
  val dataArray = Module(new SRAMTemplate(new DataBundle, set = Sets*LineBeats, way = Ways, singlePort = true))
  metaArray.reset := reset.asBool || flushTlb

  s1.io.in <> io.in.req
  PipelineConnect(s1.io.out, s2.io.in, s2.io.isFinish, io.flush(0))
  PipelineConnect(s2.io.out, s3.io.in, s3.io.isFinish, io.flush(1))
  io.in.resp <> s3.io.out
  s2.io.flush := io.flush(1)
  io.out.mem <> s3.io.mem

  //stalling ??? unknown what means
  s1.io.s2s3Miss := s3.io.in.valid && !s3.io.in.bits.hit

  //meta-data read. for coh is useles so the Arbiter is useless
  metaArray.io.r.req <> s1.io.metaReadBus.req
  s1.io.metaReadBus.resp := metaArray.io.r.resp
  metaArray.io.w <> s3.io.metaWriteBus

  val dataReadArb = Module(new Arbiter(chiselTypeOf(dataArray.io.r.req.bits), 2))
  dataReadArb.io.in(0) <> s1.io.dataReadBus.req
  dataReadArb.io.in(1) <> s3.io.dataReadBus.req
  dataArray.io.r.req <> dataReadArb.io.out
  s1.io.dataReadBus.resp := dataArray.io.r.resp
  s3.io.dataReadBus.resp := dataArray.io.r.resp
  dataArray.io.w <> s3.io.dataWriteBus

  s2.io.metaReadResp := metaArray.io.r.resp.data
  s2.io.dataReadResp := dataArray.io.r.resp.data
}