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
    Cat(Cat(0.U(paResLen.W), Cat(ppn, off)), 0.U(3.W))
  }
  
  def pteBundle = new Bundle {
    val reserved  = UInt(10.W)
    val ppn  = UInt(ppnLen.W)
    val rsw  = UInt(2.W)
    val flag = new Bundle {
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

  def satpBundle = new Bundle {
    val mode = UInt(satpModeLen.W)
    val asid = UInt(asidLen.W)
    val ppn  = UInt(ppnLen.W)
  }

  def flagBundle = new Bundle {
    val d    = Bool()//UInt(1.W)
    val a    = Bool()//UInt(1.W)
    val g    = Bool()//UInt(1.W)
    val u    = Bool()//UInt(1.W)
    val x    = Bool()//UInt(1.W)
    val w    = Bool()//UInt(1.W)
    val r    = Bool()//UInt(1.W)
    val v    = Bool()//UInt(1.W)
  }

  def vmMux(userBits: Int = 0, en: Bool, enYes: SimpleBusReqBundle, enNo: SimpleBusReqBundle) = {
    val res = Wire(new SimpleBusReqBundle(userBits))
    res.addr := Mux(en, enYes.addr, enNo.addr)
    res.size := Mux(en, enYes.size, enNo.size)
    res.cmd  := Mux(en,  enYes.cmd,  enNo.cmd)
    res.wmask:= Mux(en,enYes.wmask,enNo.wmask)
    res.wdata:= Mux(en,enYes.wdata,enNo.wdata)
    res.user.map(_ := Mux(en, enYes.user.getOrElse(0.U),enNo.user.getOrElse(0.U)))
    res
  }

  def maskPaddr(ppn:UInt, vaddr:UInt, mask:UInt) = {
    MaskData(vaddr, Cat(0.U(paResLen.W), ppn, 0.U(offLen.W)), Cat("h1ffffff".U(25.W), mask, 0.U(offLen.W)))
    //(Cat(0.U(paResLen.W), ppn, 0.U(offLen.W)) & Cat("h1ffffff".U(25.W), mask, 0.U(offLen.W))) | (vaddr & ~Cat("h1ffffff".U(25.W), mask, 0.U(offLen.W)))
  }

}

case class TLBConfig (
  name: String = "tlb",
  userBits: Int = 0,

  totalSize: Int = 128, 
  ways: Int = 4
)

sealed trait HasTlbConst {
  implicit val tlbConfig: TLBConfig

  val AddrBits: Int
  val XLEN: Int

  val tlbname = tlbConfig.name
  val userBits = tlbConfig.userBits

  val TotalSize = tlbConfig.totalSize
  val Ways = tlbConfig.ways
  val Sets = 1

  val debug = true //&& tlbname == "itlb"

  def TlbMetaArrayReadBus() = new SRAMReadBus(new TLBMetaBundle, set = Sets, way = Ways)
  def TlbDataArrayReadBus() = new SRAMReadBus(new TLBDataBundle, set = Sets, way = Ways)
  def TlbMetaArrayWriteBus() = new SRAMWriteBus(new TLBMetaBundle, set = Sets, way = Ways)
  def TlbDataArrayWriteBus() = new SRAMWriteBus(new TLBDataBundle, set = Sets, way = Ways)
}

sealed abstract class TlbBundle(implicit tlbConfig: TLBConfig) extends Bundle with HasNOOPParameter with HasTlbConst with Sv39Const
sealed abstract class TlbModule(implicit tlbConfig: TLBConfig) extends Module with HasNOOPParameter with HasTlbConst with Sv39Const

sealed class TLBMetaBundle(implicit val tlbConfig: TLBConfig) extends TlbBundle {
  val vpn = Output(UInt(vpnLen.W))
  val mask = Output(UInt(vpnLen.W))
  val asid = Output(UInt(asidLen.W))
  val flag = Output(UInt(flagLen.W))
  val addr = Output(UInt(AddrBits.W))

  def apply(vpn: UInt, mask: UInt, asid: UInt, flag: UInt, addr: UInt) = {
    this.vpn := vpn
    this.asid := asid
    this.flag := flag
    this.mask := mask
    this.addr := addr
    this
  }
}

sealed class TLBDataBundle(implicit val tlbConfig: TLBConfig) extends TlbBundle {
  val ppn = Output(UInt(ppnLen.W))

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
  io.out.valid := io.in.valid && !io.s2s3Miss && io.metaReadBus.req.ready && io.dataReadBus.req.ready //change req.ready to req.fire()??
  io.in.ready := (!io.in.valid || io.out.fire()) && io.metaReadBus.req.ready && io.dataReadBus.req.ready
}

sealed class TlbStage2IO(implicit val tlbConfig: TLBConfig) extends TlbBundle {
  val req = new TlbReq(userBits)
  val metas = Vec(Ways, new TLBMetaBundle)
  val datas = Vec(Ways, new TLBDataBundle)
  val hit = new Bundle {
    val hit = Output(Bool())
    val hitWB = Output(Bool())
    val hitExec = Output(Bool())
    val hitLoad = Output(Bool())
    val hitStore = Output(Bool())
  }
  val waymask = Output(UInt(Ways.W))
} 

class TlbStage2(implicit val tlbConfig: TLBConfig) extends TlbModule with HasCSRConst{
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new TlbStage1IO(userBits)))
    val out = Decoupled(new TlbStage2IO)

    val pf = new Bundle {
      val priviledgeMode = Input(UInt(2.W))
      val status_sum = Input(Bool())
      val status_mxr = Input(Bool())
    }
    val metaReadResp = Flipped(Vec(Ways, new TLBMetaBundle))
    val dataReadResp = Flipped(Vec(Ways, new TLBDataBundle))
  })
  
  val req = io.in.bits
  val vpn = req.addr.asTypeOf(vaBundle2).vpn
  val pf = io.pf

  val hitVec = VecInit(io.metaReadResp.map(m => m.flag.asTypeOf(flagBundle).v && ((m.vpn&m.mask) === (vpn&m.mask)) && io.in.valid)).asUInt
  val hitVecWB = VecInit(io.metaReadResp.map(m => !m.flag.asTypeOf(flagBundle).a || (!m.flag.asTypeOf(flagBundle).d && req.isWrite()))).asUInt & hitVec
  val victimWaymask = (if (Ways > 1) (1.U(log2Up(Ways).W) << LFSR64()(log2Up(Ways)-1,0)) else 1.U(1.W))
  val waymask = Mux(io.out.bits.hit.hit, hitVec, victimWaymask)
  assert(PopCount(waymask) <= 1.U)

  val hitVecCheck = VecInit(io.metaReadResp.map(m => m.flag.asTypeOf(flagBundle).v && !(pf.priviledgeMode === ModeU && !m.flag.asTypeOf(flagBundle).u) && !(pf.priviledgeMode === ModeS && m.flag.asTypeOf(flagBundle).u && pf.status_sum))).asUInt & hitVec
  val hitVecExec = VecInit(io.metaReadResp.map(m => m.flag.asTypeOf(flagBundle).x)).asUInt & hitVecCheck
  val hitVecLoad = VecInit(io.metaReadResp.map(m => m.flag.asTypeOf(flagBundle).r || pf.status_mxr && m.flag.asTypeOf(flagBundle).x)).asUInt & hitVecCheck
  val hitVecStore = VecInit(io.metaReadResp.map(m => m.flag.asTypeOf(flagBundle).w)).asUInt & hitVecCheck

  io.out.bits.metas := io.metaReadResp
  io.out.bits.datas := io.dataReadResp
  io.out.bits.hit.hit := io.in.valid && hitVec.orR
  io.out.bits.hit.hitWB := io.in.valid && hitVecWB.orR
  io.out.bits.hit.hitExec := io.in.valid && hitVecExec.orR
  io.out.bits.hit.hitLoad := io.in.valid && hitVecLoad.orR
  io.out.bits.hit.hitStore := io.in.valid && hitVecStore.orR
  io.out.bits.waymask := waymask

  io.out.bits.req <> req
  io.out.valid := io.in.valid
  io.in.ready := !io.in.valid || io.out.fire()
}

sealed class TlbStage3(implicit val tlbConfig: TLBConfig) extends TlbModule with HasCSRConst{
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new TlbStage2IO))
    val out = Decoupled(new TlbResp(userBits))
    val isFinish = Output(Bool())
    val flush = Input(Bool())
    val satp = Input(UInt(XLEN.W))
    val pf = new MMUIO
    val dataWriteBus = TlbDataArrayWriteBus()
    val metaWriteBus = TlbMetaArrayWriteBus()
    val mem = new SimpleBusUC()
    //if(debug) {
      val print = new Bundle{
        val state = Output(UInt(3.W))
        val level = Output(UInt(2.W))
        val alreadyOutFire = Output(UInt(1.W))
        val memRespStore = Output(UInt(XLEN.W))
        val hitFlag = Output(UInt(8.W))
        val refillFlag = Output(UInt(8.W))
        val instrPF = Output(Bool())
        val hitinstrPF = Output(Bool())
        val pfWire = Output(Bool())
        val hitMask = Output(UInt(27.W))
        val missMask = Output(UInt(27.W))
        val missMetaRF = Output(Bool())
      }
    //}
  })
  
  val pf = io.pf 
  val req = io.in.bits.req
  val satp = io.satp.asTypeOf(satpBundle)
  val vpn = req.addr.asTypeOf(vaBundle2).vpn.asTypeOf(vpnBundle)
  val hit = io.in.valid && io.in.bits.hit.hit
  val miss = io.in.valid && !io.in.bits.hit.hit
  val meta = Mux1H(io.in.bits.waymask, io.in.bits.metas)
  val hitFlag = meta.flag
  val hitMask = meta.mask
  val hitppn = Mux1H(io.in.bits.waymask, io.in.bits.datas).ppn

  val raddr = Reg(UInt(AddrBits.W))
  val alreadyOutFire = RegEnable(true.B, init = false.B, io.out.fire())
  val refillFlag = WireInit(0.U(8.W))

  val hitinstrPF = WireInit(false.B)
  val hitloadPF = WireInit(false.B)
  val hitstorePF = WireInit(false.B)
  val hitPF = (hitinstrPF || hitloadPF || hitstorePF)
  val hitWB = io.in.valid && io.in.bits.hit.hitWB && !hitinstrPF//hit pte write back check
  val hitExec = io.in.valid && io.in.bits.hit.hitExec
  val hitLoad = io.in.valid && io.in.bits.hit.hitLoad
  val hitStore = io.in.valid && io.in.bits.hit.hitStore
  val hitRefillFlag = Cat(req.isWrite().asUInt, 1.U(1.W), 0.U(6.W)) | hitFlag
  val hitWBStore = RegEnable(Cat(0.U(10.W), hitppn, 0.U(2.W), hitRefillFlag), hitWB)

  if (tlbname == "itlb") { hitinstrPF := !hitExec  && hit}
  if (tlbname == "dtlb") { hitloadPF := !hitLoad && req.isRead() && hit; hitstorePF := !hitStore && req.isWrite() && hit }

  val s_idle :: s_memReadReq :: s_memReadResp :: s_write_pte :: s_wait_resp :: Nil = Enum(5)
  val state = RegInit(s_idle)
  val needFlush = RegInit(false.B)
  val isFlush = needFlush || io.flush
  val memRespStore = Reg(UInt(XLEN.W))
  val missMask = WireInit("h7ffffff".U(vpnLen.W))
  val missMaskStore = Reg(UInt(vpnLen.W))
  val missMetaRF = WireInit(false.B)

  when (io.flush && (state =/= s_idle)) { needFlush := true.B}
  when (io.out.fire() && needFlush) { needFlush := false.B}

  val level = RegInit(Level.U(log2Up(Level).W))
  val ptwFinish = (level === 1.U) && io.mem.resp.fire()
  val memRdata = io.mem.resp.bits.rdata.asTypeOf(pteBundle)

  io.pf.loadPF := false.B
  io.pf.storePF := false.B
  io.pf.addr := req.addr
  val instrPF = RegInit(false.B)
  val pfWire = WireInit(false.B)

  switch (state) {
    is (s_idle) {
      alreadyOutFire := false.B
      when (!io.flush && hitWB) {
        state := s_write_pte
        needFlush := false.B
      }.elsewhen (miss && !io.flush) {
        state := s_memReadReq
        raddr := paddrApply(satp.ppn, vpn.vpn2) //
        level := Level.U
        needFlush := false.B
      }
    }

    is (s_memReadReq) { 
      when (isFlush) {
        state := s_idle
        needFlush := false.B
      }.elsewhen (io.mem.req.fire()) { state := s_memReadResp}
    }

    is (s_memReadResp) { 
      val missflag = memRdata.flag.asTypeOf(flagBundle)
      when (io.mem.resp.fire()) {
        when (isFlush) {
          state := s_idle
          needFlush := false.B
        }.elsewhen (!(missflag.r || missflag.x)/*!missflag.r && !missflag.x && !missflag.w*/ && (level===3.U || level===2.U)) {
          when(!missflag.v || (!missflag.r && missflag.w)) {
            if(tlbname == "itlb") { state := s_wait_resp } else { state := s_idle }
            if(tlbname == "dtlb") { io.pf.loadPF := req.isRead() ; io.pf.storePF := req.isWrite() }
            if(tlbname == "itlb") { instrPF := true.B }
            Debug(debug) {
              printf("%d " + tlbname +" tlbException!!! ", GTimer())
              printf(p" req:${req}  Memreq:${io.mem.req}  MemResp:${io.mem.resp}")
              printf(" level:%d",level)
              printf("\n")
            //assert(false.B)
            }
          }.otherwise {
            state := s_memReadReq
            raddr := paddrApply(memRdata.ppn, Mux(level === 3.U, vpn.vpn1, vpn.vpn0))
          }
        }.elsewhen (level =/= 0.U) {
          val permCheck = missflag.v && !(pf.priviledgeMode === ModeU && !missflag.u) && !(pf.priviledgeMode === ModeS && missflag.u && pf.status_sum)
          val permExec = permCheck && missflag.x
          val permLoad = permCheck && (missflag.r || pf.status_mxr && missflag.x)
          val permStore = permCheck && missflag.w
          val updateAD = !missflag.a || (!missflag.d && req.isWrite())
          val updateData = Cat( 0.U(56.W), req.isWrite(), 1.U(1.W), 0.U(6.W) )
          refillFlag := Cat(req.isWrite(), 1.U(1.W), 0.U(6.W)) | missflag.asUInt
          memRespStore := io.mem.resp.bits.rdata | updateData 
          if(tlbname == "itlb") {
            when (!permExec) { instrPF := true.B ; state := s_wait_resp ; pfWire := true.B }
            .otherwise { 
              state := Mux(updateAD, s_write_pte, s_wait_resp)
              missMetaRF := true.B
            }
          }
          if(tlbname == "dtlb") {
            when((!permLoad && req.isRead()) || (!permStore && req.isWrite())) { 
              state := s_idle ; pfWire := true.B
              io.pf.loadPF := req.isRead() ; io.pf.storePF := req.isWrite()
            }.otherwise {
              state := Mux(updateAD, s_write_pte, s_wait_resp)
              missMetaRF := true.B
            }
          }
          missMask := Mux(level===3.U, "h7fc0000".U(27.W), Mux(level===2.U, "h7fffe00".U(27.W), "h7ffffff".U(27.W)))
          missMaskStore := missMask
        }
        level := level - 1.U
      }
    }

    is (s_write_pte) {
      when (isFlush) {
        state := s_idle
        needFlush := false.B
      }.elsewhen (io.mem.req.fire()) { state := s_wait_resp }
    }

    is (s_wait_resp) { when (io.out.fire() || /*needFlush*/ io.flush || alreadyOutFire){
      state := s_idle
      instrPF := false.B
    }}
  }

  val cmd = Mux(state === s_write_pte, SimpleBusCmd.write, SimpleBusCmd.read)
  io.mem.req.bits.apply(addr = Mux(hitWB, meta.addr, raddr), cmd = cmd, size = (if (XLEN == 64) "b11".U else "b10".U), wdata =  Mux( hitWB, hitWBStore, memRespStore), wmask = 0xff.U)
  io.mem.req.valid := ((state === s_memReadReq || state === s_write_pte) && !isFlush)
  io.mem.resp.ready := true.B//(state === s_memReadResp)//()

  val dataRefill = memRdata.ppn
  val dataRefillWriteBus = Wire(TlbDataArrayWriteBus).apply(
    valid = missMetaRF && !pfWire && !isFlush, setIdx = 0.U,
    data = Wire(new TLBDataBundle).apply(dataRefill), waymask = io.in.bits.waymask) //need change

  io.dataWriteBus.req <> dataRefillWriteBus.req

  val metaRefillWriteBus = Wire(TlbMetaArrayWriteBus()).apply(
    valid = (missMetaRF && !pfWire && !isFlush) || (hitWB && state === s_idle && !isFlush),
    data = Wire(new TLBMetaBundle).apply(vpn = vpn.asUInt, mask = Mux(hitWB, hitMask, missMask), asid = Mux(hitWB, meta.asid, satp.asid), flag = Mux(hitWB, hitRefillFlag, refillFlag), addr = Mux(hitWB, meta.addr , raddr)), //need change
    setIdx = 0.U, waymask = io.in.bits.waymask)

  io.metaWriteBus.req <> metaRefillWriteBus.req

  //io.out.bits.addr := Cat(0.U(paResLen.W), Cat(Mux(hit, hitppn, memRespStore.asTypeOf(pteBundle).ppn), req.addr.asTypeOf(vaBundle2).off))
  io.out.bits.addr := Mux(hit, maskPaddr(hitppn, req.addr, hitMask), maskPaddr(memRespStore.asTypeOf(pteBundle).ppn, req.addr, missMaskStore))
  io.out.bits.size := req.size
  io.out.bits.cmd := req.cmd
  io.out.bits.wmask := req.wmask
  io.out.bits.wdata := req.wdata
  if(tlbname == "itlb") { io.out.bits.user.map(_:= Cat( instrPF || hitinstrPF, req.user.getOrElse(0.U)(AddrBits*2 + 3, 0))) } 
  else { io.out.bits.user.map(_:=req.user.getOrElse(0.U)) }
  //io.out.valid := io.in.valid /*???*/ && Mux(hit, true.B, state === s_wait_resp)
  io.out.valid := Mux(hit && !hitWB, true.B, state === s_wait_resp)

  if (tlbname == "dtlb"){ io.isFinish := Mux(hit && !hitWB, io.out.fire(), (state === s_wait_resp) && (io.out.fire() || alreadyOutFire) || io.pf.isPF()) } 
  else/*if(tlbname == "itlb")*/ { io.isFinish := Mux(hit && !hitWB, io.out.fire(), (state === s_wait_resp) && (io.out.fire() || alreadyOutFire)) }
  
  io.in.ready := io.out.ready && (state === s_idle) && !miss && !hitWB

  //if(debug) {
    io.print.state := state
    io.print.level := level
    io.print.alreadyOutFire := alreadyOutFire
    io.print.memRespStore := memRespStore
    io.print.refillFlag := refillFlag
    io.print.instrPF := instrPF
    io.print.hitinstrPF := hitinstrPF
    io.print.pfWire := pfWire
    io.print.hitFlag := hitFlag
    io.print.hitMask := hitMask
    io.print.missMask := missMask
    io.print.missMetaRF := missMetaRF
  //}
}

class TLB(implicit val tlbConfig: TLBConfig) extends TlbModule{
  val io = IO(new Bundle {
    val in = new TLBIO(userBits = userBits)
    val mem = new SimpleBusUC()
    val flush = Input(UInt(2.W)) 
    val exu = Flipped(new TLBExuIO)
    val csrMMU = new MMUIO
  })

  val s1 = Module(new TlbStage1)
  val s2 = Module(new TlbStage2)
  val s3 = Module(new TlbStage3)
  val metaArray = Module(new SRAMTemplate(new TLBMetaBundle, set = Sets, way = Ways, shouldReset = true, singlePort = true))
  val dataArray = Module(new SRAMTemplate(new TLBDataBundle, set = Sets, way = Ways, singlePort = true))
  metaArray.reset := reset.asBool || io.exu.sfence.valid
  BoringUtils.addSource(io.exu.sfence.valid, "TLBSFENCEVMA")

  Debug(debug) {
    when(io.exu.sfence.valid) {
      printf("%d sfence_vma\n", GTimer())
    }
  }

  val vmEnable = io.exu.satp.asTypeOf(satpBundle).mode === 8.U //how to constrict to 0/8
  s1.io.in <> io.in.req
  s1.io.in.bits := io.in.req.bits
  s1.io.in.valid :=  Mux(vmEnable, io.in.req.valid, false.B)
  io.in.req.ready := Mux(vmEnable, s1.io.in.ready, io.in.resp.ready)

  PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire(), io.flush(0))
  PipelineConnect(s2.io.out, s3.io.in, s3.io.isFinish, io.flush(1))
  //io.in.resp <> s3.io.out
  io.in.resp.bits := vmMux(userBits, vmEnable, s3.io.out.bits, io.in.req.bits)
  io.in.resp.valid := Mux(vmEnable, s3.io.out.valid, io.in.req.valid)
  s3.io.out.ready := Mux(vmEnable, io.in.resp.ready, false.B)
  
  s3.io.flush := io.flush(1)
  s3.io.satp := io.exu.satp
  io.mem <> s3.io.mem

  //stalling ??? unknown what means
  s1.io.s2s3Miss := s3.io.in.valid && !s3.io.in.bits.hit.hit

  //meta-data read. for coh is useles so the Arbiter is useless
  metaArray.io.r.req <> s1.io.metaReadBus.req
  s1.io.metaReadBus.resp := metaArray.io.r.resp
  metaArray.io.w <> s3.io.metaWriteBus

  dataArray.io.r.req <> s1.io.dataReadBus.req
  s1.io.dataReadBus.resp := dataArray.io.r.resp
  dataArray.io.w <> s3.io.dataWriteBus

  s2.io.metaReadResp := metaArray.io.r.resp.data
  s2.io.dataReadResp := dataArray.io.r.resp.data

  io.csrMMU <> s3.io.pf
  s2.io.pf.priviledgeMode := io.csrMMU.priviledgeMode
  s2.io.pf.status_sum := io.csrMMU.status_sum
  s2.io.pf.status_mxr := io.csrMMU.status_mxr

  if(tlbname == "dtlb") {
    val alreadyOutFinish = RegEnable(true.B, init=false.B, io.in.resp.valid && !io.in.resp.ready)
    when(alreadyOutFinish && io.in.resp.fire()) { alreadyOutFinish := false.B}
    val tlbFinish = (io.in.resp.valid && !alreadyOutFinish) || s3.io.pf.isPF()
    BoringUtils.addSource(tlbFinish, "DTLBFINISH")
    BoringUtils.addSource(s3.io.pf.isPF, "DTLBPF")
    BoringUtils.addSource(vmEnable, "DTLBENABLE")
  }

  Debug( debug /*&& tlbname == "itlb"*/) {
    when(true.B ) {
      //printf("-----------------------------------------------------------------------------------------------\n")
      printf("%d "+ tlbname + " ",GTimer())
      printf("InReq(%d, %d) ioInResp(%d, %d) InReqAddr:%x InRespAddr:%x \n", io.in.req.valid, io.in.req.ready, io.in.resp.valid, io.in.resp.ready, io.in.req.bits.addr, io.in.resp.bits.addr)
      printf("%d:"+ tlbname + " {IN: s1(%d, %d) s2(%d, %d) s3(%d, %d)} ",GTimer(), s1.io.in.valid, s1.io.in.ready, s2.io.in.valid, s2.io.in.ready, s3.io.in.valid, s3.io.in.ready)
      printf("{OUT: s1(%d, %d) s2(%d, %d) s3(%d, %d)} ", s1.io.out.valid, s1.io.out.ready, s2.io.out.valid, s2.io.out.ready, s3.io.out.valid, s3.io.out.ready)
      printf("\n%d:"+ tlbname + " s1ReqAddr:%x s2ReqAddr:%x s3ReqAddr:%x s3RespAddr:%x", GTimer(), s1.io.in.bits.addr, s2.io.in.bits.addr, s3.io.in.bits.req.addr, s3.io.out.bits.addr)
      if (tlbname == "itlb") { printf(" user:%x ", s3.io.out.bits.user.getOrElse(0.U))}
      printf("\n%d:"+ tlbname + " s3State:%d level:%d s3alreadOutFire:%d s3memRespStore:%x s3Hit:%d s3WayMask:%x iPF:%d hiPF:%d pfwire:%d ", GTimer(), s3.io.print.state, s3.io.print.level, s3.io.print.alreadyOutFire, s3.io.print.memRespStore, s3.io.in.bits.hit.hit, s3.io.in.bits.waymask, s3.io.print.instrPF, s3.io.print.hitinstrPF, s3.io.print.pfWire)
      printf("\n%d:"+ tlbname + " s3 hitflag:%x refillFlag:%x hitWB:%d hitExec:%d hitLoad:%d hitStore:%d isWrite:%d hitMask:%x missMask:%x ", GTimer(), s3.io.print.hitFlag, s3.io.print.refillFlag, s3.io.in.bits.hit.hitWB,  s3.io.in.bits.hit.hitExec,  s3.io.in.bits.hit.hitLoad,  s3.io.in.bits.hit.hitStore, s3.io.in.bits.req.isWrite(), s3.io.print.hitMask, s3.io.print.missMask)
      printf("satp:%x ", s3.io.satp)
      printf("flush:%x \n", io.flush)
      printf("%d "+ tlbname + " ",GTimer())
      printf("MemReq(%d, %d) ioMemResp(%d, %d) addr:%x rdata:%x cmd:%d wdata:%x \n", io.mem.req.valid, io.mem.req.ready, io.mem.resp.valid, io.mem.resp.ready, io.mem.req.bits.addr, io.mem.resp.bits.rdata, io.mem.req.bits.cmd, io.mem.req.bits.wdata)
      printf("%d "+ tlbname + " ",GTimer())
      printf("s3Meta(%d, %d) vpn:%x mask:%x flag:%x addr:%x missMetaRF:%d \n", s3.io.metaWriteBus.req.valid, s3.io.metaWriteBus.req.ready, s3.io.metaWriteBus.req.bits.data.vpn, s3.io.metaWriteBus.req.bits.data.mask, s3.io.metaWriteBus.req.bits.data.flag, s3.io.metaWriteBus.req.bits.data.addr, s3.io.print.missMetaRF)
      printf("%d "+ tlbname + " ",GTimer())
      printf("s3Data(%d, %d) ppn:%x\n", s3.io.dataWriteBus.req.valid, s3.io.dataWriteBus.req.ready, s3.io.dataWriteBus.req.bits.data.ppn)
      //printf("\n%d:"+ tlbname + " s1MetaReadReqReady:%d s1DataReadReqReady:%d ", GTimer(), s1.io.metaReadBus.req.ready, s1.io.dataReadBus.req.ready)
      //printf("s1ReqFire:%d s2ReqFire:%d s3ReqFire:%d ", s1.io.in.fire(), s2.io.in.fire(), s3.io.in.fire())
      //printf("s2Hit:%d s2Waymask:%x ", s2.io.out.bits.hit.hit, s2.io.out.bits.waymask)
      //printf("s2s3Miss:%d ", s1.io.s2s3Miss)
      
    }
  }
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
class TLBIOTran(userBits: Int = 0, name: String = "default") extends NOOPModule {
  val io = IO(new Bundle{
    val in = Flipped(new SimpleBusUC(userBits = userBits))
    val out = new SimpleBusUC(userBits = userBits)
    //val rawReq = Flipped(Decoupled(SimpleBusReqBundle(userBits = userBits)))
    //val vmEnable = Input(Bool())
  })

  io.out.req <> io.in.req
  //io.out.req.valid := Mux(vmEnable, io.in.req.valid, io.rawReq.valid)
  //io.out.req.bits := Mux(vmEnable, io.in.req.bits, io.rawReq.bits)
  

  //io.in.resp <> io.out.resp

  io.in.resp.valid := io.out.resp.valid && !io.out.resp.bits.isWriteResp()
  io.in.resp.bits := io.out.resp.bits
  io.out.resp.ready := io.in.resp.ready

  Debug() {
    when(true.B) {
      if(name == "dtran") { printf("-----------------------------------------------------------------------------------------------\n")}
      printf("%d:" + name + "InReq(%d, %d) InResp(%d, %d) ", GTimer(), io.in.req.valid, io.in.req.ready, io.in.resp.valid, io.in.resp.ready)
      printf("\n%d:" + name, GTimer())
      printf(p"InReqBits:${io.in.req.bits}, InRespBits:${io.in.resp.bits}")
      //if(userBits>0) {printf("user:%x ", io.in.resp.bits.user.getOrElse(0.U))}
      printf("\n")
      //io.in.dump(name + ".in")
      //io.out.dump(name + ".out")
    }
  }
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