package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.simplebus._
import bus.axi4._
import utils._

trait Sv39Const extends HasNOOPParameter{
  val Level = 3
  val offLen  = 12
  val ppn0Len = 9
  val ppn1Len = 9
  val ppn2Len = PAddrBits - offLen - ppn0Len - ppn1Len // 2
  val ppnLen = ppn2Len + ppn1Len + ppn0Len
  val vpn2Len = 9
  val vpn1Len = 9
  val vpn0Len = 9
  val vpnLen = vpn2Len + vpn1Len + vpn0Len
  
  //val paddrLen = PAddrBits
  //val vaddrLen = VAddrBits
  val satpLen = XLEN
  val satpModeLen = 4
  val asidLen = 16
  val flagLen = 8

  val ptEntryLen = XLEN
  val satpResLen = XLEN - ppnLen - satpModeLen - asidLen
  //val vaResLen = 25 // unused
  //val paResLen = 25 // unused 
  val pteResLen = XLEN - ppnLen - 2 - flagLen

  def vaBundle = new Bundle {
    val vpn2 = UInt(vpn2Len.W)
    val vpn1 = UInt(vpn1Len.W)
    val vpn0 = UInt(vpn0Len.W)
    val off  = UInt( offLen.W)
  }

  def vaBundle2 = new Bundle {
    val vpn  = UInt(vpnLen.W)
    val off  = UInt(offLen.W)
  }

  def vaBundle3 = new Bundle {
    val vpn = UInt(vpnLen.W)
    val off = UInt(offLen.W)
  }

  def vpnBundle = new Bundle {
    val vpn2 = UInt(vpn2Len.W)
    val vpn1 = UInt(vpn1Len.W)
    val vpn0 = UInt(vpn0Len.W)
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

  def paddrApply(ppn: UInt, vpnn: UInt):UInt = {
    Cat(Cat(ppn, vpnn), 0.U(3.W))
  }
  
  def pteBundle = new Bundle {
    val reserved  = UInt(pteResLen.W)
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
    val res = UInt(satpResLen.W)
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

  def maskPaddr(ppn:UInt, vaddr:UInt, mask:UInt) = {
    MaskData(vaddr, Cat(ppn, 0.U(offLen.W)), Cat(Fill(ppn2Len, 1.U(1.W)), mask, 0.U(offLen.W)))
  }

  def MaskEQ(mask: UInt, pattern: UInt, vpn: UInt) = {
    (Cat("h1ff".U(vpn2Len.W), mask) & pattern) === (Cat("h1ff".U(vpn2Len.W), mask) & vpn)
  }

}

case class TLBConfig (
  name: String = "tlb",
  userBits: Int = 0,

  totalEntry: Int = 4,
  ways: Int = 4
)

sealed trait HasTlbConst extends Sv39Const{
  implicit val tlbConfig: TLBConfig

  val AddrBits: Int
  val PAddrBits: Int
  val VAddrBits: Int
  val XLEN: Int

  val tlbname = tlbConfig.name
  val userBits = tlbConfig.userBits

  val maskLen = vpn0Len + vpn1Len  // 18
  val metaLen = vpnLen + asidLen + maskLen + flagLen // 27 + 16 + 18 + 8 = 69, is asid necessary 
  val dataLen = ppnLen + PAddrBits // 
  val tlbLen = metaLen + dataLen
  val Ways = tlbConfig.ways
  val TotalEntry = tlbConfig.totalEntry
  val Sets = TotalEntry / Ways
  val IndexBits = log2Up(Sets)
  val TagBits = vpnLen - IndexBits

  val debug = true //&& tlbname == "dtlb"

  def vaddrTlbBundle = new Bundle {
    val tag = UInt(TagBits.W)
    val index = UInt(IndexBits.W)
    val off = UInt(offLen.W)
  }

  def metaBundle = new Bundle {
    val vpn = UInt(vpnLen.W)
    val asid = UInt(asidLen.W)
    val mask = UInt(maskLen.W) // to support super page
    val flag = UInt(flagLen.W)
  }

  def dataBundle = new Bundle {
    val ppn = UInt(ppnLen.W)
    val pteaddr = UInt(PAddrBits.W) // pte addr, used to write back pte when flag changes (flag.d, flag.v)
  }

  def tlbBundle = new Bundle {
    val vpn = UInt(vpnLen.W)
    val asid = UInt(asidLen.W)
    val mask = UInt(maskLen.W)
    val flag = UInt(flagLen.W)
    val ppn = UInt(ppnLen.W)
    val pteaddr = UInt(PAddrBits.W)
  }

  def tlbBundle2 = new Bundle {
    val meta = UInt(metaLen.W)
    val data = UInt(dataLen.W)
  }

  def getIndex(vaddr: UInt) : UInt = {
    vaddr.asTypeOf(vaddrTlbBundle).index
  }
}

sealed abstract class TlbBundle(implicit tlbConfig: TLBConfig) extends Bundle with HasNOOPParameter with HasTlbConst with Sv39Const
sealed abstract class TlbModule(implicit tlbConfig: TLBConfig) extends Module with HasNOOPParameter with HasTlbConst with Sv39Const with HasCSRConst

class TLBMDWriteBundle (val IndexBits: Int, val Ways: Int, val tlbLen: Int) extends Bundle with HasNOOPParameter with Sv39Const {
  val wen = Output(Bool())
  val windex = Output(UInt(IndexBits.W))
  val waymask = Output(UInt(Ways.W))
  val wdata = Output(UInt(tlbLen.W))
  
  def apply(wen: UInt, windex: UInt, waymask: UInt, vpn: UInt, asid: UInt, mask: UInt, flag: UInt, ppn: UInt, pteaddr: UInt) {
    this.wen := wen
    this.windex := windex
    this.waymask := waymask
    this.wdata := Cat(vpn, asid, mask, flag, ppn, pteaddr)
  }
}

class TLBMD(implicit val tlbConfig: TLBConfig) extends TlbModule {
  val io = IO(new Bundle {
    val tlbmd = Output(Vec(Ways, UInt(tlbLen.W)))
    val write = Flipped(new TLBMDWriteBundle(IndexBits = IndexBits, Ways = Ways, tlbLen = tlbLen))
    val rindex = Input(UInt(IndexBits.W))
    val ready = Output(Bool())
  })

  //val tlbmd = Reg(Vec(Ways, UInt(tlbLen.W)))
  val tlbmd = Mem(Sets, Vec(Ways, UInt(tlbLen.W)))
  io.tlbmd := tlbmd(io.rindex)

  //val reset = WireInit(false.B)
  val resetState = RegInit(true.B)//RegEnable(true.B, init = true.B, reset)
  val (resetSet, resetFinish) = Counter(resetState, Sets)
  when (resetFinish) { resetState := false.B }

  val writeWen = io.write.wen//WireInit(false.B)
  val writeSetIdx = io.write.windex
  val writeWayMask = io.write.waymask
  val writeData = io.write.wdata

  val wen = Mux(resetState, true.B, writeWen)
  val setIdx = Mux(resetState, resetSet, writeSetIdx)
  val waymask = Mux(resetState, Fill(Ways, "b1".U), writeWayMask)
  val dataword = Mux(resetState, 0.U, writeData)
  val wdata = VecInit(Seq.fill(Ways)(dataword))

  when (wen) { tlbmd.write(setIdx, wdata, waymask.asBools) }

  io.ready := !resetState
  def rready() = !resetState
  def wready() = !resetState
}

class TLB(implicit val tlbConfig: TLBConfig) extends TlbModule{
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBusUC(userBits = userBits, addrBits = VAddrBits))
    val out = new SimpleBusUC(userBits = userBits)

    val mem = new SimpleBusUC()
    val flush = Input(Bool()) 
    val csrMMU = new MMUIO
    val cacheEmpty = Input(Bool())
    val ipf = Output(Bool())
  })

  val satp = WireInit(0.U(XLEN.W))
  BoringUtils.addSink(satp, "CSRSATP")

  // tlb exec
  val tlbExec = Module(new TLBExec)
  val tlbEmpty = Module(new TLBEmpty)
  val mdTLB = Module(new TLBMD)
  val mdUpdate = Wire(Bool())
  
  tlbExec.io.flush := io.flush
  tlbExec.io.satp := satp
  tlbExec.io.mem <> io.mem
  tlbExec.io.pf <> io.csrMMU
  tlbExec.io.md <> RegEnable(mdTLB.io.tlbmd, mdUpdate)
  tlbExec.io.mdReady := mdTLB.io.ready
  mdTLB.io.rindex := getIndex(io.in.req.bits.addr)
  mdTLB.io.write <> tlbExec.io.mdWrite
  
  io.ipf := false.B
  
  // meta reset
  val flushTLB = WireInit(false.B)
  BoringUtils.addSink(flushTLB, "MOUFlushTLB")
  mdTLB.reset := reset.asBool || flushTLB
  Debug() {
    when(flushTLB && GTimer() > 77437080.U) {
      printf("%d sfence_vma req.pc:%x valid:%d\n", GTimer(), io.in.req.bits.addr, io.in.req.valid)
    }
  }

  // VM enable && io
  val vmEnable = satp.asTypeOf(satpBundle).mode === 8.U && (io.csrMMU.priviledgeMode < ModeM)

  def PipelineConnectTLB[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T], update: Bool, rightOutFire: Bool, isFlush: Bool, vmEnable: Bool) = {
    val valid = RegInit(false.B)
    when (rightOutFire) { valid := false.B }
    when (left.valid && right.ready && vmEnable) { valid := true.B }
    when (isFlush) { valid := false.B }

    left.ready := right.ready
    right.bits <> RegEnable(left.bits, left.valid && right.ready)
    right.valid := valid //&& !isFlush

    update := left.valid && right.ready
  }

  tlbEmpty.io.in <> DontCare
  tlbEmpty.io.out.ready := DontCare
  PipelineConnectTLB(io.in.req, tlbExec.io.in, mdUpdate, tlbExec.io.isFinish, io.flush, vmEnable)
  if(tlbname == "dtlb") {
    PipelineConnect(tlbExec.io.out, tlbEmpty.io.in, tlbEmpty.io.out.fire(), io.flush)
  }
  when(!vmEnable) {
    tlbExec.io.out.ready := true.B // let existed request go out
    if( tlbname == "dtlb") { tlbEmpty.io.out.ready := true.B }
    io.out.req.valid := io.in.req.valid
    io.in.req.ready := io.out.req.ready
    io.out.req.bits.addr := io.in.req.bits.addr(PAddrBits-1, 0)
    io.out.req.bits.size := io.in.req.bits.size
    io.out.req.bits.cmd := io.in.req.bits.cmd
    io.out.req.bits.wmask := io.in.req.bits.wmask
    io.out.req.bits.wdata := io.in.req.bits.wdata
    io.out.req.bits.user.map(_ := io.in.req.bits.user.getOrElse(0.U))
  }.otherwise {
    if (tlbname == "dtlb") { io.out.req <> tlbEmpty.io.out}
    else { io.out.req <> tlbExec.io.out }
  }
  io.out.resp <> io.in.resp

  // lsu need dtlb signals
  if(tlbname == "dtlb") {
    val alreadyOutFinish = RegEnable(true.B, init=false.B, tlbExec.io.out.valid && !tlbExec.io.out.ready)
    when(alreadyOutFinish && tlbExec.io.out.fire()) { alreadyOutFinish := false.B}
    val tlbFinish = (tlbExec.io.out.valid && !alreadyOutFinish) || tlbExec.io.pf.isPF()
    BoringUtils.addSource(tlbFinish, "DTLBFINISH")
    BoringUtils.addSource(io.csrMMU.isPF(), "DTLBPF")
    BoringUtils.addSource(vmEnable, "DTLBENABLE")
  }

  // instruction page fault
  if (tlbname == "itlb") {
    when (tlbExec.io.ipf && vmEnable) {
      tlbExec.io.out.ready := io.cacheEmpty && io.in.resp.ready
      io.out.req.valid := false.B
    }

    when (tlbExec.io.ipf && vmEnable && io.cacheEmpty) {
      io.in.resp.valid := true.B
      io.in.resp.bits.rdata := 0.U
      io.in.resp.bits.cmd := SimpleBusCmd.readLast
      io.in.resp.bits.user.map(_ := tlbExec.io.in.bits.user.getOrElse(0.U))
      io.ipf := tlbExec.io.ipf
    }
  }

  Debug() {
    if (debug) {
      printf("[TLB-"  + tlbname+ "]: Timer:%d---------\n", GTimer())
      printf("[TLB-"  + tlbname+ "]: InReq(%d, %d) InResp(%d, %d) OutReq(%d, %d) OutResp(%d, %d) vmEnable:%d mode:%d\n", io.in.req.valid, io.in.req.ready, io.in.resp.valid, io.in.resp.ready, io.out.req.valid, io.out.req.ready, io.out.resp.valid, io.out.resp.ready, vmEnable, io.csrMMU.priviledgeMode)
      printf("[TLB-"  + tlbname+ "]: InReq: addr:%x cmd:%d wdata:%x OutReq: addr:%x cmd:%x wdata:%x\n", io.in.req.bits.addr, io.in.req.bits.cmd, io.in.req.bits.wdata, io.out.req.bits.addr, io.out.req.bits.cmd, io.out.req.bits.wdata)
      printf("[TLB-"  + tlbname+ "]: OutResp: rdata:%x cmd:%x Inresp: rdata:%x cmd:%x\n", io.out.resp.bits.rdata, io.out.resp.bits.cmd, io.in.resp.bits.rdata, io.in.resp.bits.cmd)
      printf("[TLB-"  + tlbname+ "]: satp:%x flush:%d cacheEmpty:%d instrPF:%d loadPF:%d storePF:%d \n", satp, io.flush, io.cacheEmpty, io.ipf, io.csrMMU.loadPF, io.csrMMU.storePF)
    }
  }  

}

class TLBExec(implicit val tlbConfig: TLBConfig) extends TlbModule{
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits, addrBits = VAddrBits)))
    val out = Decoupled(new SimpleBusReqBundle(userBits = userBits))

    val md = Input(Vec(Ways, UInt(tlbLen.W)))
    val mdWrite = new TLBMDWriteBundle(IndexBits = IndexBits, Ways = Ways, tlbLen = tlbLen)
    val mdReady = Input(Bool())

    val mem = new SimpleBusUC()
    val flush = Input(Bool()) 
    val satp = Input(UInt(XLEN.W))
    val pf = new MMUIO
    val ipf = Output(Bool())
    val isFinish = Output(Bool())
  })

  val md = io.md//RegEnable(mdTLB.io.tlbmd, io.in.ready)
  
  // lazy renaming
  val req = io.in.bits
  val vpn = req.addr.asTypeOf(vaBundle2).vpn.asTypeOf(vpnBundle)
  val pf = io.pf
  val satp = io.satp.asTypeOf(satpBundle)

  // pf init
  pf.loadPF := false.B
  pf.storePF := false.B
  pf.addr := req.addr

  // check hit or miss
  val hitVec = VecInit(md.map(m => m.asTypeOf(tlbBundle).flag.asTypeOf(flagBundle).v && (m.asTypeOf(tlbBundle).asid === satp.asid) && MaskEQ(m.asTypeOf(tlbBundle).mask, m.asTypeOf(tlbBundle).vpn, vpn.asUInt))).asUInt
  val hit = io.in.valid && hitVec.orR
  val miss = io.in.valid && !hitVec.orR

  val victimWaymask = if (Ways > 1) (1.U << LFSR64()(log2Up(Ways)-1,0)) else "b1".U
  val waymask = Mux(hit, hitVec, victimWaymask)

  val loadPF = WireInit(false.B)
  val storePF = WireInit(false.B)

  // hit
  val hitMeta = Mux1H(waymask, md).asTypeOf(tlbBundle2).meta.asTypeOf(metaBundle)
  val hitData = Mux1H(waymask, md).asTypeOf(tlbBundle2).data.asTypeOf(dataBundle)
  val hitFlag = hitMeta.flag.asTypeOf(flagBundle)
  val hitMask = hitMeta.mask
  // hit write back pte.flag
  val hitinstrPF = WireInit(false.B)
  val hitWB = hit && (!hitFlag.a || !hitFlag.d && req.isWrite()) && !hitinstrPF && !(loadPF || storePF || io.pf.isPF())
  val hitRefillFlag = Cat(req.isWrite().asUInt, 1.U(1.W), 0.U(6.W)) | hitFlag.asUInt
  val hitWBStore = RegEnable(Cat(0.U(10.W), hitData.ppn, 0.U(2.W), hitRefillFlag), hitWB)

  // hit permission check
  val hitCheck = hit /*&& hitFlag.v */&& !(pf.priviledgeMode === ModeU && !hitFlag.u) && !(pf.priviledgeMode === ModeS && hitFlag.u && !pf.status_sum)
  val hitExec = hitCheck && hitFlag.x
  val hitLoad = hitCheck && (hitFlag.r || pf.status_mxr && hitFlag.x)
  val hitStore = hitCheck && hitFlag.w
  
  val isAMO = WireInit(false.B)
  if (tlbname == "dtlb") {
    BoringUtils.addSink(isAMO, "ISAMO")
  }

  io.pf.loadPF := RegNext(loadPF, init =false.B)
  io.pf.storePF := RegNext(storePF, init = false.B)

  if (tlbname == "itlb") { hitinstrPF := !hitExec  && hit}
  if (tlbname == "dtlb") { 
    loadPF := !hitLoad && req.isRead() && hit && !isAMO
    storePF := (!hitStore && req.isWrite() && hit) || (!hitLoad && req.isRead() && hit && isAMO)
  }

  // miss
  val s_idle :: s_memReadReq :: s_memReadResp :: s_write_pte :: s_wait_resp :: s_miss_slpf :: Nil = Enum(6)
  val state = RegInit(s_idle)
  val level = RegInit(Level.U(log2Up(Level).W))
  
  val memRespStore = Reg(UInt(XLEN.W))
  val missMask = WireInit("h3ffff".U(maskLen.W))
  val missMaskStore = Reg(UInt(maskLen.W))
  val missMetaRefill = WireInit(false.B)
  val missRefillFlag = WireInit(0.U(8.W))
  val memRdata = io.mem.resp.bits.rdata.asTypeOf(pteBundle)
  val raddr = Reg(UInt(PAddrBits.W))
  val alreadyOutFire = RegEnable(true.B, init = false.B, io.out.fire)

  //handle flush
  val needFlush = RegInit(false.B)
  val ioFlush = io.flush
  val isFlush = needFlush || ioFlush
  when (ioFlush && (state =/= s_idle)) { needFlush := true.B}
  when (io.out.fire() && needFlush) { needFlush := false.B}

  val missIPF = RegInit(false.B)

  // state machine to handle miss(ptw) and pte-writing-back
  switch (state) {
    is (s_idle) {
      when (!ioFlush && hitWB) {
        state := s_write_pte
        needFlush := false.B
        alreadyOutFire := false.B
      }.elsewhen (miss && !ioFlush) {
        state := s_memReadReq
        raddr := paddrApply(satp.ppn, vpn.vpn2) //
        level := Level.U
        needFlush := false.B
        alreadyOutFire := false.B
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
        }.elsewhen (!(missflag.r || missflag.x) && (level===3.U || level===2.U)) {
          when(!missflag.v || (!missflag.r && missflag.w)) { //TODO: fix needflush
            if(tlbname == "itlb") { state := s_wait_resp } else { state := s_miss_slpf }
            if(tlbname == "itlb") { missIPF := true.B }
            if(tlbname == "dtlb") { 
              loadPF := req.isRead() && !isAMO 
              storePF := req.isWrite() || isAMO 
            }  
            Debug() {
              if(debug) {
                printf("%d " + tlbname +" tlbException!!! ", GTimer())
                printf(p" req:${req}  Memreq:${io.mem.req}  MemResp:${io.mem.resp}")
                printf(" level:%d",level)
                printf("\n")
              }
            }
          }.otherwise {
            state := s_memReadReq
            raddr := paddrApply(memRdata.ppn, Mux(level === 3.U, vpn.vpn1, vpn.vpn0))
          }
        }.elsewhen (level =/= 0.U) { //TODO: fix needFlush
          val permCheck = missflag.v && !(pf.priviledgeMode === ModeU && !missflag.u) && !(pf.priviledgeMode === ModeS && missflag.u && !pf.status_sum)
          val permExec = permCheck && missflag.x
          val permLoad = permCheck && (missflag.r || pf.status_mxr && missflag.x)
          val permStore = permCheck && missflag.w
          val updateAD = false.B //!missflag.a || (!missflag.d && req.isWrite())
          val updateData = Cat( 0.U(56.W), req.isWrite(), 1.U(1.W), 0.U(6.W) )
          missRefillFlag := Cat(req.isWrite(), 1.U(1.W), 0.U(6.W)) | missflag.asUInt
          memRespStore := io.mem.resp.bits.rdata | updateData 
          if(tlbname == "itlb") {
            when (!permExec) { missIPF := true.B ; state := s_wait_resp}
            .otherwise { 
              state := Mux(updateAD, s_write_pte, s_wait_resp)
              missMetaRefill := true.B
            }
          }
          if(tlbname == "dtlb") {
            when((!permLoad && req.isRead()) || (!permStore && req.isWrite())) { 
              state := s_miss_slpf
              loadPF := req.isRead() && !isAMO
              storePF := req.isWrite() || isAMO
            }.otherwise {
              state := Mux(updateAD, s_write_pte, s_wait_resp)
              missMetaRefill := true.B
            }
          }
          missMask := Mux(level===3.U, 0.U(maskLen.W), Mux(level===2.U, "h3fe00".U(maskLen.W), "h3ffff".U(maskLen.W)))
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

    is (s_wait_resp) { when (io.out.fire() || ioFlush || alreadyOutFire){
      state := s_idle
      missIPF := false.B
      alreadyOutFire := false.B
    }}

    is (s_miss_slpf) {
      state := s_idle
    }
  }

  // mem
  val cmd = Mux(state === s_write_pte, SimpleBusCmd.write, SimpleBusCmd.read)
  io.mem.req.bits.apply(addr = Mux(hitWB, hitData.pteaddr, raddr), cmd = cmd, size = (if (XLEN == 64) "b11".U else "b10".U), wdata =  Mux( hitWB, hitWBStore, memRespStore), wmask = 0xff.U)
  io.mem.req.valid := ((state === s_memReadReq || state === s_write_pte) && !isFlush)
  io.mem.resp.ready := true.B

  // tlb refill
  io.mdWrite.apply(wen = RegNext((missMetaRefill && !isFlush) || (hitWB && state === s_idle && !isFlush), init = false.B), 
    windex = RegNext(getIndex(req.addr)), waymask = RegNext(waymask), vpn = RegNext(vpn.asUInt), 
    asid = RegNext(Mux(hitWB, hitMeta.asid, satp.asid)), mask = RegNext(Mux(hitWB, hitMask, missMask)), 
    flag = RegNext(Mux(hitWB, hitRefillFlag, missRefillFlag)), ppn = RegNext(Mux(hitWB, hitData.ppn, memRdata.ppn)), 
    pteaddr = RegNext((Mux(hitWB, hitData.pteaddr, raddr))))

  // io
  io.out.bits := req
  io.out.bits.addr := Mux(hit, maskPaddr(hitData.ppn, req.addr(PAddrBits-1, 0), hitMask), maskPaddr(memRespStore.asTypeOf(pteBundle).ppn, req.addr(PAddrBits-1, 0), missMaskStore))
  io.out.valid := io.in.valid && Mux(hit && !hitWB, !(io.pf.isPF() || loadPF || storePF), state === s_wait_resp)// && !alreadyOutFire
  
  io.in.ready := io.out.ready && (state === s_idle) && !miss && !hitWB && io.mdReady && (!io.pf.isPF() && !loadPF && !storePF)//maybe be optimized

  io.ipf := Mux(hit, hitinstrPF, missIPF)
  io.isFinish := io.out.fire() || io.pf.isPF()

  Debug() {
    if (debug) {
      printf("[TLBExec-"  + tlbname+ "]: Timer:%d---------\n", GTimer())
      printf("[TLBExec-"  + tlbname+ "]: In(%d, %d) Out(%d, %d) InAddr:%x OutAddr:%x cmd:%d \n", io.in.valid, io.in.ready, io.out.valid, io.out.ready, req.addr, io.out.bits.addr, req.cmd)
      printf("[TLBExec-"  + tlbname+ "]: isAMO:%d io.Flush:%d needFlush:%d alreadyOutFire:%d isFinish:%d\n",isAMO, io.flush, needFlush, alreadyOutFire, io.isFinish)
      printf("[TLBExec-"  + tlbname+ "]: hit:%d hitWB:%d hitVPN:%x hitFlag:%x hitPPN:%x hitRefillFlag:%x hitWBStore:%x hitCheck:%d hitExec:%d hitLoad:%d hitStore:%d\n", hit, hitWB, hitMeta.vpn, hitFlag.asUInt, hitData.ppn, hitRefillFlag, hitWBStore, hitCheck, hitExec, hitLoad, hitStore)
      printf("[TLBExec-"  + tlbname+ "]: miss:%d state:%d level:%d raddr:%x memRdata:%x missMask:%x missRefillFlag:%x missMetaRefill:%d\n", miss, state, level, raddr, memRdata.asUInt, missMask, missRefillFlag, missMetaRefill)
      printf("[TLBExec-"  + tlbname+ "]: meta/data: (0)%x|%b|%x (1)%x|%b|%x (2)%x|%b|%x (3)%x|%b|%x rread:%d\n", md(0).asTypeOf(tlbBundle).vpn, md(0).asTypeOf(tlbBundle).flag, md(0).asTypeOf(tlbBundle).ppn, md(1).asTypeOf(tlbBundle).vpn, md(1).asTypeOf(tlbBundle).flag, md(1).asTypeOf(tlbBundle).ppn, md(2).asTypeOf(tlbBundle).vpn, md(2).asTypeOf(tlbBundle).flag, md(2).asTypeOf(tlbBundle).ppn, md(3).asTypeOf(tlbBundle).vpn, md(3).asTypeOf(tlbBundle).flag, md(3).asTypeOf(tlbBundle).ppn, io.mdReady)
      printf("[TLBExec-"  + tlbname+ "]: md: wen:%d windex:%x waymask:%x vpn:%x asid:%x mask:%x flag:%x asid:%x ppn:%x pteaddr:%x\n", io.mdWrite.wen, io.mdWrite.windex, io.mdWrite.waymask, io.mdWrite.wdata.asTypeOf(tlbBundle).vpn, io.mdWrite.wdata.asTypeOf(tlbBundle).asid, io.mdWrite.wdata.asTypeOf(tlbBundle).mask, io.mdWrite.wdata.asTypeOf(tlbBundle).flag, io.mdWrite.wdata.asTypeOf(tlbBundle).asid, io.mdWrite.wdata.asTypeOf(tlbBundle).ppn, io.mdWrite.wdata.asTypeOf(tlbBundle).pteaddr)
      printf("[TLBExec-"  + tlbname+ "]: MemReq(%d, %d) MemResp(%d, %d) addr:%x cmd:%d rdata:%x cmd:%d\n", io.mem.req.valid, io.mem.req.ready, io.mem.resp.valid, io.mem.resp.ready, io.mem.req.bits.addr, io.mem.req.bits.cmd, io.mem.resp.bits.rdata, io.mem.resp.bits.cmd)
      printf("[TLBExec-"  + tlbname+ "]: io.ipf:%d hitinstrPF:%d missIPF:%d pf.loadPF:%d pf.storePF:%d loadPF:%d storePF:%d\n", io.ipf, hitinstrPF, missIPF, io.pf.loadPF, io.pf.storePF, loadPF, storePF)
    }
  }
}

class TLBEmpty(implicit val tlbConfig: TLBConfig) extends TlbModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits)))
    val out = Decoupled(new SimpleBusReqBundle(userBits = userBits))
  })

  io.out <> io.in
}

object TLB {
  def apply(in: SimpleBusUC, mem: SimpleBusUC, flush: Bool, csrMMU: MMUIO)(implicit tlbConfig: TLBConfig) = {
    val tlb = Module(new TLB)
    tlb.io.in <> in
    tlb.io.mem <> mem
    tlb.io.flush := flush
    tlb.io.csrMMU <> csrMMU
    tlb
  }
}
