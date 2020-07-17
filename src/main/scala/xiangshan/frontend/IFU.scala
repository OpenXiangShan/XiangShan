 package xiangshan.frontend

import chisel3._
import chisel3.util._
import device.RAMHelper
import xiangshan._
import utils._

trait HasIFUConst { this: XSModule =>
  val resetVector = 0x80000000L//TODO: set reset vec
  val groupAlign = log2Up(FetchWidth * 4)
  def groupPC(pc: UInt): UInt = Cat(pc(VAddrBits-1, groupAlign), 0.U(groupAlign.W))
  def snpc(pc: UInt): UInt = pc + (1 << groupAlign).U
  
}

class IFUIO extends XSBundle
{
    val fetchPacket = DecoupledIO(new FetchPacket)
    val redirectInfo = Input(new RedirectInfo)
    val icacheReq = DecoupledIO(new FakeIcacheReq)
    val icacheResp = Flipped(DecoupledIO(new FakeIcacheResp))
}

class FakeBPU extends XSModule{
  val io = IO(new Bundle() {
    val redirectInfo = Input(new RedirectInfo)
    val in = new Bundle { val pc = Flipped(Valid(UInt(VAddrBits.W))) }
    val btbOut = ValidIO(new BranchPrediction)
    val tageOut = ValidIO(new BranchPrediction)
    val predecode = Flipped(ValidIO(new Predecode))
  })

  io.btbOut.valid := true.B
  io.btbOut.bits <> DontCare
  io.btbOut.bits.redirect := GTimer() === 1.U
  io.btbOut.bits.target := "h080001234".U
  io.tageOut.valid := false.B
  io.tageOut.bits <> DontCare
}


class IFU extends XSModule with HasIFUConst
{
    val io = IO(new IFUIO)
    val bpu = if(EnableBPU) Module(new BPU) else Module(new FakeBPU)

    //-------------------------
    //      IF1  PC update
    //-------------------------
    //local
    val if1_npc = WireInit(0.U(VAddrBits.W))
    val if1_valid = !reset.asBool 
    val if1_pc = RegInit(resetVector.U(VAddrBits.W))
    //next
    val if2_ready = WireInit(false.B)
    val if2_snpc = snpc(if1_pc) //TODO: this is ugly
    val needflush = WireInit(false.B)

    //pipe fire
    val if1_fire = if1_valid && if2_ready 
    val if1_pcUpdate = if1_fire || needflush

    when(RegNext(reset.asBool) && !reset.asBool){
    //when((GTimer() === 501.U)){ //TODO:this is ugly
      XSDebug("RESET....\n")
      if1_npc := resetVector.U(VAddrBits.W)
    } .otherwise{
      if1_npc := if2_snpc
    }

    when(if1_pcUpdate)
    { 
      if1_pc := if1_npc
    }

    bpu.io.in.pc.valid := if1_fire
    bpu.io.in.pc.bits := if1_npc
    bpu.io.redirectInfo := io.redirectInfo

    XSDebug("[IF1]if1_valid:%d  ||  if1_npc:0x%x  || if1_pcUpdate:%d if1_pc:0x%x  || if2_ready:%d",if1_valid,if1_npc,if1_pcUpdate,if1_pc,if2_ready)
    XSDebug(false,if1_fire,"------IF1->fire!!!")
    XSDebug(false,true.B,"\n")

    //-------------------------
    //      IF2  btb response 
    //           icache visit
    //-------------------------
    //local
    val if2_valid = RegEnable(next=if1_valid,init=false.B,enable=if1_fire)
    val if2_pc = if1_pc
    val if2_btb_taken = bpu.io.btbOut.valid && bpu.io.btbOut.bits.redirect
    val if2_btb_insMask = bpu.io.btbOut.bits.instrValid
    val if2_btb_target = bpu.io.btbOut.bits.target

    //next
    val if3_ready = WireInit(false.B)

    //pipe fire
    val if2_fire = if2_valid && if3_ready && io.icacheReq.fire()
    if2_ready := (if2_fire) || !if2_valid

    io.icacheReq.valid := if2_valid
    io.icacheReq.bits.addr := if2_pc

    when(if2_valid && if2_btb_taken)
    {
      if1_npc := if2_btb_target
    }

    XSDebug("[IF2]if2_valid:%d  ||  if2_pc:0x%x   || if3_ready:%d                                        ",if2_valid,if2_pc,if3_ready)
    XSDebug(false,if2_fire,"------IF2->fire!!!")
    XSDebug(false,true.B,"\n")
    XSDebug("[IF2-Icache-Req] icache_in_valid:%d  icache_in_ready:%d\n",io.icacheReq.valid,io.icacheReq.ready)
    XSDebug("[IF2-BPU-out]if2_btbTaken:%d || if2_btb_insMask:%b || if2_btb_target:0x%x \n",if2_btb_taken,if2_btb_insMask.asUInt,if2_btb_target)
    //-------------------------
    //      IF3  icache hit check
    //-------------------------
    //local
    val if3_valid = RegEnable(next=if2_valid,init=false.B,enable=if2_fire)
    val if3_pc = RegEnable(if2_pc,if2_fire)
    val if3_npc = RegEnable(if1_npc,if2_fire)
    val if3_btb_target = RegEnable(if2_btb_target,if2_fire)
    val if3_btb_taken = RegEnable(if2_btb_taken,if2_fire)
    val if3_btb_insMask = RegEnable(if2_btb_insMask, if2_fire)

    //next
    val if4_ready = WireInit(false.B)

    //pipe fire
    val if3_fire = if3_valid && if4_ready
    if3_ready := if3_fire  || !if3_valid


    XSDebug("[IF3]if3_valid:%d  ||  if3_pc:0x%x   if3_npc:0x%x || if4_ready:%d                    ",if3_valid,if3_pc,if3_npc,if4_ready)
    XSDebug(false,if3_fire,"------IF3->fire!!!")
    XSDebug(false,true.B,"\n")

    //-------------------------
    //      IF4  icache response   
    //           RAS result
    //           taget generate
    //-------------------------
    val if4_valid = RegEnable(next=if3_valid,init=false.B,enable=if3_fire)
    val if4_pc = RegEnable(if3_pc,if3_fire)
    val if4_npc = RegEnable(if3_npc,if3_fire)
    val if4_btb_target = RegEnable(if3_btb_target,if3_fire)
    val if4_btb_taken = RegEnable(if3_btb_taken,if3_fire)
    val if4_btb_insMask = RegEnable(if3_btb_insMask, if3_fire)
    val if4_tage_target = bpu.io.tageOut.bits.target
    val if4_tage_taken = bpu.io.tageOut.valid && bpu.io.tageOut.bits.redirect
    val if4_tage_insMask = bpu.io.tageOut.bits.instrValid
    val if4_btb_missPre = WireInit(false.B)

    XSDebug("[IF4]if4_valid:%d  ||  if4_pc:0x%x   if4_npc:0x%x\n",if4_valid,if4_pc,if4_npc)
    XSDebug("[IF4-TAGE-out]if4_tage_taken:%d || if4_btb_insMask:%b || if4_tage_target:0x%x \n",if4_tage_taken,if4_tage_insMask.asUInt,if4_tage_target)
    XSDebug("[IF4-ICACHE-RESP]icacheResp.valid:%d   icacheResp.ready:%d\n",io.icacheResp.valid,io.icacheResp.ready)

    when(io.icacheResp.fire() && if4_tage_taken &&if4_valid)
    {
      if1_npc := if4_tage_target
    }
    //redirect: tage result differ btb
    if4_btb_missPre := (if4_tage_taken ^ if4_btb_taken) || (if4_tage_taken && if4_btb_taken && (if4_tage_target =/=  if4_btb_target))

    /*
    if(EnableBPD){
      when(!if4_tage_taken && if4_btb_taken && if4_valid){
        if1_npc := if4_pc + (PopCount(io.fetchPacket.bits.mask) >> 2.U)
      }
    }
    */

    //redirect: miss predict
    when(io.redirectInfo.flush()){
      if1_npc := io.redirectInfo.redirect.target
    }
    XSDebug(io.redirectInfo.flush(),"[IFU-REDIRECT] target:0x%x  \n",io.redirectInfo.redirect.target.asUInt)
    
  
    //flush pipline
    if(EnableBPD){needflush := (if4_valid && if4_btb_missPre) || io.redirectInfo.flush() }
    else {needflush := io.redirectInfo.flush()}
    when(needflush){
      if3_valid := false.B
      if4_valid := false.B
    }
    //flush ICache
    io.icacheReq.bits.flush := needflush

    //Output -> iBuffer
    //io.fetchPacket <> DontCare
    if4_ready := io.fetchPacket.ready && (io.icacheResp.valid || !if4_valid) && (GTimer() > 500.U)
    io.fetchPacket.valid := if4_valid && !io.redirectInfo.flush()
    io.fetchPacket.bits.instrs := io.icacheResp.bits.icacheOut
    if(EnableBPU){
      io.fetchPacket.bits.mask := Mux(if4_tage_taken,(Fill(FetchWidth*2, 1.U(1.W)) & Reverse(Cat(if4_tage_insMask.map(i => Fill(2, i.asUInt))).asUInt)),
        Mux(if4_btb_taken, Fill(FetchWidth*2, 1.U(1.W)) & Reverse(Cat(if4_btb_insMask.map(i => Fill(2, i.asUInt))).asUInt),
        Fill(FetchWidth*2, 1.U(1.W)))
      )
    }
    else{
      io.fetchPacket.bits.mask := Fill(FetchWidth*2, 1.U(1.W)) //TODO : consider cross cacheline fetch
    }    
    io.fetchPacket.bits.pc := if4_pc

    XSDebug(io.fetchPacket.fire,"[IFU-Out-FetchPacket] starPC:0x%x   GroupPC:0x%xn\n",if4_pc.asUInt,groupPC(if4_pc).asUInt)
    XSDebug(io.fetchPacket.fire,"[IFU-Out-FetchPacket] instrmask %b\n",io.fetchPacket.bits.mask.asUInt)
    for(i <- 0 until FetchWidth){
      //io.fetchPacket.bits.pnpc(i) := if1_npc
      when (if4_btb_taken && !if4_tage_taken && i.U === OHToUInt(HighestBit(if4_btb_insMask.asUInt, FetchWidth))) {
        if(EnableBPD){io.fetchPacket.bits.pnpc(i) := if4_pc + ((i + 1).U << 2.U) }     //tage not taken use snpc
        else{io.fetchPacket.bits.pnpc(i) := if4_btb_target}//use fetch PC
      }.elsewhen (if4_tage_taken && i.U === OHToUInt(HighestBit(if4_tage_insMask.asUInt, FetchWidth))) {
        io.fetchPacket.bits.pnpc(i) := if1_npc
      }.otherwise {
        io.fetchPacket.bits.pnpc(i) := if4_pc + ((i + 1).U << 2.U) //use fetch PC
      }
      XSDebug(io.fetchPacket.fire,"[IFU-Out-FetchPacket] instruction %x    pnpc:0x%x\n",io.fetchPacket.bits.instrs(i).asUInt,io.fetchPacket.bits.pnpc(i).asUInt)
    }    
    io.fetchPacket.bits.hist := bpu.io.tageOut.bits.hist
    // io.fetchPacket.bits.btbVictimWay := bpu.io.tageOut.bits.btbVictimWay
    io.fetchPacket.bits.predCtr := bpu.io.tageOut.bits.predCtr
    io.fetchPacket.bits.btbHitWay := bpu.io.tageOut.bits.btbHitWay
    io.fetchPacket.bits.tageMeta := bpu.io.tageOut.bits.tageMeta
    io.fetchPacket.bits.rasSp := bpu.io.tageOut.bits.rasSp
    io.fetchPacket.bits.rasTopCtr := bpu.io.tageOut.bits.rasTopCtr

    //to BPU
    bpu.io.predecode.valid := io.icacheResp.fire() && if4_valid
    bpu.io.predecode.bits <> io.icacheResp.bits.predecode
    bpu.io.predecode.bits.mask := Fill(FetchWidth, 1.U(1.W)) //TODO: consider RVC && consider cross cacheline fetch
    bpu.io.redirectInfo := io.redirectInfo
    io.icacheResp.ready := io.fetchPacket.ready && (GTimer() > 500.U)

}

