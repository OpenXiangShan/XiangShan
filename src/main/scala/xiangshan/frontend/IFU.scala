package xiangshan.frontend

import chisel3._
import chisel3.util._
import chisel3.core.{withReset}
import device.RAMHelper
import xiangshan._

trait HasIFUConst { this: XSModule =>
  val resetVector = 0x80000000L//TODO: set reset vec

  val groupAlign = log2Up(FetchWidth * 4)
  def groupPC(pc: UInt): UInt = Cat(pc(VAddrBits-1, groupAlign), 0.U(groupAlign.W))
  
}

class IFUIO extends IFUBundle
{
    val fetchPacket = DecoupledIO(new FetchPacket)
    val redirect = Flipped(ValidIO(new Redirect))
    val icacheReq = DecoupledIO(UInt(VAddrBits.W)
    val icacheResp = Flipped(DecoupledIO(new FakeIcacheResp))
}

class FakeBPU extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val in = new Bundle { val pc = Flipped(Valid(UInt(VAddrBits.W))) }
    val btbOut = ValidIO(new BranchPrediction)
    val tageOut = ValidIO(new BranchPrediction)
    val predecode = Flipped(ValidIO(new Predecode))
  })

  io.btbOut.valid := false.B
  io.btbOut.bits <> DontCare
  io.tageOut.valid := false.B
  io.tageOut.bits <> DontCare
}



class IFU(implicit val p: XSConfig) extends IFUModule with HasIFUConst
{
    val io = IO(new IFUIO)
    val bpu = Module(new FakeBPU)

    //-------------------------
    //      IF1  PC update
    //-------------------------
    //local
    val if1_npc = WireInit(0.U(VAddrBits.W))
    val if1_valid = !reset.asBool
    val if1_pc = RegInit(resetVector.U(VAddrBits.W))
    //next
    val if2_ready = WireInit(false.B)
    val if1_ready = if2_ready

    //pipe fire
    val if1_fire = if1_valid && if1_ready 
    val if1_pcUpdate = io.redirect.valid || if1_fire

    when(RegNext(reset.asBool) && !reset.asBool)
    {
      if1_npc := resetVector.U(VAddrBits.W)
    }

    when(if1_pcUpdate)
    { 
      if1_pc := if1_npc
    }

    bpu.io.in.valid := if1_valid
    bpu.io.in.pc := if1_npc

    //-------------------------
    //      IF2  btb resonse 
    //           icache visit
    //-------------------------
    //local
    val if2_flush = WireInit(false.B)
    val if2_update = if1_fire && !if2_flush
    val if2_valid = RegNext(if2_update)
    val if2_pc = if1_pc
    val if2_btb_taken = bpu.io.btbOut.valid
    val if2_btb_insMask = bpu.io.btbOut.bits.instrValid
    val if2_btb_target = bpu.io.btbOut.bits.target
    val if2_snpc = Cat(if2_pc(VAddrBits-1, groupAlign) + 1.U, 0.U(groupAlign.W))
    val if2_flush = WireInit(false.B)

    //next
    val if3_ready = WireInit(false.B)

    //pipe fire
    val if2_fire = if2_valid && if3_ready 
    val if2_ready = (if2_fire && io.icacheReq.fire()) || !if2_valid

    io.icacheReq.valid := if2_fire
    io.icacheReq.bits := groupPC(if2_pc)

    when(if2_valid && if2_btb_taken)
    {
      if1_npc := if2_btb_target
    } .otherwise 
    {
      if1_npc := if2_snpc
    }

    //-------------------------
    //      IF3  icache hit check
    //-------------------------
    //local
    val if3_flush = WireInit(false.B)
    val if3_update = if2_fire && !if3_flush
    val if3_valid = RegNext(if3_update)
    val if3_pc = RegEnable(if2_pc,if3_update)
    val if3_btb_target = RegEnable(if2_btb_target,if3_update)
    val if3_btb_taken = RegEnable(if2_btb_taken,if3_update)

    //next
    val if4_ready = WireInit(false.B)

    //pipe fire
    val if3_fire = if3_valid && if4_ready
    val if3_ready = if3_fire  || !if3_valid

    //-------------------------
    //      IF4  icache resonse   
    //           RAS result
    //           taget generate
    //-------------------------
    val if4_flush = WireInit(false.B)
    val if4_update = if3_fire && !if4_flush 
    val if4_valid = RegNext(if4_update)
    val if4_pc = RegEnable(if3_pc,if4_update)
    val if4_btb_target = RegEnable(if3_btb_target,if4_update)
    val if4_btb_taken = RegEnable(if3_btb_taken,if4_update)



    when(if4_valid && io.icacheResp.fire())
    {
      if1_npc := if4_btb_target
    }
    

    //redirect
    when(io.redirect.valid){
      if1_npc := io.redirect.bits.target
      if2_flush := true.B
      if3_flush := true.B
      if4_flush := true.B
    }


    //Output -> iBuffer
    if4_ready := io.fetchPacket.ready && io.icacheResp.valid
    io.fetchPacket.valid := if4_valid && !if4_flush
    io.fetchPacket.instrs := io.icacheResp.bits.icacheOut
    io.fetchPacket.mask := Fill(FetchWidth*2, 1.U(1.W)) << if4_pc(2+log2Up(FetchWidth)-1, 1)
    io.fetchPacket.pc := if4_pc

    //to BPU
    bpu.io.predecode.valid := if4_valid
    bpu.io.predecode.bits <> io.icacheResp.bits.predecode
    bpu.io.predecode.bits.mask := Fill(FetchWidth, 1.U(1.W)) << if4_pc(2+log2Up(FetchWidth)-1, 2) //TODO: consider RVC

    io.icacheResp.ready := io.fetchPacket.ready 

}

