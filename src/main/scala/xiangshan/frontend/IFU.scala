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

sealed abstract IFUBundle extends XSBundle with HasIFUConst
sealed abstract IFUModule extends XSModule with HasIFUConst with NeedImpl

class IFUIO extends IFUBundle
{
    val fetchPacket = DecoupledIO(new FetchPacket)
    val redirect = Flipped(ValidIO(new Redirect))
    val toIcache = DecoupledIO(UInt(VAddrBits.W)
    val fromIcache = Flipped(ValidIO(new IcacheResp))
}



class IFU(implicit val p: XSConfig) extends IFUModule
{
    val io = IO(new IFUIO)
    val bpu = Module(new BPU)

    //-------------------------
    //      IF1  PC update
    //-------------------------
    //local
    val if1_npc = WireInit(0.U(VAddrBits.W))
    val if1_valid = WireInit(false.B)
    val if1_pc = RegInit(resetVector.U(VAddrBits.W))
    //next
    val if2_ready = WireInit(false.B)
    val if1_ready = bpu.io.in.ready &&  if2_ready

    //pipe fire
    val if1_fire = if1_valid && if1_ready 
    val if1_pcUpdate = io.redirect.valid || if1_fire

    when(RegNext(reset.asBool) && !reset.asBool)
    {
      if1_npc := resetVector
      if1_valid := true.B
    }

    when(if1_pcUpdate)
    { 
      if1_pc ：= if1_npc
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
    val if2_ready = (if2_fire && icache.io.in.fire()) || !if2_valid

    icache.io.in.valid := if2_fire
    icahce.io.in.bits := if2_pc

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

    //TAGE
    val tage_taken = bpu.io.tageOut.valid

    //TODO: icache predecode info
    val predecode = icache.io.out.bits.predecode

    val icache_isBR = tage_taken
    val icache_isDirectJmp = icache_isBR && 
    val icache_isCall = icache_isDirectJmp &&
    val icache_isReturn = !icache_isDirectJmp &&
    val icache_isOtherNDJmp = !icache_isDirectJmp && !icache_isReturn


    when(if4_valid && icahe.io.out.fire())
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
    if4_ready := io.fetchPacket.ready
    io.fetchPacket.valid := if4_valid && !if4_flush
    io.fetchPacket.instrs := io.icache.out.bits.rdata
    io.fetchPacket.mask := Fill(FetchWidth*2, 1.U(1.W)) << pc(2+log2Up(FetchWidth)-1, 1)
    io.fetchPacket.pc := if4_pc


}

