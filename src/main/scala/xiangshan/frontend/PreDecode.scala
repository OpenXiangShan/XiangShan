package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.decode.isa.predecode.PreDecodeInst

trait HasPdconst{ this: XSModule =>
  val halfWidth = FetchWidth * 2
  val groupAlign = log2Up(FetchWidth * 4)
  def isRVC(inst: UInt) = (inst(1,0) =/= 3.U)
  def groupPC(pc: UInt): UInt = Cat(pc(VAddrBits-1, groupAlign), 0.U(groupAlign.W))
  def isLink(reg:UInt) = reg === 1.U || reg === 5.U
  def brInfo(instr: UInt) = {
    val rd = instr(11,7)
    val rs = instr(19,15)
    val brType::Nil = ListLookup(instr, List(BrType.notBr), PreDecodeInst.brTable)
    val isCall = (brType === BrType.jal || brType === BrType.jalr) && isLink(rd) && !isRVC(instr)
    val isRet = brType === BrType.jalr && isLink(rs) && !isLink(rd) && !isRVC(instr)
    List(brType, isCall, isRet)
  }
}

object BrType {
  def notBr   = "b00".U
  def branch  = "b01".U
  def jal     = "b10".U
  def jalr    = "b11".U
  def apply() = UInt(2.W)
}

object ExcType {  //TODO:add exctype
  def notExc = "b000".U
  def apply() = UInt(3.W)
}

class PDEntry extends XSBundle{  // 8 bit
  val isRVC   = Bool()
  val brType  = UInt(2.W)
  val isCall  = Bool()
  val isRet   = Bool()
  val excType = UInt(3.W)
}

class PDPacket extends XSBundle{
  val pc = UInt(VAddrBits.W)
  val inst = UInt(32.W)
  val mask = Bool()
  val pdEntry = new PDEntry
}

class ICacheResp extends XSBundle {
  val fetchPc = UInt(VAddrBits.W)
  val data = UInt((FetchWidth * 32).W)
  val mask = UInt((FetchWidth * 2).W)
}

class PreDecode extends XSModule with HasPdconst{
  val io = IO(new Bundle() {
    val iCacheResp = Input(new ICacheResp)
    val PreDecodeInfo = Output(Vec(halfWidth, new PDEntry))
    val toIF4 = Output(Vec(halfWidth, new PDPacket))
  })

  val gpc = groupPC(io.iCacheResp.fetchPc)
  val data = io.iCacheResp.data
  val mask = io.iCacheResp.mask

  val insts = Wire(Vec(halfWidth, UInt(32.W)))
  val instsMask = Wire(Vec(halfWidth, Bool()))
  val instsRVC = Wire(Vec(halfWidth,Bool()))
  val instsPC = Wire(Vec(halfWidth, UInt(VAddrBits.W)))


  val prevHalf = Reg(UInt(16.W))
  val prevValid = RegInit(false.B)
  val prevGPC = RegInit(0.U(VAddrBits.W))
  val seriesPC = RegInit(true.B)  //two cacheline's gpc is continuous
  val nextHalf = Wire(UInt(16.W))

  for (i <- 0 until halfWidth) {
    val inst = Wire(UInt(32.W))
    val valid = Wire(Bool())
    val pc = gpc + (i << 1).U - Mux(prevValid && (i.U === 0.U), 2.U, 0.U)
    val brType::isCall::isRet::Nil = brInfo(inst)
    if (i==0) {
      inst := Mux(prevValid, Cat(data(15,0), prevHalf), data(31,0))
      valid := true.B
    } else if (i==1) {
      inst := data(47,16)
      valid := prevValid || !(instsMask(0) && !isRVC(insts(0)))
    } else if (i==halfWidth-1) {
      inst := Cat(0.U(16.W), data(i*16+15, i*16))
      valid := !(instsMask(i-1) && !isRVC(insts(i-1)) || !isRVC(inst))
    } else {
      inst := data(i*16+31, i*16)
      valid := !(instsMask(i-1) && !isRVC(insts(i-1)))
    }

    insts(i) := inst
    instsRVC(i) := isRVC(inst)
    instsMask(i) := mask(i) && valid 
    instsPC(i) := pc

    io.PreDecodeInfo(i).isRVC := instsRVC(i)
    io.PreDecodeInfo(i).brType := brType
    io.PreDecodeInfo(i).isCall := isCall
    io.PreDecodeInfo(i).isRet := isRet
    io.PreDecodeInfo(i).excType := ExcType.notExc

    io.toIF4(i).pdEntry := io.PreDecodeInfo(i)
    io.toIF4(i).inst := insts(i)
    io.toIF4(i).mask := instsMask(i)
    io.toIF4(i).pc := instsPC(i)
  }

  //update
  nextHalf := data(halfWidth*16-1, (halfWidth-1)*16)
  prevHalf := nextHalf
  seriesPC := 1.U === (gpc - prevGPC)(VAddrBits-1, groupAlign)
  prevGPC := gpc
  prevValid := !(instsMask(halfWidth-2) && !isRVC(insts(halfWidth-2))) && !isRVC(insts(halfWidth-1)) && seriesPC

//  for (i <- 0 until halfWidth) {
//    XSDebug(true.B,
//      p"instr ${Binary(io.toIF4(i).inst)}, " +
//      p"mask ${Binary(io.toIF4(i).mask)}, " +
//      //p"pc ${Binary(io.toIF4(i).pc)}, " +
//      p"isRVC ${Binary(io.PreDecodeInfo(i).isRVC)}, " +
//      p"brType ${Binary(io.PreDecodeInfo(i).brType)}, " +
//      p"isRet ${Binary(io.PreDecodeInfo(i).isRet)}, " +
//      p"isCall ${Binary(io.PreDecodeInfo(i).isCall)}\n"
//    )
//  }
//
//  for (i <- 0 until halfWidth) {
//    XSDebug(true.B,
//      p"prevhalf ${Binary(prevHalf)}, " +
//      p"prevvalid ${Binary(prevValid)}, " +
//      p"seriesPC ${Binary(seriesPC)}\n"
//    )
//  }
}
