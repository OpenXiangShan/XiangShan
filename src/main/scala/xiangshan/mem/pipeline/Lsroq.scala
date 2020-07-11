package xiangshan.mem.pipeline

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap
import xiangshan.mem._
import xiangshan.mem.cache._
import bus.simplebus._

class LsRoqEntry extends XSBundle {
  val paddr = UInt(PAddrBits.W)
  val pc = UInt(VAddrBits.W) //for debug
  val op = UInt(6.W)
  val wmask = UInt(8.W)
  val data = UInt(XLEN.W)
  val exception = UInt(8.W)
  val miss = Bool()
}

// Load/Store Roq for XiangShan Out of Order LSU
class LsRoq(implicit val p: XSConfig) extends XSModule with HasMEMConst with NeedImpl{
  val io = IO(new Bundle() {
    val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val roqIdxs = Input(Vec(RenameWidth, UInt(RoqIdxWidth.W)))
    val brqRedirect = Input(Valid(new Redirect))
    val loadIn = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new LsPipelineBundle)))
    val storeIn = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new LsPipelineBundle)))
    val out = Vec(2, DecoupledIO(new ExuOutput)) // writeback store
    val commits = Vec(CommitWidth, Valid(new RoqCommit))
    val scommit = Input(UInt(3.W))
    val forward = new LoadForwardQueryIO
    // val rollback = TODO
    val miss = new SimpleBusUC(addrBits = VAddrBits, userBits = (new DcacheUserBundle).getWidth)
  })

  val data = Mem(LSRoqSize, new LsRoqEntry)
  val valid = RegInit(VecInit(List.fill(RoqSize)(false.B)))
  val writebacked = Reg(Vec(RoqSize, Bool()))
  val redirect = Reg(Vec(RoqSize, new Redirect))
  
  val ringBufferHeadExtended = RegInit(0.U(RoqIdxWidth.W))
  val ringBufferTailExtended = RegInit(0.U(RoqIdxWidth.W))
  val ringBufferHead = ringBufferHeadExtended(InnerRoqIdxWidth-1,0)
  val ringBufferTail = ringBufferTailExtended(InnerRoqIdxWidth-1,0)
  val ringBufferEmpty = ringBufferHead === ringBufferTail && ringBufferHeadExtended(InnerRoqIdxWidth)===ringBufferTailExtended(InnerRoqIdxWidth)
  val ringBufferFull = ringBufferHead === ringBufferTail && ringBufferHeadExtended(InnerRoqIdxWidth)=/=ringBufferTailExtended(InnerRoqIdxWidth)
  val ringBufferAllowin = !ringBufferFull 

  // enqueue
  
  // misprediction recovery

  // writeback store

  // commit store

  // cache miss request

  // load forward query

  // store backward query and rollback

}
