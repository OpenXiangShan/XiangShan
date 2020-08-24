package device

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, RegionType}
import xiangshan.HasXSParameter

class RAMHelper(memByte: BigInt) extends BlackBox with HasXSParameter {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val rIdx = Input(UInt(DataBits.W))
    val rdata = Output(UInt(DataBits.W))
    val wIdx = Input(UInt(DataBits.W))
    val wdata = Input(UInt(DataBits.W))
    val wmask = Input(UInt(DataBits.W))
    val wen = Input(Bool())
  })
}

class AXI4RAM
(
  address: AddressSet,
  memByte: Long,
  useBlackBox: Boolean = false,
  executable: Boolean = true,
  beatBytes: Int = 8,
  burstLen: Int = 16
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable, beatBytes, burstLen)
{

  override lazy val module = new AXI4SlaveModuleImp(this){

    val offsetBits = log2Up(memByte)
    val offsetMask = (1 << offsetBits) - 1


    def index(addr: UInt) = ((addr & offsetMask.U) >> log2Ceil(beatBytes)).asUInt()

    def inRange(idx: UInt) = idx < (memByte / 8).U

    val wIdx = index(waddr) + writeBeatCnt
    val rIdx = index(raddr) + readBeatCnt
    val wen = in.w.fire() && inRange(wIdx)

    val rdata = if (useBlackBox) {
      val mem = Module(new RAMHelper(memByte))
      mem.io.clk := clock
      mem.io.rIdx := rIdx
      mem.io.wIdx := wIdx
      mem.io.wdata := in.w.bits.data
      mem.io.wmask := fullMask
      mem.io.wen := wen
      mem.io.rdata
    } else {
      val mem = Mem(memByte / beatBytes, Vec(beatBytes, UInt(8.W)))

      val wdata = VecInit.tabulate(beatBytes) { i => in.w.bits.data(8 * (i + 1) - 1, 8 * i) }
      when(wen) {
        mem.write(wIdx, wdata, in.w.bits.strb.asBools())
      }

      Cat(mem.read(rIdx).reverse)
    }
    in.r.bits.data := rdata
  }
}
