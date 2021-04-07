package device

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4.{AXI4SlaveNode, AXI4EdgeParameters, AXI4MasterNode}
import freechips.rocketchip.diplomacy.{AddressSet, InModuleBody, LazyModule, LazyModuleImp, RegionType}
import top.HaveAXI4MemPort
import xiangshan.HasXSParameter
import utils.MaskExpand

class RAMHelper(memByte: BigInt) extends BlackBox {
  val DataBits = 64
  val io = IO(new Bundle {
    val clk   = Input(Clock())
    val en    = Input(Bool())
    val rIdx  = Input(UInt(DataBits.W))
    val rdata = Output(UInt(DataBits.W))
    val wIdx  = Input(UInt(DataBits.W))
    val wdata = Input(UInt(DataBits.W))
    val wmask = Input(UInt(DataBits.W))
    val wen   = Input(Bool())
  })
}

class AXI4RAM
(
  address: Seq[AddressSet],
  memByte: Long,
  useBlackBox: Boolean = false,
  executable: Boolean = true,
  beatBytes: Int = 8,
  burstLen: Int = 16,
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable, beatBytes, burstLen)
{ 
  override lazy val module = new AXI4SlaveModuleImp(this){

    val split = beatBytes / 8
    val bankByte = memByte / split
    val offsetBits = log2Up(memByte)

    require(address.length >= 1)
    val baseAddress = address(0).base

    def index(addr: UInt) = ((addr - baseAddress.U)(offsetBits - 1, 0) >> log2Ceil(beatBytes)).asUInt()

    def inRange(idx: UInt) = idx < (memByte / beatBytes).U

    val wIdx = index(waddr) + writeBeatCnt
    val rIdx = index(raddr) + readBeatCnt
    val wen = in.w.fire() && inRange(wIdx)
    require(beatBytes >= 8)

    val rdata = if (useBlackBox) {
      val mems = (0 until split).map {_ => Module(new RAMHelper(bankByte))}
      mems.zipWithIndex map { case (mem, i) =>
        mem.io.clk   := clock
        mem.io.en    := !reset.asBool() && ((state === s_rdata) || (state === s_wdata))
        mem.io.rIdx  := (rIdx << log2Up(split)) + i.U
        mem.io.wIdx  := (wIdx << log2Up(split)) + i.U
        mem.io.wdata := in.w.bits.data((i + 1) * 64 - 1, i * 64)
        mem.io.wmask := MaskExpand(in.w.bits.strb((i + 1) * 8 - 1, i * 8))
        mem.io.wen   := wen
      }
      val rdata = mems.map {mem => mem.io.rdata}
      Cat(rdata.reverse)
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

class AXI4RAMWrapper
(snode: AXI4SlaveNode, memByte: Long, useBlackBox: Boolean = false)
(implicit p: Parameters)
  extends LazyModule {

  val mnode = AXI4MasterNode(List(snode.in.head._2.master))
 
  val portParam = snode.portParams.head
  val slaveParam = portParam.slaves.head
  val burstLen = portParam.maxTransfer / portParam.beatBytes
  val ram = LazyModule(new AXI4RAM(
    slaveParam.address, memByte, useBlackBox,
    slaveParam.executable, portParam.beatBytes, burstLen
  ))
  ram.node := mnode
  
  val io_axi4 = InModuleBody{ mnode.makeIOs() }
  def connectToSoC(soc: HaveAXI4MemPort) = {
    io_axi4 <> soc.memory
  }

  lazy val module = new LazyModuleImp(this){}
}
