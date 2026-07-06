package top

import chisel3._
import chisel3.experimental.dataview._
import coupledL2.tl2chi.PortIO
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, LazyModule, LazyModuleImp, ValName}
import org.chipsalliance.cde.config.{Field, Parameters}
import system.HasSoCParameter
import utils.VerilogAXI4Record

case object UseExternalLLCKey extends Field[Boolean](false)

object ExternalLLCAddressMap {
  val Control = AddressSet(0x20000000L, 0x0fffffffL)
  val BootSram = AddressSet(0x37f00000L, 0xfffffL)
  val BootSramBytes = 0x100000L
}

object ExternalLLCAxiParams {
  val VisibleIdBits = 6
  val DdrcIdBits = 14
}

// Match OpenNCB's forced-INCR memory AXI behavior expected by XiangShan AXI4 slaves.
class AXI4ForceIncr()(implicit p: Parameters, valName: ValName) extends LazyModule {
  val node = AXI4AdapterNode()

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out).foreach { case ((in, _), (out, _)) =>
      def alignToTransfer(addr: UInt, bytes1: UInt): UInt = {
        val mask = Wire(UInt(addr.getWidth.W))
        mask := bytes1
        addr & ~mask
      }

      out.aw.valid := in.aw.valid
      in.aw.ready := out.aw.ready
      out.aw.bits := in.aw.bits
      out.aw.bits.addr := alignToTransfer(in.aw.bits.addr, in.aw.bits.bytes1())
      out.aw.bits.burst := AXI4Parameters.BURST_INCR

      out.w :<>= in.w
      in.b :<>= out.b

      out.ar.valid := in.ar.valid
      in.ar.ready := out.ar.ready
      out.ar.bits := in.ar.bits
      out.ar.bits.addr := alignToTransfer(in.ar.bits.addr, in.ar.bits.bytes1())
      out.ar.bits.burst := AXI4Parameters.BURST_INCR

      in.r :<>= out.r
    }
  }
}

object AXI4ForceIncr {
  def apply()(implicit p: Parameters, valName: ValName): AXI4Node = LazyModule(new AXI4ForceIncr()).node
}

class ExternalLLC()(implicit override val p: Parameters) extends LazyModule with HasSoCParameter {
  val ddrcAXI4Node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "external-llc",
      id = IdRange(0, 1 << ExternalLLCAxiParams.DdrcIdBits),
      aligned = true,
      maxFlight = Some(1)
    ))
  )))
  val axi4node = AXI4IdentityNode()

  axi4node :=
    AXI4UserYanker(Some(1)) :=
    AXI4IdIndexer(idBits = ExternalLLCAxiParams.VisibleIdBits) :=
    AXI4ForceIncr() :=
    ddrcAXI4Node

  lazy val module = new ExternalLLCImp(this)
}

class ExternalLLCImp(wrapper: ExternalLLC)(implicit override val p: Parameters) extends LazyModuleImp(wrapper)
  with HasSoCParameter {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val rn = Vec(NumCores, Flipped(new PortIO))
  })

  val (axi, edge) = wrapper.ddrcAXI4Node.out.head
  val wrapperBlackBox = Module(new ExternalLLCWrapper(edge.bundle))

  wrapperBlackBox.io.clock := io.clock
  wrapperBlackBox.io.reset := io.reset
  wrapperBlackBox.io.rn <> io.rn
  axi <> wrapperBlackBox.io.ddrc.viewAs[AXI4Bundle]
}

class ExternalLLCWrapper(ddrcParams: AXI4BundleParameters)(implicit val p: Parameters) extends BlackBox
  with HasSoCParameter {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())

    val rn = Vec(NumCores, Flipped(new PortIO))
    val ddrc = new VerilogAXI4Record(ddrcParams)
  })
}
