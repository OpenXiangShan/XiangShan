package top

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import coupledL2.tl2chi.PortIO
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.{Field, Parameters}
import system.HasSoCParameter
import utility.ResetGen
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
  val PeriIdBits = 2
  val IMSICIdBits = 11
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

/**
  * ExtLLC exposes IMSIC as one 128-bit HN-I AXI master. XiangShan keeps one
  * 32-bit IMSIC AXI slave per hart, so retain ExtLLC's dedicated HN-I path and
  * split requests by the architectural IMSIC address ranges here.
  */
class ExternalLLCIMSICXbar()(implicit override val p: Parameters) extends LazyModule with HasSoCParameter {
  private val inputDataBytes = 16
  private val imsicParams = soc.IMSICParams
  private val sgStrideWidth = imsicParams.intFileMemWidth + log2Ceil(1 + imsicParams.geilen)

  val masterNode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "extllc-imsic",
      id = IdRange(0, 1 << ExternalLLCAxiParams.IMSICIdBits)
    ))
  )))

  private val imsicRangesByHart = Seq.tabulate(NumCores) { hart =>
    Seq(
      AddressSet(
        imsicParams.mAddr + (hart.toLong << imsicParams.intFileMemWidth),
        (1L << imsicParams.intFileMemWidth) - 1
      ),
      AddressSet(
        imsicParams.sgAddr + (hart.toLong << sgStrideWidth),
        (1L << sgStrideWidth) - 1
      )
    )
  }

  val imsicSlaveNodes = imsicRangesByHart.map { ranges =>
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      Seq(AXI4SlaveParameters(
        address = ranges,
        regionType = RegionType.UNCACHED,
        supportsWrite = TransferSizes(1, inputDataBytes),
        supportsRead = TransferSizes(1, inputDataBytes)
      )),
      beatBytes = 4
    )))
  }

  private val errorDevice = LazyModule(new TLError(
    params = DevNullParams(
      address = Seq(AddressSet(0x0L, 0x7fffffffL)).flatMap { range =>
        imsicRangesByHart.flatten.foldLeft(Seq(range)) { case (remaining, imsicRange) =>
          remaining.flatMap(_.subtract(imsicRange))
        }
      },
      maxAtomic = 1,
      maxTransfer = inputDataBytes
    ),
    beatBytes = 4
  ))

  private val xbar = TLXbar()
  xbar :=
    TLFIFOFixer() :=
    TLWidthWidget(inputDataBytes) :=
    AXI4ToTL() :=
    AXI4UserYanker(Some(1)) :=
    AXI4Fragmenter() :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4IdIndexer(1) :=
    masterNode

  imsicSlaveNodes.foreach { node =>
    node :=
      AXI4Buffer() :=
      AXI4Buffer() :=
      AXI4Buffer() :=
      AXI4IdIndexer(idBits = 2) :=
      AXI4UserYanker() :=
      AXI4Deinterleaver(64) :=
      TLToAXI4() :=
      TLSourceShrinker(64) :=
      TLBuffer.chainNode(2) :=
      xbar
  }
  errorDevice.node := xbar

  lazy val module = new ExternalLLCIMSICXbarImp(this)
}

class ExternalLLCIMSICXbarImp(wrapper: ExternalLLCIMSICXbar)(implicit override val p: Parameters)
  extends LazyRawModuleImp(wrapper) {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(AsyncReset())
    val s_axi = Flipped(new VerilogAXI4Record(wrapper.masterNode.out.head._2.bundle))
    val m_axi = Vec(wrapper.NumCores, new VerilogAXI4Record(wrapper.imsicSlaveNodes.head.in.head._2.bundle))
  })

  val resetSync = withClockAndReset(io.clock, io.reset) { ResetGen() }
  // Assert reset directly so the AXI buffers are initialized before the first
  // clock edge; ResetGen still synchronizes the release.
  val adapterReset = (io.reset.asBool || resetSync.asBool).asAsyncReset
  childClock := io.clock
  childReset := adapterReset

  io.s_axi.viewAs[AXI4Bundle] <> wrapper.masterNode.out.head._1

  io.m_axi.zip(wrapper.imsicSlaveNodes).foreach { case (axi, node) =>
    axi.viewAs[AXI4Bundle] <> node.in.head._1
  }
}

class ExternalLLC()(implicit override val p: Parameters) extends LazyModule with HasSoCParameter {
  val ddrcAXI4Node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "extllc-ddrc",
      id = IdRange(0, 1 << ExternalLLCAxiParams.DdrcIdBits),
      aligned = true,
      maxFlight = Some(1)
    ))
  )))
  val periAXI4Node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "extllc-peri",
      id = IdRange(0, 1 << ExternalLLCAxiParams.PeriIdBits),
      aligned = true,
      maxFlight = Some(1)
    ))
  )))
  val axi4node = AXI4IdentityNode()
  val imsicXbar = LazyModule(new ExternalLLCIMSICXbar())

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
    val rnNodeId = Output(Vec(NumCores, UInt(11.W)))
    val imsic = Vec(NumCores, new VerilogAXI4Record(wrapper.imsicXbar.imsicSlaveNodes.head.in.head._2.bundle))
  })

  val (axi, edge) = wrapper.ddrcAXI4Node.out.head
  val (peri, periEdge) = wrapper.periAXI4Node.out.head
  val imsicXbar = wrapper.imsicXbar.module
  val wrapperBlackBox = Module(new ExternalLLCWrapper(
    edge.bundle,
    periEdge.bundle,
    wrapper.imsicXbar.masterNode.out.head._2.bundle
  ))

  wrapperBlackBox.io.clock := io.clock
  wrapperBlackBox.io.reset := io.reset
  wrapperBlackBox.io.rn <> io.rn
  io.rnNodeId := wrapperBlackBox.io.rnNodeId
  axi <> wrapperBlackBox.io.ddrc.viewAs[AXI4Bundle]
  peri <> wrapperBlackBox.io.peri.viewAs[AXI4Bundle]

  imsicXbar.io.clock := io.clock
  imsicXbar.io.reset := io.reset.asAsyncReset
  wrapperBlackBox.io.imsic <> imsicXbar.io.s_axi
  io.imsic.zip(imsicXbar.io.m_axi).foreach { case (out, in) => out <> in }
}

class ExternalLLCWrapper(
  ddrcParams: AXI4BundleParameters,
  periParams: AXI4BundleParameters,
  imsicParams: AXI4BundleParameters
)(implicit val p: Parameters) extends BlackBox
  with HasSoCParameter {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())

    val rn = Vec(NumCores, Flipped(new PortIO))
    val rnNodeId = Output(Vec(NumCores, UInt(11.W)))
    val ddrc = new VerilogAXI4Record(ddrcParams)
    val peri = new VerilogAXI4Record(periParams)
    val imsic = new VerilogAXI4Record(imsicParams)
  })
}
