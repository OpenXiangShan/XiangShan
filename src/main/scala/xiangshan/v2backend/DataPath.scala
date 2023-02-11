package xiangshan.v2backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.v2backend.Bundles._
import xiangshan.v2backend.issue._
import xiangshan.{HasXSParameter, XSBundle}

case class DataPathParams (
  schdParams : Map[SchedulerType, SchdBlockParams],
)

class DataPath(params: DataPathParams)(implicit p: Parameters) extends LazyModule {
  private implicit val dpParams: DataPathParams = params
  lazy val module = new DataPathImp(this)
}

class DataPathImp(override val wrapper: DataPath)(implicit p: Parameters, params: DataPathParams)
  extends LazyModuleImp(wrapper) with HasXSParameter {

  val io = IO(new DataPathIO())

  private val intSchdParams = params.schdParams(IntScheduler())
  private val vfSchdParams = params.schdParams(VfScheduler())
  private val memSchdParams = params.schdParams(MemScheduler())
  private val numIntR = intSchdParams.numRfRead + memSchdParams.numTotalIntRfRead
  println(s"number of total read req of int regfile: ${numIntR}")


  private val intRfRaddr = Wire(Vec(numIntR, UInt(intSchdParams.pregIdxWidth.W)))
  private val intRfRdata = Wire(Vec(numIntR, UInt(intSchdParams.rfDataWidth.W)))
  private val intRfWen = Wire(Vec(io.fromIntWb.length, Bool()))
  private val intRfWaddr = Wire(Vec(io.fromIntWb.length, UInt(intSchdParams.pregIdxWidth.W)))
  private val intRfWdata = Wire(Vec(io.fromIntWb.length, UInt(intSchdParams.rfDataWidth.W)))

  IntRegFile("IntRegFile", intSchdParams.numPregs, intRfRaddr, intRfRdata, intRfWen, intRfWaddr, intRfWdata, None, None)

  intRfWaddr := io.fromIntWb.map(_.bits.pdest)
  intRfWdata := io.fromIntWb.map(_.bits.data)
  intRfWen := io.fromIntWb.map(_.valid)

  io.connectWithIntRfRaddrVec(intRfRaddr)
  io.connectWithExuIntRdataVec(intRfRdata)
}

class DataPathIO()(implicit p: Parameters, params: DataPathParams) extends XSBundle {
  // params
  private val intSchdParams = params.schdParams(IntScheduler())
  private val vfSchdParams = params.schdParams(VfScheduler())
  private val memSchdParams = params.schdParams(MemScheduler())
  private val numIntWb = intSchdParams.numTotalIntRfWrite + vfSchdParams.numTotalIntRfWrite + memSchdParams.numTotalIntRfWrite
  // bundles
  val fromIntIQ: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] =
    Flipped(MixedVec(intSchdParams.issueBlockParams.map(_.genIqParams.genIssueBundle)))

  val fromMemIQ: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] =
    Flipped(MixedVec(memSchdParams.issueBlockParams.map(_.genIqParams.genIssueBundle)))

  val fromVfIQ = Flipped(MixedVec(vfSchdParams.issueBlockParams.map(_.genIqParams.genIssueBundle)))

  val toExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] =
    MixedVec(intSchdParams.issueBlockParams.map(_.genIqParams.genExuInputBundle))

  val fromIntWb: Vec[ValidIO[WriteBackBundle]] = Input(Vec(numIntWb, ValidIO(new WriteBackBundle(XLEN))))

  println(this.intSchdParams.issueBlockParams.map(_.exuBlockParams.map(_.getRfReadDataCfgSet)))
  println(this.memSchdParams.issueBlockParams.map(_.exuBlockParams.map(_.getRfReadDataCfgSet)))
  println(this.vfSchdParams.issueBlockParams.map(_.exuBlockParams.map(_.getRfReadDataCfgSet)))

  def connectWithIntRfRaddrVec(rfRaddr: Vec[UInt]): Unit = {
    val addrFromIntIQ = fromIntIQ.flatMap(_.map(_.bits.getIntRfReadBundle)).flatten.map(_.addr)
    val addrFromMemIQ = fromMemIQ.flatMap(_.map(_.bits.getIntRfReadBundle)).flatten.map(_.addr)
    val addrFromIQ = WireInit(VecInit(addrFromIntIQ ++ addrFromMemIQ))
    rfRaddr := addrFromIQ
    println(s"number of read ports of int regfile: ${rfRaddr.length}")
    println(s"number of read request from iq: ${addrFromIQ.length}")
    println(s"number of read request from int iq: ${addrFromIntIQ.length}")
    println(s"number of read request from mem: ${addrFromMemIQ.length}")
  }

  def connectWithExuIntRdataVec(rfRdata: Vec[UInt]): Unit = {
    toExu.flatten.flatMap(_.bits.data).zip(rfRdata).foreach { case (exuSrc, rdata) =>
      exuSrc := rdata
    }
  }
}
