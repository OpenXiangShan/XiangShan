package xiangshan.backend.vector

import chisel3._
import chisel3.util._
import xiangshan.backend.decode.opcode.Latency
import xiangshan.backend.fu.FuType

class WbFuBusyTable(
  numWritePorts: Int,
  nonFixedLatFuPortIndices: Seq[Int],
  exLatency: Int = WbFuBusyTable.DefaultExLatency,
) extends Module {

  private val tableSize = WbFuBusyTable.fixedLatBusyTableEntries(exLatency)
  private val nonFixedLatTableSize = WbFuBusyTable.nonFixedLatBusyTableEntries(exLatency)

  val in = IO(Input(new WbFuBusyTable.In(numWritePorts, exLatency)))
  val out = IO(Output(new WbFuBusyTable.Out(numWritePorts, tableSize)))

  private val issueValid = in.fromIssueQueue.map(_.valid)
  private val issueSlot = in.fromIssueQueue.map(_.bits)

  private val fuBusyTable = RegInit(VecInit(Seq.fill(numWritePorts)(0.U(tableSize.W))))
  private val fuBusyTableNext = Wire(Vec(numWritePorts, UInt(tableSize.W)))
  fuBusyTable := fuBusyTableNext

  private val fuBusyTableShift = fuBusyTable.map(_ >> 1.U)
  // The table is observed starting from the next cycle, so newly issued ops
  // must be encoded using the remaining distance after one cycle elapses.
  private val issueSetBits = for (i <- 0 until numWritePorts) yield Mux(
    issueValid(i),
    UIntToOH(issueSlot(i), tableSize),
    0.U(tableSize.W)
  )

  (fuBusyTableNext lazyZip fuBusyTableShift lazyZip issueSetBits).foreach { case (next, shift, inSet) =>
    next := shift | inSet
  }

  private val nonFixedLatFuBusyTable = nonFixedLatFuPortIndices.map { regfilePortIdx =>
    val table = RegInit(0.U(nonFixedLatTableSize.W))
    table.suggestName(s"nonFixedLatFuBusyTable_port$regfilePortIdx")
    regfilePortIdx -> table
  }.toMap
  private val nonFixedLatFuBusyTableNext = nonFixedLatFuPortIndices.map { regfilePortIdx =>
    val table = Wire(UInt(nonFixedLatTableSize.W))
    table.suggestName(s"nonFixedLatFuBusyTableNext_port$regfilePortIdx")
    regfilePortIdx -> table
  }.toMap
  nonFixedLatFuBusyTable.foreach { case (regfilePortIdx, current) =>
    current := nonFixedLatFuBusyTableNext(regfilePortIdx)
  }

  private val ctrlBlock = nonFixedLatFuPortIndices.map { regfilePortIdx =>
    val block = RegInit(0.U.asTypeOf(new WbFuBusyTable.CtrlBlockEntry()))
    block.suggestName(s"ctrlBlock_port$regfilePortIdx")
    regfilePortIdx -> block
  }.toMap
  private val ctrlBlockNext = nonFixedLatFuPortIndices.map { regfilePortIdx =>
    val block = Wire(new WbFuBusyTable.CtrlBlockEntry())
    block.suggestName(s"ctrlBlockNext_port$regfilePortIdx")
    regfilePortIdx -> block
  }.toMap
  ctrlBlock.foreach { case (regfilePortIdx, current) =>
    current := ctrlBlockNext(regfilePortIdx)
  }

  nonFixedLatFuPortIndices.foreach { regfilePortIdx =>
    val busyTableShift = nonFixedLatFuBusyTable(regfilePortIdx) >> 1.U
    val inSet = Mux(
      in.fromNonFixedLatFu(regfilePortIdx).valid,
      UIntToOH(in.fromNonFixedLatFu(regfilePortIdx).bits, nonFixedLatTableSize),
      0.U(nonFixedLatTableSize.W)
    )
    nonFixedLatFuBusyTableNext(regfilePortIdx) := busyTableShift | inSet

    val nextNormalFuBlock = Mux(
      in.fromIssueQueueNonFixedLatFu(regfilePortIdx),
      true.B,
      Mux(in.fromNonFixedLatFu(regfilePortIdx).valid, false.B, ctrlBlock(regfilePortIdx).normalFuBlock)
    )
    ctrlBlockNext(regfilePortIdx).normalFuBlock := nextNormalFuBlock
    ctrlBlockNext(regfilePortIdx).nonFixedLatFuBlock :=
      nextNormalFuBlock || nonFixedLatFuBusyTableNext(regfilePortIdx)(nonFixedLatTableSize - 1, exLatency).orR
  }

  for (i <- 0 until numWritePorts) {
    val nonFixedLatFuBusyBits = nonFixedLatFuBusyTable.get(i) match {
      case Some(table) => table(tableSize - 1, 0)
      case None => 0.U(tableSize.W)
    }
    val portCtrlBlock = ctrlBlock.get(i) match {
      case Some(block) => block
      case None => 0.U.asTypeOf(new WbFuBusyTable.CtrlBlockEntry)
    }
    out.fuBusyTable(i) := fuBusyTable(i) | nonFixedLatFuBusyBits
    out.ctrlBlock(i) := portCtrlBlock
  }
}

object WbFuBusyTable {
  val DefaultExLatency = 4
  val NonFixedLatencyWidth = 5

  sealed trait Source

  case class ConnectInfo(
    wbType: String,
    regfileWbPortIds: Seq[Int],
    sources: Seq[Source],
  )

  object Source {
    final case class IssueQueue(wbPortIds: Seq[Int], sink: WbFuBusyTable.In) extends Source
    final case class NonFixedLatFu(wbPortId: Int, valid: Bool, latency: UInt) extends Source
  }

  private final case class PortMatch(
    issueValid: Bool,
    issueSlot: UInt,
    issuedNonFixedLatFu: Bool,
    nonFixedLatFuValid: Bool,
    nonFixedLatFuLatency: UInt,
  )

  def issueQueueSources(
    issueQueues: Seq[VecIssueQueue],
    sinkSel: VecIssueQueue => Option[WbFuBusyTable.In],
    wbPortIdsSel: IssueParam => Seq[Int],
  ): Seq[Source] = {
    issueQueues.flatMap(iq => sinkSel(iq).map(Source.IssueQueue(wbPortIdsSel(iq.param), _)))
  }

  private def busyTableSize(latencyWidth: Int, exLatency: Int): Int =
    (1 << latencyWidth) + exLatency

  def fixedLatBusyTableEntries(exLatency: Int = DefaultExLatency): Int = {
    require(exLatency >= 0, s"exLatency must be non-negative, got $exLatency")
    busyTableSize(Latency().getWidth, exLatency)
  }

  def nonFixedLatBusyTableEntries(exLatency: Int = DefaultExLatency): Int = {
    require(exLatency >= 0, s"exLatency must be non-negative, got $exLatency")
    busyTableSize(NonFixedLatencyWidth, exLatency)
  }

  def writebackSlotWidth(exLatency: Int = DefaultExLatency): Int =
    log2Up(fixedLatBusyTableEntries(exLatency))

  def writebackSlot(latency: UInt, latencyOffset: Int, exLatency: Int = DefaultExLatency): UInt = {
    require(latencyOffset >= 0, s"latencyOffset must be non-negative, got $latencyOffset")
    val width = writebackSlotWidth(exLatency)
    val slot = latency.pad(width) + latencyOffset.U(width.W)
    slot(width - 1, 0)
  }

  class CtrlBlockEntry extends Bundle {
    val normalFuBlock = Bool()
    val nonFixedLatFuBlock = Bool()
  }

  class In(numWritePorts: Int, exLatency: Int = DefaultExLatency) extends Bundle {
    val fromIssueQueue = Vec(numWritePorts, ValidIO(UInt(writebackSlotWidth(exLatency).W)))
    val fromIssueQueueNonFixedLatFu = Vec(numWritePorts, Bool())
    val fromNonFixedLatFu = Vec(numWritePorts, ValidIO(UInt(NonFixedLatencyWidth.W)))

    def =#>(connectInfo: ConnectInfo): Unit = {
      require(
        connectInfo.regfileWbPortIds.size == numWritePorts,
        s"WbFuBusyTable ${connectInfo.wbType} expects $numWritePorts ports, got ${connectInfo.regfileWbPortIds.size}"
      )

      (fromIssueQueue lazyZip fromIssueQueueNonFixedLatFu lazyZip fromNonFixedLatFu lazyZip connectInfo.regfileWbPortIds).foreach {
        case (issuePortIn, issuedNonFixedLatFuPortIn, nonFixedLatFuPortIn, regfileWbPortId) =>
          val matches = connectInfo.sources.flatMap { source =>
            source match {
              case Source.IssueQueue(wbPortIds, sink) =>
                wbPortIds.zipWithIndex.collect {
                  case (iqWbPortId, portIdx) if iqWbPortId == regfileWbPortId =>
                    PortMatch(
                      sink.fromIssueQueue(portIdx).valid,
                      sink.fromIssueQueue(portIdx).bits,
                      sink.fromIssueQueueNonFixedLatFu(portIdx),
                      sink.fromNonFixedLatFu(portIdx).valid,
                      sink.fromNonFixedLatFu(portIdx).bits,
                    )
                }
              case Source.NonFixedLatFu(wbPortId, valid, latency) if wbPortId == regfileWbPortId =>
                Seq(PortMatch(false.B, 0.U.asTypeOf(issuePortIn.bits), false.B, valid, latency))
              case _ =>
                Seq.empty
            }
          }

          issuePortIn.valid := false.B
          issuePortIn.bits := 0.U.asTypeOf(issuePortIn.bits)
          issuedNonFixedLatFuPortIn := false.B
          nonFixedLatFuPortIn.valid := false.B
          nonFixedLatFuPortIn.bits := 0.U.asTypeOf(nonFixedLatFuPortIn.bits)

          if (matches.nonEmpty) {
            val matchValid = matches.map(_.issueValid)
            val matchBits = matches.map(_.issueSlot)
            val issueNonFixedLatFuValid = matches.map(_.issuedNonFixedLatFu)
            val nonFixedLatFuValid = matches.map(_.nonFixedLatFuValid)
            val nonFixedLatFuBits = matches.map(_.nonFixedLatFuLatency)
            issuePortIn.valid := VecInit(matchValid).asUInt.orR
            when(issuePortIn.valid) {
              issuePortIn.bits := Mux1H(matchValid zip matchBits)
            }
            issuedNonFixedLatFuPortIn := VecInit(issueNonFixedLatFuValid).asUInt.orR
            nonFixedLatFuPortIn.valid := VecInit(nonFixedLatFuValid).asUInt.orR
            when(nonFixedLatFuPortIn.valid) {
              nonFixedLatFuPortIn.bits := Mux1H(nonFixedLatFuValid zip nonFixedLatFuBits)
            }
            assert(
              PopCount(matchValid) <= 1.U,
              s"VecRegion ${connectInfo.wbType} WB busy table port $regfileWbPortId is driven more than once in one cycle"
            )
            assert(
              PopCount(issueNonFixedLatFuValid) <= 1.U,
              s"VecRegion ${connectInfo.wbType} non-fixed-latency WB busy table issue port $regfileWbPortId is driven more than once in one cycle"
            )
            assert(
              PopCount(nonFixedLatFuValid) <= 1.U,
              s"VecRegion ${connectInfo.wbType} non-fixed-latency WB busy table FU port $regfileWbPortId is driven more than once in one cycle"
            )
          }
      }
    }
  }

  class Out(numWritePorts: Int, busyTableSize: Int) extends Bundle {
    val fuBusyTable = Vec(numWritePorts, UInt(busyTableSize.W))
    val ctrlBlock = Vec(numWritePorts, new CtrlBlockEntry)
  }

  private def wbBusyTableHit(
    busyTables: Vec[UInt],
    ctrlBlocks: Option[Vec[CtrlBlockEntry]],
    wbPortIds: Seq[Int],
    targetPortId: Option[Int],
    wen: Option[Bool],
    writebackSlot: UInt,
    fuType: UInt,
  ): Bool = {
    val isNonFixedLatFu = FuType.FuTypeOrR(fuType, FuType.vidiv)
    targetPortId match {
      case Some(portId) =>
        val wbPortIdx = wbPortIds.indexOf(portId)
        if (wbPortIdx < 0) {
          false.B
        } else {
          val tableHit = busyTables(wbPortIdx)(writebackSlot)
          val ctrlBlock = ctrlBlocks.map(_(wbPortIdx)).getOrElse(0.U.asTypeOf(new CtrlBlockEntry))
          wen.getOrElse(false.B) && Mux(
            isNonFixedLatFu,
            ctrlBlock.nonFixedLatFuBlock,
            ctrlBlock.normalFuBlock || tableHit
          )
        }
      case _ => false.B
    }
  }

  def entryWbConflict(
    in: VecIssueQueue.WbFuBusyTableReadBundle,
    param: IssueParam,
    entry: VecIssueQueue.Entry,
    deqIdx: Int,
    writebackSlot: UInt,
  ): Bool = {
    val exuParam = param.exuParams(deqIdx)
    def hit(
      busyTables: Vec[UInt],
      ctrlBlocks: Option[Vec[CtrlBlockEntry]],
      wbPortIds: Seq[Int],
      targetPortId: Option[Int],
      wen: Option[Bool],
    ): Bool = wbBusyTableHit(
      busyTables,
      ctrlBlocks,
      wbPortIds,
      targetPortId,
      wen,
      writebackSlot,
      entry.payload.fuType,
    )

    Seq(
      in.intWbFuBusyTableRead.map(tables =>
        hit(tables, in.intCtrlBlockRead, param.intWbPortIds, exuParam.getGpWriteCfg.map(_.port), entry.payload.gpWen)
      ),
      in.fpWbFuBusyTableRead.map(tables =>
        hit(tables, in.fpCtrlBlockRead, param.fpWbPortIds, exuParam.getFpWriteCfg.map(_.port), entry.payload.fpWen)
      ),
      in.vpWbFuBusyTableRead.map(tables =>
        hit(tables, in.vpCtrlBlockRead, param.vpWbPortIds, exuParam.getVpWriteCfg.map(_.port), entry.payload.vpWen)
      ),
      in.v0WbFuBusyTableRead.map(tables =>
        hit(tables, in.v0CtrlBlockRead, param.v0WbPortIds, exuParam.getV0WriteCfg.map(_.port), entry.payload.v0Wen)
      ),
      in.vlWbFuBusyTableRead.map(tables =>
        hit(tables, in.vlCtrlBlockRead, param.vlWbPortIds, exuParam.getVlWriteCfg.map(_.port), entry.payload.vlWen)
      ),
    ).flatten.reduceOption(_ || _).getOrElse(false.B)
  }
}
