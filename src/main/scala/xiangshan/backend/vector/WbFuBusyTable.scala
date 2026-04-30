package xiangshan.backend.vector

import chisel3._
import chisel3.util._
import xiangshan.backend.decode.opcode.Latency


class WbFuBusyTable(RfWritePorts: Int, exLatency: Int = 4) extends Module {
  private val tableSize = WbFuBusyTable.tableSize(exLatency)
  val io = IO(new Bundle {
      val in = Input(new WbFuBusyTable.In(RfWritePorts))
      val out = Output(new WbFuBusyTable.Out(RfWritePorts, tableSize))
  })

  val valid = for(i <- 0 until RfWritePorts) yield io.in.fromIssueQueue(i).valid
  val writebackSlot = for(i <- 0 until RfWritePorts) yield WbFuBusyTable.tableIdx(io.in.fromIssueQueue(i).bits, exLatency)
  val nextCycleWritebackSlot = writebackSlot.map(_ - 1.U)

  val fuBusyTable = RegInit(VecInit(Seq.fill(RfWritePorts)(0.U(tableSize.W))))
  val fuBusyTableNext = Wire(Vec(RfWritePorts, UInt(tableSize.W)))
  fuBusyTable := fuBusyTableNext

  val fuBusyTableShift = for(i <- 0 until RfWritePorts) yield (fuBusyTable(i) >> 1.U).asUInt
  // The table is observed starting from the next cycle, so newly issued ops
  // must be encoded using the remaining distance after one cycle elapses.
  val inSetfuBusyTable = for(i <- 0 until RfWritePorts) yield Mux(
    valid(i),
    UIntToOH(nextCycleWritebackSlot(i), tableSize),
    0.U(tableSize.W)
  )

  for(i <- 0 until RfWritePorts) fuBusyTableNext(i) := fuBusyTableShift(i) | inSetfuBusyTable(i)
  io.out.fuBusyTable := fuBusyTable
}

object WbFuBusyTable{
  val DefaultExLatency = 4

  case class Source(
    wbPortIds: Seq[Int],
    sink: Option[WbFuBusyTable.In],
  )

  case class ConnectInfo(
    wbType: String,
    regfileWbPortIds: Seq[Int],
    sources: Seq[Source],
  )

  def wbFuBusyTableSources(
    issueQueues: Seq[VecIssueQueue],
    sinkSel: VecIssueQueue => Option[WbFuBusyTable.In],
    wbPortIdsSel: IssueParam => Seq[Int],
  ): Seq[Source] = {
    issueQueues.map(iq => Source(wbPortIdsSel(iq.param), sinkSel(iq)))
  }

  def tableSize(exLatency: Int = DefaultExLatency): Int = {
    (1 << Latency().getWidth) + exLatency
  }

  // `latency` is FU execution cycles only. Shift it by the IQ->EXU pipeline depth
  // so the table is indexed by the future writeback cycle slot.
  def tableIdx(latency: UInt, exLatency: Int = DefaultExLatency): UInt = {
    latency + exLatency.U
  }

  class In(RfWritePorts: Int) extends Bundle{
    val fromIssueQueue = Vec(RfWritePorts, ValidIO(Latency()))

    def =#>(connectInfo: ConnectInfo): Unit = {
      require(
        connectInfo.regfileWbPortIds.size == RfWritePorts,
        s"WbFuBusyTable ${connectInfo.wbType} expects $RfWritePorts ports, got ${connectInfo.regfileWbPortIds.size}"
      )

      fromIssueQueue.zip(connectInfo.regfileWbPortIds).foreach { case (portIn, regfileWbPortId) =>
        val matches = connectInfo.sources.flatMap { source =>
          source.sink.toSeq.flatMap { sink =>
            source.wbPortIds.zipWithIndex.collect {
              case (iqWbPortId, portIdx) if iqWbPortId == regfileWbPortId => sink.fromIssueQueue(portIdx)
            }
          }
        }

        portIn.valid := false.B
        portIn.bits := 0.U.asTypeOf(portIn.bits)

        if (matches.nonEmpty) {
          val matchValid = matches.map(_.valid)
          val matchBits = matches.map(_.bits)
          portIn.valid := VecInit(matchValid).asUInt.orR
          when(portIn.valid) {
            portIn.bits := Mux1H(matchValid zip matchBits)
          }
          assert(
            PopCount(matchValid) <= 1.U,
            s"VecRegion ${connectInfo.wbType} WB busy table port $regfileWbPortId is driven more than once in one cycle"
          )
        }
      }
    }
  }

  class Out(RfWritePorts: Int, tableSize: Int) extends Bundle{
    val fuBusyTable = Vec(RfWritePorts, UInt(tableSize.W))
  }

  private def wbBusyTableHit(
    busyTables: Option[Vec[UInt]],
    wbPortIds: Seq[Int],
    targetPortId: Option[Int],
    wen: Option[Bool],
    latency: UInt,
  ): Bool = {
    val writebackSlot = WbFuBusyTable.tableIdx(latency)
    (busyTables, targetPortId) match {
      case (Some(tables), Some(portId)) =>
        wbPortIds.indexOf(portId) match {
          case -1 => false.B
          case wbPortIdx => wen.getOrElse(false.B) && tables(wbPortIdx)(writebackSlot)
        }
      case _ => false.B
    }
  }

  def entryWbConflict(
    in: VecIssueQueue.WbFuBusyTableReadBundle,
    param: IssueParam,
    entry: VecIssueQueue.Entry,
    deqIdx: Int,
  ): Bool = {
    val exuParam = param.exuParams(deqIdx)
    Seq(
      in.intWbFuBusyTableRead.map(tables =>
        wbBusyTableHit(Some(tables), param.intWbPortIds, exuParam.getGpWriteCfg.map(_.port), entry.payload.gpWen, entry.payload.latency)
      ),
      in.fpWbFuBusyTableRead.map(tables =>
        wbBusyTableHit(Some(tables), param.fpWbPortIds, exuParam.getFpWriteCfg.map(_.port), entry.payload.fpWen, entry.payload.latency)
      ),
      in.vpWbFuBusyTableRead.map(tables =>
        wbBusyTableHit(Some(tables), param.vpWbPortIds, exuParam.getVpWriteCfg.map(_.port), entry.payload.vpWen, entry.payload.latency)
      ),
      in.v0WbFuBusyTableRead.map(tables =>
        wbBusyTableHit(Some(tables), param.v0WbPortIds, exuParam.getV0WriteCfg.map(_.port), entry.payload.v0Wen, entry.payload.latency)
      ),
      in.vlWbFuBusyTableRead.map(tables =>
        wbBusyTableHit(Some(tables), param.vlWbPortIds, exuParam.getVlWriteCfg.map(_.port), entry.payload.vlWen, entry.payload.latency)
      ),
    ).flatten.reduceOption(_ || _).getOrElse(false.B)
  }
}
