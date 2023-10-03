package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import utils.MapUtils
import xiangshan.backend.fu.FuType

class FuBusyTableRead(fuLatencyMap: Map[FuType.OHType, Int])(implicit iqParams: IssueBlockParams) extends Module {
  private val numEntries = iqParams.numEntries
  private val latMax = fuLatencyMap.values.fold(0)(_ max _)

  val io = IO(new FuBusyTableReadIO(latMax, numEntries))

  val fuBusyVec = VecInit(io.in.fuBusyTable.asBools)
  val fuTypeVec = io.in.fuTypeRegVec

  /**
    * Map[latency, Set[fuType]]
    */
  private val latMappedFuTypeSet: Map[Int, Set[FuType.OHType]] = MapUtils.groupByValueUnique(fuLatencyMap)

  val readMaskVec = fuBusyVec.zipWithIndex.map { case (busy, lat) =>
    val latencyHitVec = WireInit(0.U(numEntries.W))
    when(busy) {
      latencyHitVec := VecInit(fuTypeVec.map { fuType =>
        val latencyHitFuType = latMappedFuTypeSet.getOrElse(lat, Set()).toSeq
        val isLatencyNum = Cat(latencyHitFuType.map(_.U === fuType)).orR
        isLatencyNum
      }).asUInt
    }

    latencyHitVec
  }

  io.out.fuBusyTableMask := readMaskVec.fold(0.U(iqParams.numEntries.W))(_ | _)
}

class FuBusyTableReadIO(latencyValMax: Int, numEntries: Int) extends Bundle {
  private val tableSize = latencyValMax + 1

  val in = new Bundle {
    val fuBusyTable = Input(UInt(tableSize.W))
    val fuTypeRegVec = Input(Vec(numEntries, FuType()))
  }
  val out = new Bundle {
    val fuBusyTableMask = Output(UInt(numEntries.W))
  }
}