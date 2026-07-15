package xiangshan.cache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._

class L1AccessStatEntry extends Bundle {
  val valid = Bool()
  val clear = Bool()
  val count = UInt(2.W)
  val prefetch = Bool()
}

class L1AccessStatAccess(implicit p: Parameters) extends DCacheBundle {
  val set = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
}

class L1AccessStatRefill(implicit p: Parameters) extends DCacheBundle {
  val set = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
  val prefetch = Bool()
}

class L1AccessStatClear(implicit p: Parameters) extends DCacheBundle {
  val set = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
}

class L1AccessStatArray(accessPorts: Int)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val access = Input(Vec(accessPorts, ValidIO(new L1AccessStatAccess)))
    val refill = Input(ValidIO(new L1AccessStatRefill))
    val clear = Input(ValidIO(new L1AccessStatClear))
    val perfClean = Input(Bool())
  })

  private val entries = RegInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nWays)(0.U.asTypeOf(new L1AccessStatEntry))))))
  private val newEntries = WireInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nWays)(0.U.asTypeOf(new L1AccessStatEntry))))))

  private def add(count: UInt, inc: UInt): UInt = {
    val sum = count +& inc
    Mux(sum >= 3.U, 3.U, sum(1, 0))
  }
  private def minus(value: UInt): UInt = (-(value.asSInt)).asUInt

  for (i <- 0 until nSets) {
    for (j <- 0 until nWays) {
      when (io.clear.valid && io.clear.bits.set === i.U && io.clear.bits.way_en(j)) {
        newEntries(i)(j).valid := false.B
        newEntries(i)(j).count := 0.U
        newEntries(i)(j).prefetch := false.B
        newEntries(i)(j).clear := false.B
        entries(i)(j) := newEntries(i)(j)
      } .elsewhen (io.refill.valid && io.refill.bits.set === i.U && io.refill.bits.way_en(j)) {
        newEntries(i)(j).valid := true.B
        newEntries(i)(j).count := 0.U
        newEntries(i)(j).prefetch := io.refill.bits.prefetch
        newEntries(i)(j).clear := false.B
        entries(i)(j) := newEntries(i)(j)
      } .elsewhen (io.access.map(a => a.valid && a.bits.set === i.U && a.bits.way_en(j)).reduce(_ || _)) {
        val accessCnt = io.access.map(a => Mux(a.valid && a.bits.set === i.U && a.bits.way_en(j), 1.U, 0.U)).reduce(_ +& _)
        newEntries(i)(j).valid := true.B
        newEntries(i)(j).count := add(entries(i)(j).count, accessCnt)
        newEntries(i)(j).prefetch := entries(i)(j).prefetch
        newEntries(i)(j).clear := false.B
        entries(i)(j) := newEntries(i)(j)
      }
    }
  }

  when (io.perfClean) {
    for (i <- 0 until nSets) {
      for (j <- 0 until nWays) {
        entries(i)(j).count := 0.U
        entries(i)(j).clear := true.B
      }
    }
  }

  // 不要修改上面的内容
  // 无 clear 时：
  // refill 时：_0项 +1，
  // hit 时：_n项-1，_n+1项+1
  // probe 时：无
  // 有 clear 时：
  // refill 时：_0项 +1，
  // hit 时：_n+1项+1
  // probe 时：无
  // 仅在有这些事件时进行统计
  private def sameLine(a: L1AccessStatAccess, b: L1AccessStatAccess): Bool = {
    a.set === b.set && a.way_en === b.way_en
  }

  val accessSeqOld = io.access.map(a => Mux1H(a.bits.way_en, entries(a.bits.set)))
  val accessSeqNew = io.access.map(a => Mux1H(a.bits.way_en, newEntries(a.bits.set)))
  val accessValid = io.access.map(a => a.valid && !io.perfClean)
  val accessFirst = io.access.indices.map { i =>
    if (i == 0) true.B else !io.access.indices.take(i).map { j =>
      accessValid(j) && sameLine(io.access(i).bits, io.access(j).bits)
    }.reduce(_ || _)
  }
  val accessBlocked = io.access.indices.map { i =>
    io.clear.valid && io.clear.bits.set === io.access(i).bits.set &&
      io.clear.bits.way_en === io.access(i).bits.way_en ||
      io.refill.valid && io.refill.bits.set === io.access(i).bits.set &&
        io.refill.bits.way_en === io.access(i).bits.way_en
  }
  val accessEvent = io.access.indices.map(i => accessValid(i) && accessFirst(i) && !accessBlocked(i))
  val accessFirstAfterClean = io.access.indices.map(i => !accessSeqOld(i).valid || accessSeqOld(i).clear)
  val accessChanged = io.access.indices.map(i => accessSeqNew(i).count =/= accessSeqOld(i).count)
  val accessAdd = io.access.indices.map(i => accessEvent(i) && (accessFirstAfterClean(i) || accessChanged(i)))
  val accessRemove = io.access.indices.map(i => accessEvent(i) && !accessFirstAfterClean(i) && accessChanged(i))

  val refillBlocked = io.clear.valid && io.clear.bits.set === io.refill.bits.set &&
    io.clear.bits.way_en === io.refill.bits.way_en
  val refillCounterValid = io.refill.valid && !io.perfClean && !refillBlocked

  for (bucket <- 0 until 4) {
    val oldDemand = PopCount(io.access.indices.map(i =>
      accessRemove(i) && !accessSeqOld(i).prefetch && accessSeqOld(i).count === bucket.U
    ))
    val oldPrefetch = PopCount(io.access.indices.map(i =>
      accessRemove(i) && accessSeqOld(i).prefetch && accessSeqOld(i).count === bucket.U
    ))
    val newDemand = PopCount(io.access.indices.map(i =>
      accessAdd(i) && !accessSeqNew(i).prefetch && accessSeqNew(i).count === bucket.U
    ))
    val newPrefetch = PopCount(io.access.indices.map(i =>
      accessAdd(i) && accessSeqNew(i).prefetch && accessSeqNew(i).count === bucket.U
    ))
    val refillDemand = if (bucket == 0) refillCounterValid && !io.refill.bits.prefetch else false.B
    val refillPrefetch = if (bucket == 0) refillCounterValid && io.refill.bits.prefetch else false.B

    XSPerfAccumulate(
      s"access_demand_$bucket",
      newDemand.pad(64) + Mux(refillDemand, 1.U(64.W), 0.U(64.W)) + minus(oldDemand.pad(64))
    )
    XSPerfAccumulate(
      s"access_prefetch_$bucket",
      newPrefetch.pad(64) + Mux(refillPrefetch, 1.U(64.W), 0.U(64.W)) + minus(oldPrefetch.pad(64))
    )
  }
}
