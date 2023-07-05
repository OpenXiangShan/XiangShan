/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package top

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AdapterNode, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import chisel3._
import chisel3.util._
import utils.{XSPerfAccumulate, XSPerfPrint}
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import utility.{ReqSourceField, ReqSourceKey, GTimer}
import xiangshan.MemReqSource

class BusPerfMonitor(name: String, stat_latency: Boolean, add_reqkey: Boolean)(implicit p: Parameters) extends LazyModule {
  val node = if (add_reqkey) TLAdapterNode(managerFn = { m =>
    TLSlavePortParameters.v1(
      m.managers.map { m =>
        m.v2copy()
      },
      requestKeys = Seq(ReqSourceKey),
      beatBytes = 32,
      endSinkId = m.endSinkId
    )
  }) else {
    TLAdapterNode()
  }
  lazy val module = new BusPerfMonitorImp(this, name, stat_latency)
}

class BusPerfMonitorImp(outer: BusPerfMonitor, name: String, stat_latency: Boolean)
  extends LazyModuleImp(outer)
{

  outer.node.in.zip(outer.node.out).foreach{
    case ((in, edgeIn), (out, edgeOut)) =>
      out <> in
  }

  def PERF_CHN[T <: TLChannel](clientName: String, chn: DecoupledIO[T]) = {

    val channelName = chn.bits.channelName.replaceAll(" ", "_").replaceAll("'", "")
    XSPerfAccumulate(s"${clientName}_${channelName}_fire", chn.fire)
    XSPerfAccumulate(s"${clientName}_${channelName}_stall", chn.valid && !chn.ready)

    val ops = chn.bits match {
      case _: TLBundleA => TLMessages.a.map(_._1)
      case _: TLBundleB => TLMessages.b.map(_._1)
      case _: TLBundleC => TLMessages.c.map(_._1)
      case _: TLBundleD => TLMessages.d.map(_._1)
      case _: TLBundleE => Nil
    }

    for((op_raw, i) <- ops.zipWithIndex){
      val op = s"${op_raw}".replaceAll(" ", "_")
      chn.bits match {
        case a: TLBundleA =>
          XSPerfAccumulate(s"${clientName}_${channelName}_${op}_fire",
            i.U === a.opcode && chn.fire
          )
          XSPerfAccumulate(s"${clientName}_${channelName}_${op}_stall",
            i.U === a.opcode && chn.valid && !chn.ready
          )
        case b: TLBundleB =>
          XSPerfAccumulate(s"${clientName}_${channelName}_${op}_fire",
            i.U === b.opcode && chn.fire
          )
          XSPerfAccumulate(s"${clientName}_${channelName}_${op}_stall",
            i.U === b.opcode && chn.valid && !chn.ready
          )
        case c: TLBundleC =>
          XSPerfAccumulate(s"${clientName}_${channelName}_${op}_fire",
            i.U === c.opcode && chn.fire
          )
          XSPerfAccumulate(s"${clientName}_${channelName}_${op}_stall",
            i.U === c.opcode && chn.valid && !chn.ready
          )
        case d: TLBundleD =>
          XSPerfAccumulate(s"${clientName}_${channelName}_${op}_fire",
            i.U === d.opcode && chn.fire
          )
          XSPerfAccumulate(s"${clientName}_${channelName}_${op}_stall",
            i.U === d.opcode && chn.valid && !chn.ready
          )
      }
    }
  }

  for (((in, edgeIn), i) <- outer.node.in.zipWithIndex) {
    val clientName = s"${name}_${edgeIn.master.masters.head.name}_bank_$i"
    PERF_CHN(clientName, in.a)
    PERF_CHN(clientName, in.d)
    if (in.params.hasBCE) {
      PERF_CHN(clientName, in.b)
      PERF_CHN(clientName, in.c)
      PERF_CHN(clientName, in.e)
    }
  }

  if (stat_latency) {
    val nrEdge = outer.node.in.length.toInt
    val edgeIn = outer.node.in.head._2

    class RecordEntry()(implicit p: Parameters) extends Bundle {
      val valid = Bool()
      val timeStamp = UInt(64.W)
      val reqType = UInt(8.W)
    }

    // For simplicity, latency statistic works between nodes with SINGLE edge
    require(nrEdge == 1)
    val timer = GTimer()
    val nrSource = math.pow(2, edgeIn.bundle.sourceBits).toInt
    val latencyRecord = RegInit(VecInit(Seq.fill(nrSource)(0.U.asTypeOf(new RecordEntry()))))
    val latencySum = RegInit(0.U(128.W))
    val nrRecord = RegInit(0.U(128.W))

    outer.node.in.zip(outer.node.out).zipWithIndex.foreach {
      case (((in, edgeIn), (out, edgeOut)), i) =>
        val channelA = in.a
        when(channelA.fire &&
          channelA.bits.opcode =/= Hint &&
          channelA.bits.opcode =/= PutFullData &&
          channelA.bits.opcode =/= PutPartialData
        ) {
          // Valid channel A fire, record it
          assert(latencyRecord(channelA.bits.source).valid === false.B)
          latencyRecord(channelA.bits.source).valid := true.B
          latencyRecord(channelA.bits.source).timeStamp := timer
          latencyRecord(channelA.bits.source).reqType := channelA.bits.user.lift(ReqSourceKey).getOrElse(MemReqSource.NoWhere.id.U)
        }
        val channelD = in.d
        val (first, _, _, _) = edgeIn.count(channelD)
        // Valid channel D fire, resolve it
        val resolveRecord = channelD.fire && first &&
          channelD.bits.opcode =/= ReleaseAck &&
          channelD.bits.opcode =/= AccessAck
        val latency = WireInit(0.U(64.W))
        when(resolveRecord) {
          assert(latencyRecord(channelD.bits.source).valid === true.B)
          latencyRecord(channelD.bits.source).valid := false.B
          latency := timer - latencyRecord(channelD.bits.source).timeStamp
          latencySum := latencySum + timer
          nrRecord := nrRecord + 1.U
          // printf("timer: %x\n", latency)
        }
        XSPerfAccumulate(name + "_nrRecord_all", resolveRecord)
        XSPerfAccumulate(name + "_latencySum_all", Mux(resolveRecord, latency, 0.U))

        for (j <- 0 until MemReqSource.ReqSourceCount.id) {
          val typeMatch = latencyRecord(channelD.bits.source).reqType === j.U
          XSPerfAccumulate(name + s"_nrRecord_type${j}", resolveRecord && typeMatch)
          XSPerfAccumulate(name + s"_latencySum_type${j}", Mux(resolveRecord && typeMatch, latency, 0.U))
        }
    }
  }

}

object BusPerfMonitor {
  def apply(
     name: String,
     enable: Boolean = false,
     stat_latency: Boolean = false,
     add_reqkey: Boolean = false)(implicit p: Parameters) =
  {
    if(enable){
      val busPMU = LazyModule(new BusPerfMonitor(name, stat_latency, add_reqkey))
      busPMU.node
    } else {
      TLTempNode()
    }
  }
}
