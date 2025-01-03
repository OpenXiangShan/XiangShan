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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import difftest._
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.Bundles._

class OrderArbiter(issueTypes: Seq[Int])(implicit p: Parameters) extends XSModule {
  protected def numSources: Int = issueTypes.length

  protected def orderSelect(seq: Seq[Bool]) = {
    seq.zipWithIndex.map {
      case (b, 0) => b
      case (b, i) => b && !Cat(seq.take(i)).orR
    }
  }

  protected def connectReady(
    in: Seq[DecoupledIO[LsPipelineBundle]],
    out: DecoupledIO[LsPipelineBundle],
    chosenOH: Vec[Bool]
  ) = {
    in.map(_.ready).zip(chosenOH).foreach {
      case (rdy, grant) => rdy := grant && out.ready
    }
  }

  val io = IO(new Bundle() {
    val in = Vec(numSources, Flipped(DecoupledIO(new LsPipelineBundle)))
    val out = DecoupledIO(new LsPipelineBundle)
    val chosen = Output(UInt(numSources.W))
  })

  val orderChosenOH = Wire(Vec(numSources, Bool()))
  orderChosenOH := VecInit(orderSelect(io.in.map(_.valid)))

  io.out.valid := io.in.map(_.valid).reduce(_|_)
  io.out.bits := ParallelPriorityMux(orderChosenOH, io.in.map(_.bits))

  connectReady(io.in, io.out, orderChosenOH)

  io.chosen := orderChosenOH.asUInt
}

class PriorityArbiter(issueTypes: Seq[Int])(implicit p: Parameters) extends OrderArbiter(issueTypes)
  with HasXSParameter
  with HasCircularQueuePtrHelper {

  private def isOneOf(seq: Seq[Int], _type: Int): Boolean = {
    seq.map(s => s == _type).reduce(_ || _)
  }

  private def needCheckLQRWithData(_type: Int): Boolean = {
    isOneOf(Seq(MemIssueType.Fr, MemIssueType.Mmio, MemIssueType.Nc), _type)
  }

  private def hasLQRWithData(seq: Seq[(DecoupledIO[LsPipelineBundle], Int)]): Bool = {
    if (seq.nonEmpty) {
      Cat(seq.filter(x => MemIssueType.isLqr(x._2)).map(x => x._1.valid && x._1.bits.forwardTLDchannel)).orR
    } else {
      false.B
    }
  }

  private def needCheckPF(_type: Int): Boolean = {
    isOneOf(Seq(MemIssueType.Vld, MemIssueType.Ld, MemIssueType.Fr), _type)
  }

  private def hasHighConfPF(seq: Seq[(DecoupledIO[LsPipelineBundle], Int)]): Bool = {
    if (seq.nonEmpty) {
      Cat(seq.filter(x => MemIssueType.isPf(x._2)).map(x => x._1.valid && x._1.bits.confidence > 0.U)).orR
    } else {
      false.B
    }
  }

  private def hasIqStall(seq: Seq[(DecoupledIO[LsPipelineBundle], Int)], robIdx: RobPtr): Bool = {
    if (seq.nonEmpty) {
      Cat(seq.filter(x => MemIssueType.isLd(x._2)).map(x => x._1.valid && isBefore(x._1.bits.uop.robIdx, robIdx))).orR
    } else {
      false.B
    }
  }

  orderChosenOH := VecInit(orderSelect(io.in.zip(issueTypes).zipWithIndex.map {
    case ((in, _type), i) =>
      val lowerSources = io.in.zip(issueTypes).drop(i).filter(
        x => MemIssueType.isLd(x._2) || MemIssueType.isVld(x._2)
      )
      if (MemIssueType.isLqr(_type)) {
        in.valid && (!hasIqStall(lowerSources, in.bits.uop.robIdx) || in.bits.forwardTLDchannel)
      } else {
        in.valid
      }
    }
  ))

  val priorityChosenOH = Wire(Vec(numSources, Bool()))
  io.in.zip(issueTypes).zip(priorityChosenOH).zipWithIndex.foreach {
    case (((in, _type), chosen), i) =>
      val orderSel = orderChosenOH(i)
      if (needCheckLQRWithData(_type))
        chosen := orderSel && !hasLQRWithData(io.in.zip(issueTypes).drop(i))
      else if (needCheckPF(_type))
        chosen := orderSel && !hasHighConfPF(io.in.zip(issueTypes).drop(i))
      else
        chosen := orderSel
  }

  io.out.bits := ParallelPriorityMux(priorityChosenOH, io.in.map(_.bits))

  connectReady(io.in, io.out, priorityChosenOH)

  io.chosen := priorityChosenOH.asUInt
}

object Arbiter {
  def apply(
    in: Seq[(DecoupledIO[LsPipelineBundle], Int)],
    out: DecoupledIO[LsPipelineBundle],
    chosen: Vec[Bool],
    arbiter: String,
    name: Option[String])(implicit p: Parameters) = {
      require(in.length > 0, "No sources!")
      val inPort = in.map(_._1)
      val inTypes = in.map(_._2)
      if (in.length == 1) {
        out <> inPort.head
        chosen := 1.U.asBools
      } else {

        val mod = arbiter match {
          case "order" => Module(new OrderArbiter(inTypes))
          case "priority" => Module(new PriorityArbiter(inTypes))
          case _ => throw new IllegalArgumentException("Unknown arbiter type")
        }
        mod.suggestName(name.getOrElse("arbiter"))
        mod.io.in <> inPort
        out <> mod.io.out
        chosen := mod.io.chosen.asBools
      }
  }
}
