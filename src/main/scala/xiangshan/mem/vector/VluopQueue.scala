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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._

class VluopPtr(implicit p: Parameters) extends CircularQueuePtr[VluopPtr](
  p => p(XSCoreParamsKey).VlUopSize
){
}

object VluopPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): VluopPtr = {
    val ptr = Wire(new VluopPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class VluopBundle(implicit p: Parameters) extends XSBundle {
  val rob_idx = UInt(log2Ceil(RobSize).W)
  val rob_inner_idx = UInt(3.W)
  //val rob_total_num = UInt(3.W) // Is it useful?
  val wb_dest = UInt(PhyRegIdxWidth.W)
  val flow_idx = UInt(log2Up(VlFlowSize).W)
  val flow_inner_idx = UInt(4.W)
  val flow_offset = UInt(6.W)
  val mask = UInt((CacheLineSize / 8).W)
  val data = UInt(VLEN.W)

  def apply(uop: VecOperand, is_pre: Boolean = false, is_allo: Boolean = false, inner_idx : Int = 0) = {
    if (is_pre) {
      this.rob_idx := uop.uop.robIdx.value
      this.rob_inner_idx := inner_idx.U
      this.mask := VecGenMask(inner_idx.U)
    } else if (is_allo) {
      this.wb_dest := uop.uop.pdest
    } else{
      this.rob_idx := uop.uop.robIdx.value
      this.rob_inner_idx := inner_idx.U
      this.mask := VecGenMask(inner_idx.U)
      //this.rob_total_num := uop.total_num
      this.wb_dest := uop.uop.pdest
    }
    this
  }

  def apply(flow: Flow2UopBuddle) = {
    this.flow_idx := flow.flow_index
    this.flow_inner_idx := flow.flow_inner_index
    this.flow_offset := flow.flow_offset
  }
}

class VluopQueueIOBundle(implicit p: Parameters) extends XSBundle {
  val loadRegIn = Vec(2, Flipped(Decoupled(new VecOperand())))
  val loadFlow2UopOut = Vec(LoadPipelineWidth, Flipped(Decoupled(new Flow2UopBuddle())))
  val loadPipeIn = Vec(exuParameters.LduCnt, Flipped(Decoupled(new VecExuOutput)))
  // TODO: Only one writeback port, Maybe is enough
  val loadWriteback = DecoupledIO(new ExuOutput)
}

class VluopQueue(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper
{
  val io = IO(new VluopQueueIOBundle())

  println("LoadUopQueue: size:" + VlUopSize)

  val VluopEntry = Reg(Vec(VlUopSize, new VluopBundle))
  val Vluop2robEntry = Reg(Vec(VlUopSize, new ExuOutput))
  // For example, an inst -> 4 uops,
  // When first uop comes, 4 entries are all valid and pre_allocated
  val valid = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
  val pre_allocated = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
  // When an uop really comes, an entry will be allocated
  val allocated = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
  // When data of an uop comes, an entry will be received
  val received = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
  // When both data and pdest are readym, this entry is finished
  val finished = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))

  val data_buffer = RegInit(VecInit(Seq.fill(exuParameters.LduCnt)(0.U(CacheLineSize.W))))
  val flow_index_buffer = RegInit(VecInit(Seq.fill(2)(0.U(5.W))))
  val uop2rob_buffer = RegInit(VecInit(Seq.fill(2)(0.U.asTypeOf(new ExuOutput))))
  val buffer_valid = RegInit(VecInit(Seq.fill(2)(false.B)))

  val enqPtr = RegInit(0.U.asTypeOf(new VluopPtr))
  val deqPtr = RegInit(0.U.asTypeOf(new VluopPtr))

  val enqnum = WireInit(0.U(5.W))

  val already_in = WireInit(VecInit(Seq.fill(2)(false.B)))
  val enq_valid = WireInit(VecInit(Seq.fill(2)(false.B)))
  for (i <- 0 until 2) {
    already_in(i) := VluopEntry.map(_.rob_idx === io.loadRegIn(i).bits.uop.robIdx.value).reduce(_ || _)
    enq_valid(i) := io.loadRegIn(i).fire && !already_in(i)
  }

  for (i <- 0 until 2) {
    io.loadFlow2UopOut(i).ready := (distanceBetween(enqPtr, deqPtr) >= 16.U)
    io.loadRegIn(i).ready := true.B // TODO: should always ready?
  }
  for (i <- 0 until exuParameters.LduCnt) {
    io.loadPipeIn(i).ready := true.B // TODO: should always ready?
  }

  io.loadWriteback.valid := false.B
  io.loadWriteback.bits := DontCare

  // TODO: How to simplify these codes?
  //  And timing...?
  when (enq_valid(0) || enq_valid(1)) {
    when (enq_valid(0) && enq_valid(1)) {
      enqPtr := enqPtr + io.loadRegIn(0).bits.total_num + io.loadRegIn(1).bits.total_num

      for (i <- 0 until 8) {
        when (i.U < io.loadRegIn(0).bits.total_num) {
          VluopEntry(enqPtr.value + i.U).apply(io.loadRegIn(0).bits, is_pre = (i > 0), inner_idx = i)
          valid(enqPtr.value + i.U) := true.B
        }
        if (i == 0) {
          allocated(enqPtr.value) := true.B
        } else {
          pre_allocated(enqPtr.value + i.U) := true.B
        }
      }

      for (i <- 0 until 8) {
        when (i.U < io.loadRegIn(1).bits.total_num) {
          VluopEntry(enqPtr.value + io.loadRegIn(0).bits.total_num + i.U).apply(io.loadRegIn(1).bits, is_pre = (i > 0), inner_idx = i)
          valid(enqPtr.value + io.loadRegIn(0).bits.total_num + i.U) := true.B
          pre_allocated(enqPtr.value + io.loadRegIn(0).bits.total_num + i.U) := true.B
        }
        if (i == 0) {
          allocated(enqPtr.value + io.loadRegIn(0).bits.total_num) := true.B
        } else {
          pre_allocated(enqPtr.value + io.loadRegIn(0).bits.total_num + i.U) := true.B
        }
      }

    } .elsewhen (enq_valid(0)) {
      enqPtr := enqPtr + io.loadRegIn(0).bits.total_num

      for (i <- 0 until 8) {
        when (i.U < io.loadRegIn(0).bits.total_num) {
          VluopEntry(enqPtr.value + i.U).apply(io.loadRegIn(0).bits, is_pre = (i > 0), inner_idx = i)
          valid(enqPtr.value + i.U) := true.B
        }
        if (i == 0) {
          allocated(enqPtr.value) := true.B
        } else {
          pre_allocated(enqPtr.value + i.U) := true.B
        }
      }
    } .elsewhen (enq_valid(1)) {
      enqPtr := enqPtr + io.loadRegIn(1).bits.total_num

      for (i <- 0 until 8) {
        when (i.U < io.loadRegIn(1).bits.total_num) {
          VluopEntry(enqPtr.value + i.U).apply(io.loadRegIn(1).bits, is_pre = (i > 0), inner_idx = i)
          valid(enqPtr.value + i.U) := true.B
        }
        if (i == 0) {
          allocated(enqPtr.value) := true.B
        } else {
          pre_allocated(enqPtr.value + i.U) := true.B
        }
      }
    }
  } .elsewhen (io.loadRegIn(0).fire || io.loadRegIn(1).fire) {
    when (io.loadRegIn(0).fire) {
      val debug_hit = WireInit(VecInit(Seq.fill(VlUopSize)(false.B))) // for debug
      for (i <- 0 until VlUopSize) {
        when (VluopEntry(i).rob_idx === io.loadRegIn(0).bits.uop.robIdx.value &&
          VluopEntry(i).rob_inner_idx === io.loadRegIn(0).bits.inner_idx &&
          valid(i) && pre_allocated(i)) {
          VluopEntry(i).apply(io.loadRegIn(0).bits, is_allo = true)
          pre_allocated(i) := false.B
          when (received(i)) {
            finished(i) := true.B
            received(i) := false.B
          } .otherwise {
            allocated(i) := true.B
          }
          debug_hit(i) := true.B
        }
        assert(PopCount(debug_hit) <= 1.U, "VluopQueue Multi-Hit!")
      }
    }
    when (io.loadRegIn(1).fire) {
      val debug_hit = WireInit(VecInit(Seq.fill(VlUopSize)(false.B))) // for debug
      for (i <- 0 until VlUopSize) {
        when (VluopEntry(i).rob_idx === io.loadRegIn(1).bits.uop.robIdx.value &&
          VluopEntry(i).rob_inner_idx === io.loadRegIn(1).bits.inner_idx &&
          valid(i) && pre_allocated(i)) {
          VluopEntry(i).apply(io.loadRegIn(1).bits, is_allo = true)
          pre_allocated(i) := false.B
          when (received(i)) {
            finished(i) := true.B
            received(i) := false.B
          } .otherwise {
            allocated(i) := true.B
          }
          debug_hit(i) := true.B
        }
        assert(PopCount(debug_hit) <= 1.U, "VluopQueue Multi-Hit!")
      }
    }
  }

  val deq_index = deqPtr.value
  when (valid(deq_index) && finished(deq_index)) {
    io.loadWriteback.valid := true.B
    io.loadWriteback.bits := Vluop2robEntry(deq_index)
    io.loadWriteback.bits.data := VluopEntry(deq_index).data
    io.loadWriteback.bits.uop.pdest := VluopEntry(deq_index).wb_dest
    finished(deq_index) := false.B
    valid(deq_index) := false.B
  }


  // TODO: Timing...?
  // TODO: What will happen when this robidx is not in uop queue?
  //  FlowQueue should first know that this robidx is in uop queue (at least one)
  //  then send request to Load Pipe
  //  Otherwise should replay to uop queue
  for (i <- 0 until 2) {
    when (io.loadFlow2UopOut(i).fire) {
      for (j <- 0 until VlUopSize) {
        when (VluopEntry(j).rob_idx === io.loadFlow2UopOut(i).bits.flow_robIdx) {
          VluopEntry(j).apply(io.loadFlow2UopOut(i).bits)
        }
      }
    }
  }

  for (i <- 0 until exuParameters.LduCnt) {
    when (io.loadPipeIn(i).fire) {
      data_buffer(i) := io.loadPipeIn(i).bits.data
      flow_index_buffer(i) := io.loadPipeIn(i).bits.flow_index
      uop2rob_buffer(i) := io.loadPipeIn(i).bits
      buffer_valid(i) := true.B
    }
  }

  for (i <- 0 until exuParameters.LduCnt) {
    when (buffer_valid(i)) {
      for (j <- 0 until VlUopSize) {
        when (VluopEntry(j).flow_idx === flow_index_buffer(i)) {
          VluopEntry(j).data := VecGenData(VluopEntry(j).mask, data_buffer(i))
          when (allocated(j)) {
            finished(j) := true.B
            allocated(j) := false.B
          } .otherwise {
            received(j) := true.B
          }
        }
      }
    }
  }

}