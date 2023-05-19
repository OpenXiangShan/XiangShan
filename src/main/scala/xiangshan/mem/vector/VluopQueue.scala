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
import chisel3.{util, _}
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.rob.RobPtr

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

object VecGenMask {
  def apply(rob_idx_valid: Vec[Bool], reg_offset: Vec[UInt], offset: Vec[UInt], mask: Vec[UInt]):Vec[UInt] = {
    val vMask = VecInit(Seq.fill(2)(0.U(16.W)))
    for (i <- 0 until 2){
      when (rob_idx_valid(i)) {
        when (offset(i) <= reg_offset(i)) {
          vMask(i) := mask(i) << (reg_offset(i) - offset(i))
        }.otherwise {
          vMask(i) := mask(i) >> (offset(i) - reg_offset(i))
        }
      }
    }
    vMask
  }
}

object VecGenData {
  def apply (rob_idx_valid: Vec[Bool], reg_offset: Vec[UInt], offset: Vec[UInt], data:UInt):Vec[UInt] = {
    val vData = VecInit(Seq.fill(2)(0.U(128.W)))
    for (i <- 0 until 2){
      when (rob_idx_valid(i)) {
        when (offset(i) <= reg_offset(i)) {
          vData(i) := data << ((reg_offset(i) - offset(i)) << 3.U)
        }.otherwise {
          vData(i) := data >> ((offset(i) - reg_offset(i)) << 3.U)
        }
      }
    }
    vData
  }
}
/**
 * */
class VluopBundle(implicit p: Parameters) extends XSBundle {
  val uop       = new MicroOp
  val mul      = UInt(3.W)
  val dataVMask = Vec(VLEN/8,Bool())
  val data      = Vec(VLEN/8,UInt(8.W))

  def apply(uop: MicroOp, mul: UInt, is_pre: Bool = false.B, is_allo: Bool = false.B) = {
    when (is_pre) {
      this.uop  := uop
      this.mul := mul
    }.elsewhen (is_allo) {
      this.uop := uop
      //this.wb_dest := uop.uop.pdest  // TODO: this is scalar reg,we need vector reg
    }.otherwise {
      this.uop := uop
      this.mul:= mul
    }
    this
  }
}

class VluopQueueIOBundle(implicit p: Parameters) extends XSBundle {
  val loadRegIn   = Vec(VecLoadPipelineWidth, Flipped(DecoupledIO(new ExuInput(isVpu = true))))
  val Redirect    = Flipped(ValidIO(new Redirect))
  val instType    = Vec(VecLoadPipelineWidth, Input(UInt(3.W)))
  val emul        = Vec(VecLoadPipelineWidth, Input(UInt(3.W)))
  val loadPipeIn  = Vec(VecLoadPipelineWidth, Flipped(DecoupledIO(new VecExuOutput)))
  val uopVecFeedback = Vec(VecLoadPipelineWidth,ValidIO(Bool()))
  val vecLoadWriteback = Vec(2,DecoupledIO(new ExuOutput(isVpu = true)))
  //val vecData = Vec(2,DecoupledIO(UInt(VLEN.W)))
}

class VluopQueue(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper
{
  val io = IO(new VluopQueueIOBundle())

  println("LoadUopQueue: size:" + VlUopSize)

  val VluopEntry = Reg(Vec(VlUopSize, new VluopBundle))
  // For example, an inst -> 4 uops,
  // When first uop comes, 4 entries are all valid and pre_allocated
  val valid = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
  val pre_allocated = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
  // When an uop really comes, an entry will be allocated
  val allocated = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
  // When both data and pdest are readym, this entry is finished
  val finished = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))

  val enqPtr = RegInit(0.U.asTypeOf(new VluopPtr))
  val deqPtr = RegInit(0.U.asTypeOf(new VluopPtr))
  val already_in = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(false.B)))
  val already_in_vec = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(VlUopSize)(false.B)))))
  val enq_valid = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(false.B)))
  val instType = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
  val mul      = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
  val loadRegInValid = WireInit(VecInit(Seq.fill(VecStorePipelineWidth)(false.B)))
  val needFlush = WireInit(VecInit(Seq.fill(VlUopSize)(false.B)))

  //First-level buffer
  val buffer_valid_s0  = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(false.B)))
  val data_buffer_s0   = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U(VLEN.W))))
  val mask_buffer_s0   = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U((VLEN/8).W))))))
  val rob_idx_valid_s0 = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(false.B)))))
  val inner_idx_s0     = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U(3.W))))))
  val rob_idx_s0       = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U.asTypeOf(new RobPtr))))))
  val reg_offset_s0    = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U(4.W))))))
  val offset_s0        = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U(4.W))))))
  val uop_s0           = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U.asTypeOf(new MicroOp))))
  //Second-level buffer
  val buffer_valid_s1  = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(false.B)))))
  val data_buffer_s1   = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U(VLEN.W))))))
  val mask_buffer_s1   = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U((VLEN/8).W))))))
  val rob_idx_valid_s1 = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(false.B)))))
  val inner_idx_s1     = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U(3.W))))))
  val rob_idx_s1       = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U.asTypeOf(new RobPtr))))))
  val uop_s1           = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U.asTypeOf(new MicroOp))))
/**
 * Only unit-stride instructions use vecFeedback;
 * PopCount(valid) >= 16.U, Only pre_allocated uop can enqueue*/
  for (i <- 0 until VecLoadPipelineWidth) {
    io.uopVecFeedback(i).valid := io.loadRegIn(i).valid
    io.uopVecFeedback(i).bits  := already_in(i)
    io.loadRegIn(i).ready := PopCount(valid) <= 16.U // TODO: should always ready? or count valid_entry?????
    io.loadPipeIn(i).ready := true.B
  }

  for (i <- 0 until VecLoadPipelineWidth) {
    mul(i) := Mux(instType(i)(1,0) === "b00".U || instType(i)(1,0) === "b10".U,io.emul(i),io.loadRegIn(i).bits.uop.ctrl.vconfig.vtype.vlmul(i))
    for (entry <- 0 until VlUopSize) {
      already_in_vec(i)(entry) := VluopEntry(entry).uop.robIdx.value === io.loadRegIn(i).bits.uop.robIdx.value &&
                                  VluopEntry(entry).uop.ctrl.uopIdx === io.loadRegIn(i).bits.uop.ctrl.uopIdx &&
                                  pre_allocated(entry)
      val debug_hit = WireInit(VecInit(Seq.fill(VlUopSize)(false.B))) // for debug
      when (already_in_vec(i)(entry) && io.loadRegIn(i).valid) {
        VluopEntry(entry).apply(uop = io.loadRegIn(i).bits.uop, mul = mul(i)(i), is_allo = true.B)
        allocated(entry)     := true.B
        debug_hit(entry)     := true.B
      }
      assert(PopCount(debug_hit) <= 1.U, "VluopQueue Multi-Hit!")
    }
    already_in(i) := already_in_vec(i).asUInt.orR
    enq_valid(i)  := !already_in(i) && loadRegInValid(i)
    instType(i)   := io.instType(i)
  }

  for (entry <- 0 until VlUopSize) {
    needFlush(entry) := VluopEntry(entry).uop.robIdx.needFlush(io.Redirect) && valid(entry)
    when (needFlush(entry)) {
      valid(entry)         := false.B
      allocated(entry)     := false.B
      pre_allocated(entry) := false.B
      finished(entry)      := false.B
      VluopEntry(entry).dataVMask := VecInit(Seq.fill(VLEN / 8)(false.B))
    }
  }
  for (i <- 0 until VecLoadPipelineWidth) {
    loadRegInValid(i) := !io.loadRegIn(i).bits.uop.robIdx.needFlush(io.Redirect) && io.loadRegIn(i).fire
  }

  //enqPtr update
  val lastRedirect    = RegNext(io.Redirect)
  val uopRedirectCnt  = RegNext(PopCount(needFlush))
  when (lastRedirect.valid) {
    enqPtr.value := enqPtr.value - uopRedirectCnt
  }.otherwise {
    when (enq_valid(0) && enq_valid(1)) {
      when (instType(0) === "b000".U && instType(1) === "b000".U) {
        when(io.loadRegIn(0).bits.uop.robIdx.value === io.loadRegIn(1).bits.uop.robIdx.value) {
          enqPtr.value := enqPtr.value + io.loadRegIn(0).bits.uop.ctrl.total_num + 1.U
        }.otherwise {
          enqPtr.value := enqPtr.value + io.loadRegIn(0).bits.uop.ctrl.total_num + io.loadRegIn(1).bits.uop.ctrl.total_num + 2.U
        }
      }.elsewhen (instType(0) === "b000".U && instType(1) =/= "b000".U) {
        enqPtr.value := enqPtr.value + io.loadRegIn(0).bits.uop.ctrl.total_num + 2.U
      }.elsewhen (instType(0) =/= "b000".U && instType(1) === "b000".U) {
        enqPtr.value := enqPtr.value + io.loadRegIn(1).bits.uop.ctrl.total_num + 2.U
      }.otherwise {
        enqPtr.value := enqPtr.value + 2.U
      }
    }.elsewhen (enq_valid(0) && !enq_valid(1)) {
      when(instType(0) === "b000".U) {
        enqPtr.value := enqPtr.value + io.loadRegIn(0).bits.uop.ctrl.total_num + 1.U
      }.otherwise {
        enqPtr.value := enqPtr.value + 1.U
      }
    }.elsewhen (!enq_valid(0) && enq_valid(1)) {
      when (instType(1) === "b000".U) {
        enqPtr.value := enqPtr.value + io.loadRegIn(1).bits.uop.ctrl.total_num + 1.U
      }.otherwise {
        enqPtr.value := enqPtr.value + 1.U
      }
    }
  }


  // TODO: How to simplify these codes?
  //  And timing...?
  dontTouch(io.loadRegIn)
  when (enq_valid(0) && enq_valid(1)) {
    when (instType(0) === "b000".U && instType(1) === "b000".U ) {
      when (io.loadRegIn(0).bits.uop.robIdx.value === io.loadRegIn(1).bits.uop.robIdx.value) {
        //enqPtr.value := enqPtr.value + io.loadRegIn(0).bits.uop.ctrl.total_num + 1.U
        for (i <- 0 until 8) {
          when (i.U <= io.loadRegIn(0).bits.uop.ctrl.total_num) {
            val inUop = WireInit(io.loadRegIn(0).bits.uop)
            val isPer = !(i.U === io.loadRegIn(0).bits.uop.ctrl.uopIdx || i.U === io.loadRegIn(1).bits.uop.ctrl.uopIdx)
            inUop.ctrl.uopIdx := i.U
            VluopEntry(enqPtr.value + i.U).apply(uop = inUop, mul = mul(0), is_pre = isPer)
            valid(enqPtr.value + i.U) := true.B
            pre_allocated(enqPtr.value + i.U) := true.B
            when (i.U === io.loadRegIn(0).bits.uop.ctrl.uopIdx || i.U === io.loadRegIn(1).bits.uop.ctrl.uopIdx) {
              allocated(enqPtr.value + i.U) := true.B
            }
          }
        }
      }.otherwise {
        //enqPtr.value := enqPtr.value + io.loadRegIn(0).bits.uop.ctrl.total_num + io.loadRegIn(1).bits.uop.ctrl.total_num + 2.U
        for (i <- 0 until 8) {
          when (i.U <= io.loadRegIn(0).bits.uop.ctrl.total_num) {
            val inUop = WireInit(io.loadRegIn(0).bits.uop)
            val isPer = !(i.U === io.loadRegIn(0).bits.uop.ctrl.uopIdx)
            //inUop.uop.robIdx.value := io.loadRegIn(0).bits.uop.robIdx.value - io.loadRegIn(0).bits.inner_idx + i.U
            inUop.ctrl.uopIdx := i.U
            VluopEntry(enqPtr.value + i.U).apply(uop = inUop, mul = mul(0), is_pre = isPer)
            valid(enqPtr.value + i.U) := true.B
            pre_allocated(enqPtr.value + i.U) := true.B
            when (i.U === io.loadRegIn(0).bits.uop.ctrl.uopIdx) {
              allocated(enqPtr.value + i.U) := true.B
            }
          }
        }
        for (i <- 0 until 8) {
          when (i.U <= io.loadRegIn(1).bits.uop.ctrl.total_num) {
            val inUop = WireInit(io.loadRegIn(1).bits.uop)
            val isPer = !(i.U === io.loadRegIn(1).bits.uop.ctrl.uopIdx)
            inUop.ctrl.uopIdx := i.U
            VluopEntry(enqPtr.value + io.loadRegIn(0).bits.uop.ctrl.total_num + 1.U + i.U).apply(uop = inUop, mul = mul(1), is_pre = isPer)
            valid(enqPtr.value + io.loadRegIn(0).bits.uop.ctrl.total_num + 1.U + i.U) := true.B
            pre_allocated(enqPtr.value + io.loadRegIn(0).bits.uop.ctrl.total_num + 1.U + i.U) := true.B
            when (i.U === io.loadRegIn(1).bits.uop.ctrl.uopIdx) {
              allocated(enqPtr.value + io.loadRegIn(0).bits.uop.ctrl.total_num + 1.U + i.U) := true.B
            }
          }
        }
      }
    }.elsewhen (instType(0) === "b000".U && instType(1) =/= "b000".U ) {
      //enqPtr.value := enqPtr.value + io.loadRegIn(0).bits.uop.ctrl.total_num + 2.U
      for (i <- 0 until 8) {
        when (i.U <= io.loadRegIn(0).bits.uop.ctrl.total_num) {
          val inUop = WireInit(io.loadRegIn(0).bits.uop)
          val isPer = !(i.U === io.loadRegIn(0).bits.uop.ctrl.uopIdx)
          //inUop.uop.robIdx.value := io.loadRegIn(0).bits.uop.robIdx.value - io.loadRegIn(0).bits.inner_idx + i.U
          inUop.ctrl.uopIdx := i.U
          VluopEntry(enqPtr.value + i.U).apply(uop = inUop, mul = mul(0), is_pre = isPer)
          valid(enqPtr.value + i.U) := true.B
          pre_allocated(enqPtr.value + i.U) := true.B
          when (i.U === io.loadRegIn(0).bits.uop.ctrl.uopIdx) {
            allocated(enqPtr.value + i.U) := true.B
          }
        }
      }
      VluopEntry   (enqPtr.value + io.loadRegIn(0).bits.uop.ctrl.total_num + 1.U).apply(uop = io.loadRegIn(1).bits.uop, mul = mul(1))
      valid        (enqPtr.value + io.loadRegIn(0).bits.uop.ctrl.total_num + 1.U) := true.B
      pre_allocated(enqPtr.value + io.loadRegIn(0).bits.uop.ctrl.total_num + 1.U) := true.B
      allocated    (enqPtr.value + io.loadRegIn(0).bits.uop.ctrl.total_num + 1.U) := true.B

    }.elsewhen (instType(0) =/= "b000".U && instType(1) === "b000".U ) {
      //enqPtr.value := enqPtr.value + io.loadRegIn(1).bits.uop.ctrl.total_num + 2.U
      for (i <- 0 until 8) {
        when (i.U <= io.loadRegIn(1).bits.uop.ctrl.total_num) {
          val inUop = WireInit(io.loadRegIn(1).bits.uop)
          val isPer = !(i.U === io.loadRegIn(1).bits.uop.ctrl.uopIdx)
          //inUop.uop.robIdx.value := io.loadRegIn(0).bits.uop.robIdx.value - io.loadRegIn(0).bits.inner_idx + i.U
          inUop.ctrl.uopIdx := i.U
          VluopEntry(enqPtr.value + i.U).apply(uop = inUop, mul = mul(1), is_pre = isPer)
          valid(enqPtr.value + i.U) := true.B
          pre_allocated(enqPtr.value + i.U) := true.B
          when (i.U === io.loadRegIn(1).bits.uop.ctrl.uopIdx) {
            allocated(enqPtr.value + i.U) := true.B
          }
        }
      }
      VluopEntry   (enqPtr.value + io.loadRegIn(1).bits.uop.ctrl.total_num + 1.U).apply(uop = io.loadRegIn(0).bits.uop, mul = mul(0))
      valid        (enqPtr.value + io.loadRegIn(1).bits.uop.ctrl.total_num + 1.U) := true.B
      pre_allocated(enqPtr.value + io.loadRegIn(1).bits.uop.ctrl.total_num + 1.U) := true.B
      allocated    (enqPtr.value + io.loadRegIn(1).bits.uop.ctrl.total_num + 1.U) := true.B
    }.otherwise {
      //enqPtr.value := enqPtr.value + 2.U
      for (i <- 0 until 2) {
        VluopEntry   (enqPtr.value + i.U).apply(uop = io.loadRegIn(i).bits.uop,mul = mul(i))
        valid        (enqPtr.value + i.U) := true.B
        pre_allocated(enqPtr.value + i.U) := true.B
        allocated    (enqPtr.value + i.U) := true.B
      }
    }
  }.elsewhen (enq_valid(0) && !enq_valid(1)) {
    when (instType(0) === "b000".U) {
      //enqPtr.value := enqPtr.value + io.loadRegIn(0).bits.uop.ctrl.total_num + 1.U
      for (i <- 0 until 8) {
        when (i.U <= io.loadRegIn(0).bits.uop.ctrl.total_num) {
          val inUop = WireInit(io.loadRegIn(0).bits.uop)
          val isPer = !(i.U === io.loadRegIn(0).bits.uop.ctrl.uopIdx)
          //inUop.uop.robIdx.value := io.loadRegIn(0).bits.uop.robIdx.value - io.loadRegIn(0).bits.inner_idx + i.U
          inUop.ctrl.uopIdx := i.U
          VluopEntry(enqPtr.value + i.U).apply(uop = inUop, mul = mul(0), is_pre = isPer)
          valid(enqPtr.value + i.U) := true.B
          pre_allocated(enqPtr.value + i.U) := true.B
          when (i.U === io.loadRegIn(0).bits.uop.ctrl.uopIdx) {
            allocated(enqPtr.value + i.U) := true.B
          }
        }
      }
    }.otherwise {
      //enqPtr.value := enqPtr.value + 1.U
      VluopEntry   (enqPtr.value).apply(uop = io.loadRegIn(0).bits.uop, mul = mul(0))
      valid        (enqPtr.value) := true.B
      pre_allocated(enqPtr.value) := true.B
      allocated    (enqPtr.value) := true.B
    }
  }.elsewhen (!enq_valid(0) && enq_valid(1)) {
    when (instType(1) === "b000".U) {
      //enqPtr.value := enqPtr.value + io.loadRegIn(1).bits.uop.ctrl.total_num + 1.U
      for (i <- 0 until 8) {
        when (i.U <= io.loadRegIn(1).bits.uop.ctrl.total_num) {
          val inUop = WireInit(io.loadRegIn(1).bits.uop)
          val isPer = !(i.U === io.loadRegIn(1).bits.uop.ctrl.uopIdx)
          //inUop.uop.robIdx.value := io.loadRegIn(0).bits.uop.robIdx.value - io.loadRegIn(0).bits.inner_idx + i.U
          inUop.ctrl.uopIdx := i.U
          VluopEntry(enqPtr.value + i.U).apply(uop = inUop, mul = mul(1), is_pre = isPer)
          valid(enqPtr.value + i.U) := true.B
          pre_allocated(enqPtr.value + i.U) := true.B
          when (i.U === io.loadRegIn(1).bits.uop.ctrl.uopIdx) {
            allocated(enqPtr.value + i.U) := true.B
          }
        }
      }
    }.otherwise {
      //enqPtr.value := enqPtr.value + 1.U
      VluopEntry   (enqPtr.value).apply(uop = io.loadRegIn(1).bits.uop, mul = mul(1))
      valid        (enqPtr.value) := true.B
      pre_allocated(enqPtr.value) := true.B
      allocated    (enqPtr.value) := true.B
    }
  }.otherwise {
    enqPtr.value := enqPtr.value
  }

/*
  for (i <- 0 until VecLoadPipelineWidth) {
    when (already_in(i)) {
      val debug_hit = WireInit(VecInit(Seq.fill(VlUopSize)(false.B))) // for debug
      for (entry <- 0 until VlUopSize) {
        when (VluopEntry(entry).rob_idx === io.loadRegIn(i).bits.uop.robIdx.value &&
              VluopEntry(entry).inner_idx === io.loadRegIn(i).bits.inner_idx &&
              valid(entry) && pre_allocated(i)) {
          VluopEntry(entry).apply(uop = io.loadRegIn(i).bits, emul = io.emul(i), is_allo = true.B)
          allocated(entry)     := true.B
          debug_hit(entry)     := true.B
        }
        assert(PopCount(debug_hit) <= 1.U, "VluopQueue Multi-Hit!")
      }
    }
  }*/

  for (entry <- 0 until VlUopSize) {
    when (VluopEntry(entry).mul(2) === 0.U ) {
     finished(entry) := valid(entry) && allocated(entry) && VluopEntry(entry).dataVMask.asUInt.andR
    }.elsewhen (VluopEntry(entry).mul === "b111".U) { // 1/2
      finished(entry) := valid(entry) && allocated(entry) && VluopEntry(entry).dataVMask.asUInt(7,0).andR
    }.elsewhen(VluopEntry(entry).mul === "b110".U) { // 1/4
      finished(entry) := valid(entry) && allocated(entry) && VluopEntry(entry).dataVMask.asUInt(3,0).andR
    }.elsewhen(VluopEntry(entry).mul === "b101".U) { // 1/8
      finished(entry) := valid(entry) && allocated(entry) && VluopEntry(entry).dataVMask.asUInt(1,0).andR
    }
  }

  // write data from loadpipe to first_level buffer
  for (i <- 0 until VecLoadPipelineWidth) {
    when (io.loadPipeIn(i).fire) {
      buffer_valid_s0(i)  := true.B
      data_buffer_s0(i)   := io.loadPipeIn(i).bits.vecdata
      //mask_buffer_s0(i)   := io.loadPipeIn(i).bits.mask
      inner_idx_s0(i)     := io.loadPipeIn(i).bits.inner_idx
      rob_idx_s0(i)       := io.loadPipeIn(i).bits.rob_idx
      rob_idx_valid_s0(i) := io.loadPipeIn(i).bits.rob_idx_valid
      reg_offset_s0(i)    := io.loadPipeIn(i).bits.reg_offset
      offset_s0(i)        := io.loadPipeIn(i).bits.offset
      uop_s0(i)           := io.loadPipeIn(i).bits.uop
      for (j <- 0 until 2) {
        mask_buffer_s0(i)(j) := io.loadPipeIn(i).bits.mask << io.loadPipeIn(i).bits.offset(j)
      }
      //printf{p"buffer_valid_s0 = ${data_buffer_s0(i)}"}
    }.otherwise {
      buffer_valid_s0(i)  := false.B
      rob_idx_valid_s0(i) := VecInit(Seq.fill(2)(false.B))
    }
  }
  // write data from first_level buffer to second_level buffer
  for (i <- 0 until VecLoadPipelineWidth) {
    when (buffer_valid_s0(i) === true.B) {
      buffer_valid_s1(i)  := VecInit(Seq.fill(2)(true.B))
      mask_buffer_s1(i)   := VecGenMask(rob_idx_valid = rob_idx_valid_s0(i), reg_offset = reg_offset_s0(i), offset = offset_s0(i), mask = mask_buffer_s0(i))
      data_buffer_s1(i)   := VecGenData(rob_idx_valid = rob_idx_valid_s0(i), reg_offset = reg_offset_s0(i), offset = offset_s0(i), data = data_buffer_s0(i))
      rob_idx_valid_s1(i) := rob_idx_valid_s0(i)
      inner_idx_s1(i)     := inner_idx_s0(i)
      rob_idx_s1(i)       := rob_idx_s0(i)
      uop_s1(i)           := uop_s0(i)
    }.otherwise {
      buffer_valid_s1(i)  := VecInit(Seq.fill(2)(false.B))
      rob_idx_valid_s1(i) := VecInit(Seq.fill(2)(false.B))
    }
  }

  //write data from second_level buffer to VluopEntry
  for (i <- 0 until VecLoadPipelineWidth) {
    for (j <- 0 until 2) {
      when (buffer_valid_s1(i)(j) === true.B && rob_idx_valid_s1(i)(j) === true.B) {
        for (entry <- 0 until VlUopSize) {
          when (rob_idx_s1(i)(j).value === VluopEntry(entry).uop.robIdx.value && inner_idx_s1(i)(j) === VluopEntry(entry).uop.ctrl.uopIdx) {
            for (k <- 0 until VLEN/8) {
              when (mask_buffer_s1(i)(j)(k)) {
                VluopEntry(entry).data(k)      := data_buffer_s1(i)(j)(k*8 + 7,k*8)
                VluopEntry(entry).dataVMask(k) := mask_buffer_s1(i)(j)(k)
              }
            }
          }
        }
      }
    }
  }

  // writeback to regfile/rob (two ports)
  //eg: if 0-port is not fire, 1-port‘s valid signal must be invalid
  io.vecLoadWriteback(0).valid := valid(deqPtr.value) && finished(deqPtr.value)
  io.vecLoadWriteback(1).valid := io.vecLoadWriteback(0).fire && valid(deqPtr.value + 1.U) && finished(deqPtr.value + 1.U)
  for (i <- 0 until 2) {
    io.vecLoadWriteback(i).bits := DontCare
    val deq_index = deqPtr.value + i.U
    io.vecLoadWriteback(i).bits.uop              := VluopEntry(deq_index).uop
    io.vecLoadWriteback(i).bits.data             := VluopEntry(deq_index).data.asUInt
    //io.vecLoadWriteback(i).bits.uop.pdest        := VluopEntry(deq_index).wb_dest
    when (io.vecLoadWriteback(i).fire) {//TODO:need optimization?
      valid(deq_index)                             := false.B
      allocated(deq_index)                         := false.B
      pre_allocated(deq_index)                     := false.B
      finished(deq_index)                          := false.B
      VluopEntry(deq_index).dataVMask              := VecInit(Seq.fill(VLEN/8)(false.B))
    }
  }

  when(io.vecLoadWriteback(0).fire && io.vecLoadWriteback(1).fire) {
    deqPtr := deqPtr + 2.U
  }.elsewhen(io.vecLoadWriteback(0).fire) {
    deqPtr := deqPtr + 1.U
  }

}