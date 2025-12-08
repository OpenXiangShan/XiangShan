/***************************************************************************************
 * Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
package tracertl

import circt.stage.ChiselStage
import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._
// import firrtl.{AnnotationSeq, EmissionOptions}

class AxisBundle(AXI_BUS_WIDTH: Int) extends Bundle {
  val tdata = Input(UInt(AXI_BUS_WIDTH.W))
  val tkeep = Input(UInt((AXI_BUS_WIDTH / 8).W))
  val tlast = Input(Bool())
  val tvalid = Input(Bool())
  val tready = Output(Bool())
}

class XDmaQueueRepeater(AXI_BUS_WIDTH: Int, CYCLE_NUM: Int) extends Module {
  val io = IO(new Bundle {
    val in = new AxisBundle(AXI_BUS_WIDTH)
    val out = Flipped(new AxisBundle(AXI_BUS_WIDTH))
  })

  val queue = Module(new Queue(UInt(AXI_BUS_WIDTH.W), CYCLE_NUM * 4))

  io.in.tready := queue.io.enq.ready
  queue.io.enq.bits := io.in.tdata
  queue.io.enq.valid := io.in.tvalid

  io.out.tdata := queue.io.deq.bits
  io.out.tkeep := Fill((AXI_BUS_WIDTH / 8), 1.U(1.W))
  io.out.tlast := false.B
  io.out.tvalid := queue.io.deq.valid
  queue.io.deq.ready := io.out.tready
}


class XDmaNoRepeater(AXI_BUS_WIDTH: Int, CYCLE_NUM: Int) extends Module {
  val io = IO(new Bundle {
    val in = new AxisBundle(AXI_BUS_WIDTH)
    val out = Flipped(new AxisBundle(AXI_BUS_WIDTH))
  })

  val counter = RegInit(0.U(log2Ceil(CYCLE_NUM).W))
  when (io.out.tready && io.out.tvalid) {
    counter := counter + 1.U
  }
  when (io.out.tready && io.out.tvalid && io.out.tlast) {
    counter := 0.U
  }

  val outData = Wire(Vec(AXI_BUS_WIDTH / 64, UInt(64.W)))
  for (i <- 0 until (AXI_BUS_WIDTH / 64)) {
    outData(i) := i.U
  }

  io.in.tready := true.B
  io.out.tdata := outData.asUInt
  io.out.tkeep := Fill((AXI_BUS_WIDTH / 8), 1.U(1.W))
  io.out.tlast := counter === (CYCLE_NUM - 1).U
  io.out.tvalid := true.B
}

class XDmaRepeater(AXI_BUS_WIDTH: Int, CYCLE_NUM: Int) extends Module {
  val io = IO(new Bundle {
    val in = new AxisBundle(AXI_BUS_WIDTH)
    val out = Flipped(new AxisBundle(AXI_BUS_WIDTH))
  })

  // receive
  val buffer = Reg(Vec(CYCLE_NUM, UInt(AXI_BUS_WIDTH.W)))

  val s_receive :: s_send :: Nil = Enum(2)
  val state = RegInit(s_receive)
  val cycle_cnt = RegInit(0.U(log2Ceil(CYCLE_NUM+1).W))

  io.in.tready := state === s_receive
  when (state === s_receive) {
    when (io.in.tvalid && io.in.tready) {
      buffer(cycle_cnt) := io.in.tdata
      cycle_cnt := cycle_cnt + 1.U
      when (cycle_cnt === (CYCLE_NUM - 1).U) {
        assert(io.in.tlast, "tlast should be true when tvalid is true")
        state := s_send
        cycle_cnt := 0.U
      }
    }
  }

  // send
  io.out.tdata := buffer(cycle_cnt)
  io.out.tkeep := Fill((AXI_BUS_WIDTH / 8), 1.U(1.W))
  io.out.tlast := (cycle_cnt === (CYCLE_NUM - 1).U)
  io.out.tvalid := state === s_send
  when (state === s_send) {
    when (io.out.tvalid && io.out.tready) {
      cycle_cnt := cycle_cnt + 1.U
      when (cycle_cnt === (CYCLE_NUM - 1).U) {
        state := s_receive
        cycle_cnt := 0.U
      }
    }
  }
}

object XDmaTestModGen extends App {
  val targetDir = "./build/tracertl"

  val BUS_WIDTH = 256
  val FIX_CYCLE_NUM = 13

  // Simple Repeater with fixed cycle number assumption
  (new ChiselStage).execute(
    Array(
      "--target-dir", targetDir,
      "--target", "systemverilog",
      // "--emission-options", "disableMemRandomization,disableRegRandomization"
    ),
    Seq(ChiselGeneratorAnnotation(() => new XDmaRepeater(
      AXI_BUS_WIDTH = BUS_WIDTH,
      CYCLE_NUM = FIX_CYCLE_NUM 
    )))
  )
  println(s"Generated $targetDir/XDmaRepeater.sv")

  // Qeuue repeater, with no fix cycle assumption
  (new ChiselStage).execute(
    Array(
      "--target-dir", targetDir,
      "--target", "systemverilog",
      // "--emission-options", "disableMemRandomization,disableRegRandomization"
    ),
    Seq(ChiselGeneratorAnnotation(() => new XDmaQueueRepeater(
      AXI_BUS_WIDTH = BUS_WIDTH,
      CYCLE_NUM = FIX_CYCLE_NUM 
    )))
  )
  println(s"Generated $targetDir/XDmaQueueRepeater.sv")

  // no repeater, just output 
  (new ChiselStage).execute(
    Array(
      "--target-dir", targetDir,
      "--target", "systemverilog",
      // "--emission-options", "disableMemRandomization,disableRegRandomization"
    ),
    Seq(ChiselGeneratorAnnotation(() => new XDmaNoRepeater(
      AXI_BUS_WIDTH = BUS_WIDTH,
      CYCLE_NUM = FIX_CYCLE_NUM 
    )))
  )

  println(s"Generated $targetDir/XDmaNoRepeater.sv")
}
