package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._

class vlExcSignalBundle(implicit p: Parameters) extends XSBundle{
  val vecloadRegIn = Vec(2,Decoupled(new VecOperand()))
  val vecwriteback = Vec(2,Flipped(Decoupled(new VecWriteback)))
  val vecFeedback = Vec(2,Input(Bool()))
}


class VlExcSignal(implicit p: Parameters) extends XSModule{
  val io = IO(new vlExcSignalBundle)

  val loadRegIn = Wire(Vec(2,new VecOperand()))
  val loadRegIn_valid = Wire(Vec(2,Bool()))

  //for(i <- 0 until LoadPipelineWidth){
  //  loadRegIn(i).uop       := DontCare
  //  loadRegIn_valid(i)     := LFSR64(seed=Some(123L))(3,0) === 0.U
  //  //loadRegIn(i).vmask     := LFSR64(seed = Some(23L))(63,0)
  //  loadRegIn(i).baseaddr  := 0x80000000L.U + LFSR64(seed = Some(231L))(8,0)
  //  //loadRegIn(i).stride    := LFSR64(seed = Some(3L))(XLEN-1,0)
  //  //loadRegIn(i).index     := LFSR64(seed = Some(12L))(63,0)
  //  //loadRegIn(i).pvd       := LFSR64(seed = Some(11L))(4,0)
  //  loadRegIn(i).lmul      := LFSR64(seed = Some(31L))(2,0)
  //  loadRegIn(i).sew       := LFSR64(seed = Some(41L))(1,0)
  //  //loadRegIn(i).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
  //  //loadRegIn(i).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
  //  loadRegIn(i).inner_idx := LFSR64(seed = Some(98L))(2,0)
  //  loadRegIn(i).vl        := LFSR64(seed = Some(100L))(7,0)
  //  loadRegIn(i).total_num := LFSR64(seed = Some(71L))(3,0)
  //  loadRegIn(i).uop.robIdx.value := LFSR64(seed = Some(78L))(7,0)
  //  loadRegIn(i).uop.cf.instr     := LFSR64(seed = Some(56L))(32,0)
  //  loadRegIn(i).uop.pdest        := LFSR64(seed = Some(99L))(PhyRegIdxWidth-1,0)
  //}

  val s_idle::s_1::s_2::s_3::Nil = Enum(4)
  val state_0 = RegInit(s_idle)
  val state_1 = RegInit(s_idle)
  val baseaddr_0 = 0x80000000L.U + 0x1000.U
  val baseaddr_1 = 0x80000000L.U + 0x1100.U
  val robIdx_0 = 16.U
  val robIdx_1 = 32.U
  val counter = RegInit(0.U(8.W))

  loadRegIn(0) := DontCare
  loadRegIn(1) := DontCare
  loadRegIn_valid(0) := DontCare
  loadRegIn_valid(1) := DontCare

  counter := counter + 1.U


  switch (state_0) {
    is (s_idle) {
        when (counter === 254.U) {
          loadRegIn_valid(0)     := true.B
        }.otherwise {
          loadRegIn_valid(0)     := false.B
        }
        //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
        loadRegIn(0).baseaddr  := baseaddr_0
        //loadRegIn(0).stride    := LFSR64(seed = Some(3L))(XLEN-1,0)
        //loadRegIn(0).index     := LFSR64(seed = Some(12L))(63,0)
        //loadRegIn(0).pvd       := LFSR64(seed = Some(11L))(4,0)
        loadRegIn(0).lmul      := "b010".U
        //loadRegIn(0).sew       := LFSR64(seed = Some(41L))(1,0)
        //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
        //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
        loadRegIn(0).inner_idx := 0.U
        loadRegIn(0).vl        := (16 * 4).U
        loadRegIn(0).total_num := 4.U
        loadRegIn(0).uop.robIdx.value := robIdx_0
        loadRegIn(0).uop.cf.instr     := 0x00000000.U
        loadRegIn(0).uop.pdest        := LFSR64(seed = Some(99L))(PhyRegIdxWidth-1,0)
      when (counter === 255.U) {
        state_0 := s_1
      }
    }
    is (s_1) {
        //loadRegIn(0).uop       := DontCare
        loadRegIn_valid(0)     := true.B
        loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
        loadRegIn(0).baseaddr  := baseaddr_0 + 16.U
        //loadRegIn(i).stride    := LFSR64(seed = Some(3L))(XLEN-1,0)
        //loadRegIn(i).index     := LFSR64(seed = Some(12L))(63,0)
        //loadRegIn(i).pvd       := LFSR64(seed = Some(11L))(4,0)
        loadRegIn(0).lmul      := "b010".U
        //loadRegIn(i).sew       := LFSR64(seed = Some(41L))(1,0)
        //loadRegIn(i).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
        //loadRegIn(i).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
        loadRegIn(0).inner_idx := 1.U
        loadRegIn(0).vl        := ((16 * 4)).U
        loadRegIn(0).total_num := 4.U
        loadRegIn(0).uop.robIdx.value := robIdx_0 + 1.U
        loadRegIn(0).uop.cf.instr     := 0x10000000.U
        loadRegIn(0).uop.pdest        := LFSR64(seed = Some(99L))(PhyRegIdxWidth-1,0)
      state_0 := s_2
    }
    is (s_2) {
        //loadRegIn(0).uop       := DontCare
        loadRegIn_valid(0)     := true.B
        //loadRegIn(i).vmask     := LFSR64(seed = Some(23L))(63,0)
        loadRegIn(0).baseaddr  := baseaddr_0 + 32.U
        //loadRegIn(i).stride    := LFSR64(seed = Some(3L))(XLEN-1,0)
        //loadRegIn(i).index     := LFSR64(seed = Some(12L))(63,0)
        //loadRegIn(i).pvd       := LFSR64(seed = Some(11L))(4,0)
        loadRegIn(0).lmul      := "b010".U
        //loadRegIn(i).sew       := LFSR64(seed = Some(41L))(1,0)
        //loadRegIn(i).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
        //loadRegIn(i).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
        loadRegIn(0).inner_idx := 2.U
        loadRegIn(0).vl        := ((16 * 4)).U
        loadRegIn(0).total_num := 4.U
        loadRegIn(0).uop.robIdx.value := robIdx_0 + 2.U
        loadRegIn(0).uop.cf.instr     := 0x20000000.U
        loadRegIn(0).uop.pdest        := LFSR64(seed = Some(99L))(PhyRegIdxWidth-1,0)
      state_0 := s_3
    }
    is (s_3) {
        //loadRegIn(0).uop       := DontCare
        loadRegIn_valid(0)     := true.B
        //loadRegIn(i).vmask     := LFSR64(seed = Some(23L))(63,0)
        loadRegIn(0).baseaddr  := baseaddr_0 + 48.U
        //loadRegIn(i).stride    := LFSR64(seed = Some(3L))(XLEN-1,0)
        //loadRegIn(i).index     := LFSR64(seed = Some(12L))(63,0)
        //loadRegIn(i).pvd       := LFSR64(seed = Some(11L))(4,0)
        loadRegIn(0).lmul      := "b010".U
        //loadRegIn(i).sew       := LFSR64(seed = Some(41L))(1,0)
        //loadRegIn(i).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
        //loadRegIn(i).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
        loadRegIn(0).inner_idx := 3.U
        loadRegIn(0).vl        := ((16 * 4)).U
        loadRegIn(0).total_num := 4.U
        loadRegIn(0).uop.robIdx.value := robIdx_0 + 3.U
        loadRegIn(0).uop.cf.instr     := 0x30000000.U
        loadRegIn(0).uop.pdest        := LFSR64(seed = Some(99L))(PhyRegIdxWidth-1,0)
      state_0 := s_idle
    }
  }



  switch (state_1) {
    is (s_idle) {
      when (counter === 254.U) {
        loadRegIn_valid(1)     := true.B
      }.otherwise {
        loadRegIn_valid(1)     := false.B
      }
      //loadRegIn(1).uop       := DontCare
      //loadRegIn_valid(1)     := true.B
      //loadRegIn(i).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_1
      //loadRegIn(i).stride    := LFSR64(seed = Some(3L))(XLEN-1,0)
      //loadRegIn(i).index     := LFSR64(seed = Some(12L))(63,0)
      //loadRegIn(i).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := "b010".U
      //loadRegIn(i).sew       := LFSR64(seed = Some(41L))(1,0)
      //loadRegIn(i).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(i).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := 2.U
      loadRegIn(1).vl        := ((16 * 4)).U
      loadRegIn(1).total_num := 4.U
      loadRegIn(1).uop.robIdx.value := robIdx_1 + 2.U
      loadRegIn(1).uop.cf.instr     := 0x00000000.U
      loadRegIn(1).uop.pdest        := LFSR64(seed = Some(99L))(PhyRegIdxWidth-1,0)
      when (counter === 255.U) {
        state_1 := s_1
      }
    }
    is (s_1) {
      //loadRegIn(1).uop       := DontCare
      loadRegIn_valid(1)     := true.B
      //loadRegIn(i).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_1 + 16.U
      //loadRegIn(i).stride    := LFSR64(seed = Some(3L))(XLEN-1,0)
      //loadRegIn(i).index     := LFSR64(seed = Some(12L))(63,0)
      //loadRegIn(i).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := "b010".U
      //loadRegIn(i).sew       := LFSR64(seed = Some(41L))(1,0)
      //loadRegIn(i).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(i).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := 1.U
      loadRegIn(1).vl        := ((16 * 4)).U
      loadRegIn(1).total_num := 4.U
      loadRegIn(1).uop.robIdx.value := robIdx_1 + 1.U
      loadRegIn(1).uop.cf.instr     := 0x10000000.U
      loadRegIn(1).uop.pdest        := LFSR64(seed = Some(99L))(PhyRegIdxWidth-1,0)
      state_1 := s_2
    }
    is (s_2) {
      //loadRegIn(1).uop       := DontCare
      loadRegIn_valid(1)     := true.B
      //loadRegIn(i).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_1 + 32.U
      //loadRegIn(i).stride    := LFSR64(seed = Some(3L))(XLEN-1,0)
      //loadRegIn(i).index     := LFSR64(seed = Some(12L))(63,0)
      //loadRegIn(i).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := "b010".U
      //loadRegIn(i).sew       := LFSR64(seed = Some(41L))(1,0)
      //loadRegIn(i).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(i).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := 0.U
      loadRegIn(1).vl        := ((16 * 4)).U
      loadRegIn(1).total_num := 4.U
      loadRegIn(1).uop.robIdx.value := robIdx_1 + 0.U
      loadRegIn(1).uop.cf.instr     := 0x20000000.U
      loadRegIn(1).uop.pdest        := LFSR64(seed = Some(99L))(PhyRegIdxWidth-1,0)
      state_1 := s_3
    }
    is (s_3) {
      //loadRegIn(1).uop       := DontCare
      loadRegIn_valid(1)     := true.B
      //loadRegIn(i).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_1 + 48.U
      //loadRegIn(i).stride    := LFSR64(seed = Some(3L))(XLEN-1,0)
      //loadRegIn(i).index     := LFSR64(seed = Some(12L))(63,0)
      //loadRegIn(i).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := "b010".U
      //loadRegIn(i).sew       := LFSR64(seed = Some(41L))(1,0)
      //loadRegIn(i).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(i).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := 3.U
      loadRegIn(1).vl        := (16 * 4).U
      loadRegIn(1).total_num := 4.U
      loadRegIn(1).uop.robIdx.value := robIdx_1 + 3.U
      loadRegIn(1).uop.cf.instr     := 0x30000000.U
      loadRegIn(1).uop.pdest        := LFSR64(seed = Some(99L))(PhyRegIdxWidth-1,0)
      state_1 := s_idle
    }
  }


  for(i <- 0 until LoadPipelineWidth){
    io.vecloadRegIn(i).bits  := DontCare
    io.vecloadRegIn(i).valid := loadRegIn_valid(i)
    io.vecloadRegIn(i).bits  := loadRegIn(i)
  }

  io.vecwriteback.map(_.ready := LFSR64(seed=Some(123L))(3,0) === 0.U)

}