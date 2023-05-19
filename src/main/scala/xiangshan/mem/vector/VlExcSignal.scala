package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._

class vlExcSignalBundle(implicit p: Parameters) extends XSBundle{
  val vecloadRegIn = Vec(2,Decoupled(new VecOperand()))
  val vecwriteback = Vec(2,Flipped(Decoupled(new ExuOutput(isVpu = true))))
  //val vecData = Vec(2,Flipped(Valid(UInt(VLEN.W))))
  val vecFeedback = Vec(2,Input(Bool()))
}


class VlExcSignal(implicit p: Parameters) extends XSModule{
  val io = IO(new vlExcSignalBundle)

  //dontTouch(io.vecData)
  //val data_0 = io.vecData(0).bits
  //val data_1 = io.vecData(1).bits
  dontTouch(io.vecwriteback)
  val loadRegIn = Wire(Vec(2,new VecOperand()))
  val loadRegIn_valid = Wire(Vec(2,Bool()))

  val s_idle::s_1::s_2::s_3::s_4::s_5::s_6::s_7::s_8::s_9::s_10::s_11::s_12::s_13::s_14::s_15::s_16::s_17::s_18::Nil = Enum(19)
  val state_0 = RegInit(s_idle)
  val state_1 = RegInit(s_idle)

  val counter = RegInit(0.U(10.W))

  loadRegIn(0) := DontCare
  loadRegIn(1) := DontCare
  loadRegIn_valid(0) := DontCare
  loadRegIn_valid(1) := DontCare

  counter := counter + 1.U

  //unit-stride
  // 1   1 =(0145)
  val robIdx_0 = 4.U
  val baseaddr_0 = 0x80000000L.U + 0x8.U
  val lqIdx_0 = 2.U
  val stride_0 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_0 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_0 = "b011".U // 8
  val sew_0 = "b001".U // 2
  val inner_idx_0 = 2.U
  val vl_0 = (16 * 4).U
  val total_num_0 = 3.U
  val instr_0 = 0x00000000.U // eew=1
  val robIdx_1 = 4.U
  val baseaddr_1 = 0x80000000L.U + 0x0008.U
  val lqIdx_1 = 1.U
  val stride_1 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_1 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_1 = "b011".U // 8
  val sew_1 = "b001".U // 2
  val inner_idx_1 = 1.U
  val vl_1 = (16 * 4).U
  val total_num_1 = 3.U
  val instr_1 = 0x00000000.U // eew=1
  // 1   1 !=(26)(37)
  val robIdx_2 = 5.U
  val baseaddr_2 = 0x80000000L.U + 0x10.U
  val lqIdx_2 = 5.U
  val stride_2 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_2 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_2 = "b000".U // 1
  val sew_2 = "b001".U   //2
  val inner_idx_2 = 1.U
  val vl_2 = (4 * 2).U
  val total_num_2 = 1.U
  val instr_2 = 0x00006000.U  // eew = 4
  val robIdx_3 = 6.U
  val baseaddr_3 = 0x80000000L.U + 0x4.U
  val lqIdx_3 = 7.U
  val stride_3 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_3 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_3 = "b010".U // 4
  val sew_3 = "b001".U   //2
  val inner_idx_3 = 1.U
  val vl_3 = (16 * 2).U
  val total_num_3 = 1.U
  val instr_3 = 0x00000000.U //eew = 1
  // 1   1 (0145)
  val robIdx_4 = 4.U
  val baseaddr_4 = 0x80000000L.U + 0x8.U
  val lqIdx_4 = 0.U
  val stride_4 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_4 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_4 = "b011".U // 8
  val sew_4 = "b001".U // 2
  val inner_idx_4 = 0.U
  val vl_4 = (16 * 4).U
  val total_num_4 = 3.U
  val instr_4 =0x00000000.U //eew = 1
  val robIdx_5 = 4.U
  val baseaddr_5 =0x80000000L.U + 0x8.U
  val lqIdx_5 = 3.U
  val stride_5 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_5 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_5 = "b011".U // 8
  val sew_5 = "b001".U // 2
  val inner_idx_5 = 3.U
  val vl_5 = (16 * 4).U
  val total_num_5 = 3.U
  val instr_5 = 0x00000000.U //eew = 1
  // 1   1(26)(37)
  val robIdx_6 = 5.U
  val baseaddr_6 = 0x80000000L.U + 0x10.U
  val lqIdx_6 = 4.U
  val stride_6 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_6 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_6 = "b000".U // 1
  val sew_6 = "b001".U   //2
  val inner_idx_6 = 0.U
  val vl_6 = (4 * 2).U
  val total_num_6 = 1.U
  val instr_6 = 0x00006000.U  // eew = 4
  val robIdx_7 = 6.U
  val baseaddr_7 = 0x80000000L.U + 0x4.U
  val lqIdx_7 = 6.U
  val stride_7 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_7 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_7 = "b010".U // 4
  val sew_7 = "b001".U   //2
  val inner_idx_7 = 0.U
  val vl_7 = (16 * 2).U
  val total_num_7 = 1.U
  val instr_7 = 0x00000000.U //eew = 1

  //stride
  // 1   1 =
  val robIdx_8 = 3.U
  val baseaddr_8 = 0x80000000L.U + 0x1000.U
  val lqIdx_8 = 8.U
  val stride_8 = 8.U
  val index_8 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_8 = "b011".U //8
  val sew_8 = "b011".U //8
  val inner_idx_8 = 0.U
  val vl_8 = 16.U
  val total_num_8 = 1.U
  val instr_8 = 0x08005000.U // eew = 2
  val robIdx_9 = 3.U
  val baseaddr_9 = 0x80000000L.U + 0x1000.U
  val lqIdx_9 = 9.U
  val stride_9 = 8.U
  val index_9 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_9 = "b011".U //8
  val sew_9 = "b011".U //8
  val inner_idx_9 = 1.U
  val vl_9 = 16.U
  val total_num_9 = 1.U
  val instr_9 = 0x08005000.U // eew = 2
  // 1   1 != (10 and 12 are a group)
  val robIdx_10 = 7.U
  val baseaddr_10 = 0x80000000L.U + 0x1003.U
  val lqIdx_10 = 10.U
  val stride_10 = 10.U
  val index_10 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_10 = "b011".U //8
  val sew_10 = "b010".U //4
  val inner_idx_10 = 0.U
  val vl_10 = (16 * 2).U
  val total_num_10 = 1.U
  val instr_10 = 0x08000000.U // eew = 1
  val robIdx_11 = 20.U
  val baseaddr_11 = 0x80000000L.U + 0x1020.U
  val lqIdx_11 = 11.U
  val stride_11 = 80.U
  val index_11 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_11 = "b000".U //1
  val sew_11 = "b011".U //8
  val inner_idx_11 = 0.U
  val vl_11 = 2.U
  val total_num_11 = 0.U
  val instr_11 = 0x08006000.U  //eew = 4
  // 1   0
  val robIdx_12 = 7.U
  val baseaddr_12 = 0x80000000L.U + 0x1003.U
  val lqIdx_12 = 12.U
  val stride_12 = 10.U
  val index_12 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_12 = "b011".U //8
  val sew_12 = "b010".U //4
  val inner_idx_12 = 1.U
  val vl_12 = (16 * 2).U
  val total_num_12 = 1.U
  val instr_12 = 0x08000000.U // eew = 1
  val robIdx_13 = 16.U
  val baseaddr_13 = 0x80000000L.U + 0x1005.U
  val lqIdx_13 = 13.U
  val stride_13 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_13 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_13 = "b010".U
  val sew_13 = "b001".U
  val inner_idx_13 = 1.U
  val vl_13 = (16 * 2).U
  val total_num_13 = 1.U
  val instr_13 = 0x08000000.U

  // index
  // 1   1 =
  val robIdx_14 = 13.U
  val baseaddr_14 = 0x80000000L.U
  val lqIdx_14 = 14.U
  val stride_14 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_14 = "h00000000005566778899aabbccddeeff".U
  val lmul_14 = "b010".U //4
  val sew_14 = "b001".U //2
  val inner_idx_14 = 0.U
  val vl_14 = (16 * 2).U
  val total_num_14 = 1.U
  val instr_14 = 0x0c000000.U //eew = 1
  val robIdx_15 = 13.U
  val baseaddr_15 = 0x80000000L.U
  val lqIdx_15 = 15.U
  val stride_15 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_15 = "h00112233445566778899aabbccddeeff".U
  val lmul_15 = "b010".U //4
  val sew_15 = "b001".U //2
  val inner_idx_15 = 1.U
  val vl_15 = (16 * 2).U
  val total_num_15 = 1.U
  val instr_15 = 0x0c000000.U //eew = 1
  // 1   1 !=      (16 and 18)
  val robIdx_16 = 23.U
  val baseaddr_16 = 0x80000000L.U
  val lqIdx_16 = 16.U
  val stride_16 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_16 = "h0123456789abcdef0123456789abcdef".U
  val lmul_16 = "b010".U // 4
  val sew_16 = "b001".U  // 2
  val inner_idx_16 = 0.U
  val vl_16 = (16 * 2).U
  val total_num_16 = 1.U
  val instr_16 = 0x0c000000.U //eew = 1
  val robIdx_17 = 27.U
  val baseaddr_17 = 0x80000000L.U
  val lqIdx_17 = 17.U
  val stride_17 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_17 = "h00110011001100110011001100110011".U
  val lmul_17 = "b111".U // 1/2
  val sew_17 = "b001".U //2
  val inner_idx_17 = 0.U
  val vl_17 = 4.U
  val total_num_17 = 0.U
  val instr_17 = 0x0c005000.U // eew = 2
  // 1   0
  val robIdx_18 = 23.U
  val baseaddr_18 = 0x80000000L.U
  val lqIdx_18 = 18.U
  val stride_18 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_18 = "h0123456789abcdef0123456789abcdef".U
  val lmul_18 = "b010".U // 4
  val sew_18 = "b001".U  // 2
  val inner_idx_18 = 1.U
  val vl_18 = (16 * 2).U
  val total_num_18 = 1.U
  val instr_18 = 0x0c000000.U //eew = 1
  val robIdx_19 = 16.U
  val baseaddr_19 = 0x80000000L.U
  val lqIdx_19 = 19.U
  val stride_19 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_19 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_19 = "b010".U
  val sew_19 = "b001".U
  val inner_idx_19 = 1.U
  val vl_19 = (16 * 2).U
  val total_num_19 = 1.U
  val instr_19 = 0x0c000000.U

  //segment unit-stride
  // 1  1 = (20-23)
  val robIdx_20 = 7.U
  val baseaddr_20 = 0x80000000L.U + 0x1011.U
  val lqIdx_20 = 20.U
  val stride_20 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_20 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_20 = "b011".U //8
  val sew_20 = "b010".U //4
  val inner_idx_20 = 0.U
  val vl_20 = (16 * 4).U
  val total_num_20 = 3.U
  val instr_20 = 0x20000000.U // nf = 2; eew = 1
  val robIdx_21 = 7.U
  val baseaddr_21 = 0x80000000L.U + 0x1011.U
  val lqIdx_21 = 21.U
  val stride_21 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_21 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_21 = "b011".U //8
  val sew_21 = "b010".U //4
  val inner_idx_21 = 1.U
  val vl_21 = (16 * 4).U
  val total_num_21 = 3.U
  val instr_21 = 0x20000000.U // nf = 2; eew = 1
  // 1  1 =
  val robIdx_22 = 7.U
  val baseaddr_22 = 0x80000000L.U + 0x1011.U
  val lqIdx_22 = 22.U
  val stride_22 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_22 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_22 = "b011".U //8
  val sew_22 = "b010".U //4
  val inner_idx_22 = 2.U
  val vl_22 = (16 * 4).U
  val total_num_22 = 3.U
  val instr_22 = 0x20000000.U // nf = 2; eew = 1
  val robIdx_23 = 7.U
  val baseaddr_23 = 0x80000000L.U + 0x1011.U
  val lqIdx_23 = 23.U
  val stride_23 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_23 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_23 = "b011".U //8
  val sew_23 = "b010".U //4
  val inner_idx_23 = 3.U
  val vl_23 = (16 * 4).U
  val total_num_23 = 3.U
  val instr_23 = 0x20000000.U // nf = 2; eew = 1
  // 1  1 =
  val robIdx_24 = 16.U
  val baseaddr_24 = 0x80000000L.U + 0x1005.U
  val lqIdx_24 = 24.U
  val stride_24 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_24 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_24 = "b001".U //2
  val sew_24 = "b010".U //4
  val inner_idx_24 = 0.U
  val vl_24 = (8 * 2).U
  val total_num_24 = 1.U
  val instr_24 = 0x20000000.U // nf = 2, eew = 1;
  val robIdx_25 = 16.U
  val baseaddr_25 = 0x80000000L.U + 0x1005.U
  val lqIdx_25 = 25.U
  val stride_25 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_25 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_25 = "b001".U //2
  val sew_25 = "b010".U //4
  val inner_idx_25 = 1.U
  val vl_25 = (8 * 2).U
  val total_num_25 = 1.U
  val instr_25 = 0x20000000.U // nf = 2, eew = 1;

  //segment strided
  // 1  1 =(26-28)
  val robIdx_26 = 29.U
  val baseaddr_26 = 0x80000000L.U + 0x1000.U
  val lqIdx_26 = 26.U
  val stride_26 = 2.U
  val index_26 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_26 = "b000".U //1
  val sew_26 = "b001".U //2
  val inner_idx_26 = 0.U
  val vl_26 = 24.U
  val total_num_26 = 2.U
  val instr_26 = 0x48005000.U //nf= 3; eew = 2
  val robIdx_27 = 29.U
  val baseaddr_27 = 0x80000000L.U + 0x1000.U
  val lqIdx_27 = 27.U
  val stride_27 = 2.U
  val index_27 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_27 = "b000".U //1
  val sew_27 = "b001".U //2
  val inner_idx_27 = 1.U
  val vl_27 = 24.U
  val total_num_27 = 2.U
  val instr_27 = 0x48005000.U //nf= 3; eew = 2
  // 1  1 !=
  val robIdx_28 = 29.U
  val baseaddr_28 = 0x80000000L.U + 0x1000.U
  val lqIdx_28 = 28.U
  val stride_28 = 2.U
  val index_28 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_28 = "b000".U //1
  val sew_28 = "b001".U //2
  val inner_idx_28 = 2.U
  val vl_28 = 24.U
  val total_num_28 = 2.U
  val instr_28 = 0x48005000.U //nf= 3; eew = 2
  //(29-30)
  val robIdx_29 = 16.U
  val baseaddr_29 = 0x80000000L.U + 0x1009.U
  val lqIdx_29 = 29.U
  val stride_29 = 4.U
  val index_29 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_29 = "b111".U // 1/2
  val sew_29 = "b001".U //2
  val inner_idx_29 = 0.U
  val vl_29 = 8.U
  val total_num_29 = 1.U
  val instr_29 = 0x28006000.U // nf=2; eew = 4
  // 1  0
  val robIdx_30 = 16.U
  val baseaddr_30 = 0x80000000L.U + 0x1009.U
  val lqIdx_30 = 30.U
  val stride_30 = 4.U
  val index_30 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_30 = "b111".U // 1/2
  val sew_30 = "b001".U //2
  val inner_idx_30 = 1.U
  val vl_30 = 8.U
  val total_num_30 = 1.U
  val instr_30 = 0x28006000.U // nf=2; eew = 4
  val robIdx_31 = 16.U
  val baseaddr_31 = 0x80000000L.U + 0x1005.U
  val lqIdx_31 = 31.U
  val stride_31 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_31 = Cat(LFSR64(seed = Some(999L)),LFSR64(seed = Some(888L)))(VLEN-1,0)
  val lmul_31 = "b010".U
  val sew_31 = "b001".U
  val inner_idx_31 = 1.U
  val vl_31 = (16 * 2).U
  val total_num_31 = 1.U
  val instr_31 = 0x00000000.U

  //segment index
  // 1  1 = (32-35)
  val robIdx_32 = 10.U
  val baseaddr_32 = 0x80000000L.U + 0x1070.U
  val lqIdx_32 = 32.U
  val stride_32 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_32 = "h00220022002200220022002200220022".U
  val lmul_32 = "b001".U //2
  val sew_32 = "b010".U //4
  val inner_idx_32 = 0.U
  val vl_32 = (8 * 4).U
  val total_num_32 = 3.U
  val instr_32 = 0x6c000000.U // nf = 4; eew = 1
  val robIdx_33 = 10.U
  val baseaddr_33 = 0x80000000L.U + 0x1070.U
  val lqIdx_33 = 33.U
  val stride_33 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_33 = "h00220022002200220022002200220022".U
  val lmul_33 = "b001".U //2
  val sew_33 = "b010".U //4
  val inner_idx_33 = 1.U
  val vl_33 = (8 * 4).U
  val total_num_33 = 3.U
  val instr_33 = 0x6c000000.U // nf = 4; eew = 1
  // 1  1 =
  val robIdx_34 = 10.U
  val baseaddr_34 = 0x80000000L.U + 0x1070.U
  val lqIdx_34 = 34.U
  val stride_34 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_34 = "h00220022002200220022002200220022".U
  val lmul_34 = "b001".U //2
  val sew_34 = "b010".U //4
  val inner_idx_34 = 2.U
  val vl_34 = (8 * 4).U
  val total_num_34 = 3.U
  val instr_34 = 0x6c000000.U // nf = 4; eew = 1
  val robIdx_35 = 10.U
  val baseaddr_35 = 0x80000000L.U + 0x1070.U
  val lqIdx_35 = 35.U
  val stride_35 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_35 = "h00220022002200220022002200220022".U
  val lmul_35 = "b001".U //2
  val sew_35 = "b010".U //4
  val inner_idx_35 = 3.U
  val vl_35 = (8 * 4).U
  val total_num_35 = 3.U
  val instr_35 = 0x6c000000.U // nf = 4; eew = 1
  // 1  1
  val robIdx_36 = 5.U
  val baseaddr_36 = 0x80000000L.U + 0x1000.U
  val lqIdx_36 = 36.U
  val stride_36 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_36 = "h11223344112233441122334411223344".U
  val lmul_36 = "b111".U // 1/2
  val sew_36 = "b010".U //4
  val inner_idx_36 = 0.U
  val vl_36 = 4.U
  val total_num_36 = 1.U
  val instr_36 = 0x2c006000.U // nf =2; eew= 4
  val robIdx_37 = 5.U
  val baseaddr_37 = 0x80000000L.U + 0x1000.U
  val lqIdx_37 = 37.U
  val stride_37 = LFSR64(seed = Some(3L))(XLEN-1,0)
  val index_37 = "h11223344112233441122334411223344".U
  val lmul_37 = "b111".U // 1/2
  val sew_37 = "b010".U //4
  val inner_idx_37 = 1.U
  val vl_37 = 4.U
  val total_num_37 = 1.U
  val instr_37 = 0x2c006000.U // nf =2; eew= 4


  switch (state_0) {
    is (s_idle) {
      when (counter === 100.U) {
        loadRegIn_valid(0)     := true.B
      }.otherwise {
        loadRegIn_valid(0)     := false.B
      }
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_0
      loadRegIn(0).stride    := stride_0
      loadRegIn(0).index     := index_0
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_0
      loadRegIn(0).sew       := sew_0
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_0
      loadRegIn(0).vl        := vl_0
      loadRegIn(0).total_num := total_num_0
      loadRegIn(0).uop.robIdx.value := robIdx_0
      loadRegIn(0).uop.lqIdx.value  := lqIdx_0
      loadRegIn(0).uop.cf.instr     := instr_0
      when (counter === 101.U) {
        state_0 := s_1
      }
    }
    is (s_1) {
      loadRegIn_valid(0)     := true.B
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_2
      loadRegIn(0).stride    := stride_2
      loadRegIn(0).index     := index_2
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_2
      loadRegIn(0).sew       := sew_2
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_2
      loadRegIn(0).vl        := vl_2
      loadRegIn(0).total_num := total_num_2
      loadRegIn(0).uop.robIdx.value := robIdx_2
      loadRegIn(0).uop.lqIdx.value  := lqIdx_2
      loadRegIn(0).uop.cf.instr     := instr_2
      state_0 := s_2
    }
    is (s_2) {
      when (counter === 200.U) {
        loadRegIn_valid(0)     := true.B
      }.otherwise {
        loadRegIn_valid(0)     := false.B
      }
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_4
      loadRegIn(0).stride    := stride_4
      loadRegIn(0).index     := index_4
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_4
      loadRegIn(0).sew       := sew_4
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_4
      loadRegIn(0).vl        := vl_4
      loadRegIn(0).total_num := total_num_4
      loadRegIn(0).uop.robIdx.value := robIdx_4
      loadRegIn(0).uop.lqIdx.value  := lqIdx_4
      loadRegIn(0).uop.cf.instr     := instr_4
      when (counter === 201.U) {
        state_0 := s_3
      }
    }
    is (s_3) {
      loadRegIn_valid(0)     := true.B
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_6
      loadRegIn(0).stride    := stride_6
      loadRegIn(0).index     := index_6
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_6
      loadRegIn(0).sew       := sew_6
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_6
      loadRegIn(0).vl        := vl_6
      loadRegIn(0).total_num := total_num_6
      loadRegIn(0).uop.robIdx.value := robIdx_6
      loadRegIn(0).uop.lqIdx.value  := lqIdx_6
      loadRegIn(0).uop.cf.instr     := instr_6
      state_0 := s_4
    }
    is (s_4) {
      when (counter === 300.U) {
        loadRegIn_valid(0)     := true.B
      }.otherwise {
        loadRegIn_valid(0)     := false.B
      }
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_8
      loadRegIn(0).stride    := stride_8
      loadRegIn(0).index     := index_8
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_8
      loadRegIn(0).sew       := sew_8
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_8
      loadRegIn(0).vl        := vl_8
      loadRegIn(0).total_num := total_num_8
      loadRegIn(0).uop.robIdx.value := robIdx_8
      loadRegIn(0).uop.lqIdx.value  := lqIdx_8
      loadRegIn(0).uop.cf.instr     := instr_8
      when (counter === 301.U) {
        state_0 := s_5
      }
    }
    is (s_5) {
      loadRegIn_valid(0)     := true.B
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_10
      loadRegIn(0).stride    := stride_10
      loadRegIn(0).index     := index_10
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_10
      loadRegIn(0).sew       := sew_10
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_10
      loadRegIn(0).vl        := vl_10
      loadRegIn(0).total_num := total_num_10
      loadRegIn(0).uop.robIdx.value := robIdx_10
      loadRegIn(0).uop.lqIdx.value  := lqIdx_10
      loadRegIn(0).uop.cf.instr     := instr_10
      state_0 := s_6
    }
    is (s_6) {
      when (counter === 400.U) {
        loadRegIn_valid(0)     := true.B
      }.otherwise {
        loadRegIn_valid(0)     := false.B
      }
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_12
      loadRegIn(0).stride    := stride_12
      loadRegIn(0).index     := index_12
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_12
      loadRegIn(0).sew       := sew_12
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_12
      loadRegIn(0).vl        := vl_12
      loadRegIn(0).total_num := total_num_12
      loadRegIn(0).uop.robIdx.value := robIdx_12
      loadRegIn(0).uop.lqIdx.value  := lqIdx_12
      loadRegIn(0).uop.cf.instr     := instr_12
      when (counter === 401.U) {
        state_0 := s_7
      }
    }
    is (s_7) {
      loadRegIn_valid(0)     := true.B
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_14
      loadRegIn(0).stride    := stride_14
      loadRegIn(0).index     := index_14
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_14
      loadRegIn(0).sew       := sew_14
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_14
      loadRegIn(0).vl        := vl_14
      loadRegIn(0).total_num := total_num_14
      loadRegIn(0).uop.robIdx.value := robIdx_14
      loadRegIn(0).uop.lqIdx.value  := lqIdx_14
      loadRegIn(0).uop.cf.instr     := instr_14
      state_0 := s_8
    }
    is (s_8) {
      when (counter === 500.U) {
        loadRegIn_valid(0)     := true.B
      }.otherwise {
        loadRegIn_valid(0)     := false.B
      }
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_16
      loadRegIn(0).stride    := stride_16
      loadRegIn(0).index     := index_16
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_16
      loadRegIn(0).sew       := sew_16
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_16
      loadRegIn(0).vl        := vl_16
      loadRegIn(0).total_num := total_num_16
      loadRegIn(0).uop.robIdx.value := robIdx_16
      loadRegIn(0).uop.lqIdx.value  := lqIdx_16
      loadRegIn(0).uop.cf.instr     := instr_16
      when (counter === 501.U) {
        state_0 := s_9
      }
    }
    is (s_9) {
      loadRegIn_valid(0)     := true.B
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_18
      loadRegIn(0).stride    := stride_18
      loadRegIn(0).index     := index_18
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_18
      loadRegIn(0).sew       := sew_18
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_18
      loadRegIn(0).vl        := vl_18
      loadRegIn(0).total_num := total_num_18
      loadRegIn(0).uop.robIdx.value := robIdx_18
      loadRegIn(0).uop.lqIdx.value  := lqIdx_18
      loadRegIn(0).uop.cf.instr     := instr_18
      state_0 := s_10
    }
    is (s_10) {
      when (counter === 600.U) {
        loadRegIn_valid(0)     := true.B
      }.otherwise {
        loadRegIn_valid(0)     := false.B
      }
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_20
      loadRegIn(0).stride    := stride_20
      loadRegIn(0).index     := index_20
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_20
      loadRegIn(0).sew       := sew_20
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_20
      loadRegIn(0).vl        := vl_20
      loadRegIn(0).total_num := total_num_20
      loadRegIn(0).uop.robIdx.value := robIdx_20
      loadRegIn(0).uop.lqIdx.value  := lqIdx_20
      loadRegIn(0).uop.cf.instr     := instr_20
      when (counter === 601.U) {
        state_0 := s_11
      }
    }
    is (s_11) {
      loadRegIn_valid(0)     := true.B
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_22
      loadRegIn(0).stride    := stride_22
      loadRegIn(0).index     := index_22
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_22
      loadRegIn(0).sew       := sew_22
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_22
      loadRegIn(0).vl        := vl_22
      loadRegIn(0).total_num := total_num_22
      loadRegIn(0).uop.robIdx.value := robIdx_22
      loadRegIn(0).uop.lqIdx.value  := lqIdx_22
      loadRegIn(0).uop.cf.instr     := instr_22
      state_0 := s_12
    }
    is (s_12) {
      when (counter === 700.U) {
        loadRegIn_valid(0)     := true.B
      }.otherwise {
        loadRegIn_valid(0)     := false.B
      }
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_24
      loadRegIn(0).stride    := stride_24
      loadRegIn(0).index     := index_24
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_24
      loadRegIn(0).sew       := sew_24
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_24
      loadRegIn(0).vl        := vl_24
      loadRegIn(0).total_num := total_num_24
      loadRegIn(0).uop.robIdx.value := robIdx_24
      loadRegIn(0).uop.lqIdx.value  := lqIdx_24
      loadRegIn(0).uop.cf.instr     := instr_24
      when (counter === 701.U) {
        state_0 := s_13
      }
    }
    is (s_13) {
      loadRegIn_valid(0)     := true.B
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_26
      loadRegIn(0).stride    := stride_26
      loadRegIn(0).index     := index_26
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_26
      loadRegIn(0).sew       := sew_26
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_26
      loadRegIn(0).vl        := vl_26
      loadRegIn(0).total_num := total_num_26
      loadRegIn(0).uop.robIdx.value := robIdx_26
      loadRegIn(0).uop.lqIdx.value  := lqIdx_26
      loadRegIn(0).uop.cf.instr     := instr_26
      state_0 := s_14
    }
    is (s_14) {
      when (counter === 800.U) {
        loadRegIn_valid(0)     := true.B
      }.otherwise {
        loadRegIn_valid(0)     := false.B
      }
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_28
      loadRegIn(0).stride    := stride_28
      loadRegIn(0).index     := index_28
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_28
      loadRegIn(0).sew       := sew_28
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_28
      loadRegIn(0).vl        := vl_28
      loadRegIn(0).total_num := total_num_28
      loadRegIn(0).uop.robIdx.value := robIdx_28
      loadRegIn(0).uop.lqIdx.value  := lqIdx_28
      loadRegIn(0).uop.cf.instr     := instr_28
      when (counter === 801.U) {
        state_0 := s_15
      }
    }
    is (s_15) {
      loadRegIn_valid(0)     := true.B
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_30
      loadRegIn(0).stride    := stride_30
      loadRegIn(0).index     := index_30
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_30
      loadRegIn(0).sew       := sew_30
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_30
      loadRegIn(0).vl        := vl_30
      loadRegIn(0).total_num := total_num_30
      loadRegIn(0).uop.robIdx.value := robIdx_30
      loadRegIn(0).uop.lqIdx.value  := lqIdx_30
      loadRegIn(0).uop.cf.instr     := instr_30
      state_0 := s_16
    }
    is (s_16) {
      when (counter === 900.U) {
        loadRegIn_valid(0)     := true.B
      }.otherwise {
        loadRegIn_valid(0)     := false.B
      }
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_32
      loadRegIn(0).stride    := stride_32
      loadRegIn(0).index     := index_32
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_32
      loadRegIn(0).sew       := sew_32
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_32
      loadRegIn(0).vl        := vl_32
      loadRegIn(0).total_num := total_num_32
      loadRegIn(0).uop.robIdx.value := robIdx_32
      loadRegIn(0).uop.lqIdx.value  := lqIdx_32
      loadRegIn(0).uop.cf.instr     := instr_32
      when (counter === 901.U) {
        state_0 := s_17
      }
    }
    is (s_17) {
      loadRegIn_valid(0)     := true.B
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_34
      loadRegIn(0).stride    := stride_34
      loadRegIn(0).index     := index_34
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_34
      loadRegIn(0).sew       := sew_34
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_34
      loadRegIn(0).vl        := vl_34
      loadRegIn(0).total_num := total_num_34
      loadRegIn(0).uop.robIdx.value := robIdx_34
      loadRegIn(0).uop.lqIdx.value  := lqIdx_34
      loadRegIn(0).uop.cf.instr     := instr_34
      state_0 := s_18
    }
    is (s_18) {
      when (counter === 1000.U) {
        loadRegIn_valid(0)     := true.B
      }.otherwise {
        loadRegIn_valid(0)     := false.B
      }
      //loadRegIn(0).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(0).baseaddr  := baseaddr_36
      loadRegIn(0).stride    := stride_36
      loadRegIn(0).index     := index_36
      loadRegIn(0).pvd       := LFSR64(seed = Some(110L))(4,0)
      loadRegIn(0).lmul      := lmul_36
      loadRegIn(0).sew       := sew_36
      //loadRegIn(0).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(0).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(0).inner_idx := inner_idx_36
      loadRegIn(0).vl        := vl_36
      loadRegIn(0).total_num := total_num_36
      loadRegIn(0).uop.robIdx.value := robIdx_36
      loadRegIn(0).uop.lqIdx.value  := lqIdx_36
      loadRegIn(0).uop.cf.instr     := instr_36
      when (counter === 1001.U) {
        state_0 := s_idle
      }
    }
  }



  switch (state_1) {
    is (s_idle) {
      when (counter === 100.U) {
        loadRegIn_valid(1)     := true.B
      }.otherwise {
        loadRegIn_valid(1)     := false.B
      }
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_1
      loadRegIn(1).stride    := stride_1
      loadRegIn(1).index     := index_1
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_1
      loadRegIn(1).sew       := sew_1
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_1
      loadRegIn(1).vl        := vl_1
      loadRegIn(1).total_num := total_num_1
      loadRegIn(1).uop.robIdx.value := robIdx_1
      loadRegIn(1).uop.lqIdx.value := lqIdx_1
      loadRegIn(1).uop.cf.instr     := instr_1
      when (counter === 101.U) {
        state_1 := s_1
      }
    }
    is (s_1) {
      loadRegIn_valid(1)     := true.B
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_3
      loadRegIn(1).stride    := stride_3
      loadRegIn(1).index     := index_3
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_3
      loadRegIn(1).sew       := sew_3
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_3
      loadRegIn(1).vl        := vl_3
      loadRegIn(1).total_num := total_num_3
      loadRegIn(1).uop.robIdx.value := robIdx_3
      loadRegIn(1).uop.lqIdx.value := lqIdx_3
      loadRegIn(1).uop.cf.instr     := instr_3
      state_1 := s_2
    }
    is (s_2) {
      when (counter === 200.U) {
        loadRegIn_valid(1)     := true.B
      }.otherwise {
        loadRegIn_valid(1)     := false.B
      }
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_5
      loadRegIn(1).stride    := stride_5
      loadRegIn(1).index     := index_5
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_5
      loadRegIn(1).sew       := sew_5
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_5
      loadRegIn(1).vl        := vl_5
      loadRegIn(1).total_num := total_num_5
      loadRegIn(1).uop.robIdx.value := robIdx_5
      loadRegIn(1).uop.lqIdx.value := lqIdx_5
      loadRegIn(1).uop.cf.instr     := instr_5
      when (counter === 201.U) {
        state_1 := s_3
      }
    }
    is (s_3) {
      loadRegIn_valid(1)     := true.B
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_7
      loadRegIn(1).stride    := stride_7
      loadRegIn(1).index     := index_7
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_7
      loadRegIn(1).sew       := sew_7
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_7
      loadRegIn(1).vl        := vl_7
      loadRegIn(1).total_num := total_num_7
      loadRegIn(1).uop.robIdx.value := robIdx_7
      loadRegIn(1).uop.lqIdx.value := lqIdx_7
      loadRegIn(1).uop.cf.instr     := instr_7
      state_1 := s_4
    }
    is (s_4) {
      when (counter === 300.U) {
        loadRegIn_valid(1)     := true.B
      }.otherwise {
        loadRegIn_valid(1)     := false.B
      }
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_9
      loadRegIn(1).stride    := stride_9
      loadRegIn(1).index     := index_9
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_9
      loadRegIn(1).sew       := sew_9
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_9
      loadRegIn(1).vl        := vl_9
      loadRegIn(1).total_num := total_num_9
      loadRegIn(1).uop.robIdx.value := robIdx_9
      loadRegIn(1).uop.lqIdx.value := lqIdx_9
      loadRegIn(1).uop.cf.instr     := instr_9
      when (counter === 301.U) {
        state_1 := s_5
      }
    }
    is (s_5) {
      loadRegIn_valid(1)     := true.B
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_11
      loadRegIn(1).stride    := stride_11
      loadRegIn(1).index     := index_11
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_11
      loadRegIn(1).sew       := sew_11
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_11
      loadRegIn(1).vl        := vl_11
      loadRegIn(1).total_num := total_num_11
      loadRegIn(1).uop.robIdx.value := robIdx_11
      loadRegIn(1).uop.lqIdx.value := lqIdx_11
      loadRegIn(1).uop.cf.instr     := instr_11
      state_1 := s_6
    }
    is (s_6) {
      when (counter === 400.U) {
        loadRegIn_valid(1)     := false.B
      }.otherwise {
        loadRegIn_valid(1)     := false.B
      }
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_13
      loadRegIn(1).stride    := stride_13
      loadRegIn(1).index     := index_13
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_13
      loadRegIn(1).sew       := sew_13
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_13
      loadRegIn(1).vl        := vl_13
      loadRegIn(1).total_num := total_num_13
      loadRegIn(1).uop.robIdx.value := robIdx_13
      loadRegIn(1).uop.lqIdx.value := lqIdx_13
      loadRegIn(1).uop.cf.instr     := instr_13
      when (counter === 401.U) {
        state_1 := s_7
      }
    }
    is (s_7) {
      loadRegIn_valid(1)     := true.B
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_15
      loadRegIn(1).stride    := stride_15
      loadRegIn(1).index     := index_15
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_15
      loadRegIn(1).sew       := sew_15
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_15
      loadRegIn(1).vl        := vl_15
      loadRegIn(1).total_num := total_num_15
      loadRegIn(1).uop.robIdx.value := robIdx_15
      loadRegIn(1).uop.lqIdx.value := lqIdx_15
      loadRegIn(1).uop.cf.instr     := instr_15
      state_1 := s_8
    }
    is (s_8) {
      when (counter === 500.U) {
        loadRegIn_valid(1)     := true.B
      }.otherwise {
        loadRegIn_valid(1)     := false.B
      }
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_17
      loadRegIn(1).stride    := stride_17
      loadRegIn(1).index     := index_17
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_17
      loadRegIn(1).sew       := sew_17
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_17
      loadRegIn(1).vl        := vl_17
      loadRegIn(1).total_num := total_num_17
      loadRegIn(1).uop.robIdx.value := robIdx_17
      loadRegIn(1).uop.lqIdx.value := lqIdx_17
      loadRegIn(1).uop.cf.instr     := instr_17
      when (counter === 501.U) {
        state_1 := s_9
      }
    }
    is (s_9) {
      loadRegIn_valid(1)     := false.B
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_19
      loadRegIn(1).stride    := stride_19
      loadRegIn(1).index     := index_19
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_19
      loadRegIn(1).sew       := sew_19
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_19
      loadRegIn(1).vl        := vl_19
      loadRegIn(1).total_num := total_num_19
      loadRegIn(1).uop.robIdx.value := robIdx_19
      loadRegIn(1).uop.lqIdx.value := lqIdx_19
      loadRegIn(1).uop.cf.instr     := instr_19
      state_1 := s_10
    }
    is (s_10) {
      when (counter === 600.U) {
        loadRegIn_valid(1)     := true.B
      }.otherwise {
        loadRegIn_valid(1)     := false.B
      }
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_21
      loadRegIn(1).stride    := stride_21
      loadRegIn(1).index     := index_21
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_21
      loadRegIn(1).sew       := sew_21
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_21
      loadRegIn(1).vl        := vl_21
      loadRegIn(1).total_num := total_num_21
      loadRegIn(1).uop.robIdx.value := robIdx_21
      loadRegIn(1).uop.lqIdx.value := lqIdx_21
      loadRegIn(1).uop.cf.instr     := instr_21
      when (counter === 601.U) {
        state_1 := s_11
      }
    }
    is (s_11) {
      loadRegIn_valid(1)     := true.B
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_23
      loadRegIn(1).stride    := stride_23
      loadRegIn(1).index     := index_23
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_23
      loadRegIn(1).sew       := sew_23
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_23
      loadRegIn(1).vl        := vl_23
      loadRegIn(1).total_num := total_num_23
      loadRegIn(1).uop.robIdx.value := robIdx_23
      loadRegIn(1).uop.lqIdx.value := lqIdx_23
      loadRegIn(1).uop.cf.instr     := instr_23
      state_1 := s_12
    }
    is (s_12) {
      when (counter === 700.U) {
        loadRegIn_valid(1)     := true.B
      }.otherwise {
        loadRegIn_valid(1)     := false.B
      }
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_25
      loadRegIn(1).stride    := stride_25
      loadRegIn(1).index     := index_25
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_25
      loadRegIn(1).sew       := sew_25
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_25
      loadRegIn(1).vl        := vl_25
      loadRegIn(1).total_num := total_num_25
      loadRegIn(1).uop.robIdx.value := robIdx_25
      loadRegIn(1).uop.lqIdx.value := lqIdx_25
      loadRegIn(1).uop.cf.instr     := instr_25
      when (counter === 701.U) {
        state_1 := s_13
      }
    }
    is (s_13) {
      loadRegIn_valid(1)     := true.B
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_27
      loadRegIn(1).stride    := stride_27
      loadRegIn(1).index     := index_27
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_27
      loadRegIn(1).sew       := sew_27
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_27
      loadRegIn(1).vl        := vl_27
      loadRegIn(1).total_num := total_num_27
      loadRegIn(1).uop.robIdx.value := robIdx_27
      loadRegIn(1).uop.lqIdx.value := lqIdx_27
      loadRegIn(1).uop.cf.instr     := instr_27
      state_1 := s_14
    }
    is (s_14) {
      when (counter === 800.U) {
        loadRegIn_valid(1)     := true.B
      }.otherwise {
        loadRegIn_valid(1)     := false.B
      }
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_29
      loadRegIn(1).stride    := stride_29
      loadRegIn(1).index     := index_29
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_29
      loadRegIn(1).sew       := sew_29
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_29
      loadRegIn(1).vl        := vl_29
      loadRegIn(1).total_num := total_num_29
      loadRegIn(1).uop.robIdx.value := robIdx_29
      loadRegIn(1).uop.lqIdx.value := lqIdx_29
      loadRegIn(1).uop.cf.instr     := instr_29
      when (counter === 801.U) {
        state_1 := s_15
      }
    }
    is (s_15) {
      loadRegIn_valid(1)     := false.B
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_31
      loadRegIn(1).stride    := stride_31
      loadRegIn(1).index     := index_31
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_31
      loadRegIn(1).sew       := sew_31
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_31
      loadRegIn(1).vl        := vl_31
      loadRegIn(1).total_num := total_num_31
      loadRegIn(1).uop.robIdx.value := robIdx_31
      loadRegIn(1).uop.lqIdx.value := lqIdx_31
      loadRegIn(1).uop.cf.instr     := instr_31
      state_1 := s_16
    }
    is (s_16) {
      when (counter === 900.U) {
        loadRegIn_valid(1)     := true.B
      }.otherwise {
        loadRegIn_valid(1)     := false.B
      }
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_33
      loadRegIn(1).stride    := stride_33
      loadRegIn(1).index     := index_33
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_33
      loadRegIn(1).sew       := sew_33
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_33
      loadRegIn(1).vl        := vl_33
      loadRegIn(1).total_num := total_num_33
      loadRegIn(1).uop.robIdx.value := robIdx_33
      loadRegIn(1).uop.lqIdx.value := lqIdx_33
      loadRegIn(1).uop.cf.instr     := instr_33
      when (counter === 901.U) {
        state_1 := s_17
      }
    }
    is (s_17) {
      loadRegIn_valid(1)     := true.B
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_35
      loadRegIn(1).stride    := stride_35
      loadRegIn(1).index     := index_35
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_35
      loadRegIn(1).sew       := sew_35
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_35
      loadRegIn(1).vl        := vl_35
      loadRegIn(1).total_num := total_num_35
      loadRegIn(1).uop.robIdx.value := robIdx_35
      loadRegIn(1).uop.lqIdx.value := lqIdx_35
      loadRegIn(1).uop.cf.instr     := instr_35
      state_1 := s_18
    }
    is (s_18) {
      when (counter === 1000.U) {
        loadRegIn_valid(1)     := true.B
      }.otherwise {
        loadRegIn_valid(1)     := false.B
      }
      //loadRegIn(1).vmask     := LFSR64(seed = Some(23L))(63,0)
      loadRegIn(1).baseaddr  := baseaddr_37
      loadRegIn(1).stride    := stride_37
      loadRegIn(1).index     := index_37
      loadRegIn(1).pvd       := LFSR64(seed = Some(11L))(4,0)
      loadRegIn(1).lmul      := lmul_37
      loadRegIn(1).sew       := sew_37
      //loadRegIn(1).vma       := LFSR64(seed = Some(52L))(5,0) === 0.U
      //loadRegIn(1).vta       := LFSR64(seed = Some(47L))(4,0) === 0.U
      loadRegIn(1).inner_idx := inner_idx_37
      loadRegIn(1).vl        := vl_37
      loadRegIn(1).total_num := total_num_37
      loadRegIn(1).uop.robIdx.value := robIdx_37
      loadRegIn(1).uop.lqIdx.value := lqIdx_37
      loadRegIn(1).uop.cf.instr     := instr_37
      when (counter === 1001.U) {
        state_1 := s_idle
      }
    }

  }

  for(i <- 0 until LoadPipelineWidth){
    io.vecloadRegIn(i).bits  := DontCare
    io.vecloadRegIn(i).valid := loadRegIn_valid(i)
    io.vecloadRegIn(i).bits  := loadRegIn(i)
  }

  io.vecwriteback.map(_.ready := true.B)
}