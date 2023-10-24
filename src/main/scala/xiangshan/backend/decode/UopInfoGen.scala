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

package xiangshan.backend.decode

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.Instructions
import freechips.rocketchip.util.uintToBitPat
import utils._
import utility._
import xiangshan.ExceptionNO.illegalInstr
import xiangshan._
import xiangshan.backend.fu.fpu.FPU
import xiangshan.backend.fu.FuType
import freechips.rocketchip.rocket.Instructions._
import xiangshan.backend.Bundles.{DecodedInst, StaticInst}
import xiangshan.backend.fu.vector.Bundles.{VType, VLmul, VSew}
import yunsuan.VpermType
import chisel3.util.experimental.decode.{QMCMinimizer, TruthTable, decoder}

class strdiedLSNumOfUopTable() extends Module {
  val src = IO(Input(UInt(5.W)))
  val out = IO(Output(UInt(4.W)))
  // strided load/store
  var combVemulNf : Seq[(Int, Int, Int)] = Seq()
  for (emul <- 0 until 4) {
    for (nf <- 0 until 8) {
      if ((1 << emul) * (nf + 1) <= 8) {
        combVemulNf :+= (emul, nf, (1 << emul) * (nf + 1))
      } else {
        combVemulNf :+= (emul, nf, 0)
      }
    }
  }
  out := decoder(QMCMinimizer, src, TruthTable(combVemulNf.map {
    case (emul, nf, uopNum) => (BitPat((emul << 3 | nf).U(5.W)), BitPat(uopNum.U(4.W)))
  }, BitPat.N(4)))
}

class indexedLSNumOfUopTable() extends Module {
  val src = IO(Input(UInt(7.W)))
  val out = IO(Output(UInt(7.W)))
  // strided load/store
  var combVemulNf : Seq[(Int, Int, Int, Int)] = Seq()
  for (emul <- 0 until 4) {
    for (lmul <- 0 until 4) {
      var max_mul = if (lmul > emul) lmul else emul
      for (nf <- 0 until 8) {
        if ((1 << lmul) * (nf + 1) <= 8) {    // indexed load/store must ensure that the lmul * nf is less or equal to 8
          combVemulNf :+= (emul, lmul, nf, (1 << max_mul) * (nf + 1))
        } else {
          combVemulNf :+= (emul, lmul, nf, 0)
        }
      }
    }
  }
  out := decoder(QMCMinimizer, src, TruthTable(combVemulNf.map {
    case (emul, lmul, nf, uopNum) => (BitPat((emul << 5 | lmul << 3 | nf).U(7.W)), BitPat(uopNum.U(7.W)))
  }, BitPat.N(7)))
}

class UopInfoGen (implicit p: Parameters) extends XSModule {
  val io = IO(new UopInfoGenIO)

  val stridedLSTable = Module(new strdiedLSNumOfUopTable)     // decoder for strided load/store
  val indexedLSTable = Module(new indexedLSNumOfUopTable)     // decoder for indexed load/store

  val typeOfSplit = io.in.preInfo.typeOfSplit
  val vsew = Cat(0.U(1.W), io.in.preInfo.vsew)
  val veew = Cat(0.U(1.W), io.in.preInfo.vwidth(1, 0))
  val vlmul = io.in.preInfo.vlmul
  val nf = io.in.preInfo.nf
  val isComplex = io.out.isComplex

  val lmul = MuxLookup(vlmul, 1.U(4.W), Array(
    "b001".U -> 2.U,
    "b010".U -> 4.U,
    "b011".U -> 8.U
  ))
  val simple_lmul = MuxLookup(vlmul, 0.U(2.W), Array(
    "b001".U -> 1.U,
    "b010".U -> 2.U,
    "b011".U -> 3.U
  ))

  val vemul: UInt = veew.asUInt + 1.U + vlmul.asUInt + ~vsew.asUInt

  val emul = MuxLookup(vemul, 1.U(4.W), Array(
    "b001".U -> 2.U,
    "b010".U -> 4.U,
    "b011".U -> 8.U
  ))                                                              //TODO : eew and emul illegal exception need to be handled
  val simple_emul = MuxLookup(vemul, 0.U(2.W), Array(
    "b001".U -> 1.U,
    "b010".U -> 2.U,
    "b011".U -> 3.U
  ))

  val numOfUopVslide = MuxLookup(vlmul, 1.U(log2Up(MaxUopSize + 1).W), Array(
    "b001".U -> 3.U,
    "b010".U -> 10.U,
    "b011".U -> 36.U
  ))
  val numOfUopVrgather = MuxLookup(vlmul, 1.U(log2Up(MaxUopSize + 1).W), Array(
    "b001".U -> 4.U,
    "b010".U -> 16.U,
    "b011".U -> 64.U
  ))
  val numOfUopVrgatherei16 = Mux((!vsew.orR) && (vlmul =/= "b011".U),
    Cat(numOfUopVrgather, 0.U(1.W)),
    numOfUopVrgather
  )
  val numOfUopVcompress = MuxLookup(vlmul, 1.U(4.W), Array(
    "b001".U -> 4.U,
    "b010".U -> 13.U,
    "b011".U -> 43.U
  ))
  val numOfUopVFRED = {
    // addTime include add frs1
     val addTime = MuxLookup(vlmul, 1.U(4.W), Array(
       VLmul.m2 -> 2.U,
       VLmul.m4 -> 4.U,
       VLmul.m8 -> 8.U,
     ))
    val foldLastVlmul = MuxLookup(vsew, "b000".U, Array(
      VSew.e16 -> VLmul.mf8,
      VSew.e32 -> VLmul.mf4,
      VSew.e64 -> VLmul.mf2,
    ))
    // lmul < 1, foldTime = vlmul - foldFastVlmul
    // lmul >= 1, foldTime = 0.U - foldFastVlmul
    val foldTime = Mux(vlmul(2), vlmul, 0.U) - foldLastVlmul
    addTime + foldTime
  }
  val numOfUopVFREDOSUM = {
    val uvlMax = MuxLookup(vsew, 0.U, Array(
      VSew.e16 -> 8.U,
      VSew.e32 -> 4.U,
      VSew.e64 -> 2.U,
    ))
    val vlMax = Wire(UInt(7.W))
    vlMax := Mux(vlmul(2), uvlMax >> (-vlmul)(1,0), uvlMax << vlmul(1,0)).asUInt
    vlMax
  }

  stridedLSTable.src := Cat(simple_emul, nf)
  val numOfUopVLoadStoreStrided = stridedLSTable.out
  indexedLSTable.src := Cat(simple_emul, simple_lmul, nf)
  val numOfUopVLoadStoreIndexed = indexedLSTable.out

  //number of uop
  val numOfUop = MuxLookup(typeOfSplit, 1.U(log2Up(MaxUopSize + 1).W), Array(
    UopSplitType.VEC_0XV -> 2.U,
    UopSplitType.VEC_VVV -> lmul,
    UopSplitType.VEC_VFV -> lmul,
    UopSplitType.VEC_EXT2 -> lmul,
    UopSplitType.VEC_EXT4 -> lmul,
    UopSplitType.VEC_EXT8 -> lmul,
    UopSplitType.VEC_VVM -> lmul,
    UopSplitType.VEC_VFM -> lmul,
    UopSplitType.VEC_VFRED -> numOfUopVFRED,
    UopSplitType.VEC_VFREDOSUM -> numOfUopVFREDOSUM,
    UopSplitType.VEC_VXM -> (lmul +& 1.U),
    UopSplitType.VEC_VXV -> (lmul +& 1.U),
    UopSplitType.VEC_VFW -> Cat(lmul, 0.U(1.W)), // lmul <= 4
    UopSplitType.VEC_WFW -> Cat(lmul, 0.U(1.W)), // lmul <= 4
    UopSplitType.VEC_VVW -> Cat(lmul, 0.U(1.W)), // lmul <= 4
    UopSplitType.VEC_WVW -> Cat(lmul, 0.U(1.W)), // lmul <= 4
    UopSplitType.VEC_VXW -> Cat(lmul, 1.U(1.W)), // lmul <= 4
    UopSplitType.VEC_WXW -> Cat(lmul, 1.U(1.W)), // lmul <= 4
    UopSplitType.VEC_WVV -> Cat(lmul, 0.U(1.W)), // lmul <= 4
    UopSplitType.VEC_WXV -> Cat(lmul, 1.U(1.W)), // lmul <= 4
    UopSplitType.VEC_SLIDE1UP -> (lmul +& 1.U),
    UopSplitType.VEC_FSLIDE1UP -> lmul,
    UopSplitType.VEC_SLIDE1DOWN -> Cat(lmul, 0.U(1.W)),
    UopSplitType.VEC_FSLIDE1DOWN -> (Cat(lmul, 0.U(1.W)) - 1.U),
    UopSplitType.VEC_VRED -> lmul,
    UopSplitType.VEC_SLIDEUP -> (numOfUopVslide + 1.U),
    UopSplitType.VEC_ISLIDEUP -> numOfUopVslide,
    UopSplitType.VEC_SLIDEDOWN -> (numOfUopVslide + 1.U),
    UopSplitType.VEC_ISLIDEDOWN -> numOfUopVslide,
    UopSplitType.VEC_M0X -> (lmul +& 1.U),
    UopSplitType.VEC_MVV -> (Cat(lmul, 0.U(1.W)) - 1.U),
    UopSplitType.VEC_M0X_VFIRST -> 2.U,
    UopSplitType.VEC_VWW -> Cat(lmul, 0.U(1.W)),
    UopSplitType.VEC_RGATHER -> numOfUopVrgather,
    UopSplitType.VEC_RGATHER_VX -> (numOfUopVrgather +& 1.U),
    UopSplitType.VEC_RGATHEREI16 -> numOfUopVrgatherei16,
    UopSplitType.VEC_US_LDST -> (numOfUopVLoadStoreStrided +& 1.U),   // with one move instruction
    UopSplitType.VEC_S_LDST -> (numOfUopVLoadStoreStrided +& 2.U),    // with two move instructions
    UopSplitType.VEC_I_LDST -> (numOfUopVLoadStoreIndexed +& 1.U),
  ))

  isComplex := (numOfUop > 1.U) || (typeOfSplit === UopSplitType.DIR)
  io.out.uopInfo.numOfUop := numOfUop
  io.out.uopInfo.lmul := lmul

}

class UopInfoGenIO(implicit p: Parameters) extends XSBundle {
  val in = new Bundle {
    val preInfo = Input(new PreInfo)
  }
  val out = new Bundle {
    val isComplex = Output(Bool())
    val uopInfo = Output(new UopInfo)
  }
}

class PreInfo(implicit p: Parameters) extends XSBundle {
  val typeOfSplit = UopSplitType()
  val vsew = VSew()          //2 bit
  val vlmul = VLmul()
  val vwidth = UInt(3.W)     //eew
  val nf = UInt(3.W)
}

class UopInfo(implicit p: Parameters) extends XSBundle {
  val numOfUop = UInt(log2Up(MaxUopSize + 1).W)
  val lmul = UInt(4.W)
}