package xiangshan.backend.fu.wrapper

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import utils.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.{Mgu, VecPipedFuncUnit}
import yunsuan.VfpuType
import yunsuan.vector.VectorConvert.VectorCvt


class VCVT(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "Vfcvt OpType not supported")

  // params alias
  private val dataWidth = cfg.dataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  // io alias
  private val opcode = fuOpType(7, 0)
  private val sew = vsew

  private val isRtz = opcode(2) & opcode(1)
  private val isRod = opcode(2) & !opcode(1) & opcode(0)
  private val isFrm = !isRtz && !isRod
  private val rm = Mux1H(
    Seq(isRtz, isRod, isFrm),
    Seq(1.U, 6.U, frm)
  )

  private val lmul = vlmul // -3->3 => 1/8 ->8

  val widen = opcode(4, 3) // 0->single 1->widen 2->norrow => width of result
  val isSingleCvt = !widen(1) & !widen(0)
  val isWidenCvt = !widen(1) & widen(0)
  val isNarrowCvt = widen(1) & !widen(0)

  // output width 8， 16， 32， 64
  val output1H = Wire(UInt(4.W))
  output1H := chisel3.util.experimental.decode.decoder(
    widen ## sew,
    TruthTable(
      Seq(
        BitPat("b00_01") -> BitPat("b0010"), // 16
        BitPat("b00_10") -> BitPat("b0100"), // 32
        BitPat("b00_11") -> BitPat("b1000"), // 64

        BitPat("b01_00") -> BitPat("b0010"), // 16
        BitPat("b01_01") -> BitPat("b0100"), // 32
        BitPat("b01_10") -> BitPat("b1000"), // 64

        BitPat("b10_00") -> BitPat("b0001"), // 8
        BitPat("b10_01") -> BitPat("b0010"), // 16
        BitPat("b10_10") -> BitPat("b0100"), // 32
      ),
      BitPat.N(4)
    )
  )
  dontTouch(output1H)
  val outputWidth1H = output1H

  val outEew = RegNext(RegNext(Mux1H(output1H, Seq(0,1,2,3).map(i => i.U))))
  private val needNoMask = outVecCtrl.fpu.isFpToVecInst
  val maskToMgu = Mux(needNoMask, allMaskTrue, outSrcMask)

  // modules
  private val vfcvt = Module(new VectorCvtTop(dataWidth, dataWidthOfDataModule))
  private val mgu = Module(new Mgu(dataWidth)) //看看那mgu里干了什么活

  val vs2Vec = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  vs2Vec := vs2.asTypeOf(vs2Vec)

  /**
   * [[vfcvt]]'s in connection
   */
  vfcvt.uopIdx := vuopIdx(0)
  vfcvt.src := vs2Vec
  vfcvt.opType := opcode
  vfcvt.sew := sew
  vfcvt.rm := rm
  vfcvt.outputWidth1H := outputWidth1H
  vfcvt.isWiden := isWidenCvt
  vfcvt.isNarrow := isNarrowCvt
  val vfcvtResult = vfcvt.io.result
  val vfcvtFflags = vfcvt.io.fflags

  /** fflags:
   */
  //uopidx：每一个uopidex:都有相应的元素个数以及对应的mask的位置
  //vl: 决定每个向量寄存器组里有多少个元素参与运算
  //  128/(sew+1)*8
  //  val num = 16.U >> sew
  //  val eNum = Mux(isWiden || isNorrow , num, num >> 1).asUInt //每个uopidx的向量元素的个数
  // num of vector element in each uopidx

  // 每个uopidx最大的元素个数，即一个向量寄存器所能容纳的Max(inwidth, outwidth)的元素的个数
  // single/narrow的话是一个向量寄存器所容纳输入元素的个数， widen的话是是一个向量寄存器输出元素的的个数
  // todo: 为什么用4bit，虽然一个Vector reg中的元素个数至少是2，但是存在lmul=1/2时 64->64的single，方便下面计算eNum的max(可能为1)
  val eNum1H = chisel3.util.experimental.decode.decoder(sew ## (isWidenCvt || isNarrowCvt),
    TruthTable(
      Seq(                     // 8, 4, 2, 1
        BitPat("b001") -> BitPat("b1000"), //8
        BitPat("b010") -> BitPat("b1000"), //8
        BitPat("b011") -> BitPat("b0100"), //4
        BitPat("b100") -> BitPat("b0100"), //4
        BitPat("b101") -> BitPat("b0010"), //2
        BitPat("b110") -> BitPat("b0010"), //2
      ),
      BitPat.N(4)
    )
  )
  val eNumMax1H = Mux(lmul.head(1).asBool, eNum1H >> ((~lmul.tail(1)).asUInt +1.U), eNum1H << lmul.tail(1)).asUInt(6, 0)
  val eNumMax = Mux1H(eNumMax1H, Seq(1,2,4,8,16,32,64).map(i => i.U)) //only for cvt intr, don't exist 128 in cvt
  val eNumEffectIdx = Mux(vl > eNumMax, eNumMax, vl)

  //  mask，vl和lmul => 某个输入的向量元素是否有效
  //  val numGrounp = lmul * 128/eew
  // val mask = Mux1H(eNum1H, Seq(1, 2, 4, 8).map(num => (srcMask >> vuopIdx * num.U)(num-1, 0))) //mask是从每个uop的最大元素个数来的
  val eNum = Mux1H(eNum1H, Seq(1, 2, 4, 8).map(num =>num.U))
  val eStart = vuopIdx * eNum
  val maskPart = srcMask >> eStart
  val mask =  Mux1H(eNum1H, Seq(1, 2, 4, 8).map(num => maskPart(num-1, 0)))
  val fflagsEn = Wire(Vec(4 * numVecModule, Bool()))

  //  fflagsEn := mask.asBools.zipWithIndex.map { case (mask, i) =>
  //    mask & (eNumEffect > Mux1H(eNum1H, Seq(1, 2, 4, 8).map(num => vuopIdx * num.U + i.U)))
  //  }
  //  vl: [0, vl)  eNumMax: [0, eNumMax) => eNumEffect为索引加1即length, 其右边的都为索引即length-1，所以 >
  fflagsEn := mask.asBools.zipWithIndex.map{case(mask, i) => mask & (eNumEffectIdx > eStart + i.U) } //被vl和lmul所约束

  val fflagsEnCycle2 = RegNext(RegNext(fflagsEn))
  val fflagsAll = Wire(Vec(8, UInt(5.W)))
  fflagsAll := vfcvtFflags.asTypeOf(fflagsAll)
  val fflags = fflagsEnCycle2.zip(fflagsAll).map{case(en, fflag) => Mux(en, fflag, 0.U(5.W))}.reduce(_ | _)
  io.out.bits.res.fflags.get := fflags


  /**
   * [[mgu]]'s in connection 后面看看是否必须要这个mgu
   */
  val resultDataUInt = Wire(UInt(dataWidth.W)) //todo: from vfcvt
  resultDataUInt := vfcvtResult

  mgu.io.in.vd := resultDataUInt
  mgu.io.in.oldVd := outOldVd
  mgu.io.in.mask := maskToMgu
  mgu.io.in.info.ta := outVecCtrl.vta
  mgu.io.in.info.ma := outVecCtrl.vma
  mgu.io.in.info.vl := Mux(outVecCtrl.fpu.isFpToVecInst, 1.U, outVl)
  mgu.io.in.info.vlmul := outVecCtrl.vlmul
  mgu.io.in.info.valid := io.out.valid
  mgu.io.in.info.vstart := Mux(outVecCtrl.fpu.isFpToVecInst, 0.U, outVecCtrl.vstart)
  mgu.io.in.info.eew := outEew
  mgu.io.in.info.vsew := outVecCtrl.vsew
  mgu.io.in.info.vdIdx := outVecCtrl.vuopIdx
  mgu.io.in.info.narrow := RegNext(RegNext(isNarrowCvt))
  mgu.io.in.info.dstMask := outVecCtrl.isDstMask

  io.out.bits.res.data := mgu.io.out.vd
}

class VectorCvtTopIO(vlen :Int, xlen: Int) extends Bundle{
  val uopIdx = Input(Bool())
  val src = Input(Vec(vlen / xlen, UInt(xlen.W)))
  val opType = Input(UInt(8.W))
  val sew = Input(UInt(2.W))
  val rm = Input(UInt(3.W))
  val outputWidth1H = Input(UInt(4.W))
  val isWiden = Input(Bool())
  val isNarrow = Input(Bool())

  val result = Output(UInt(vlen.W))
  val fflags = Output(UInt((vlen/16*5).W))
}



//according to uopindex, 1: high64 0:low64
class VectorCvtTop(vlen :Int, xlen: Int) extends Module{
  val io = IO(new VectorCvtTopIO(vlen, xlen))

  val (uopIdx, src, opType, sew, rm, outputWidth1H, isWiden, isNarrow) = (
    io.uopIdx, io.src, io.opType, io.sew, io.rm, io.outputWidth1H, io.isWiden, io.isNarrow
  )

  val in0 = Mux(isWiden,
    Mux(uopIdx, src(1).tail(32), src(0).tail(32)),
    src(0)
  )

  val in1 = Mux(isWiden,
    Mux(uopIdx, src(1).head(32), src(0).head(32)),
    src(1)
  )

  val vectorCvt0 = Module(new VectorCvt(xlen))
  vectorCvt0.src := in0
  vectorCvt0.opType := opType
  vectorCvt0.sew := sew
  vectorCvt0.rm := rm

  val vectorCvt1 = Module(new VectorCvt(xlen))
  vectorCvt1.src := in1
  vectorCvt1.opType := opType
  vectorCvt1.sew := sew
  vectorCvt1.rm := rm

  val isNarrowCycle2 = RegNext(RegNext(isNarrow))
  val outputWidth1HCycle2 = RegNext(RegNext(outputWidth1H))

  //cycle2
  io.result := Mux(isNarrowCycle2,
    vectorCvt1.io.result.tail(32) ## vectorCvt0.io.result.tail(32),
    vectorCvt1.io.result ## vectorCvt0.io.result)

  io.fflags := Mux1H(outputWidth1HCycle2, Seq( // todo: map between fflags and result
    vectorCvt1.io.fflags ## vectorCvt0.io.fflags,
    Mux(isNarrowCycle2, vectorCvt1.io.fflags.tail(10) ## vectorCvt0.io.fflags.tail(10), vectorCvt1.io.fflags ## vectorCvt0.io.fflags),
    Mux(isNarrowCycle2, vectorCvt1.io.fflags(4,0) ## vectorCvt0.io.fflags(4,0), vectorCvt1.io.fflags.tail(10) ## vectorCvt0.io.fflags.tail(10)),
    vectorCvt1.io.fflags(4,0) ## vectorCvt0.io.fflags(4,0)
  ))
}


