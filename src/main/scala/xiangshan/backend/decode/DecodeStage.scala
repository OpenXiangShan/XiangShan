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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import xiangshan.backend.rename.RatReadPort

class CfCtrlReorder(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle(){
    // from ibuffer
    val ibufValid = Vec(DecodeWidth, Input(Bool()))
    // to   ibuffer
    val ibufReady = Vec(DecodeWidth, Output(Bool()))
    // from DecodeUnit s
    val isVset    = Vec(DecodeWidth, Input(Bool()))
    val decodeRes = Vec(DecodeWidth, Input(new CfCtrl))
    // to Rename
    val out       = Vec(RenameWidth, DecoupledIO(new CfCtrl))
  })
  // vset split
  val vsetUop0 = Wire(new CfCtrl)
  val vsetUop1 = Wire(new CfCtrl)
  vsetUop0 := io.decodeRes(0)
  vsetUop1 := io.decodeRes(0)

  // modify uop0
  vsetUop0.ctrl.uopIdx := 0.U
  vsetUop0.ctrl.flushPipe := false.B
  vsetUop0.ctrl.fuOpType := ALUOpType.vsetExchange(io.decodeRes(0).ctrl.fuOpType)
  // modify uop1
  vsetUop1.ctrl.ldest := 32.U

  val isVsetvli = FuType.isIntExu(io.decodeRes(0).ctrl.fuType) && ALUOpType.isVsetvli(io.decodeRes(0).ctrl.fuOpType)
  val rs1NotZero = io.decodeRes(0).ctrl.lsrc(0).orR
  when(isVsetvli && rs1NotZero){
    vsetUop1.ctrl.flushPipe := true.B
  }


  val firstIsVset = io.isVset(0) && io.ibufValid(0)
  // for io.out.bits
  io.out.map(_.bits).zip(io.decodeRes.zip((vsetUop0 +: vsetUop1 +:io.decodeRes.drop(1)).take(DecodeWidth))).map{ case(dst, (src0, src1)) => dst := Mux(firstIsVset, src1, src0)}

  val isVsetVec = io.isVset.zip(io.ibufValid).map(x => x._1 && x._2)
  val canAcceptVec = true.B +: (1 until DecodeWidth).map(i => !(Cat(isVsetVec.drop(1).take(i)).orR))

  // for io.out.valid
  val realValid = io.ibufValid.zip(canAcceptVec).map(x => x._1 && x._2)
  io.out.map(_.valid).zip(realValid.zip(true.B +: true.B +:realValid.drop(1).take(DecodeWidth - 2))).map{ case(dst, (src0, src1)) => dst := Mux(firstIsVset, src1, src0)}

  // for io.ibufReady
  val realReady = io.out.map(_.ready).zip(canAcceptVec).map(x => x._1 && x._2)
  val spRealReady = (Cat(io.out.map(_.ready).take(2)).andR +: io.out.map(_.ready).drop(2) :+ false.B).zip(canAcceptVec).map(x => x._1 && x._2)
  io.ibufReady.zip(realReady.zip(spRealReady)).map{ case(dst, (src0, src1)) => dst := Mux(firstIsVset, src1, src0)}
}

class VConfigGen(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle(){
    // from Ibuffer
    val firstInstr = Input(UInt(32.W))
    // from DecodeUnit
    val isFirstVset = Input(Bool())

    val isVsetFlushPipe = Input(Bool())
    val vconfig = Input(UInt(XLEN.W))
    val isRedirect = Input(Bool())
    val robCommits = Input(new RobCommitIO)

    val vconfigPre = Output(UInt(8.W))
    val vconfigNxt = Output(UInt(8.W))
  })
  val vconfig_arch = RegInit(0.U(XLEN.W))
  val vconfig_spec = RegInit(0.U(XLEN.W))

  // compute vlmaxs
  val vtype = io.firstInstr(27, 20)
  val vlmul = vtype(2, 0)
  val vsew  = vtype(5, 3)
  val vma   = vtype(6)
  val vta   = vtype(7)

//  val avlImm = Cat(0.U(3.W), io.src1(14, 10))
  val vlLast = vconfig_spec(15, 8)

  val rd = io.firstInstr(11, 7)
  val rs1 = io.firstInstr(19, 15)
  val vl = WireInit(0.U(8.W))
  val vconfig_spec_next = WireInit(0.U(XLEN.W))

  // vlen = 128
  val vlmaxVec = (0 to 7).map(i => if(i < 4) (16 << i).U(8.W) else (16 >> (8 - i)).U(8.W))
  val shamt = vlmul + (~vsew).asUInt + 1.U
  val vlmax = ParallelMux((0 to 7).map(_.U).map(_ === shamt), vlmaxVec)

  vl := Mux(rs1 =/= 0.U, Mux(rs1 > vlmax, vlmax, rs1),
          Mux(rd === 0.U, vlLast, vlmax))

  vconfig_spec_next := Cat(0.U(48.W), vl, vtype)

  // vconfig update
  val isWalkVConfigVec = io.robCommits.walkValid.zip(io.robCommits.info).map{ case (valid, info) => valid && (info.ldest === 32.U)}
  when(io.isVsetFlushPipe){
    vconfig_arch := io.vconfig
  }.elsewhen(io.robCommits.isWalk && Cat(isWalkVConfigVec).orR){
    vconfig_arch := PriorityMux(isWalkVConfigVec.zip(io.robCommits.info.reverse)).vconfig
  }

  when(io.isVsetFlushPipe){
    vconfig_spec := io.vconfig
  }.elsewhen(io.isRedirect){
    vconfig_spec := vconfig_arch
  }.elsewhen(io.robCommits.isWalk && Cat(isWalkVConfigVec).orR){
    vconfig_spec := PriorityMux(isWalkVConfigVec.zip(io.robCommits.info.reverse)).vconfig
  }.elsewhen(io.isFirstVset){
    vconfig_spec := vconfig_spec_next
  }

  io.vconfigPre := vconfig_spec
  io.vconfigNxt := Mux(io.isFirstVset, vconfig_spec_next, vconfig_spec)
}

class DecodeStage(implicit p: Parameters) extends XSModule with HasPerfEvents {
  val io = IO(new Bundle() {
    // from Ibuffer
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new CtrlFlow)))
    // to Rename
    val out = Vec(DecodeWidth, DecoupledIO(new CfCtrl))
    // RAT read
    val intRat = Vec(RenameWidth, Vec(3, Flipped(new RatReadPort)))
    val fpRat = Vec(RenameWidth, Vec(4, Flipped(new RatReadPort)))
    val vecRat = Vec(RenameWidth, Vec(5, Flipped(new RatReadPort)))
    // csr control
    val csrCtrl = Input(new CustomCSRCtrlIO)
    // perf only
    val fusion = Vec(DecodeWidth - 1, Input(Bool()))

    val isVsetFlushPipe = Input(Bool())
    val vconfig = Input(UInt(XLEN.W))
    val isRedirect = Input(Bool())
    // from rob
    val robCommits = Input(new RobCommitIO)
  })

  val decoders = Seq.fill(DecodeWidth)(Module(new DecodeUnit))

  val cfCtrlReorder = Module(new CfCtrlReorder)
  cfCtrlReorder.io.ibufValid.zip(io.in).map(x => x._1 := x._2.valid)
  cfCtrlReorder.io.ibufReady.zip(io.in).map(x => x._2.ready := x._1)
  cfCtrlReorder.io.decodeRes.zip(decoders).map(x => x._1 := x._2.io.deq.cf_ctrl)
  cfCtrlReorder.io.isVset.zip(decoders).map(x => x._1 := x._2.io.deq.isVset)
  cfCtrlReorder.io.out.zip(io.out).map(x => x._2 <> x._1)

  val vconfigGen = Module(new VConfigGen)
  vconfigGen.io.firstInstr := io.in(0).bits.instr
  // from DecodeUnit
  vconfigGen.io.isFirstVset := decoders(0).io.deq.isVset && io.in(0).valid
  vconfigGen.io.isVsetFlushPipe := io.isVsetFlushPipe
  vconfigGen.io.vconfig := io.vconfig
  vconfigGen.io.isRedirect := io.isRedirect
  vconfigGen.io.robCommits := io.robCommits

  for (i <- 0 until DecodeWidth) {
    decoders(i).io.enq.ctrl_flow <> io.in(i).bits

    // csr control
    decoders(i).io.csrCtrl := io.csrCtrl

    if(i == 0){
      decoders(i).io.vconfig := vconfigGen.io.vconfigPre
    }else{
      decoders(i).io.vconfig := vconfigGen.io.vconfigNxt
    }

    // We use the lsrc/ldest before fusion decoder to read RAT for better timing.
    io.intRat(i)(0).addr := io.out(i).bits.ctrl.lsrc(0)
    io.intRat(i)(1).addr := io.out(i).bits.ctrl.lsrc(1)
    io.intRat(i)(2).addr := io.out(i).bits.ctrl.ldest
    io.intRat(i).foreach(_.hold := !io.out(i).ready)

    // Floating-point instructions can not be fused now.
    io.fpRat(i)(0).addr := io.out(i).bits.ctrl.lsrc(0)
    io.fpRat(i)(1).addr := io.out(i).bits.ctrl.lsrc(1)
    io.fpRat(i)(2).addr := io.out(i).bits.ctrl.lsrc(2)
    io.fpRat(i)(3).addr := io.out(i).bits.ctrl.ldest
    io.fpRat(i).foreach(_.hold := !io.out(i).ready)

    // Vec instructions
    // TODO: vec uop dividers need change this
    io.vecRat(i)(0).addr := decoders(i).io.deq.cf_ctrl.ctrl.lsrc(0)
    io.vecRat(i)(1).addr := decoders(i).io.deq.cf_ctrl.ctrl.lsrc(1)
    io.vecRat(i)(2).addr := decoders(i).io.deq.cf_ctrl.ctrl.lsrc(2)
    io.vecRat(i)(3).addr := decoders(i).io.deq.cf_ctrl.ctrl.ldest
    io.vecRat(i)(4).addr := 0.U
    io.vecRat(i).foreach(_.hold := !io.out(i).ready)
  }

  val hasValid = VecInit(io.in.map(_.valid)).asUInt.orR
  XSPerfAccumulate("utilization", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("waitInstr", PopCount((0 until DecodeWidth).map(i => io.in(i).valid && !io.in(i).ready)))
  XSPerfAccumulate("stall_cycle", hasValid && !io.out(0).ready)

  if (env.EnableTopDown) {
    XSPerfAccumulate("slots_issued", PopCount(io.out.map(_.fire)))
    XSPerfAccumulate("decode_bubbles", PopCount(io.out.map(x => !x.valid && x.ready))) // Unutilized issue-pipeline slots while there is no backend-stall
    XSPerfAccumulate("fetch_bubbles", PopCount((0 until DecodeWidth).map(i => !io.in(i).valid && io.in(i).ready))) //slots
    XSPerfAccumulate("ifu2id_allNO_cycle", VecInit((0 until DecodeWidth).map(i => !io.in(i).valid && io.in(i).ready)).asUInt.andR)
  }

  val fusionValid = RegNext(io.fusion)
  val inFire = io.in.map(in => RegNext(in.valid && !in.ready))
  val perfEvents = Seq(
    ("decoder_fused_instr", PopCount(fusionValid)       ),
    ("decoder_waitInstr",   PopCount(inFire)            ),
    ("decoder_stall_cycle", hasValid && !io.out(0).ready),
    ("decoder_utilization", PopCount(io.in.map(_.valid))),
  )
  generatePerfEvent()
}
