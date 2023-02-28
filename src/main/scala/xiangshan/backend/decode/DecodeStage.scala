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



class VConfigGen(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle(){
    // from Ibuffer
    val firstInstr = Input(UInt(32.W))
    // from DecodeUnit
    val isFirstVset = Input(Bool())

    val isVsetFlushPipe = Input(Bool())
    val vconfig = Input(new VConfig)
    val isRedirect = Input(Bool())
    val robCommits = Input(new RobCommitIO)

    val vconfigPre = Output(new VConfig)
    val vconfigNxt = Output(new VConfig)
  })

  val vconfig_arch = RegInit(0.U.asTypeOf(new VConfig))
  val vconfig_spec = RegInit(0.U.asTypeOf(new VConfig))
  // compute vlmaxs
  val vtype = io.firstInstr(27, 20).asTypeOf(new VType)
  val vlmul = vtype.vlmul
  val vsew  = vtype.vsew
  val vma   = vtype.vma
  val vta   = vtype.vta

  //  val avlImm = Cat(0.U(3.W), io.src1(14, 10))
  val vlLast = vconfig_spec.vl

  val rd = io.firstInstr(11, 7)
  val rs1 = io.firstInstr(19, 15)
  val vl = WireInit(0.U(8.W))

  val vconfig_spec_next = WireInit(0.U.asTypeOf(new VConfig))
  // vlen = 128
  val vlmaxVec = (0 to 7).map(i => if(i < 4) (16 << i).U(8.W) else (16 >> (8 - i)).U(8.W))
  val shamt = vlmul + (~vsew).asUInt + 1.U
  val vlmax = ParallelMux((0 to 7).map(_.U).map(_ === shamt), vlmaxVec)

  vl := Mux(rs1 =/= 0.U, Mux(rs1 > vlmax, vlmax, rs1),
    Mux(rd === 0.U, vlLast, vlmax))

  vconfig_spec_next.vl := vl
  vconfig_spec_next.vtype := vtype

  // vconfig update
  val isWalkVConfigVec = io.robCommits.walkValid.zip(io.robCommits.info).map{ case (valid, info) => valid && (info.ldest === 32.U)}
  val isCommitConfigVec = io.robCommits.commitValid.zip(io.robCommits.info).map{ case (valid, info) => valid && (info.ldest === 32.U)}
  when(io.isVsetFlushPipe){                                          // for vsetvl vsetvli
    vconfig_arch := io.vconfig
  }.elsewhen(io.robCommits.isCommit && Cat(isCommitConfigVec).orR){  // for vsetivli
    vconfig_arch := io.vconfig
  }

  when(io.isVsetFlushPipe){
    vconfig_spec := io.vconfig
  }.elsewhen(io.isRedirect){
    vconfig_spec := vconfig_arch
  }.elsewhen(io.robCommits.isWalk && Cat(isWalkVConfigVec).orR){     // for VconfigInst between rob.deq and redirectInst
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
    val vconfig = Input(new VConfig)
    val isRedirect = Input(Bool())
    // from rob
    val robCommits = Input(new RobCommitIO)
  })

  val decoderComp = Module(new DecodeUnitComp(10))
  val decoders = Seq.fill(DecodeWidth - 1)(Module(new DecodeUnit))
  val debug_globalCounter = RegInit(0.U(XLEN.W))
  val vconfigGen = Module(new VConfigGen)

  val isComplex = Wire(Vec(DecodeWidth - 1, Bool()))
  val cfComplex = Wire(Vec(DecodeWidth, new CfCtrl))
  val isFirstVset = Wire(Bool())
  val complexNum = Wire(UInt(3.W))

  val cfSimple = Wire(Vec(DecodeWidth - 1, new CfCtrl))

  //Comp 1
  decoderComp.io.enq.ctrl_flow := io.in(0).bits
  decoderComp.io.csrCtrl := io.csrCtrl
  decoderComp.io.vconfig := vconfigGen.io.vconfigPre
  decoderComp.io.isComplex := isComplex
  decoderComp.io.validFromIBuf.zip(io.in).map{ case (dst, src) => dst := src.valid}
  decoderComp.io.readyFromRename.zip(io.out).map{ case (dst, src) => dst := src.ready}
  cfComplex := decoderComp.io.deq.cf_ctrl
  io.out.zip(decoderComp.io.deq.validToRename).map{ case (dst, src) => dst.valid := src}
  io.in.zip(decoderComp.io.deq.readyToIBuf).map{ case (dst, src) => dst.ready := src}
  isFirstVset := decoderComp.io.deq.isVset
  complexNum := decoderComp.io.deq.complexNum

  //Simple 5
  decoders.zip(io.in.drop(1)).map { case (dst, src) => dst.io.enq.ctrl_flow := src.bits }
  decoders.map { case dst => dst.io.csrCtrl := io.csrCtrl }
  decoders.map { case dst => dst.io.vconfig := vconfigGen.io.vconfigNxt }
  isComplex.zip(decoders.map(_.io.deq.isComplex)).map{ case (dst, src) => dst := src}
  cfSimple.zip(decoders.map(_.io.deq.cf_ctrl)).map { case (dst, src) => dst := src }

  //vconfigGen
  vconfigGen.io.firstInstr := io.in(0).bits.instr
  // from DecodeUnit
  vconfigGen.io.isFirstVset := isFirstVset && io.in(0).valid
  vconfigGen.io.isVsetFlushPipe := io.isVsetFlushPipe
  vconfigGen.io.vconfig := io.vconfig
  vconfigGen.io.isRedirect := io.isRedirect
  vconfigGen.io.robCommits := io.robCommits


  //output
  io.out.zip(0 until RenameWidth).map { case (dst, i) => dst.bits := Mux(complexNum > i.U, cfComplex(i), cfSimple(i.U - complexNum)) }

  for (i <- 0 until DecodeWidth) {

    io.out(i).bits.ctrl.debug_globalID := debug_globalCounter + PopCount((0 until i+1).map(io.out(_).fire))
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
    io.vecRat(i)(0).addr := io.out(i).bits.ctrl.lsrc(0)
    io.vecRat(i)(1).addr := io.out(i).bits.ctrl.lsrc(1)
    io.vecRat(i)(2).addr := io.out(i).bits.ctrl.lsrc(2)
    io.vecRat(i)(3).addr := 0.U
    io.vecRat(i)(4).addr := io.out(i).bits.ctrl.ldest
    io.vecRat(i).foreach(_.hold := !io.out(i).ready)
  }

  val hasValid = VecInit(io.in.map(_.valid)).asUInt.orR

  debug_globalCounter := debug_globalCounter + PopCount(io.out.map(_.fire))

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
