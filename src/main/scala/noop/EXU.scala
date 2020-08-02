package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils._
import bus.simplebus._
import noop.fu.FPU

class EXU(implicit val p: NOOPConfig) extends NOOPModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new DecodeIO))
    val out = Decoupled(new CommitIO)
    val flush = Input(Bool())
    val dmem = new SimpleBusUC(addrBits = VAddrBits)
    val forward = new ForwardIO
    val memMMU = Flipped(new MemMMUIO)
  })

  val src1 = io.in.bits.data.src1
  val src2 = io.in.bits.data.src2

  val (fuType, fuOpType) = (io.in.bits.ctrl.fuType, io.in.bits.ctrl.fuOpType)

  val fuValids = Wire(Vec(FuType.num, Bool()))
  (0 until FuType.num).map (i => fuValids(i) := (fuType === i.U) && io.in.valid && !io.flush)

  val alu = Module(new ALU)
  val aluOut = alu.access(valid = fuValids(FuType.alu), src1 = src1, src2 = src2, func = fuOpType)
  alu.io.cfIn := io.in.bits.cf
  alu.io.offset := io.in.bits.data.imm
  alu.io.out.ready := true.B

  val lsu = Module(new LSU)
  val lsuTlbPF = WireInit(false.B)
  val lsuOut = lsu.access(valid = fuValids(FuType.lsu), src1 = src1, src2 = io.in.bits.data.imm, func = fuOpType, dtlbPF = lsuTlbPF)
  lsu.io.wdata := src2
  lsu.io.instr := io.in.bits.cf.instr
  io.out.bits.isMMIO := lsu.io.isMMIO || (AddressSpace.isMMIO(io.in.bits.cf.pc) && io.out.valid)
  io.dmem <> lsu.io.dmem
  lsu.io.out.ready := true.B

  val mdu = Module(new MDU)
  val mduOut = mdu.access(valid = fuValids(FuType.mdu), src1 = src1, src2 = src2, func = fuOpType)
  mdu.io.out.ready := true.B

  val csr = Module(new CSR)
  val csrOut = csr.access(valid = fuValids(FuType.csr), src1 = src1, src2 = src2, func = fuOpType)
  csr.io.cfIn := io.in.bits.cf
  csr.io.cfIn.exceptionVec(loadAddrMisaligned) := lsu.io.loadAddrMisaligned
  csr.io.cfIn.exceptionVec(storeAddrMisaligned) := lsu.io.storeAddrMisaligned
  csr.io.instrValid := io.in.valid && !io.flush
  io.out.bits.intrNO := csr.io.intrNO
  csr.io.out.ready := true.B

  csr.io.imemMMU <> io.memMMU.imem
  csr.io.dmemMMU <> io.memMMU.dmem

  val mou = Module(new MOU)
  // mou does not write register
  mou.access(valid = fuValids(FuType.mou), src1 = src1, src2 = src2, func = fuOpType)
  mou.io.cfIn := io.in.bits.cf
  mou.io.out.ready := true.B

  val (fpuOut,fpuOutValid) = if(HasFPU){
    val fpu = Module(new FPU)
    Debug(){
      when(io.in.valid){
        printf(p"[EXU] at pc=${Hexadecimal(io.in.bits.cf.pc)} " +
          p"fpu in valid=${fpu.io.in.valid} " +
          p"fpu out valid=${fpu.io.out.valid}\n")
      }
    }
    fpu.io.out.ready := true.B
    csr.io.fpu_csr <> fpu.io.fpu_csr
    fpu.io.fpWen := io.in.bits.ctrl.fpWen
    fpu.io.inputFunc := io.in.bits.ctrl.fpInputFunc
    fpu.io.outputFunc := io.in.bits.ctrl.fpOutputFunc
    fpu.io.instr := io.in.bits.cf.instr
    (fpu.access(fuValids(FuType.fpu), src1, src2, io.in.bits.data.imm, io.in.bits.ctrl.fuOpType), fpu.io.out.valid)
  } else {
    csr.io.fpu_csr <> DontCare
    (0.U,false.B)
  }

  
  io.out.bits.decode := DontCare
  (io.out.bits.decode.ctrl, io.in.bits.ctrl) match { case (o, i) =>
    o.rfWen := i.rfWen && (!lsuTlbPF && !lsu.io.loadAddrMisaligned && !lsu.io.storeAddrMisaligned || !fuValids(FuType.lsu)) && !(csr.io.wenFix && fuValids(FuType.csr))
    o.rfDest := i.rfDest
    o.fuType := i.fuType
    o.fpWen := i.fpWen && (!lsuTlbPF && !lsu.io.loadAddrMisaligned && !lsu.io.storeAddrMisaligned || !fuValids(FuType.lsu)) && !(csr.io.wenFix && fuValids(FuType.csr))
  }
  io.out.bits.decode.cf.pc := io.in.bits.cf.pc

  io.out.bits.decode.cf.instr := io.in.bits.cf.instr
  io.out.bits.decode.cf.redirect <>
    Mux(mou.io.redirect.valid, mou.io.redirect,
      Mux(csr.io.redirect.valid, csr.io.redirect, alu.io.redirect))
  Debug(){
    //when(mou.io.redirect.valid || csr.io.redirect.valid || alu.io.redirect.valid){
      printf("[REDIRECT] inValid:%d mou %x csr %x alu %x \n", io.in.valid, mou.io.redirect.valid, csr.io.redirect.valid, alu.io.redirect.valid)
      printf("[REDIRECT] flush: %d mou %x csr %x alu %x\n", io.flush, mou.io.redirect.target, csr.io.redirect.target, alu.io.redirect.target)
    //}
  }

  // FIXME: should handle io.out.ready == false
  io.out.valid := io.in.valid && MuxLookup(fuType, true.B, List(
    FuType.lsu -> lsu.io.out.valid,
    FuType.mdu -> mdu.io.out.valid,
    FuType.fpu -> fpuOutValid
  ))

  io.out.bits.commits(FuType.alu) := aluOut
  io.out.bits.commits(FuType.lsu) := lsuOut
  io.out.bits.commits(FuType.csr) := csrOut
  io.out.bits.commits(FuType.mdu) := mduOut
  io.out.bits.commits(FuType.mou) := 0.U
  io.out.bits.commits(FuType.fpu) := fpuOut

  io.in.ready := !io.in.valid || io.out.fire()

  io.forward.valid := io.in.valid
  io.forward.wb.rfWen := io.in.bits.ctrl.rfWen
  io.forward.wb.fpWen := io.in.bits.ctrl.fpWen
  io.forward.wb.rfDest := io.in.bits.ctrl.rfDest
  io.forward.wb.rfData := Mux(alu.io.out.fire(), aluOut, lsuOut)
  io.forward.fuType := io.in.bits.ctrl.fuType

  val isBru = ALUOpType.isBru(fuOpType)
  BoringUtils.addSource(alu.io.out.fire() && !isBru, "perfCntCondMaluInstr")
  BoringUtils.addSource(alu.io.out.fire() && isBru, "perfCntCondMbruInstr")
  BoringUtils.addSource(lsu.io.out.fire(), "perfCntCondMlsuInstr")
  BoringUtils.addSource(mdu.io.out.fire(), "perfCntCondMmduInstr")
  BoringUtils.addSource(csr.io.out.fire(), "perfCntCondMcsrInstr")

  if (!p.FPGAPlatform) {
    val nooptrap = io.in.bits.ctrl.isNoopTrap && io.in.valid
    val cycleCnt = WireInit(0.U(XLEN.W))
    val instrCnt = WireInit(0.U(XLEN.W))

    BoringUtils.addSink(cycleCnt, "simCycleCnt")
    BoringUtils.addSink(instrCnt, "simInstrCnt")

    BoringUtils.addSource(nooptrap, "trapValid")
    BoringUtils.addSource(io.in.bits.data.src1, "trapCode")
    BoringUtils.addSource(io.in.bits.cf.pc, "trapPC")
    BoringUtils.addSource(cycleCnt, "trapCycleCnt")
    BoringUtils.addSource(instrCnt, "trapInstrCnt")
  }
}
