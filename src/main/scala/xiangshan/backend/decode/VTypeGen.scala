package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import top.ArgParser
import utility.PriorityMuxDefault
import xiangshan._
import xiangshan.backend.decode.isa.Instructions
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.vector.Decoder.VSetFuncUnit
import xiangshan.backend.vector.util.Verilog


class VTypeGen(implicit p: Parameters) extends XSModule {
  import VTypeGen._

  val in = IO(Input(new In))
  val out = IO(Output(new Out))

  private val vtypeSpec = RegInit(VType.init())

  private val vtypeArch = RegInit(VType.init())

  private val vtypeSpecNext = WireInit(vtypeSpec)

  private val vtypeArchNext = WireInit(vtypeArch)
  /** instructions code */
  private val instFieldVec = in.insts.map(_.bits.asTypeOf(new XSInstBitFields))

  private val isVsetivli = VecInit(in.insts.map { inst => inst.valid && Instructions.VSETIVLI === inst.bits })
  private val isVsetvli = VecInit(in.insts.map { inst => inst.valid && Instructions.VSETVLI === inst.bits })

  private val isVsetiVec: Vec[Bool] = VecInit(isVsetivli zip isVsetvli map { case(l, r) => l || r })

  private val vsetModuleVec = Seq.fill(DecodeWidth)(Module(new VSetFuncUnit(vlen = VLEN, elen = ELEN, xlen = XLEN)))
  private val oldVTypeVec = Wire(Vec(DecodeWidth, chiselTypeOf(vsetModuleVec.head.in.oldVType.bits)))
  for ((vsetModule, i) <- vsetModuleVec.zipWithIndex) {
    vsetModule.in.vsetvlVType.valid := false.B // don't handle vsetvl in VTypeGen
    vsetModule.in.vsetvliVType.valid := isVsetvli(i)
    vsetModule.in.vsetivliVType.valid := isVsetivli(i)
    vsetModule.in.vsetvlVType.bits := DontCare
    vsetModule.in.vsetvliVType.bits := instFieldVec(i).ZIMM_VSETVLI
    vsetModule.in.vsetivliVType.bits := instFieldVec(i).ZIMM_VSETIVLI
    vsetModule.in.readVl.valid := false.B
    vsetModule.in.readVl.bits := DontCare
    vsetModule.in.rdIsZero := instFieldVec(i).RD === 0.U
    vsetModule.in.rs1IsZero := instFieldVec(i).RS1 === 0.U
    vsetModule.in.oldVType.valid := true.B
    vsetModule.in.oldVType.bits := oldVTypeVec(i)
    vsetModule.in.vlFromGp.valid := false.B
    vsetModule.in.vlFromGp.bits := DontCare
    vsetModule.in.vlFromImm.valid := isVsetivli(i)
    vsetModule.in.vlFromImm.bits := instFieldVec(i).UIMM_VSETIVLI
    vsetModule.in.vlFromVl.valid := false.B
    vsetModule.in.vlFromVl.bits := DontCare
    vsetModule.in.vill := false.B
  }

  private val vtypeNewVec = vsetModuleVec.map(_.out.vtype)

  private val specvtype = vtypeSpec +: out.vtype
  out.specvtype := specvtype.take(out.specvtype.length)
  // select vtype for each instruction in a priority manner
  for(i <- 0 until DecodeWidth) {
    out.vtype(i) := PriorityMuxDefault((isVsetiVec zip vtypeNewVec).take(i + 1).reverse, vtypeSpec)
  }

  oldVTypeVec := specvtype.take(oldVTypeVec.length)

  // assign the last vtype to vtypeSpec
  private val vtypeNew = out.vtype(DecodeWidth - 1)

  vtypeSpec := vtypeSpecNext
  vtypeArch := vtypeArchNext

  when(in.commitVType.hasVsetvl) {
    vtypeArchNext := in.vsetvlVType
  } .elsewhen(in.commitVType.vtype.valid) {
    vtypeArchNext := in.commitVType.vtype.bits
  }

  /**
   * Set the source of vtypeSpec from the following sources:
   * 1. committed vsetvl instruction, which flushes the pipeline.
   * 2. walk-vtype, which is used to update vtype when walking.
   * 3. walking to architectural vtype
   * 4. new vset instruction
   */
  when(in.commitVType.hasVsetvl) {
    // when vsetvl instruction commit, also update vtypeSpec, because vsetvl flush pipe
    vtypeSpecNext := in.vsetvlVType
  }.elsewhen(in.walkVType.valid) {
    vtypeSpecNext := in.walkVType.bits
  }.elsewhen(in.walkToArchVType) {
    vtypeSpecNext := vtypeArch
  }.elsewhen(in.canUpdateVType) {
    vtypeSpecNext := vtypeNew
  }
}

object VTypeGen {
  def main(args: Array[String]): Unit = {
    val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
      args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform" :+ "--target" :+ "verilog")

    val defaultConfig = config.alterPartial({
      case XSCoreParamsKey => config(XSTileKey).head
    })

    Verilog.emitVerilog(
      new VTypeGen()(defaultConfig),
      Array(
        "--full-stacktrace",
        "--target-dir", "build/VTypeGen",
      )
    )
  }

  class In()(implicit p: Parameters) extends XSBundle {
    val insts = Flipped(Vec(DecodeWidth, ValidIO(UInt(32.W))))
    val walkToArchVType = Input(Bool())
    val walkVType   = Flipped(Valid(new VType))
    val canUpdateVType = Input(Bool())
    val vsetvlVType = Input(new VType)
    val commitVType = new Bundle {
      val vtype = Flipped(Valid(new VType))
      val hasVsetvl = Input(Bool())
    }
  }

  class Out()(implicit p: Parameters) extends XSBundle {
    val vtype = Output(Vec(DecodeWidth, new VType))
    /**
     *  Speculated vtype, for snapshot, different from vtype when the instruction is vset.
     *  However, there's no need for every instruction to take a specvtype. Should be modified in the future.
     */
    val specvtype = Output(Vec(DecodeWidth, new VType))
  }
}
