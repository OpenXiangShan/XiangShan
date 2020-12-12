package xiangshan.backend.decode

import xiangshan.CtrlFlow
import xiangshan.CfCtrl
import xiangshan.backend.fu.HasExceptionNO
import xiangshan.XSModule
import xiangshan.frontend.HasTageParameter

import chisel3._
import chisel3.util._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import chiseltest.ChiselScalatestTester
import scala.util.Random._
import scala.collection.mutable
import scala.io.Source.fromFile
import scala.io.BufferedSource

class DualDecodeUnitDut extends XSModule {
  val io = IO(new Bundle {
    val in = Input(new CtrlFlow)
    val out_ref = Output(new CfCtrl)
    val out_dut = Output(new CfCtrl)
  })

  val ref = Module(new Decoder)
  val dut = Module(new DecodeUnit)

  ref.io.in := io.in
  io.out_ref := ref.io.out

  dut.io.enq.ctrl_flow := io.in
  io.out_dut := dut.io.deq.cf_ctrl
}

class CtrlFlowGenerator(fileName: String) extends HasExceptionNO {
  private var source: BufferedSource = _
  private var instructions: Iterator[String] = _
  var current_line: String = _
  def init: Unit = {
    source = fromFile(fileName)
    instructions = source.getLines()
  }
  def close: Unit = source.close()

  private val pattern = """[0-9a-z]{8,}:\s[0-9a-z]{8,}""".r

  def getPcInstrPair: Option[Tuple2[Long, Long]] = {
    instructions.hasNext match {
      case true => { pattern.findFirstIn({
        current_line = instructions.next()
        current_line
      }) match {
        case None => getPcInstrPair
        case Some(value) => Some(str2Tuple(value))
      }}
      case false => None
    }
  }

  def str2Tuple(str: String): Tuple2[Long, Long] = {
    val pair = str.split("""[\s]?:\s""")
    require(pair.length == 2)
    new Tuple2(java.lang.Long.parseLong(pair(0), 16), java.lang.Long.parseLong(pair(1), 16))
  }

  def genCtrlFlow(x: => CtrlFlow): Boolean = {
    // read pc and instr from file
    getPcInstrPair match {
      case None => false
      case Some(value) => genCtrlFlow(x, value._1, value._2)
    }
  }

  def genCtrlFlow(x: => CtrlFlow, pc: Long, inst: Long): Boolean = {
    x.instr.poke(inst.U)
    x.pc.poke(pc.U)
    x.crossPageIPFFix.poke(nextBoolean().B)
    // input: instrPageFault -> true or false  others: false
    // output: may modify illegalInstr , others : false , instrPageFault: hold
    x.exceptionVec.map(_.poke(nextBoolean().B))
    x.intrVec.map(_.poke(nextBoolean().B))
    true
  }
}

class DecodeUnitDiffTest
  extends AnyFlatSpec
  with ChiselScalatestTester
  with Matchers
  with BeforeAndAfterAllConfigMap
{
  var file = "not set"

  override protected def beforeAll(configMap: ConfigMap): Unit = {
    if (configMap.get("file").isDefined) {
      file = configMap.get("file").fold("parameter not found")(_.toString())
    }
    println(s"input file: ${file}")
  }
  
  "DecodeUnit" must "yield same output as old Decoder" in {
    test(new DualDecodeUnitDut) { c =>
      val generator = new CtrlFlowGenerator(file) // * use -Dfile=<path> to set input file
      generator.init

      while (generator.genCtrlFlow(c.io.in)) {
        c.clock.step(1)
        println(generator.current_line)
        // 1. Ctrl Flow
        // do not care about brUpdate and crossPageIPFFix and instr
        c.io.out_dut.cf.pc.expect(c.io.out_ref.cf.pc.peek())
        c.io.out_dut.cf.exceptionVec.indices.foreach(i => {
          c.io.out_dut.cf.exceptionVec(i).expect(c.io.out_ref.cf.exceptionVec(i).peek())
        })
        // c.io.out_dut.cf.intrVec.indices.foreach(i => {
        //   c.io.out_dut.cf.intrVec(i).expect(c.io.out_ref.cf.intrVec(i).peek())
        // })

        // 2. Ctrl Signals
        // ignore isRVF and ldest and commitType
        // c.io.out_dut.ctrl.src1Type.expect(c.io.out_ref.ctrl.src1Type.peek())
        // c.io.out_dut.ctrl.src2Type.expect(c.io.out_ref.ctrl.src2Type.peek())
        // c.io.out_dut.ctrl.src3Type.expect(c.io.out_ref.ctrl.src3Type.peek())
        // c.io.out_dut.ctrl.lsrc1.expect(c.io.out_ref.ctrl.lsrc1.peek())
        // c.io.out_dut.ctrl.lsrc2.expect(c.io.out_ref.ctrl.lsrc2.peek())
        // c.io.out_dut.ctrl.lsrc3.expect(c.io.out_ref.ctrl.lsrc3.peek())
        // c.io.out_dut.ctrl.ldest.expect(c.io.out_ref.ctrl.ldest.peek())
        // c.io.out_dut.ctrl.fuType.expect(c.io.out_ref.ctrl.fuType.peek())
        // c.io.out_dut.ctrl.fuOpType.expect(c.io.out_ref.ctrl.fuOpType.peek())
        // c.io.out_dut.ctrl.rfWen.expect(c.io.out_ref.ctrl.rfWen.peek())
        // c.io.out_dut.ctrl.fpWen.expect(c.io.out_ref.ctrl.fpWen.peek())
        // c.io.out_dut.ctrl.isXSTrap.expect(c.io.out_ref.ctrl.isXSTrap.peek())
        // c.io.out_dut.ctrl.noSpecExec.expect(c.io.out_ref.ctrl.noSpecExec.peek())
        // c.io.out_dut.ctrl.blockBackward.expect(c.io.out_ref.ctrl.blockBackward.peek())
        // c.io.out_dut.ctrl.flushPipe.expect(c.io.out_ref.ctrl.flushPipe.peek())
        // c.io.out_dut.ctrl.isRVF.expect(c.io.out_ref.ctrl.isRVF.peek())
        // c.io.out_dut.ctrl.imm.expect(c.io.out_ref.ctrl.imm.peek())
        // c.io.out_dut.ctrl.commitType.expect(c.io.out_ref.ctrl.commitType.peek())

        // 3. Branch Tag: ignore
      }
      generator.close
    }
  }

  // it should "refer to `legacy` decoder output" in {
  //   test(new Decoder) { c =>
  //     c.clock.step(1)
  //     // print output
  //     println("[input]")
  //     println(s"instr=${c.io.in.instr.peek()} pc=${c.io.in.pc.peek()} crossPageIPFFix=${c.io.in.crossPageIPFFix.peek()}")
  //     println("[output]")
  //     println(s"src1Type=${c.io.out.ctrl.src1Type.peek()} src2Type=${c.io.out.ctrl.src2Type.peek()} src3Type=${c.io.out.ctrl.src3Type.peek()}")
  //     println(s"lsrc1=${c.io.out.ctrl.lsrc1.peek()} lsrc2=${c.io.out.ctrl.lsrc2.peek()} lsrc3=${c.io.out.ctrl.lsrc3.peek()} ldest=${c.io.out.ctrl.ldest.peek()}")
  //     println(s"fuType=${c.io.out.ctrl.fuType.peek()} fuOpType=${c.io.out.ctrl.fuOpType.peek()}")
  //     println(s"rfWen=${c.io.out.ctrl.rfWen.peek()} fpWen=${c.io.out.ctrl.fpWen.peek()} isXSTrap=${c.io.out.ctrl.isXSTrap.peek()} noSpecExec=${c.io.out.ctrl.noSpecExec.peek()}")
  //     println(s"isBlocked=${c.io.out.ctrl.blockBackward.peek()} flushPipe=${c.io.out.ctrl.flushPipe.peek()} isRVF=${c.io.out.ctrl.isRVF.peek()} imm=${c.io.out.ctrl.imm.peek()}")
  //   }
  // }
}
