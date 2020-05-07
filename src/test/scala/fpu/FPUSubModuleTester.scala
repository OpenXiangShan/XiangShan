package fpu

import scala.sys.process._
import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chisel3.experimental.BundleLiterals._
import chiseltest.internal._
import fpu.divsqrt._
import fpu.RoudingMode._
import fpu.fma._

class MyDecoupledDriver[T <: Data](x: ReadyValidIO[T]) extends DecoupledDriver[T](x){
  def expectDequeue(data: T, message: => String): Unit = timescope {
    // TODO: check for init
    x.ready.poke(true.B)
    fork.withRegion(Monitor) {
      waitForValid()
      x.valid.expect(true.B)
      x.bits.expect(data, message)
    }.joinAndStep(getSinkClock)
  }
  def expectDequeueSeq(data: Seq[T], message: => Seq[String]): Unit = timescope {
    for ((elt, msg) <- data.zip(message)) {
      expectDequeue(elt, msg)
    }
  }
}

case class FpuTest
(
  name: String,
  roundingModes: Seq[UInt],
  backend: String = "verilator",
  writeVcd: Boolean = false,
  pipeline: Boolean = true
)

class FPUSubModuleTester extends FlatSpec
  with ChiselScalatestTester
  with Matchers
  with ParallelTestExecution {

  implicit def decoupledToDriver[T <: Data](x: ReadyValidIO[T]) = new MyDecoupledDriver[T](x)

  val rmAll = Seq(RNE, RTZ, RUP, RDN, RMM)

  val sqrt_tests = Seq(
    FpuTest("f64_sqrt", rmAll, pipeline = false),
    FpuTest("f32_sqrt", rmAll, pipeline = false)
  )
  val div_tests = Seq(
    FpuTest("f64_div", rmAll, pipeline = false),
    FpuTest("f32_div", rmAll, pipeline = false)
  )

  val i2f_tests = Seq(
    FpuTest("i32_to_f64",  rmAll),
    FpuTest("ui32_to_f64", rmAll),
    FpuTest("i64_to_f64",  rmAll),
    FpuTest("ui64_to_f64", rmAll),
    FpuTest("i32_to_f32",  rmAll),
    FpuTest("ui32_to_f32", rmAll),
    FpuTest("i64_to_f32",  rmAll),
    FpuTest("ui64_to_f32", rmAll)
  )

  val f2i_tests = Seq(
    FpuTest("f64_to_i64",  rmAll),
    FpuTest("f64_to_ui64", rmAll),
    FpuTest("f32_to_i64",  rmAll),
    FpuTest("f32_to_ui64", rmAll),
    FpuTest("f64_to_i32",  rmAll),
    FpuTest("f64_to_ui32", rmAll),
    FpuTest("f32_to_i32",  rmAll),
    FpuTest("f32_to_ui32", rmAll)
  )

  val f2f_tests = Seq(
    FpuTest("f64_to_f32", rmAll),
    FpuTest("f32_to_f64", rmAll)
  )

  val fcmp_tests = Seq(
    FpuTest("f64_le", rmAll),
    FpuTest("f64_lt", rmAll),
    FpuTest("f64_eq", rmAll),
    FpuTest("f32_le", rmAll),
    FpuTest("f32_lt", rmAll),
    FpuTest("f32_eq", rmAll)

  )

  val fma_tests = Seq(

  )

  val fadd_tests = Seq(
    FpuTest("f64_add", rmAll),
    FpuTest("f64_sub", rmAll),
    FpuTest("f32_add", rmAll),
    FpuTest("f32_sub", rmAll)
  )

  val fmul_tests = Seq(
    FpuTest("f64_mul", rmAll),
    FpuTest("f32_mul", rmAll)
  )


  val tests = fmul_tests ++ fadd_tests ++
    sqrt_tests ++ div_tests ++
    fcmp_tests ++ i2f_tests ++
    f2i_tests ++ f2f_tests

  val backendMap = Map(
    "treadle" -> TreadleBackendAnnotation,
    "verilator" -> VerilatorBackendAnnotation,
    "vcs" -> VcsBackendAnnotation
  )
  val rmMap = Map(
    RoudingMode.RNE -> "rnear_even",
    RoudingMode.RTZ -> "rminMag",
    RoudingMode.RUP -> "rmax",
    RoudingMode.RDN -> "rmin",
    RoudingMode.RMM -> "rnear_maxMag"
  )

  val testMap = Map[String, (()=>FPUSubModule, Boolean, Int)]( elems=
    "f64_sqrt" -> (()=>new DivSqrt, true, 1),
    "f32_sqrt" -> (()=>new DivSqrt, false, 1),
    "f64_div" -> (()=>new DivSqrt, true, 0),
    "f32_div" -> (()=>new DivSqrt, false, 0),

    "i32_to_f64"  -> (()=>new IntToFloat, true, 0),
    "ui32_to_f64" -> (()=>new IntToFloat, true, 1),
    "i64_to_f64"  -> (()=>new IntToFloat, true, 2),
    "ui64_to_f64" -> (()=>new IntToFloat, true, 3),
    "i32_to_f32"  -> (()=>new IntToFloat, false, 0),
    "ui32_to_f32" -> (()=>new IntToFloat, false, 1),
    "i64_to_f32"  -> (()=>new IntToFloat, false, 2),
    "ui64_to_f32" -> (()=>new IntToFloat, false, 3),

    "f64_to_i32" ->  (()=>new FloatToInt, true, 0),
    "f32_to_i32" ->  (()=>new FloatToInt, false, 0),
    "f64_to_ui32" -> (()=>new FloatToInt, true, 1),
    "f32_to_ui32" -> (()=>new FloatToInt, false, 1),
    "f64_to_i64" ->  (()=>new FloatToInt, true, 2),
    "f32_to_i64" ->  (()=>new FloatToInt, false, 2),
    "f64_to_ui64" -> (()=>new FloatToInt, true, 3),
    "f32_to_ui64" -> (()=>new FloatToInt, false, 3),

    // 'isDouble' was not used in FloatToFloat
    "f32_to_f64" -> (()=>new F32toF64, true, 0),
    "f64_to_f32" -> (()=>new F64toF32, true, 1),

    "f64_le" -> (()=>new FCMP, true, 2),
    "f64_lt" -> (()=>new FCMP, true, 3),
    "f64_eq" -> (()=>new FCMP, true, 4),
    "f32_le" -> (()=>new FCMP, false, 2),
    "f32_lt" -> (()=>new FCMP, false, 3),
    "f32_eq" -> (()=>new FCMP, false, 4),

    "f64_add" -> (()=> new FMA, true, 0),
    "f32_add" -> (()=> new FMA, false, 0),
    "f64_sub" -> (()=> new FMA, true, 1),
    "f32_sub" -> (()=> new FMA, false, 1),
    "f64_mul" -> (()=> new FMA, true, 2),
    "f32_mul" -> (()=> new FMA, false, 2)

  )
  for(t <- tests){
    val (dutGen, isDouble, op) = testMap(t.name)
    for(rm <- t.roundingModes){
      it should s"compute [${t.name}, rm=${rm.litValue()}] correctly" in {

        val input = s"./debug/testfloat_gen ${t.name} -${rmMap(rm)} -exact -tininessafter".!!
        val testCases = input.split("\n").map(line => line.split(" "))
        val annos = Seq(backendMap(t.backend)) ++ (if(t.writeVcd) Seq(WriteVcdAnnotation) else Nil)
        test(new Delayer(dutGen())).withAnnotations(annos){ c =>

          c.io.in.initSource().setSourceClock(c.clock)
          c.io.out.initSink().setSinkClock(c.clock)
          c.io.out.expectInvalid()

          c.io.out.ready.poke(true.B)

          def dutEnQueue(testCase: Array[String], idx: Int): Unit ={
            val srcCnt = testCases.length - 2 // - output - fflags
            c.io.in.enqueue(chiselTypeOf(c.io.in.bits).Lit(
              _.op -> op.U,
              _.rm -> rm,
              _.isDouble -> isDouble.B,
              _.a -> (if(srcCnt > 0) ("h"+testCase(0)).U(64.W) else 0.U(64.W)),
              _.b -> (if(srcCnt > 1) ("h"+testCase(1)).U(64.W) else 0.U(64.W)),
              _.c -> (if(srcCnt > 2) ("h"+testCase(2)).U(64.W) else 0.U(64.W))
            ))
          }

          def dutDeQueue(testCase: Array[String], idx: Int): Unit ={
            val srcCnt = testCase.length - 2
            val refResult = ("h"+testCase(srcCnt)).U(64.W)
            val refFflags = ("h"+testCase(srcCnt+1)).U
            c.io.out.expectDequeue(
              chiselTypeOf(c.io.out.bits).Lit(
                _.result -> refResult,
                _.fflags -> chiselTypeOf(c.io.out.bits.fflags).Lit(
                  _.invalid -> refFflags(4),
                  _.infinite -> refFflags(3),
                  _.overflow -> refFflags(2),
                  _.underflow -> refFflags(1),
                  _.inexact -> refFflags(0))
              ),
              message = s"\nn:$idx testCase: ${testCase.mkString(" ")}\n" +
                s"dut res:${c.io.out.bits.result.peek().litValue().toString(16)} " +
                s"inv:${c.io.out.bits.fflags.invalid.peek().litValue()} " +
                s"inf:${c.io.out.bits.fflags.infinite.peek().litValue()} " +
                s"ov:${c.io.out.bits.fflags.overflow.peek().litValue()} " +
                s"uf:${c.io.out.bits.fflags.underflow.peek().litValue()} " +
                s"ix:${c.io.out.bits.fflags.inexact.peek().litValue()}" +
                s"\n"
            )
          }

          if(t.pipeline){
            fork{
              testCases.zipWithIndex.foreach({case (testCase, idx) => dutEnQueue(testCase, idx)})
            }.fork{
              testCases.zipWithIndex.foreach({case (testCase, idx) => dutDeQueue(testCase, idx)})
            }.join()
          } else {
            testCases.zipWithIndex.foreach({case (testCase, idx) =>
              dutEnQueue(testCase, idx)
              dutDeQueue(testCase, idx)
            })
          }

        }
      }
    }
  }
}
