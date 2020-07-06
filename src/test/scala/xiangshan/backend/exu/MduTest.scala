package xiangshan.backend.exu

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import noop.MDUOpType
import xiangshan._
import xiangshan.testutils._
import xiangshan.testutils.TestCaseGenerator._




class MduTest extends FlatSpec
  with ChiselScalatestTester
  with Matchers
  with ParallelTestExecution
  with HasPartialDecoupledDriver
{
  it should "" in {
    test(new Mul){ c =>

      c.io.in.initSource().setSourceClock(c.clock)
      c.io.out.initSink().setSinkClock(c.clock)

      c.io.redirect.valid.poke(true.B)
      c.io.redirect.bits.isException.poke(true.B)
      c.clock.step(1)

      c.io.redirect.valid.poke(false.B)

      fork{
        // 110
        c.io.in.enqueuePartial(chiselTypeOf(c.io.in.bits).Lit(
          _.uop.ctrl.fuOpType -> MDUOpType.mulw,
          _.uop.cf.pc -> 1.U
        ))
        // 111
        c.io.in.enqueuePartial(chiselTypeOf(c.io.in.bits).Lit(
          _.uop.ctrl.fuOpType -> MDUOpType.mulw,
          _.uop.cf.pc -> 2.U
        ))
        // 112, 113
        c.clock.step(2)
        // 114
        c.io.in.enqueuePartial(chiselTypeOf(c.io.in.bits).Lit(
          _.uop.ctrl.fuOpType -> MDUOpType.mulw,
          _.uop.cf.pc -> 3.U
        ))
      }.fork{
        c.io.out.expectDequeuePartial(chiselTypeOf(c.io.out.bits).Lit())
        c.io.out.expectDequeuePartial(chiselTypeOf(c.io.out.bits).Lit())
        c.io.out.expectDequeuePartial(chiselTypeOf(c.io.out.bits).Lit())
      }.join()

    }
  }


}
