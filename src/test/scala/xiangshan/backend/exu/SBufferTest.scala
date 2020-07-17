package xiangshan.backend.exu

import chiseltest.ChiselScalatestTester
import org.scalatest.{FlatSpec, Matchers, ParallelTestExecution}
import xiangshan.testutils.HasPartialDecoupledDriver

class SBufferTest extends FlatSpec
  with ChiselScalatestTester
  with Matchers
  with ParallelTestExecution
  with HasPartialDecoupledDriver
{
  it should "behave like its emulator" in {
    // TODO
  }
}
