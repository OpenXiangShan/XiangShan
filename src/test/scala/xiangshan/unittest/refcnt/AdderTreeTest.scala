package xiangshan.unittest.refcnt

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import system.SoCParamsKey
import top._
import xiangshan.XSCoreParamsKey
import xiangshan.backend.rename.refcnt.AdderTree

import scala.util.Random

class AdderTreeTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  val (config, firrtlOpts) = ArgParser.parse(Array("--config", "DefaultConfig"), fpga = false)

  val NRPhyRegs = 160

  def nextPhyRegIdxArray(len: Int): Array[Int] = {

    val raw = Array.fill(len)(-1)
    /* make sure no duplicate entries exist in raw array */
    for (i <- raw.indices) {
      raw(i) = Random.nextInt(NRPhyRegs)
      while (raw.slice(0, i).contains(raw(i))) raw(i) = Random.nextInt(NRPhyRegs)
    }
    raw
  }

  behavior of "AdderTree"
  it should "update counter when no overlap between inc and dec" in {

    test (new AdderTree()(config.alterPartial{
      case XSCoreParamsKey => config(SoCParamsKey).cores.head
    })) { dut =>
      val round = 100 // running [round] rounds of random tests
      for (r <- 0 until round) {

        val indicesNeedInc = nextPhyRegIdxArray(6)
        val indicesNeedDec = nextPhyRegIdxArray(6)
        val oldCntVec = Array.fill(NRPhyRegs)(Random.nextInt(2) + 1) // not include 0 and 3

        dut.io.incVec zip indicesNeedInc foreach { case (port, value) => port.poke(value.U)}

        dut.io.decVec zip indicesNeedDec foreach { case (port, value) => port.poke(value.U)}

        dut.io.cntVec zip oldCntVec foreach { case (port, value) => port.poke(value.U)}

        // nextCntVec(i) = cntVec(i) + incBitVec(i) - decBitVec(i)
        val newCntVec =  oldCntVec.clone()
        indicesNeedInc zip indicesNeedDec foreach {
          case (incIdx, decIdx) =>
            newCntVec(incIdx) += 1
            newCntVec(decIdx) -= 1
        }

        println(s"inc: ${indicesNeedInc.mkString(",")}")
        println(s"dec: ${indicesNeedDec.mkString(",")}")
        oldCntVec zip (newCntVec.zipWithIndex) foreach {
          case (o, (n, i)) => if (o != n) {
            print(s"#$i: ($o -> $n) ")
          }
        }
        println()

        dut.io.nextCntVec zip newCntVec foreach { case (nextCnt, exp) =>
          nextCnt.expect(exp.U)
        }
      }
    }
  }

  it should "update counter when inc and dec are the same" in {
    test (new AdderTree()(config.alterPartial{
      case XSCoreParamsKey => config(SoCParamsKey).cores.head
    })) { dut =>
      val round = 10 // running [round] rounds of random tests
      for (r <- 0 until round) {

        val indicesNeedInc = nextPhyRegIdxArray(6)
        val indicesNeedDec = indicesNeedInc.clone()
        val oldCntVec = Array.fill(NRPhyRegs)(Random.nextInt(2) + 1) // not include 0 and 3

        dut.io.incVec zip indicesNeedInc foreach { case (port, value) => port.poke(value.U)}

        dut.io.decVec zip indicesNeedDec foreach { case (port, value) => port.poke(value.U)}

        dut.io.cntVec zip oldCntVec foreach { case (port, value) => port.poke(value.U)}

        // nextCntVec(i) = cntVec(i) + incBitVec(i) - decBitVec(i)
        val newCntVec =  oldCntVec.clone()

        dut.io.nextCntVec zip newCntVec foreach { case (nextCnt, exp) =>
          nextCnt.expect(exp.U)
        }
      }
    }
  }
}
