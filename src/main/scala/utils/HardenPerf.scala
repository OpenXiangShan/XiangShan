package utils

import chisel3. _
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.DebugOptionsKey
import chisel3.util.experimental.BoringUtils

import scala.collection.mutable.ListBuffer

object Constants {
  val PerfEventLen = 32
}

case class HardenPerfConfig(commitWidth: Int) {
  val batchSize = 64
  val MaxDataByteLen = 240
  val MaxDataBitLen = MaxDataByteLen * 8
  val validWidth = commitWidth
  val timeoutThreshold = 200000
}

class PerfEventBundle extends Bundle {
  val value: UInt = UInt(Constants.PerfEventLen.W)
}

object HardenXSPerfAccumulate {
  private val enabled = true
  private val instances = ListBuffer.empty[(Data, String)]
  private val structInstances = List.fill(4)(ListBuffer.empty[(Data, String)])

  // apply for normal performance counter
  def apply[T <: Data](
                        name: String,
                        perfCnt: T,
                      )(implicit p: Parameters): Unit = {
    if (enabled) {
      // register performance counter
      val id = register(perfCnt, name)
      println(s"# Hardened Counter $id: $name")
      // counter implementation
      val counter = RegInit(0.U(Constants.PerfEventLen.W)).suggestName(name + "Counter")
      val next_counter = WireInit(0.U(Constants.PerfEventLen.W)).suggestName(name + "Next")
      next_counter := counter + perfCnt.asTypeOf(UInt(Constants.PerfEventLen.W))
      
      val env = p(DebugOptionsKey)
      if (!env.FPGAPlatform) {
        val helper = Module(new LogPerfHelper)
        val perfClean = helper.io.clean
        counter := Mux(perfClean, 0.U, next_counter)
      } else {
        counter := next_counter
      }

      // add wire-source for the counter
      BoringUtils.addSource(RegNext(RegNext(RegNext(counter))), name)
    }
  }

  // apply for performance counter for DSE
  def apply[T <: Data](
                        name: String,
                        perfCnt: T,
                        commitIdx: Int
                      )(implicit p: Parameters): Unit = {
    if (enabled) {
      // register performance counter
      val id = register_deg(perfCnt, name, commitIdx)
      println(s"# DEG Hardened Counter $id: $name")

      // counter implementation
      val counter = RegInit(0.U(Constants.PerfEventLen.W)).suggestName(name + "Counter")
      val next_counter = WireInit(0.U(Constants.PerfEventLen.W)).suggestName(name + "Next")
      next_counter := counter + perfCnt.asTypeOf(UInt(Constants.PerfEventLen.W))

      val env = p(DebugOptionsKey)
      if (!env.FPGAPlatform) {
        val helper = Module(new LogPerfHelper)
        val perfClean = helper.io.clean
        counter := Mux(perfClean, 0.U, next_counter)
      } else {
        counter := next_counter
      }

      // add wire-source for the counter
      val probe = perfCnt.asTypeOf(UInt(Constants.PerfEventLen.W))
      BoringUtils.addSource(RegNext(RegNext(RegNext(probe))), s"${name}_$commitIdx")
    }
  }

  // declare valid signal
  def declareValid(index: Int, perfCnt: Bool)(implicit p: Parameters): Unit = {
    if (enabled) {
      val portal = WireInit(perfCnt)
      BoringUtils.addSource(RegNext(RegNext(RegNext(portal))), s"valid_$index")
    }
  }

  def register[T <: Data](gen: T, name: String): Int = {
    val id = instances.length
    val element = (gen, name)
    instances += element
    id
  }

  def register_deg[T <: Data](gen: T, name: String, commitIdx: Int): Int = {
    val id = structInstances.map(_.length).sum
    val element = (gen, name)
    structInstances(commitIdx) += element
    id
  }

  // reclaim at top IO
  def reclaim(commitWidth: Int): (Vec[PerfEventBundle], Int, Vec[Bool], Vec[Vec[PerfEventBundle]], Int) = {
    lazy val nrStructCnt = structInstances.head.length
    lazy val nrNormalCnt = instances.length
    lazy val nrCntAll = nrStructCnt * commitWidth + nrNormalCnt

    // add wire-sink for all registered counters
    assert(structInstances.take(commitWidth).map(_.length).distinct.size <= 1, "All ListBuffers in 'instances' must have the same length")
    lazy val io_perf: Vec[PerfEventBundle] = IO(Output(Vec(nrNormalCnt, new PerfEventBundle)))
    lazy val deg_data = WireInit(VecInit(Seq.fill(commitWidth)(VecInit(Seq.fill(nrStructCnt)(0.U.asTypeOf(new PerfEventBundle))))))

    for (i <- 0 until commitWidth) {
      structInstances(i).zip(deg_data(i)).zipWithIndex.foreach{ case ((instance, data), idx) =>
        val portal = WireInit(0.U(Constants.PerfEventLen.W))
        BoringUtils.addSink(portal, s"${instance._2}_$i")
        data.value := portal
        // io_perf(i*nrStructCnt + idx).value := portal
      }
    }
    instances.zipWithIndex.foreach { case ((_, name), i) =>
      val portal = WireInit(0.U(Constants.PerfEventLen.W))
      BoringUtils.addSink(portal, name)
      io_perf(i).value := portal
    }

    // add sire-sink for valid signal
    lazy val deg_valids_vec = VecInit(Seq.fill(commitWidth){false.B})
    deg_valids_vec.zipWithIndex.foreach{
      case (v, i) =>
        val portal = WireInit(false.B)
        BoringUtils.addSink(portal, s"valid_$i")
        v := portal
    }
    // generate cpp & verilog file for post-process
    /*
    FileRegisters.add("HardenPerf.cpp", generateCppParser(true))
    FileRegisters.add("DSEMacro.v", generateVerilog())
    */
    FileRegisters.add("NormalPerfList.txt", generateNormalPerfList())
    FileRegisters.add("DEGPerfList.txt", generateDEGPerfList())
    (io_perf, nrNormalCnt, deg_valids_vec, deg_data, nrStructCnt)
  }

  def generateNormalPerfList(): String = {
    val perfList = ListBuffer.empty[String]
    for (i <- instances.indices) {
      perfList += instances(i)._2
    }
    perfList.mkString("\n")
  }

  def generateDEGPerfList(): String = {
    val perfList = ListBuffer.empty[String]
    for (i <- structInstances.head.indices) {
      perfList += structInstances.head(i)._2
    }
    perfList.mkString("\n")
  }

  def generateCppParser_legacy(pldm: Boolean = false): String = {
    val parserCpp = ListBuffer.empty[String]
    parserCpp +=
      s"""
         |#include <cstdint>
         |#include <vector>
         |#include <string>
         |""".stripMargin

    if (pldm) {
      parserCpp += "#include \"perfprocess.h\""
    } else {
      parserCpp +=
        s"""
          |#include "verilated.h"
          |#include "VSimTop.h"
          |""".stripMargin
    }

    if (pldm) {}
    else {
      parserCpp +=
        s"""
          |std::vector<uint64_t> getIOPerfCnts(VSimTop *dut_ptr) {
          |    std::vector<uint64_t> perfCnts;
          |""".stripMargin
      for (i <- instances.indices) {
        parserCpp += s"    perfCnts.push_back(dut_ptr->io_perf_${i}_value);"
      }
      parserCpp +=
        s"""
          |    return perfCnts;
          |}
          |""".stripMargin

      parserCpp +=
        s"""
          |std::vector<std::string> getIOPerfNames() {
          |    std::vector<std::string> perfNames;
          |""".stripMargin
      instances.foreach {
        case (_, s) =>
          parserCpp += s"    perfNames.push_back(\"${s}\");"
      }
      parserCpp +=
        s"""
          |    return perfNames;
          |}
          |""".stripMargin
    }
  
    if (pldm) {
      parserCpp += "int get_perfCnt_id(std::string perfName) {"
      
      for (i <- instances.indices) {
        parserCpp += s"  if (perfName == \"${instances(i)._2}\") { return $i; }"
      }
      parserCpp += "}"
    
      parserCpp += "extern Perfprocess* perfprocess;"
      parserCpp += "extern \"C\" void pushIOPerfCnts("
      for (i <- instances.indices) {
        parserCpp += s"  uint64_t io_perf_${i}_value, "
      }
      parserCpp += "  char dse_reset_valid) {"
      parserCpp += "  perfprocess->perfCnts.clear();"
      for (i <- instances.indices) {
        parserCpp += s"  perfprocess->perfCnts.push_back(io_perf_${i}_value);"
      }
      parserCpp += "}"
    }
  
    parserCpp.mkString("\n")
  }

  def generateVerilog_legacy(): String = {
    val parserVerilog = ListBuffer.empty[String]
    
    parserVerilog += "`define DEFINE_HARDEN_PERFCNT \\"
    for (i <- instances.indices) {
      parserVerilog += s"  wire [63:0] io_perf_${i}_value;\\"
    }
    parserVerilog += ""

    parserVerilog += "`define INPUT_HARDEN_PERFCNT \\"
    for (i <- instances.indices) {
      parserVerilog += s"  input wire [63:0] io_perf_${i}_value,\\"
    }
    parserVerilog += ""

    parserVerilog += "`define PERFCNT_CONNECTIONS \\"
    for (i <- instances.indices) {
      parserVerilog += s"  .io_perf_${i}_value   (io_perf_${i}_value), \\"
    }
    parserVerilog += ""

    parserVerilog += "`define DECLEAR_PUSH_HARDEN_PERFCNT \\"
    parserVerilog += s"import \"DPI-C\" function void pushIOPerfCnts( \\"
    for (i <- instances.indices) {
      parserVerilog += s"  longint io_perf_${i}_value, \\"
    }
    parserVerilog += "  byte dse_reset_valid);"
    parserVerilog += ""

    parserVerilog += "`define PUSH_HARDEN_PERFCNT \\"
    parserVerilog += "  pushIOPerfCnts( \\"
    for (i <- instances.indices) {
      parserVerilog += s"    io_perf_${i}_value, \\"
    }
    parserVerilog += "    dse_reset_valid \\\n);"
    
    parserVerilog.mkString("\n")
  }

}


/*================================================================================================================*/

class BatchOutput(param: HardenPerfConfig) extends Bundle {
  val enable = Bool()
  val data = UInt(param.MaxDataBitLen.W)
}

class BatchInfo extends Bundle {
  // val id = UInt(8.W)
  val num = UInt(8.W)
}

class IOPerfOutput(len: Int) extends Bundle {
  val data = UInt(len.W)
}

object Batch {
  def apply( deg_bundles: Vec[Vec[PerfEventBundle]],
             deg_valids: Vec[Bool],
             io_perf_bundles: Vec[PerfEventBundle],
             nr_perf: Int,
             param: HardenPerfConfig,
             nrStructCnt: Int) =
  {
    val cluster = Module(new BatchCluster(nrStructCnt, param))
    cluster.in_data := deg_bundles
    cluster.in_valids := deg_valids
    
    val perf_width = nr_perf * (new PerfEventBundle).getWidth
    lazy val deg_out = IO(Output(new BatchOutput(param)))
    lazy val io_perf_out = IO(Output(new IOPerfOutput(perf_width)))
    deg_out <> cluster.out
    io_perf_out.data := Cat(io_perf_bundles.asUInt)

    FileRegisters.add("DSEMacro.v", generateVerilogHeader(param, perf_width))

    (deg_out, io_perf_out)
  }
  
  def generateVerilogHeader(param: HardenPerfConfig, perf_width: Int): String = {
    val parserVerilog = ListBuffer.empty[String]
    parserVerilog += s"`define DEG_DATA_WIDTH ${param.MaxDataBitLen}"
    parserVerilog += s"`define PERF_DATA_WIDTH ${perf_width}"
    parserVerilog += s"`define MAGIC_NUM_WIDTH 8"
    parserVerilog.mkString("\n")
  }
}

class BatchCluster(cntNum: Int, param: HardenPerfConfig) extends Module {
  val in_data = IO(Input(Vec(param.validWidth, Vec(cntNum, new PerfEventBundle))))  // commitWidth x nrPerfcnt
  val in_valids = IO(Input(Vec(param.validWidth, Bool())))
  val out = IO(new BatchOutput(param))

  val has_valids = Cat(in_valids).orR
  val nr_valids = PopCount(in_valids)
  val in_data_latch = RegNext(in_data)

  val in_valids_latch = RegNext(in_valids, VecInit(Seq.fill(param.validWidth)(false.B)))
  val nr_valids_latch = PopCount(in_valids_latch)
  val has_valids_latch = Cat(in_valids_latch).orR

  val batch_slot_bit = cntNum * (new PerfEventBundle).getWidth
  val batch_slot_byte = batch_slot_bit / 8
  val batch_slot_num = param.MaxDataByteLen / batch_slot_byte

  val batch_data = RegInit(VecInit(Seq.fill(batch_slot_num)(0.U.asTypeOf(UInt(batch_slot_bit.W)))))
  val send_batch_data = WireInit(VecInit(Seq.fill(batch_slot_num)(0.U.asTypeOf(UInt(batch_slot_bit.W)))))
  val slot_ptr = RegInit(0.U(log2Ceil(batch_slot_num).W))
  val remain_slots = batch_slot_num.U - slot_ptr

  val slot_overflow = has_valids_latch && nr_valids_latch >= remain_slots

  val timeout_count = RegInit(0.U(32.W))
  val timeout = timeout_count === 200000.U

  val state_update = has_valids_latch || slot_overflow || timeout
  val send_out = slot_overflow || timeout

  val append_slots = Mux(nr_valids_latch >= remain_slots, nr_valids_latch - remain_slots, 0.U)

  for (i <- 0 until batch_slot_num) {
    when(i.U < slot_ptr) {
      send_batch_data(i) := batch_data(i)
    }.otherwise{
      when(slot_overflow) {
        send_batch_data(i) := Cat(in_data_latch(i.U - slot_ptr).asUInt)
      }.otherwise {
        send_batch_data(i) := 0.U
      }
    }
  }

  when(!send_out) {
    timeout_count := timeout_count + 1.U
  }.otherwise {
    timeout_count := 0.U
  }

  out.enable := send_out
  out.data := Cat(send_batch_data)

  when (state_update) {
    when(has_valids_latch) {
      when(send_out) {
        when(append_slots === 0.U) { // nr_valids_latch == remain_slots
          batch_data.foreach( _ := 0.U )
          slot_ptr := 0.U
        }.otherwise{
          for (i <- 0 until param.validWidth) {
            when(in_valids_latch(i) && i.U >= remain_slots) {
              val enq_data = Cat(in_data_latch(i).asUInt)
              val enq_index = i.U - remain_slots
              batch_data(enq_index) := enq_data
            }
          }
          slot_ptr := nr_valids_latch - remain_slots
        }
      }.otherwise {  // normal enqueue
        for (i <- 0 until param.validWidth) {
          when(in_valids_latch(i)) {
            val enq_data = Cat(in_data_latch(i).asUInt)
            val enq_index = slot_ptr + i.U
            batch_data(enq_index) := enq_data
          }
        }
        slot_ptr := slot_ptr + nr_valids_latch
      }
    }.otherwise{  // timeout
      batch_data.foreach( _ := 0.U )
      slot_ptr := 0.U
    }
  }

//  in_data.zip(in_valids).zipWithIndex.foreach{
//    case ((d, v), i) =>
//      out_data(i) := Mux(v, d.asUInt, 0.U)
//      if (i > 1) { assert(!(v && in_valids(i-1))) }
//  }
//
//  val info = Wire(new BatchInfo)
//  info.num := v_size
//  out_info := Mux(v_size =/= 0.U, info.asUInt, 0.U)
//
//  val bytes_map = Seq.tabulate(cntNum + 1) { vi => (vi.U, (bundleType.getWidth / 8 * vi).U) }
//  status.data_bytes := LookupTree(v_size, bytes_map)
//  status.info_size := Mux(v_size =/= 0.U, 1.U, 0.U)
}
