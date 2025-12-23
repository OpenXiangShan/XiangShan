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
*
*
* Acknowledgement
*
* This implementation is inspired by several key papers:
* [1] Santhosh Srinath, Onur Mutlu, Hyesoon Kim, and Yale N. Patt "[Feedback directed prefetching: Improving the
* performance and bandwidth-efficiency of hardware prefetchers.](https://doi.org/10.1109/HPCA.2007.346185)" IEEE 13th
* International Symposium on High Performance Computer Architecture (HPCA). 2007.
***************************************************************************************/

package xiangshan.mem.prefetch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientStates._
import freechips.rocketchip.tilelink.MemoryOpCategories._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates, TLPermissions}
import xiangshan.backend.rob.RobDebugRollingIO
import utils._
import utility._
import xiangshan.{L1CacheErrorInfo, XSCoreParamsKey}
import xiangshan.mem.HasL1PrefetchSourceParameter
import utility.{CircularQueuePtr}
import xiangshan.cache._
import xiangshan.{XSBundle, XSModule}

//----------------------------------------
// Feedback Direct Prefetching
class CounterFilterDataBundle(implicit p: Parameters) extends DCacheBundle {
  val idx = UInt(idxBits.W)
  val way = UInt(wayBits.W)
}

class CounterFilterQueryBundle(implicit p: Parameters) extends DCacheBundle {
  val req = ValidIO(new CounterFilterDataBundle())
  val resp = Input(Bool())
}

// no Set Blocking in LoadPipe, so when counting useful prefetch counter, duplicate result occurs
// s0    s1     s2     s3     s4(in PrefetchArray, write next cycle of wreq)
// r                   wreq   w
// if 3 load insts is accessing the same cache line(set0, way0) in s0, s1, s2, s3
// they think they all prefetch hit, increment useful prefetch counter 3 times
// so when load arrives at s3, save it's set&way to an FIFO, all loads will search this FIFO to avoid this case
class CounterFilter()(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    // input, only from load for now
    val ld_in = Flipped(Vec(LoadPipelineWidth, ValidIO(new CounterFilterDataBundle())))
    val query = Flipped(Vec(LoadPipelineWidth, new CounterFilterQueryBundle()))
  })

  val LduStages = 4 + 1 // +1 is for prefetch wirte in next cycle of s3
  val SIZE = (LduStages) * LduCnt
  class Ptr(implicit p: Parameters) extends CircularQueuePtr[Ptr]( p => SIZE ){}
  object Ptr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): Ptr = {
      val ptr = Wire(new Ptr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }

  val entries = RegInit(VecInit(Seq.fill(SIZE){ (0.U.asTypeOf(new CounterFilterDataBundle())) }))
  val valids = RegInit(VecInit(Seq.fill(SIZE){ (false.B) }))

  // enq
  val enqLen = LduCnt
  val deqLen = LduCnt
  val enqPtrExt = RegInit(VecInit((0 until enqLen).map(_.U.asTypeOf(new Ptr))))
  val deqPtrExt = RegInit(VecInit((0 until deqLen).map(_.U.asTypeOf(new Ptr))))

  val deqPtr = WireInit(deqPtrExt(0).value)

  val reqs_l = io.ld_in.map(_.bits)
  val reqs_vl = io.ld_in.map(_.valid)
  val needAlloc = Wire(Vec(enqLen, Bool()))
  val canAlloc = Wire(Vec(enqLen, Bool()))
  val last3CycleAlloc = RegInit(0.U(log2Ceil(LduCnt + 1).W))

  for(i <- (0 until enqLen)) {
    val req = reqs_l(i)
    val req_v = reqs_vl(i)
    val index = PopCount(needAlloc.take(i))
    val allocPtr = enqPtrExt(index)

    needAlloc(i) := req_v
    canAlloc(i) := needAlloc(i) && allocPtr >= deqPtrExt(0)

    when(canAlloc(i)) {
      valids(allocPtr.value) := true.B
      entries(allocPtr.value) := req
    }

    assert(!needAlloc(i) || canAlloc(i), s"port${i} can not accept CounterFilter enq request, check if SIZE >= (Ldu stages + 1) * LduCnt")
  }
  val allocNum = PopCount(canAlloc)

  enqPtrExt.foreach{case x => 
    when(canAlloc.asUInt.orR){
      x := x + allocNum
    }
  }
  last3CycleAlloc := RegNext(RegNext(RegNext(allocNum)))

  // deq
  for(i <- (0 until deqLen)) {
    when(i.U < last3CycleAlloc) {
      valids(deqPtrExt(i).value) := false.B
    }
  }

  deqPtrExt.foreach{case x => x := x + last3CycleAlloc}

  // query
  val querys_l = io.query.map(_.req.bits)
  val querys_vl = io.query.map(_.req.valid)
  for(i <- (0 until LduCnt + HyuCnt)) {
    val q = querys_l(i)
    val q_v = querys_vl(i)

    val entry_match = Cat(entries.zip(valids).map {
      case(e, v) => v && (q.idx === e.idx) && (q.way === e.way)
    }).orR

    io.query(i).resp := q_v && entry_match
  }

  XSPerfAccumulate("req_nums", PopCount(io.query.map(_.req.valid)))
  XSPerfAccumulate("req_set_way_match", PopCount(io.query.map(_.resp)))
}

class BloomQueryBundle(n: Int)(implicit p: Parameters) extends DCacheBundle {
  val addr = UInt(BLOOMADDRWIDTH.W)

  def BLOOMADDRWIDTH = log2Ceil(n)

  def get_addr(paddr: UInt): UInt = {
    assert(paddr.getWidth == PAddrBits)
    assert(paddr.getWidth >= (blockOffBits + 2 * BLOOMADDRWIDTH))
    val block_paddr = paddr(paddr.getWidth - 1, blockOffBits)
    val low_part = block_paddr(BLOOMADDRWIDTH - 1, 0)
    val high_part = block_paddr(2 * BLOOMADDRWIDTH - 1, BLOOMADDRWIDTH)
    low_part ^ high_part
  }
}

class BloomRespBundle(implicit p: Parameters) extends DCacheBundle {
  val res = Bool()
}
class BloomFilter(n: Int, bypass: Boolean = true)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new DCacheBundle {
    val set = Flipped(ValidIO(new BloomQueryBundle(n)))
    val clr = Flipped(ValidIO(new BloomQueryBundle(n)))
    val query = Vec(LoadPipelineWidth, Flipped(ValidIO(new BloomQueryBundle(n))))
    val resp = Vec(LoadPipelineWidth, ValidIO(new BloomRespBundle))
  })

  val data = RegInit(0.U(n.W))
  val data_next = Wire(Vec(n, Bool()))

  for (i <- 0 until n) {
    when(io.clr.valid && i.U === io.clr.bits.addr) {
      data_next(i) := false.B
    }.elsewhen(io.set.valid && i.U === io.set.bits.addr) {
      data_next(i) := true.B
    }.otherwise {
      data_next(i) := data(i).asBool
    }
  }

  // resp will valid in next cycle
  for(i <- 0 until LoadPipelineWidth) {
    io.resp(i).valid := GatedValidRegNext(io.query(i).valid)
    if(bypass) {
      io.resp(i).bits.res := RegEnable(data_next(io.query(i).bits.addr), io.query(i).valid)
    }else {
      io.resp(i).bits.res := RegEnable(data(io.query(i).bits.addr), io.query(i).valid)
    }
  }

  data := data_next.asUInt

  assert(PopCount(data ^ data_next.asUInt) <= 2.U)

  XSPerfHistogram("valid_nums", PopCount(data), true.B, 0, n + 1, 20)
}