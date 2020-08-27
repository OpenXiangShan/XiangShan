package xiangshan.mem

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.cache._
import utils.ParallelAND
import utils.TrueLRU


class SbufferUserBundle extends XSBundle {
  val pc = UInt(VAddrBits.W) //for debug
  val lsroqId = UInt(log2Up(LsroqSize).W)
}

trait HasSBufferConst extends HasXSParameter {
  val sBufferIndexWidth: Int = log2Up(StoreBufferSize) // a.k.a. index of cache line

  // paddr = tag + offset
  val tagWidth: Int = PAddrBits - log2Up(CacheLineSize / 8)
  val offsetWidth: Int = log2Up(CacheLineSize / 8)

  val cacheMaskWidth: Int = CacheLineSize / 8
  val instMaskWidth: Int = XLEN / 8
}

class SBufferCacheLine extends XSBundle with HasSBufferConst {
  val valid = Bool()
  val tag = UInt(tagWidth.W)
  val data = Vec(cacheMaskWidth, UInt(8.W))// UInt(CacheLineSize.W)
  val mask = Vec(cacheMaskWidth, Bool())
}

class UpdateInfo extends XSBundle with HasSBufferConst {
  val idx: UInt = UInt(sBufferIndexWidth.W) // cache index effected by this store req
  val newTag: UInt = UInt(tagWidth.W)
  val newMask: Vec[Bool] = Vec(cacheMaskWidth, Bool())
  val newData: Vec[UInt] = Vec(cacheMaskWidth, UInt(8.W))

  val isForward: Bool = Bool() // this req has same tag as some former req
  val isUpdated: Bool = Bool()
  val isInserted: Bool = Bool()
  val isIgnored: Bool = Bool()
}

// Store buffer for XiangShan Out of Order LSU
class Sbuffer extends XSModule with HasSBufferConst {
  val io = IO(new Bundle() {
    val in = Vec(StorePipelineWidth, Flipped(Decoupled(new DCacheWordReq )))
    val dcache = new DCacheStoreIO
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
  })

  val cache: Vec[SBufferCacheLine] = RegInit(VecInit(Seq.fill(StoreBufferSize)(0.U.asTypeOf(new SBufferCacheLine))))

  val updateInfo = WireInit(VecInit(Seq.fill(StorePipelineWidth)(0.U.asTypeOf(new UpdateInfo))))
  updateInfo := DontCare

  val lru = new TrueLRU(StoreBufferSize)

  def getTag(pa: UInt): UInt =
    pa(PAddrBits - 1, PAddrBits - tagWidth)

  def getAddr(tag: UInt): UInt =
    Cat(tag, 0.U((PAddrBits - tagWidth).W))

  def getByteOffset(pa: UInt): UInt =
    Cat(pa(offsetWidth - 1, log2Up(8)), Fill(3, 0.U))

  // check if cacheIdx is modified by former request in this cycle
  def busy(cacheIdx: UInt, max: Int): Bool = {
    if (max == 0)
      false.B
    else
      ParallelOR((0 until max).map(i => updateInfo(i).idx === cacheIdx && io.in(i).valid)).asBool()
  }



  // Get retired store from lsroq
  //--------------------------------------------------------------------------------------------------------------------
  for (storeIdx <- 0 until StorePipelineWidth) {
    io.in(storeIdx).ready := false.B // when there is empty line or target address already in this buffer, assert true
    // otherwise, assert false
    // when d-cache write port is valid, write back the oldest line to d-cache

    updateInfo(storeIdx).isForward := false.B
    updateInfo(storeIdx).isUpdated := false.B
    updateInfo(storeIdx).isInserted := false.B
    updateInfo(storeIdx).isIgnored := false.B

    // 0. compare with former requests
    for (formerIdx <- 0 until storeIdx) {
      // i: former request
      when ((getTag(io.in(storeIdx).bits.addr) === updateInfo(formerIdx).newTag) &&
        (updateInfo(formerIdx).isUpdated || updateInfo(formerIdx).isInserted) && io.in(storeIdx).valid && io.in(formerIdx).valid) {
        updateInfo(storeIdx).isForward := true.B
        updateInfo(formerIdx).isIgnored := true.B
        updateInfo(storeIdx).idx := updateInfo(formerIdx).idx
        XSDebug("req#%d writes same line with req#%d\n", storeIdx.U, formerIdx.U)

        updateInfo(storeIdx).isInserted := updateInfo(formerIdx).isInserted
        updateInfo(storeIdx).isUpdated := updateInfo(formerIdx).isUpdated


        updateInfo(storeIdx).newTag := updateInfo(formerIdx).newTag
        // update mask and data
        (0 until cacheMaskWidth).foreach(i => {
          when (i.U < getByteOffset(io.in(storeIdx).bits.addr).asUInt() ||
            i.U > (getByteOffset(io.in(storeIdx).bits.addr) | 7.U)) {
            updateInfo(storeIdx).newMask(i) := updateInfo(formerIdx).newMask(i)
            updateInfo(storeIdx).newData(i) := updateInfo(formerIdx).newData(i)
          } otherwise {
            when (io.in(storeIdx).bits.mask.asBools()(i % 8)) {
              updateInfo(storeIdx).newMask(i) := true.B
              updateInfo(storeIdx).newData(i) := io.in(storeIdx).bits.data(8 * (i % 8 + 1) - 1, 8 * (i % 8))
            } .otherwise {
              updateInfo(storeIdx).newMask(i) := updateInfo(formerIdx).newMask(i)
              updateInfo(storeIdx).newData(i) := updateInfo(formerIdx).newData(i)
            }
          }
        })

      }
    }


    // 1. search for existing lines
    for (bufIdx <- 0 until StoreBufferSize) {
      when (!updateInfo(storeIdx).isForward && (getTag(io.in(storeIdx).bits.addr) === cache(bufIdx).tag) && cache(bufIdx).valid && io.in(storeIdx).valid) {
        // mark this line as UPDATE
        updateInfo(storeIdx).isUpdated := true.B
        updateInfo(storeIdx).idx := bufIdx.U
        updateInfo(storeIdx).newTag := getTag(io.in(storeIdx).bits.addr)

        // update mask and data
        (0 until cacheMaskWidth).foreach(i => {
          when (i.U < getByteOffset(io.in(storeIdx).bits.addr).asUInt() ||
            i.U > (getByteOffset(io.in(storeIdx).bits.addr) | 7.U)) {
            updateInfo(storeIdx).newMask(i) := cache(bufIdx).mask(i)
            updateInfo(storeIdx).newData(i) := cache(bufIdx).data(i)
          } otherwise {
            when (io.in(storeIdx).bits.mask.asBools()(i % 8)) {
              updateInfo(storeIdx).newMask(i) := true.B
              updateInfo(storeIdx).newData(i) := io.in(storeIdx).bits.data(8 * (i % 8 + 1) - 1, 8 * (i % 8))
            } .otherwise {
              updateInfo(storeIdx).newMask(i) := cache(bufIdx).mask(i)
              updateInfo(storeIdx).newData(i) := cache(bufIdx).data(i)
            }
          }
        })


      }
    }


    // 2. not found target address in existing lines, try to insert a new line
    val freeVec = WireInit(VecInit((0 until StoreBufferSize).map(i => cache(i).valid || busy(i.U, storeIdx))))
    val hasFree = !ParallelAND(freeVec)
    val nextFree = PriorityEncoder(freeVec.map(i => !i))
    //    XSInfo("hasFree: %d, nextFreeIdx: %d\n", hasFree, nextFree)

    when (!updateInfo(storeIdx).isForward && !updateInfo(storeIdx).isUpdated && hasFree && io.in(storeIdx).valid) {
      updateInfo(storeIdx).isInserted := true.B
      updateInfo(storeIdx).idx := nextFree
      updateInfo(storeIdx).newTag := getTag(io.in(storeIdx).bits.addr)

      // set mask and data
      (0 until cacheMaskWidth).foreach(i => {
        when (i.U < getByteOffset(io.in(storeIdx).bits.addr).asUInt() ||
          i.U > (getByteOffset(io.in(storeIdx).bits.addr) | 7.U)) {
          updateInfo(storeIdx).newMask(i) := false.B
          updateInfo(storeIdx).newData(i) := 0.U
        } otherwise {
          when (io.in(storeIdx).bits.mask.asBools()(i % 8)) {
            updateInfo(storeIdx).newMask(i) := true.B
            updateInfo(storeIdx).newData(i) := io.in(storeIdx).bits.data(8 * (i % 8 + 1) - 1, 8 * (i % 8))
//            XSInfo("[%d] write data %x\n", i.U, io.in(storeIdx).bits.data(8 * (i % 8 + 1) - 1, 8 * (i % 8)))
          } .otherwise {
            updateInfo(storeIdx).newMask(i) := false.B
            updateInfo(storeIdx).newData(i) := 0.U
          }
        }
      })


    }

    // 3. not enough space for this query
    when (!updateInfo(storeIdx).isForward && !updateInfo(storeIdx).isUpdated && !updateInfo(storeIdx).isInserted) {
      updateInfo(storeIdx).isIgnored := true.B
    }

    XSInfo(updateInfo(storeIdx).isUpdated && updateInfo(storeIdx).isInserted, "Error: one line is both updated and inserted!\n")

    if (storeIdx > 0)
      io.in(storeIdx).ready := io.in(storeIdx - 1).ready && (updateInfo(storeIdx).isUpdated || updateInfo(storeIdx).isInserted)
    else
      io.in(storeIdx).ready := updateInfo(storeIdx).isUpdated || updateInfo(storeIdx).isInserted

    when(io.in(storeIdx).fire()){


      when(updateInfo(storeIdx).isIgnored) {
        XSInfo("Ignore req#%d with paddr %x, mask %x, data %x\n", storeIdx.U, io.in(storeIdx).bits.addr, io.in(storeIdx).bits.mask, io.in(storeIdx).bits.data)


        // Update
        // ----------------------------------------
      } .elsewhen(updateInfo(storeIdx).isUpdated) {
        // clear lruCnt
//        cache(updateInfo(storeIdx).idx).lruCnt := 0.U
        lru.access(updateInfo(storeIdx).idx)
        // update mask and data
//        cache(updateInfo(storeIdx).idx).data := updateInfo(storeIdx).newData
        cache(updateInfo(storeIdx).idx).data.zipWithIndex.foreach { case (int, i) =>
          int := updateInfo(storeIdx).newData(i)
        }
//        cache(updateInfo(storeIdx).idx).mask := updateInfo(storeIdx).newMask
        cache(updateInfo(storeIdx).idx).mask.zipWithIndex.foreach { case (int, i) =>
          int := updateInfo(storeIdx).newMask(i)
        }

        XSInfo("Update line#%d with tag %x, mask %x, data %x\n", updateInfo(storeIdx).idx, cache(updateInfo(storeIdx).idx).tag,
          io.in(storeIdx).bits.mask, io.in(storeIdx).bits.data)


        // Insert
        // ----------------------------------------
      } .elsewhen(updateInfo(storeIdx).isInserted) {
        // clear lruCnt
//        cache(updateInfo(storeIdx).idx).lruCnt := 0.U
        lru.access(updateInfo(storeIdx).idx)
        // set valid
        cache(updateInfo(storeIdx).idx).valid := true.B
        // set tag
        cache(updateInfo(storeIdx).idx).tag := updateInfo(storeIdx).newTag
        // update mask and data
//        cache(updateInfo(storeIdx).idx).data := updateInfo(storeIdx).newData
//        cache(updateInfo(storeIdx).idx).mask := updateInfo(storeIdx).newMask
        cache(updateInfo(storeIdx).idx).data.zipWithIndex.foreach { case (int, i) =>
          int := updateInfo(storeIdx).newData(i)
        }
        cache(updateInfo(storeIdx).idx).mask.zipWithIndex.foreach { case (int, i) =>
          int := updateInfo(storeIdx).newMask(i)
        }

        XSInfo("Insert into line#%d with tag %x, mask: %x, data: %x, pa: %x\n", updateInfo(storeIdx).idx, getTag(io.in(storeIdx).bits.addr),
          io.in(storeIdx).bits.mask, io.in(storeIdx).bits.data, io.in(storeIdx).bits.addr)
      } // ignore UNCHANGED & EVICTED state
    }
  }


  // Write back to d-cache
  //--------------------------------------------------------------------------------------------------------------------
  val waitingCacheLine: SBufferCacheLine = RegInit(0.U.asTypeOf(new SBufferCacheLine))

  val validCnt: UInt = Wire(UInt((sBufferIndexWidth + 1).W))
  validCnt := PopCount((0 until StoreBufferSize).map(i => cache(i).valid))
  XSInfo("[ %d ] lines valid this cycle\n", validCnt)

  val oldestLineIdx: UInt = Wire(UInt(sBufferIndexWidth.W))
  oldestLineIdx := lru.way
  XSInfo("Least recently used #[ %d ] line\n", oldestLineIdx)

  val dcacheData = Wire(UInt(io.dcache.req.bits.data.getWidth.W))
  val dcacheMask = Wire(UInt(io.dcache.req.bits.mask.getWidth.W))
  dcacheData := DontCare
  dcacheMask := DontCare

  io.dcache.req.valid := false.B //needWriteToCache
  io.dcache.req.bits.addr := DontCare
  io.dcache.req.bits.data := dcacheData
  io.dcache.req.bits.mask := dcacheMask
  io.dcache.req.bits.cmd  := MemoryOpConstants.M_XWR
  io.dcache.req.bits.meta := DontCare // NOT USED
  io.dcache.resp.ready := waitingCacheLine.valid


  when (validCnt === StoreBufferSize.U && !waitingCacheLine.valid) {
    // assert valid and send data + mask + addr(ends with 000b) to d-cache
    io.dcache.req.bits.addr := getAddr(cache(oldestLineIdx).tag)

    when (!busy(oldestLineIdx, StorePipelineWidth)) {
      dcacheData := cache(oldestLineIdx).data.asUInt()
      dcacheMask := cache(oldestLineIdx).mask.asUInt()

      XSDebug("[New D-Cache Req] idx: %d, addr: %x, mask: %x, data: %x\n", oldestLineIdx, io.dcache.req.bits.addr, waitingCacheLine.mask.asUInt(), waitingCacheLine.data.asUInt())
    } .otherwise {
      XSDebug("[Pending Write Back] tag: %x, mask: %x, data: %x\n", waitingCacheLine.tag, waitingCacheLine.mask.asUInt(), waitingCacheLine.data.asUInt())
    }

    for (i <- 0 until StorePipelineWidth) {
      when (updateInfo(i).idx === oldestLineIdx && updateInfo(i).isUpdated && io.in(i).valid) {
        dcacheData := updateInfo(i).newData.asUInt()
        dcacheMask := updateInfo(i).newMask.asUInt()
      }
    }

    io.dcache.req.valid := true.B
  }

  when(io.dcache.req.fire()){
    // save current req
    waitingCacheLine := cache(oldestLineIdx)
    waitingCacheLine.data := dcacheData.asTypeOf(Vec(cacheMaskWidth, UInt(8.W)))
    waitingCacheLine.mask := dcacheMask.asTypeOf(Vec(cacheMaskWidth, Bool()))
    XSError(!cache(oldestLineIdx).valid, "!cache(oldestLineIdx).valid\n")
    // waitingCacheLine.valid := true.B

    cache(oldestLineIdx).valid := false.B
    XSInfo("send req to dcache %x\n", oldestLineIdx)
  }

  when(io.dcache.resp.fire()) {
    waitingCacheLine.valid := false.B
    lru.miss
    XSInfo("recv resp from dcache. wb tag %x mask %x data %x\n", waitingCacheLine.tag, waitingCacheLine.mask.asUInt(), waitingCacheLine.data.asUInt())
  }


  // loadForwardQuery
  //--------------------------------------------------------------------------------------------------------------------
  (0 until LoadPipelineWidth).map(loadIdx => {
    io.forward(loadIdx).forwardMask := VecInit(List.fill(instMaskWidth)(false.B))
    io.forward(loadIdx).forwardData := DontCare

    when(getTag(io.forward(loadIdx).paddr) === waitingCacheLine.tag && waitingCacheLine.valid) {
      (0 until XLEN / 8).foreach(i => {
        when (waitingCacheLine.mask(i.U + getByteOffset(io.forward(loadIdx).paddr)) && io.forward(loadIdx).mask(i)) {
          io.forward(loadIdx).forwardData(i) := waitingCacheLine.data(i.U + getByteOffset(io.forward(loadIdx).paddr))
          io.forward(loadIdx).forwardMask(i) := true.B
        } .otherwise {
          io.forward(loadIdx).forwardData(i) := 0.U
          io.forward(loadIdx).forwardMask(i) := false.B
        }

      })
    } .otherwise {
      (0 until StoreBufferSize).foreach(sBufIdx => {
        when(getTag(io.forward(loadIdx).paddr) === cache(sBufIdx).tag && cache(sBufIdx).valid) {
          // send data with mask in this line
          // this mask is not 'mask for cache line' and we need to check low bits of paddr
          // to get certain part of one line
          // P.S. data in io.in will be manipulated by lsroq

          (0 until XLEN / 8).foreach(i => {

            when (cache(sBufIdx).mask(i.U + getByteOffset(io.forward(loadIdx).paddr)) && io.forward(loadIdx).mask(i)) {
              io.forward(loadIdx).forwardData(i) := cache(sBufIdx).data(i.U + getByteOffset(io.forward(loadIdx).paddr))
              io.forward(loadIdx).forwardMask(i) := true.B
            } .otherwise {
              io.forward(loadIdx).forwardData(i) := 0.U
              io.forward(loadIdx).forwardMask(i) := false.B
            }
//            io.forward(loadIdx).forwardData(i) := cache(sBufIdx).data(i.U + getByteOffset(io.forward(loadIdx).paddr))
//            io.forward(loadIdx).forwardMask(i) := cache(sBufIdx).mask(i.U + getByteOffset(io.forward(loadIdx).paddr))
          })

          when (io.forward(loadIdx).valid) {
            XSDebug("[ForwardReq] paddr: %x mask: %x pc: %x\n", io.forward(loadIdx).paddr, io.forward(loadIdx).mask, io.forward(loadIdx).pc)
            XSDebug("[Forwarding] forward-data: %x forward-mask: %x\n", io.forward(loadIdx).forwardData.asUInt(),
              io.forward(loadIdx).forwardMask.asUInt())
          }
        }
      })
    }
  })

  // additional logs
  XSInfo(io.in(0).fire(), "ensbuffer addr 0x%x wdata 0x%x\n", io.in(0).bits.addr, io.in(0).bits.data)
  XSInfo(io.in(1).fire(), "ensbuffer addr 0x%x wdata 0x%x\n", io.in(1).bits.addr, io.in(1).bits.data)
  XSInfo(io.dcache.req.fire(), "desbuffer addr 0x%x wdata 0x%x\n", io.dcache.req.bits.addr, io.dcache.req.bits.data)

  // output cache line
  cache.zipWithIndex.foreach { case (line, i) => {
    XSDebug(line.valid, "[#%d line] Tag: %x, data: %x, mask: %x\n", i.U, line.tag, line.data.asUInt(), line.mask.asUInt())
  }}
}
