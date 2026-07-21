/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
* [1] Gurindar S. Sohi, and Manoj Franklin. "[High-bandwidth data memory systems for superscalar processors.]
* (https://doi.org/10.1145/106972.106980)" 4th International Conference on Architectural Support for Programming
* Languages and Operating Systems (ASPLOS). 1991.
***************************************************************************************/

package xiangshan.cache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import utils._
import utility._
import utility.sram.SRAMTemplate
import chisel3.util._
import utility.mbist.MbistPipeline
import xiangshan.mem.LqPtr
import xiangshan.{L1CacheErrorInfo, XSCoreParamsKey}

import scala.math.max

class BankConflictDB(implicit p: Parameters) extends DCacheBundle{
  val addr = Vec(LoadPipelineWidth, Bits(PAddrBits.W))
  val setIndex = Vec(LoadPipelineWidth, UInt((DCacheAboveIndexOffset - DCacheSetOffset).W))
  val bankIndex = Vec(VLEN/DCacheSRAMRowBits, UInt((DCacheSetOffset - DCacheBankOffset).W))
  val wayIndex = UInt(wayBits.W)
  val fakeRrBankConflict = Bool()
}

class L1BankedDataReadReq(implicit p: Parameters) extends DCacheBundle
{
  val wayEn = Bits(DCacheWays.W)
  val addr = Bits(PAddrBits.W)
}

class L1BankedDataReadReqWithMask(implicit p: Parameters) extends DCacheBundle
{
  val wayEn = Bits(DCacheWays.W)
  val addr = Bits(PAddrBits.W)
  val addrDup = Bits(PAddrBits.W)
  val bankMask = Bits(DCacheBanks.W)
  val lqIdx = new LqPtr
}

class L1BankedDataReadLineReq(implicit p: Parameters) extends L1BankedDataReadReq
{
  val rmask = Bits(DCacheBanks.W)
  val way = Bits(log2Up(DCacheWays).W) // UInt format of wayEn for better timing
}

// Now, we can write a cache-block in a single cycle
class L1BankedDataWriteReq(implicit p: Parameters) extends L1BankedDataReadReq
{
  val wmask = Bits(DCacheBanks.W)
  val data = Vec(DCacheBanks, Bits(DCacheSRAMRowBits.W))
}

// cache-block write request without data
class L1BankedDataWriteReqCtrl(implicit p: Parameters) extends L1BankedDataReadReq

class L1BankedDataReadResult(implicit p: Parameters) extends DCacheBundle
{
  // you can choose which bank to read to save power
  val ecc = Bits(dataECCBits.W)
  val rawData = Bits(DCacheSRAMRowBits.W)
  val errorDelayed = Bool() // 1 cycle later than data resp

  def asECCData() = {
    Cat(ecc, rawData)
  }
}

class DataSRAMBankWriteReq(implicit p: Parameters) extends DCacheBundle {
  val en = Bool()
  val addr = UInt()
  val wayEn = UInt(DCacheWays.W)
  val data = UInt(encDataBits.W)
}

// wrap a sram
class DataSRAM(bankIdx: Int, wayIdx: Int)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val w = new Bundle() {
      val en = Input(Bool())
      val addr = Input(UInt())
      val data = Input(UInt(encDataBits.W))
    }

    val r = new Bundle() {
      val en = Input(Bool())
      val addr = Input(UInt())
      val data = Output(UInt(encDataBits.W))
    }
  })

  // data sram
  val dataSram = Module(new SRAMTemplate(
    Bits(encDataBits.W),
    set = DCacheSets / DCacheSetDiv,
    way = 1,
    shouldReset = false,
    holdRead = false,
    singlePort = true,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl
  ))

  dataSram.io.w.req.valid := io.w.en
  dataSram.io.w.req.bits.apply(
    setIdx = io.w.addr,
    data = io.w.data,
    waymask = 1.U
  )
  dataSram.io.r.req.valid := io.r.en
  dataSram.io.r.req.bits.apply(setIdx = io.r.addr)
  io.r.data := dataSram.io.r.resp.data(0)
  XSPerfAccumulate("part_data_read_counter", dataSram.io.r.req.valid)

  def dumpR() = {
    XSDebug(RegNext(io.r.en),
      "bank read set %x bank %x way %x data %x\n",
      RegEnable(io.r.addr, io.r.en),
      bankIdx.U,
      wayIdx.U,
      io.r.data
    )
  }

  def dumpW() = {
    XSDebug(io.w.en,
      "bank write set %x bank %x way %x data %x\n",
      io.w.addr,
      bankIdx.U,
      wayIdx.U,
      io.w.data
    )
  }

  def dump() = {
    dumpW()
    dumpR()
  }
}

// wrap data rows of 8 ways
class DataSRAMBank(index: Int)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val w = Input(new DataSRAMBankWriteReq)

    val r = new Bundle() {
      val en = Input(Bool())
      val addr = Input(UInt())
      val data = Output(Vec(DCacheWays, UInt(encDataBits.W)))
    }
  })

  assert(RegNext(!io.w.en || PopCount(io.w.wayEn) <= 1.U))

  // external controls do not read and write at the same time
  val wInfo = io.w
  // val rw_bypass = RegNext(io.w.addr === io.r.addr && io.w.wayEn === io.r.wayEn && io.w.en)

  private val packedRowBits = DCacheWays * encDataBits
  private val dataBank = Module(new SRAMTemplate(
    Bits(packedRowBits.W),
    set = DCacheSets / DCacheSetDiv,
    way = 1,
    shouldReset = false,
    holdRead = false,
    singlePort = true,
    useBitmask = true,
    withClockGate = true,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl,
    suffix = Some("dcsh_dat")
  ))

  val packedWriteData = Cat((0 until DCacheWays).reverse.map(_ => wInfo.data))
  val packedWriteMask = Cat((0 until DCacheWays).reverse.map(w => Fill(encDataBits, wInfo.wayEn(w))))

  dataBank.io.w.req.valid := wInfo.en
  dataBank.io.w.req.bits.apply(
    setIdx = wInfo.addr,
    data = packedWriteData,
    waymask = 1.U,
    bitmask = packedWriteMask
  )
  dataBank.io.r.req.valid := io.r.en
  dataBank.io.r.req.bits.apply(setIdx = io.r.addr)
  XSPerfAccumulate("part_data_read_counter", dataBank.io.r.req.valid)

  val packedReadData = dataBank.io.r.resp.data(0).asUInt
  io.r.data := VecInit((0 until DCacheWays).map(w => {
    packedReadData(encDataBits * (w + 1) - 1, encDataBits * w)
  }))

  def dumpR() = {
    XSDebug(RegNext(io.r.en),
      "bank read addr %x data %x\n",
      RegEnable(io.r.addr, io.r.en),
      io.r.data.asUInt
    )
  }

  def dumpW() = {
    XSDebug(io.w.en,
      "bank write addr %x wayEn %x data %x\n",
      io.w.addr,
      io.w.wayEn,
      io.w.data
    )
  }

  def dump() = {
    dumpW()
    dumpR()
  }
}

case object HasDataEccParam

//                     Banked DCache Data
// -----------------------------------------------------------------
// | Bank0 | Bank1 | Bank2 | Bank3 | Bank4 | Bank5 | Bank6 | Bank7 |
// -----------------------------------------------------------------
// | Way0  | Way0  | Way0  | Way0  | Way0  | Way0  | Way0  | Way0  |
// | Way1  | Way1  | Way1  | Way1  | Way1  | Way1  | Way1  | Way1  |
// | ....  | ....  | ....  | ....  | ....  | ....  | ....  | ....  |
// -----------------------------------------------------------------
abstract class AbstractBankedDataArray(implicit p: Parameters) extends DCacheModule
{
  val DataEccParam = if(EnableDataEcc) Some(HasDataEccParam) else None
  val ReadlinePortErrorIndex = LoadPipelineWidth
  val io = IO(new DCacheBundle {
    // load pipeline read word req
    val read = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new L1BankedDataReadReqWithMask)))
    val is128Req = Input(Vec(LoadPipelineWidth, Bool()))
    // main pipeline read / write line req
    val readlineIntend = Input(Bool())
    val readline = Flipped(DecoupledIO(new L1BankedDataReadLineReq))
    val readlineCanGo = Input(Bool())
    val readlineStall = Input(Bool())
    val readlineCanResp = Input(Bool())
    val write = Flipped(DecoupledIO(new L1BankedDataWriteReq))
    val writeDup = Vec(DCacheBanks, Flipped(Decoupled(new L1BankedDataWriteReqCtrl)))
    // data for readline and loadpipe
    val readlineResp = Output(Vec(DCacheBanks, new L1BankedDataReadResult()))
    val readlineError = Output(Bool())
    val readlineErrorDelayed = Output(Bool())
    val readResp          = Output(Vec(LoadPipelineWidth, Vec(VLEN/DCacheSRAMRowBits, new L1BankedDataReadResult())))
    val readErrorDelayed = Output(Vec(LoadPipelineWidth,Vec(VLEN/DCacheSRAMRowBits, Bool())))
    // val nacks = Output(Vec(LoadPipelineWidth, Bool()))
    // val errors = Output(Vec(LoadPipelineWidth + 1, ValidIO(new L1CacheErrorInfo))) // read ports + readline port
    // when bank_conflict, read (1) port should be ignored
    val bankConflictSlow = Output(Vec(LoadPipelineWidth, Bool()))
    val disableLdFastWakeup = Output(Vec(LoadPipelineWidth, Bool()))
    val pseudoError = Flipped(DecoupledIO(Vec(DCacheBanks, new CtrlUnitSignalingBundle)))
  })

  // Half of the data banks use the duplicate address path to reduce fanout.
  def DuplicatedQueryBankSeq = 0 until DCacheBanks / 2

  def pipeMap[T <: Data](f: Int => T) = VecInit((0 until LoadPipelineWidth).map(f))

  def getECCFromEncWord(encWord: UInt) = {
    if (EnableDataEcc) {
      require(encWord.getWidth == encDataBits, s"encDataBits=$encDataBits != encDataBits=$encDataBits!")
      encWord(encDataBits-1, DCacheSRAMRowBits)
    } else {
      0.U
    }
  }

  def getDataFromEncWord(encWord: UInt) = {
    encWord(DCacheSRAMRowBits-1, 0)
  }

  def asECCData(ecc: UInt, data: UInt) = {
    if (EnableDataEcc) {
      Cat(ecc, data)
    } else {
      data
    }
  }

  def dumpRead = {
    (0 until LoadPipelineWidth) map { w =>
      XSDebug(io.read(w).valid,
        s"DataArray Read channel: $w valid wayEn: %x addr: %x\n",
        io.read(w).bits.wayEn, io.read(w).bits.addr)
    }
    XSDebug(io.readline.valid,
      s"DataArray Read Line, valid wayEn: %x addr: %x rmask %x\n",
      io.readline.bits.wayEn, io.readline.bits.addr, io.readline.bits.rmask)
  }

  def dumpWrite = {
    XSDebug(io.write.valid,
      s"DataArray Write valid wayEn: %x addr: %x\n",
      io.write.bits.wayEn, io.write.bits.addr)

    (0 until DCacheBanks) map { r =>
      XSDebug(io.write.valid,
        s"cycle: $r data: %x wmask: %x\n",
        io.write.bits.data(r), io.write.bits.wmask(r))
    }
  }

  def dumpResp = {
    XSDebug(s"DataArray ReadeResp channel:\n")
    (0 until LoadPipelineWidth) map { r =>
      val req128Data = Cat((0 until DCacheVWordBankCount).reverse.map(i => io.readResp(r)(i).rawData))
      val data = Cat((0 until DCacheVWordBankCount / 2).reverse.map(i => io.readResp(r)(i).rawData))
      XSDebug(s"cycle: $r data: %x\n", Mux(io.is128Req(r), req128Data, data))
    }
  }

  def dump() = {
    dumpRead
    dumpWrite
    dumpResp
  }

  def selcetOldestPort(valid: Seq[Bool], bits: Seq[LqPtr], index: Seq[UInt]):((Bool, LqPtr), UInt) = {
    require(valid.length == bits.length &&  bits.length == index.length, s"length must eq, valid:${valid.length}, bits:${bits.length}, index:${index.length}")
    ParallelOperation(valid zip bits zip index,
      (a: ((Bool, LqPtr), UInt), b: ((Bool, LqPtr), UInt)) => {
        val au = a._1._2
        val bu = b._1._2
        val aValid = a._1._1
        val bValid = b._1._1
        val bSel = au > bu
        val bits = Mux(
          aValid && bValid,
          Mux(bSel, b._1._2, a._1._2),
          Mux(aValid && !bValid, a._1._2, b._1._2)
        )
        val idx = Mux(
          aValid && bValid,
          Mux(bSel, b._2, a._2),
          Mux(aValid && !bValid, a._2, b._2)
        )
        ((aValid || bValid, bits), idx)
      }
    )
  }

}

// the smallest access unit is sram
class SramedDataArray(implicit p: Parameters) extends AbstractBankedDataArray {
  println("  DCacheType: SramedDataArray")
  val ReduceReadlineConflict = false

  io.write.ready := true.B
  io.writeDup.foreach(_.ready := true.B)

  val dataBanks = List.tabulate(DCacheSetDiv)( k => {
    val banks = List.tabulate(DCacheBanks)(i => List.tabulate(DCacheWays)(j => Module(new DataSRAM(i,j))))
    val mbistPl = MbistPipeline.PlaceMbistPipeline(1, s"MbistPipeDataSet$k", hasMbist)
    banks
  })
  dataBanks.map(_.map(_.map(_.dump())))

  val wayEn = Wire(Vec(LoadPipelineWidth, io.read(0).bits.wayEn.cloneType))
  val setAddrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val divAddrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val bankAddrs = Wire(Vec(LoadPipelineWidth, Vec(VLEN/DCacheSRAMRowBits, UInt())))

  val lineSetAddr = addrToDcacheDivSet(io.readline.bits.addr)
  val lineDivAddr = addrToDcacheDiv(io.readline.bits.addr)
  // when WPU is enabled, lineWayEn is all enabled when read data
  val lineWayEn = Fill(DCacheWays, 1.U) // val lineWayEn = io.readline.bits.wayEn
  val lineWayEnReg = RegEnable(io.readline.bits.wayEn, 0.U(DCacheWays.W),io.readline.valid)

  val writeBankMaskReg = RegEnable(io.write.bits.wmask, 0.U(DCacheBanks.W), io.write.valid)
  val writeDataReg = RegEnable(io.write.bits.data, io.write.valid)
  val writeValidReg = RegNext(io.write.valid)
  val writeValidDupReg = io.writeDup.map(x => RegNext(x.valid))
  val writeWayEnDupReg = io.writeDup.map(x => RegEnable(x.bits.wayEn, 0.U(DCacheWays.W), x.valid))
  val writeSetAddrDupReg = io.writeDup.map(x => RegEnable(addrToDcacheDivSet(x.bits.addr), x.valid))
  val writeDivAddrDupReg = io.writeDup.map(x => RegEnable(addrToDcacheDiv(x.bits.addr), x.valid))

  // read dataBanks and ecc_banks
  // for single port SRAM, do not allow read and write in the same cycle
  val rrhazard = false.B // io.readline.valid
  (0 until LoadPipelineWidth).map(rportIndex => {
    divAddrs(rportIndex) := addrToDcacheDiv(io.read(rportIndex).bits.addr)
    setAddrs(rportIndex) := addrToDcacheDivSet(io.read(rportIndex).bits.addr)
    bankAddrs(rportIndex)(0) := addrToDcacheBank(io.read(rportIndex).bits.addr)
    bankAddrs(rportIndex)(1) := bankAddrs(rportIndex)(0) + 1.U

    // use wayEn to select a way after data read out
    assert(!(RegNext(io.read(rportIndex).fire && PopCount(io.read(rportIndex).bits.wayEn) > 1.U)))
    wayEn(rportIndex) := io.read(rportIndex).bits.wayEn
  })

  // read conflict
  val rrBankConflict = Seq.tabulate(LoadPipelineWidth)(x => Seq.tabulate(LoadPipelineWidth)(y => {
    if (x == y) {
      false.B
    } else {
      io.read(x).valid && io.read(y).valid &&
        divAddrs(x) === divAddrs(y) &&
        (io.read(x).bits.bankMask & io.read(y).bits.bankMask) =/= 0.U &&
        io.read(x).bits.wayEn === io.read(y).bits.wayEn &&
        setAddrs(x) =/= setAddrs(y)
    }
  }))
  val loadReqWithBankConflict = rrBankConflict.map(_.reduce(_ || _))
  val loadReqValid = io.read.map(_.valid)
  val loadReqLqIdx = io.read.map(_.bits.lqIdx)
  val loadReqIndex = (0 until LoadPipelineWidth).map(_.asUInt)


  val loadReqBankConflictSelcet = selcetOldestPort(loadReqWithBankConflict, loadReqLqIdx, loadReqIndex)
  val loadReqBankSelectPort  = UIntToOH(loadReqBankConflictSelcet._2).asBools

  val rrBankConflictOldest = (0 until LoadPipelineWidth).map(i =>
    !loadReqBankSelectPort(i) && loadReqWithBankConflict(i)
  )

  val rrlBankConflict = Wire(Vec(LoadPipelineWidth, Bool()))
  val rrlBankConflictIntend = Wire(Vec(LoadPipelineWidth, Bool()))
  (0 until LoadPipelineWidth).foreach { i =>
    val judge = if (ReduceReadlineConflict) io.read(i).valid && (io.readline.bits.rmask & io.read(i).bits.bankMask) =/= 0.U && lineDivAddr === divAddrs(i) && lineSetAddr =/= setAddrs(i)
                else io.read(i).valid && lineDivAddr === divAddrs(i) && lineSetAddr =/= setAddrs(i)
    rrlBankConflict(i) := judge && io.readline.valid
    rrlBankConflictIntend(i) := judge && io.readlineIntend
  }
  val wrBankConflict = Seq.tabulate(LoadPipelineWidth)(x =>
    io.read(x).valid && writeValidReg &&
    divAddrs(x) === writeDivAddrDupReg.head &&
    wayEn(x) === writeWayEnDupReg.head &&
    (writeBankMaskReg(bankAddrs(x)(0)) || writeBankMaskReg(bankAddrs(x)(1)) && io.is128Req(x))
  )
  val wrlBankConflict = io.readline.valid && writeValidReg && lineDivAddr === writeDivAddrDupReg.head
  // ready
  io.readline.ready := !(wrlBankConflict)
  io.read.zipWithIndex.map { case (x, i) => x.ready := !(wrBankConflict(i) || rrhazard) }

  val perfMultiRead = PopCount(io.read.map(_.valid)) >= 2.U
  val bankConflictFast = Wire(Vec(LoadPipelineWidth, Bool()))
  (0 until LoadPipelineWidth).foreach(i => {
    bankConflictFast(i) := wrBankConflict(i) || rrlBankConflict(i) ||
    rrBankConflictOldest(i)
    io.bankConflictSlow(i) := RegNext(bankConflictFast(i))
    io.disableLdFastWakeup(i) := wrBankConflict(i) || rrlBankConflictIntend(i) ||
      (if (i == 0) 0.B else (0 until i).map(rrBankConflict(_)(i)).reduce(_ || _))
  })
  XSPerfAccumulate("data_array_multi_read", perfMultiRead)
  val rrBankConflictCount = PopCount((1 until LoadPipelineWidth).flatMap(y =>
    (0 until y).map(x => rrBankConflict(x)(y))
  ))
  XSPerfAccumulate("data_array_multi_rr_bank_conflict", rrBankConflictCount >= 2.U)
  (1 until LoadPipelineWidth).foreach(y => (0 until y).foreach(x =>
    XSPerfAccumulate(s"data_array_rr_bank_conflict_${x}_${y}", rrBankConflict(x)(y))
  ))
  (0 until LoadPipelineWidth).foreach(i => {
    XSPerfAccumulate(s"data_array_rrl_bank_conflict_${i}", rrlBankConflict(i))
    XSPerfAccumulate(s"data_array_rw_bank_conflict_${i}", wrBankConflict(i))
    XSPerfAccumulate(s"data_array_read_${i}", io.read(i).valid)
  })
  XSPerfAccumulate("data_array_access_total", PopCount(io.read.map(_.valid)))
  XSPerfAccumulate("data_array_read_line", io.readline.valid)
  XSPerfAccumulate("data_array_write", io.write.valid)

  val readResult = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, Vec(DCacheWays,new L1BankedDataReadResult()))))
  val readResultDelayed = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, Vec(DCacheWays,new L1BankedDataReadResult()))))
  val readErrorDelayedResult = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, Vec(DCacheWays, Bool()))))
  dontTouch(readResult)
  dontTouch(readErrorDelayedResult)

  val pseudoDataToggleMask = io.pseudoError.bits.map {
    case bank =>
      Mux(io.pseudoError.valid && bank.valid, bank.mask(DCacheSRAMRowBits - 1, 0), 0.U)
  }
  val readlineHit = io.readline.fire &&
                     (io.readline.bits.rmask & VecInit(io.pseudoError.bits.map(_.valid)).asUInt).orR
  val readBankHit = io.read.zip(bankAddrs.zip(io.is128Req)).zipWithIndex.map {
                          case ((read, (bankAddr, is128Req)), i) =>
                            val errorBank0 = io.pseudoError.bits(bankAddr(0))
                            val errorBank1 = io.pseudoError.bits(bankAddr(1))
                            read.fire && (errorBank0.valid || errorBank1.valid && is128Req) && !io.bankConflictSlow(i)
                      }.reduce(_|_)
  io.pseudoError.ready := RegNext(readlineHit || readBankHit)

  for (divIndex <- 0 until DCacheSetDiv){
    for (bankIndex <- 0 until DCacheBanks) {
      for (wayIndex <- 0 until DCacheWays) {
        //     Set Addr & Read Way Mask
        //
        //    Pipe 0   ....  Pipe (n-1)
        //      +      ....     +
        //      |      ....     |
        // +----+---------------+-----+
        //  X                        X
        //   X                      +------+ Bank Addr Match
        //    +---------+----------+
        //              |
        //     +--------+--------+
        //     |    Data Bank    |
        //     +-----------------+
        val loadPipeEn = WireInit(VecInit(List.tabulate(LoadPipelineWidth)(i => {
          io.read(i).valid && divAddrs(i) === divIndex.U && (bankAddrs(i)(0) === bankIndex.U || bankAddrs(i)(1) === bankIndex.U && io.is128Req(i)) &&
          wayEn(i)(wayIndex) &&
          !rrBankConflictOldest(i)
        })))
        val readlineEn = Wire(Bool())
        if (ReduceReadlineConflict) {
          readlineEn := io.readline.valid && io.readline.bits.rmask(bankIndex) && lineWayEn(wayIndex) && divIndex.U === lineDivAddr
        } else {
          readlineEn := io.readline.valid && lineWayEn(wayIndex) && divIndex.U === lineDivAddr
        }
        val sramSetAddr = Mux(readlineEn,
          addrToDcacheDivSet(io.readline.bits.addr),
          PriorityMux(Seq.tabulate(LoadPipelineWidth)(i => loadPipeEn(i) -> setAddrs(i)))
        )
        val readEn = loadPipeEn.asUInt.orR || readlineEn
        // read raw data
        val dataBank = dataBanks(divIndex)(bankIndex)(wayIndex)
        dataBank.io.r.en := readEn
        dataBank.io.r.addr := sramSetAddr

        readResult(divIndex)(bankIndex)(wayIndex).ecc := getECCFromEncWord(dataBank.io.r.data)
        readResult(divIndex)(bankIndex)(wayIndex).rawData := getDataFromEncWord(dataBank.io.r.data) ^ pseudoDataToggleMask(bankIndex)

        if (EnableDataEcc) {
          val eccData = readResult(divIndex)(bankIndex)(wayIndex).asECCData()
          val eccDataDelayed = RegEnable(eccData, RegNext(readEn))
          readResult(divIndex)(bankIndex)(wayIndex).errorDelayed := dcacheParameters.dataCode.decode(eccDataDelayed).error
          readErrorDelayedResult(divIndex)(bankIndex)(wayIndex) := readResult(divIndex)(bankIndex)(wayIndex).errorDelayed
        } else {
          readResult(divIndex)(bankIndex)(wayIndex).errorDelayed := false.B
          readErrorDelayedResult(divIndex)(bankIndex)(wayIndex) := false.B
        }

        readResultDelayed(divIndex)(bankIndex)(wayIndex) := RegEnable(readResult(divIndex)(bankIndex)(wayIndex), RegNext(readEn))
      }
    }
  }

  val dataReadOh = WireInit(VecInit(Seq.fill(DCacheSetDiv * DCacheBanks * DCacheWays)(0.U(1.W))))
  for(divIndex <- 0 until DCacheSetDiv){
    for (bankIndex <- 0 until DCacheBanks) {
      for (wayIndex <- 0 until DCacheWays) {
        dataReadOh(divIndex *  DCacheBanks * DCacheWays + bankIndex * DCacheWays + wayIndex) := dataBanks(divIndex)(bankIndex)(wayIndex).io.r.en
      }
    }
  }
  XSPerfAccumulate("data_read_counter", PopCount(Cat(dataReadOh)))

  // read result: expose banked read result
  // TODO: clock gate
  (0 until LoadPipelineWidth).map(i => {
    // io.readResp(i) := readResult(RegNext(bankAddrs(i)))(RegNext(OHToUInt(wayEn(i))))
    val rReadFire = RegNext(io.read(i).fire)
    val rDivAddr  = RegEnable(divAddrs(i), io.read(i).fire)
    val rBankAddr = RegEnable(bankAddrs(i), io.read(i).fire)
    val rWayAddr  = RegNext(OHToUInt(wayEn(i)))
    val rrReadFire = RegNext(RegNext(io.read(i).fire))
    val rrDivAddr = RegEnable(RegEnable(divAddrs(i), io.read(i).fire), rReadFire)
    val rrBankAddr = RegEnable(RegEnable(bankAddrs(i), io.read(i).fire), rReadFire)
    val rrWayAddr = RegEnable(RegEnable(OHToUInt(wayEn(i)), io.read(i).fire), rReadFire)
    (0 until VLEN/DCacheSRAMRowBits).map( j =>{
      io.readResp(i)(j) := readResult(rDivAddr)(rBankAddr(j))(rWayAddr)
      // error detection
      // normal read ports
      io.readErrorDelayed(i)(j) := rrReadFire && readErrorDelayedResult(rrDivAddr)(rrBankAddr(j))(rrWayAddr) && !RegNext(io.bankConflictSlow(i))
    })
  })

  // readline port
  val readlineErrorDelayed = Wire(Vec(DCacheBanks, Bool()))
  val readlineRWayAddr = RegEnable(io.readline.bits.way, io.readline.valid)
  val readlineRrWayAddr = RegEnable(readlineRWayAddr, RegNext(io.readline.valid))
  val readlineRDivAddr = RegEnable(lineDivAddr, io.readline.valid)
  val readlineRrDivAddr = RegEnable(readlineRDivAddr, RegNext(io.readline.valid))
  (0 until DCacheBanks).map(i => {
    io.readlineResp(i) := readResult(readlineRDivAddr)(i)(readlineRWayAddr)
    readlineErrorDelayed(i) := readResult(readlineRrDivAddr)(i)(readlineRrWayAddr).errorDelayed
  })
  io.readlineError := RegNext(RegNext(io.readline.fire)) && readlineErrorDelayed.asUInt.orR
  io.readlineErrorDelayed := RegNext(RegNext(io.readline.fire)) && readlineErrorDelayed.asUInt.orR

  // write dataBanks & ecc_banks
  for (divIndex <- 0 until DCacheSetDiv) {
    for (bankIndex <- 0 until DCacheBanks) {
      for (wayIndex <- 0 until DCacheWays) {
        // data write
        val wenReg = writeBankMaskReg(bankIndex) &&
          writeValidDupReg(bankIndex) &&
          writeDivAddrDupReg(bankIndex) === divIndex.U &&
          writeWayEnDupReg(bankIndex)(wayIndex)
        val writeEccReg = RegEnable(getECCFromEncWord(cacheParams.dataCode.encode(io.write.bits.data(bankIndex))), io.write.valid)
        val dataBank = dataBanks(divIndex)(bankIndex)(wayIndex)
        dataBank.io.w.en := wenReg
        dataBank.io.w.addr := writeSetAddrDupReg(bankIndex)
        dataBank.io.w.data := asECCData(writeEccReg, writeDataReg(bankIndex))
      }
    }
  }

  val tableName =  "BankConflict" + p(XSCoreParamsKey).HartId.toString
  val siteName = "BankedDataArray" + p(XSCoreParamsKey).HartId.toString
  val bankConflictTable = ChiselDB.createTable(tableName, new BankConflictDB)
  val bankConflictData = Wire(new BankConflictDB)
  for (i <- 0 until LoadPipelineWidth) {
    bankConflictData.setIndex(i) := setAddrs(i)
    bankConflictData.addr(i) := io.read(i).bits.addr
  }

  // FIXME: rrBankConflict(0)(1) no generalization
  when(rrBankConflict(0)(1)) {
    (0 until (VLEN/DCacheSRAMRowBits)).map(i => {
      bankConflictData.bankIndex(i) := bankAddrs(0)(i)
    })
    bankConflictData.wayIndex  := OHToUInt(wayEn(0))
    bankConflictData.fakeRrBankConflict := setAddrs(0) === setAddrs(1) && divAddrs(0) === divAddrs(1)
  }.otherwise {
    (0 until (VLEN/DCacheSRAMRowBits)).map(i => {
      bankConflictData.bankIndex(i) := 0.U
    })
    bankConflictData.wayIndex := 0.U
    bankConflictData.fakeRrBankConflict := false.B
  }

  val isWriteBankConflictTable = Constantin.createRecord(s"isWriteBankConflictTable${p(XSCoreParamsKey).HartId}")
  bankConflictTable.log(
    data = bankConflictData,
    en = isWriteBankConflictTable.orR && rrBankConflict(0)(1),
    site = siteName,
    clock = clock,
    reset = reset
  )

  (1 until LoadPipelineWidth).foreach(y => (0 until y).foreach(x =>
    XSPerfAccumulate(s"data_array_fake_rr_bank_conflict_${x}_${y}", rrBankConflict(x)(y) && setAddrs(x)===setAddrs(y) && divAddrs(x) === divAddrs(y))
  ))

  if (backendParams.debugEn){
    loadReqWithBankConflict.map(dontTouch(_))
    dontTouch(readResult)
    dontTouch(readErrorDelayedResult)
  }
}

// the smallest access unit is bank
class BankedDataArray(implicit p: Parameters) extends AbstractBankedDataArray {
  println("  DCacheType: BankedDataArray")
  // Keep precise readline bank conflicts enabled for the 32x2B path.
  val ReduceReadlineConflict = true
  require(
    io.pseudoError.bits.head.mask.getWidth >= DCacheSRAMRowBits,
    "pseudo-error masks must cover the data-bank row width"
  )

  io.write.ready := true.B
  io.writeDup.foreach(_.ready := true.B)

  val dataBanks = Seq.tabulate(DCacheSetDiv, DCacheBanks)({(k, i) => Module(new DataSRAMBank(i))})
  val mbistPl = MbistPipeline.PlaceMbistPipeline(1, s"MbistPipeDCacheData", hasMbist)
  val mbistSramPorts = mbistPl.map(pl => Seq.tabulate(DCacheSetDiv, DCacheBanks) { (i, j) =>
    pl.toSRAM(i * DCacheBanks + j)
  })
  private val mbistAck = mbistPl.map(_.mbist.ack).getOrElse(false.B)

  dataBanks.map(_.map(_.dump()))

  val wayEn = Wire(Vec(LoadPipelineWidth, io.read(0).bits.wayEn.cloneType))
  val setAddrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val setAddrsDup = Wire(Vec(LoadPipelineWidth, UInt()))
  val divAddrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val divAddrsDup = Wire(Vec(LoadPipelineWidth, UInt()))
  val bankAddrs = Wire(Vec(LoadPipelineWidth, Vec(DCacheVWordBankCount, UInt(log2Up(DCacheBanks).W))))
  val bankAddrsDup = Wire(Vec(LoadPipelineWidth, Vec(DCacheVWordBankCount, UInt(log2Up(DCacheBanks).W))))
  val wayEnReg = Wire(Vec(LoadPipelineWidth, io.read(0).bits.wayEn.cloneType))
  val setAddrsReg = Wire(Vec(LoadPipelineWidth, UInt()))
  val setAddrsDupReg = Wire(Vec(LoadPipelineWidth, UInt()))

  val lineSetAddr = addrToDcacheDivSet(io.readline.bits.addr)
  val lineDivAddr = addrToDcacheDiv(io.readline.bits.addr)
  val lineWayEn = io.readline.bits.wayEn

  val writeBankMaskReg = RegEnable(io.write.bits.wmask, io.write.valid)
  val writeDataReg = RegEnable(io.write.bits.data, io.write.valid)
  val writeValidReg = RegNext(io.write.valid)
  val writeValidDupReg = io.writeDup.map(x => RegNext(x.valid))
  val writeWayEnDupReg = io.writeDup.map(x => RegEnable(x.bits.wayEn, x.valid))
  val writeSetAddrDupReg = io.writeDup.map(x => RegEnable(addrToDcacheDivSet(x.bits.addr), x.valid))
  val writeDivAddrDupReg = io.writeDup.map(x => RegEnable(addrToDcacheDiv(x.bits.addr), x.valid))

  // read dataBanks and ecc_banks
  // for single port SRAM, do not allow read and write in the same cycle
  val rwhazard = RegNext(io.write.valid)
  val rrhazard = false.B // io.readline.valid
  (0 until LoadPipelineWidth).map(rportIndex => {
    divAddrs(rportIndex) := addrToDcacheDiv(io.read(rportIndex).bits.addr)
    divAddrsDup(rportIndex) := addrToDcacheDiv(io.read(rportIndex).bits.addrDup)
    val bankAddrVWordBase = addrToVWordBankBase(io.read(rportIndex).bits.addr)
    val bankAddrDupVWordBase = addrToVWordBankBase(io.read(rportIndex).bits.addrDup)
    (0 until DCacheVWordBankCount).foreach { bankOffset =>
      bankAddrs(rportIndex)(bankOffset) :=
        (bankAddrVWordBase + bankOffset.U)(log2Up(DCacheBanks) - 1, 0)
      bankAddrsDup(rportIndex)(bankOffset) :=
        (bankAddrDupVWordBase + bankOffset.U)(log2Up(DCacheBanks) - 1, 0)
    }
    setAddrs(rportIndex) := addrToDcacheDivSet(io.read(rportIndex).bits.addr)
    setAddrsDup(rportIndex) := addrToDcacheDivSet(io.read(rportIndex).bits.addrDup)
    setAddrsReg(rportIndex) := RegEnable(addrToDcacheDivSet(io.read(rportIndex).bits.addr), io.read(rportIndex).valid)
    setAddrsDupReg(rportIndex) := RegEnable(addrToDcacheDivSet(io.read(rportIndex).bits.addrDup), io.read(rportIndex).valid)

    // use wayEn to select a way after data read out
    assert(!(RegNext(io.read(rportIndex).fire && PopCount(io.read(rportIndex).bits.wayEn) > 1.U)))
    wayEn(rportIndex) := io.read(rportIndex).bits.wayEn
    wayEnReg(rportIndex) := RegEnable(io.read(rportIndex).bits.wayEn, io.read(rportIndex).valid)
  })

  // read each bank, get bank result
  val rrBankConflict = Seq.tabulate(LoadPipelineWidth)(x => Seq.tabulate(LoadPipelineWidth)(y => {
    if (x == y) {
      false.B
    } else {
      io.read(x).valid && io.read(y).valid &&
      divAddrs(x) === divAddrs(y) &&
      (io.read(x).bits.bankMask & io.read(y).bits.bankMask) =/= 0.U &&
      setAddrs(x) =/= setAddrs(y)
    }
  }
  ))

  val loadReqWithBankConflict = rrBankConflict.map(_.reduce(_ || _))
  val loadReqValid = io.read.map(_.valid)
  val loadReqLqIdx = io.read.map(_.bits.lqIdx)
  val loadReqIndex = (0 until LoadPipelineWidth).map(_.asUInt)

  val loadReqBankConflictSelcet = selcetOldestPort(loadReqWithBankConflict, loadReqLqIdx, loadReqIndex)
  val loadReqBankSelectPort  = UIntToOH(loadReqBankConflictSelcet._2).asBools

  val rrBankConflictOldest = (0 until LoadPipelineWidth).map(i =>
    !loadReqBankSelectPort(i) && loadReqWithBankConflict(i)
  )

  val rrlBankConflict = Wire(Vec(LoadPipelineWidth, Bool()))
  val rrlBankConflictIntend = Wire(Vec(LoadPipelineWidth, Bool()))
  (0 until LoadPipelineWidth).foreach { i =>
    val judge = if (ReduceReadlineConflict) {
      io.read(i).valid &&
      (io.readline.bits.rmask & io.read(i).bits.bankMask) =/= 0.U &&
      divAddrs(i) === lineDivAddr &&
      setAddrs(i) =/= lineSetAddr
    } else {
      io.read(i).valid &&
      divAddrs(i) === lineDivAddr &&
      setAddrs(i) =/= lineSetAddr
    }
    rrlBankConflict(i) := judge && io.readline.valid
    rrlBankConflictIntend(i) := judge && io.readlineIntend
  }
  val wrBankConflict = Seq.tabulate(LoadPipelineWidth)(x =>
    io.read(x).valid &&
    writeValidReg &&
    divAddrs(x) === writeDivAddrDupReg.head &&
    (writeBankMaskReg & io.read(x).bits.bankMask) =/= 0.U
  )
  val wrlBankConflict = io.readline.valid && writeValidReg &&
    lineDivAddr === writeDivAddrDupReg.head &&
    (writeBankMaskReg & io.readline.bits.rmask) =/= 0.U
  // ready
  io.readline.ready := !(wrlBankConflict)
  io.read.zipWithIndex.map{case(x, i) => x.ready := !(wrBankConflict(i) || rrhazard)}

  val perfMultiRead = PopCount(io.read.map(_.valid)) >= 2.U
  (0 until LoadPipelineWidth).foreach(i => {
    // remove fake rrBankConflict situation in s2
    val realOtherBankConflictReg = RegNext(wrBankConflict(i) || rrlBankConflict(i))
    val realRrBankConflictReg = RegNext(rrBankConflictOldest(i))
    io.bankConflictSlow(i) := realOtherBankConflictReg || realRrBankConflictReg

    // get result in s1
    io.disableLdFastWakeup(i) := wrBankConflict(i) || rrlBankConflictIntend(i) ||
      (if (i == 0) 0.B else (0 until i).map(rrBankConflict(_)(i)).reduce(_ || _))
  })
  XSPerfAccumulate("data_array_multi_read", perfMultiRead)
  val rrBankConflictCount = PopCount((1 until LoadPipelineWidth).flatMap(y =>
    (0 until y).map(x => rrBankConflict(x)(y))
  ))
  XSPerfAccumulate("data_array_multi_rr_bank_conflict", rrBankConflictCount >= 2.U)
  (1 until LoadPipelineWidth).foreach(y => (0 until y).foreach(x =>
    XSPerfAccumulate(s"data_array_rr_bank_conflict_${x}_${y}", rrBankConflict(x)(y))
  ))
  (0 until LoadPipelineWidth).foreach(i => {
    XSPerfAccumulate(s"data_array_rrl_bank_conflict_${i}", rrlBankConflict(i))
    XSPerfAccumulate(s"data_array_rw_bank_conflict_${i}", wrBankConflict(i))
    XSPerfAccumulate(s"data_array_read_${i}", io.read(i).valid)
  })
  XSPerfAccumulate("data_array_access_total", PopCount(io.read.map(_.valid)))
  XSPerfAccumulate("data_array_read_line", io.readline.valid)
  XSPerfAccumulate("data_array_write", io.write.valid)

  val bankResult = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, Vec(DCacheWays, new L1BankedDataReadResult()))))
  val bankResultDelayed = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, Vec(DCacheWays, new L1BankedDataReadResult()))))
  val readBankErrorDelayed = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, Vec(DCacheWays, Bool()))))

  val pseudoDataToggleMask = io.pseudoError.bits.map {
    case bank =>
      Mux(io.pseudoError.valid && bank.valid, bank.mask(DCacheSRAMRowBits - 1, 0), 0.U)
  }
  val readlineHit = io.readline.fire &&
                     (io.readline.bits.rmask & VecInit(io.pseudoError.bits.map(_.valid)).asUInt).orR
  val pseudoErrorBankValid = VecInit(io.pseudoError.bits.map(_.valid)).asUInt
  val readBankHit = io.read.zipWithIndex.map {
    case (read, i) =>
      read.fire && (read.bits.bankMask & pseudoErrorBankValid).orR && !io.bankConflictSlow(i)
  }.reduce(_|_)
  io.pseudoError.ready := RegNext(readlineHit || readBankHit)

  for (divIndex <- 0 until DCacheSetDiv) {
    for (bankIndex <- 0 until DCacheBanks) {
      //     Set Addr & Read Way Mask
      //
      //    Pipe 0   ....  Pipe (n-1)
      //      +      ....     +
      //      |      ....     |
      // +----+---------------+-----+
      //  X                        X
      //   X                      +------+ Bank Addr Match
      //    +---------+----------+
      //              |
      //     +--------+--------+
      //     |    Data Bank    |
      //     +-----------------+
      val bankAddrMatchs = WireInit(VecInit(List.tabulate(LoadPipelineWidth)(i => {
        io.read(i).valid && divAddrs(i) === divIndex.U &&
          io.read(i).bits.bankMask(bankIndex) && !rrBankConflictOldest(i)
      })))
      val bankAddrMatchsDup = WireInit(VecInit(List.tabulate(LoadPipelineWidth)(i => {
        io.read(i).valid && divAddrsDup(i) === divIndex.U &&
          io.read(i).bits.bankMask(bankIndex) && !rrBankConflictOldest(i)
      })))
      val readlineMatch = Wire(Bool())
      if (ReduceReadlineConflict) {
        readlineMatch := io.readline.valid && io.readline.bits.rmask(bankIndex) && lineDivAddr === divIndex.U
      } else {
        readlineMatch := io.readline.valid && lineDivAddr === divIndex.U
      }
      val bankSetAddr = Mux(readlineMatch,
        lineSetAddr,
        PriorityMux(Seq.tabulate(LoadPipelineWidth)(i => bankAddrMatchs(i) -> setAddrs(i)))
      )
      val bankSetAddrDup = Mux(readlineMatch,
        lineSetAddr,
        PriorityMux(Seq.tabulate(LoadPipelineWidth)(i => bankAddrMatchsDup(i) -> setAddrsDup(i)))
      )
      val readEnable = bankAddrMatchs.asUInt.orR || readlineMatch

      // read raw data
      val dataBank = dataBanks(divIndex)(bankIndex)
      dataBank.io.r.en := readEnable

      if (DuplicatedQueryBankSeq.contains(bankIndex)) {
        dataBank.io.r.addr := bankSetAddrDup
      } else {
        dataBank.io.r.addr := bankSetAddr
      }
      for (wayIndex <- 0 until DCacheWays) {
        val mbistAck = mbistSramPorts.map(_(divIndex)(bankIndex).ack).getOrElse(false.B)
        bankResult(divIndex)(bankIndex)(wayIndex).ecc := getECCFromEncWord(dataBank.io.r.data(wayIndex))
        bankResult(divIndex)(bankIndex)(wayIndex).rawData := getDataFromEncWord(dataBank.io.r.data(wayIndex)) ^ Mux(mbistAck, 0.U, pseudoDataToggleMask(bankIndex))

        if (EnableDataEcc) {
          val eccData = bankResult(divIndex)(bankIndex)(wayIndex).asECCData()
          val eccDataDelayed = RegEnable(eccData, RegNext(readEnable))
          bankResult(divIndex)(bankIndex)(wayIndex).errorDelayed := dcacheParameters.dataCode.decode(eccDataDelayed).error
          readBankErrorDelayed(divIndex)(bankIndex)(wayIndex) := bankResult(divIndex)(bankIndex)(wayIndex).errorDelayed
        } else {
          bankResult(divIndex)(bankIndex)(wayIndex).errorDelayed := false.B
          readBankErrorDelayed(divIndex)(bankIndex)(wayIndex) := false.B
        }
        bankResultDelayed(divIndex)(bankIndex)(wayIndex) := RegEnable(bankResult(divIndex)(bankIndex)(wayIndex), RegNext(readEnable))
      }
    }
  }

  val dataReadOh = WireInit(VecInit(Seq.fill(DCacheSetDiv)(0.U(XLEN.W))))
  for (divIndex <- 0 until DCacheSetDiv){
    val temp = WireInit(VecInit(Seq.fill(DCacheBanks)(0.U(XLEN.W))))
    for (bankIndex <- 0 until DCacheBanks) {
      temp(bankIndex) := dataBanks(divIndex)(bankIndex).io.r.en
    }
    dataReadOh(divIndex) := temp.reduce(_ + _)
  }
  XSPerfAccumulate("data_read_counter", dataReadOh.foldLeft(0.U)(_ + _))

  (0 until LoadPipelineWidth).map(i => {
    // 1 cycle after read fire(load s2)
    val rReadFire = RegNext(io.read(i).fire)
    val rDivAddr = RegEnable(divAddrs(i), io.read(i).fire)
    val rBankAddr = RegEnable(bankAddrs(i), io.read(i).fire)
    val rWayAddr = RegEnable(OHToUInt(wayEn(i)), io.read(i).fire)
    // 2 cycles after read fire(load s3)
    val rrReadFire = RegNext(rReadFire)
    val rrDivAddr = RegEnable(RegEnable(divAddrs(i), io.read(i).fire), rReadFire)
    val rrBankAddr = RegEnable(RegEnable(bankAddrs(i), io.read(i).fire), rReadFire)
    val rrWayAddr = RegEnable(RegEnable(OHToUInt(wayEn(i)), io.read(i).fire), rReadFire)
    (0 until VLEN/DCacheSRAMRowBits).map( j =>{
      io.readResp(i)(j)          := bankResult(rDivAddr)(rBankAddr(j))(rWayAddr)
      // error detection
      io.readErrorDelayed(i)(j) := rrReadFire && readBankErrorDelayed(rrDivAddr)(rrBankAddr(j))(rrWayAddr) && !RegNext(io.bankConflictSlow(i))
    })
  })

  val readlineError = Wire(Vec(DCacheBanks, Bool()))
  val readlineErrorDelayed = Wire(Vec(DCacheBanks, Bool()))
  val readlineRWayAddr = RegEnable(OHToUInt(io.readline.bits.wayEn), io.readline.fire)
  val readlineRrWayAddr = RegEnable(readlineRWayAddr, RegNext(io.readline.fire))
  val readlineRDivAddr = RegEnable(lineDivAddr, io.readline.fire)
  val readlineRrDivAddr = RegEnable(readlineRDivAddr, RegNext(io.readline.fire))
  val readlineResp = Wire(io.readlineResp.cloneType)
  val mbistPackedResp = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, UInt((DCacheWays * encDataBits).W))))
  for (div <- 0 until DCacheSetDiv; bank <- 0 until DCacheBanks) {
    val packedBankResult = Cat((0 until DCacheWays).reverse.map { way =>
      bankResult(div)(bank)(way).asECCData()
    })
    mbistPackedResp(div)(bank) := Mux(
      io.readlineCanGo | mbistAck,
      packedBankResult,
      RegEnable(mbistPackedResp(div)(bank), io.readlineStall | mbistAck)
    )
  }
  mbistSramPorts.foreach { ports =>
    for (div <- 0 until DCacheSetDiv; bank <- 0 until DCacheBanks) {
      val port = ports(div)(bank)
      val row = mbistPackedResp(div)(bank)
      require(port.params.dataWidth * port.params.nodeNum == row.getWidth)
      port.rdata := RegEnable(
        Mux1H(port.selectedOH, row.asTypeOf(Vec(port.params.nodeNum, UInt(port.params.dataWidth.W)))),
        io.readlineStall | mbistAck
      )
    }
  }
  (0 until DCacheBanks).foreach(i => {
    readlineResp(i) := Mux(
      io.readlineCanGo,
      bankResult(readlineRDivAddr)(i)(readlineRWayAddr),
      RegEnable(readlineResp(i), io.readlineStall)
    )

    if (EnableDataEcc) {
      readlineError(i) := bankResult(readlineRrDivAddr)(i)(readlineRrWayAddr).errorDelayed
     //
      val eccDataDelayed = io.readlineResp(i).asECCData()
      readlineErrorDelayed(i) := dcacheParameters.dataCode.decode(eccDataDelayed).error
    } else {
      readlineError(i) := false.B
      readlineErrorDelayed(i) := false.B
    }
  })
  io.readlineResp := RegEnable(readlineResp, io.readlineCanResp)
  io.readlineError := readlineError.asUInt.orR
  io.readlineErrorDelayed := readlineErrorDelayed.asUInt.orR

  // write dataBanks & ecc_banks
  for (divIndex <- 0 until DCacheSetDiv) {
    for (bankIndex <- 0 until DCacheBanks) {
      // data write
      val wenReg = writeBankMaskReg(bankIndex) &&
        writeValidDupReg(bankIndex) &&
        writeDivAddrDupReg(bankIndex) === divIndex.U && RegNext(io.write.valid)
      val writeEccReg = RegEnable(getECCFromEncWord(cacheParams.dataCode.encode(io.write.bits.data(bankIndex))), io.write.valid)
      val dataBank = dataBanks(divIndex)(bankIndex)
      dataBank.io.w.en := wenReg
      dataBank.io.w.wayEn := writeWayEnDupReg(bankIndex)
      dataBank.io.w.addr := writeSetAddrDupReg(bankIndex)
      dataBank.io.w.data := asECCData(writeEccReg, writeDataReg(bankIndex))
    }
  }

  val tableName = "BankConflict" + p(XSCoreParamsKey).HartId.toString
  val siteName = "BankedDataArray" + p(XSCoreParamsKey).HartId.toString
  val bankConflictTable = ChiselDB.createTable(tableName, new BankConflictDB)
  val bankConflictData = Wire(new BankConflictDB)
  for (i <- 0 until LoadPipelineWidth) {
    bankConflictData.setIndex(i) := setAddrs(i)
    bankConflictData.addr(i) := io.read(i).bits.addr
  }

  // FIXME: rrBankConflict(0)(1) no generalization
  when(rrBankConflict(0)(1)) {
    (0 until (VLEN/DCacheSRAMRowBits)).map(i => {
      bankConflictData.bankIndex(i) := bankAddrs(0)(i)
    })
    bankConflictData.wayIndex := OHToUInt(wayEn(0))
    bankConflictData.fakeRrBankConflict := setAddrs(0) === setAddrs(1) && divAddrs(0) === divAddrs(1)
  }.otherwise {
    (0 until (VLEN/DCacheSRAMRowBits)).map(i => {
      bankConflictData.bankIndex(i) := 0.U
    })
    bankConflictData.wayIndex := 0.U
    bankConflictData.fakeRrBankConflict := false.B
  }

  val isWriteBankConflictTable = Constantin.createRecord(s"isWriteBankConflictTable${p(XSCoreParamsKey).HartId}")
  bankConflictTable.log(
    data = bankConflictData,
    en = isWriteBankConflictTable.orR && rrBankConflict(0)(1),
    site = siteName,
    clock = clock,
    reset = reset
  )

  (1 until LoadPipelineWidth).foreach(y => (0 until y).foreach(x =>
    XSPerfAccumulate(s"data_array_fake_rr_bank_conflict_${x}_${y}", rrBankConflict(x)(y) && setAddrs(x) === setAddrs(y) && divAddrs(x) === divAddrs(y))
  ))

  if (backendParams.debugEn){
    loadReqWithBankConflict.map(dontTouch(_))
    dontTouch(bankResult)
    dontTouch(readBankErrorDelayed)
  }
}
