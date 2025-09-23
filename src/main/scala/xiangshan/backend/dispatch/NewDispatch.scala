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
***************************************************************************************/

package xiangshan.backend.dispatch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import xiangshan.ExceptionNO._
import xiangshan._
import xiangshan.backend.rob.{RobDispatchTopDownIO, RobEnqIO}
import xiangshan.backend.Bundles.{DecodeOutUop, DispatchOutUop, DispatchUpdateUop, DynInst, ExuVec, IssueQueueIQWakeUpBundle, RenameOutUop, connectSamePort}
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.rename.{BusyTable, VlBusyTable}
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.rename.BusyTableReadIO
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.datapath.WbConfig.VfWB
import xiangshan.backend.fu.FuType.FuTypeOrR
import xiangshan.backend.regcache.{RCTagTableReadPort, RegCacheTagTable}
import xiangshan.mem.MemCoreTopDownIO
import xiangshan.mem.mdp._
import xiangshan.mem._

class CoreDispatchTopDownIO extends Bundle {
  val l2MissMatch = Input(Bool())
  val l3MissMatch = Input(Bool())
  val fromMem = Flipped(new MemCoreTopDownIO)
}
// TODO delete trigger message from frontend to iq
class NewDispatch(implicit p: Parameters) extends XSModule with HasPerfEvents with HasVLSUParameters {
  // std IQ donot need dispatch, only copy sta IQ, but need sta IQ's ready && std IQ's ready
  val allIssueParams = backendParams.allIssueParams.filter(_.StdCnt == 0)
  val allExuParams = allIssueParams.map(_.exuBlockParams).flatten
  val allFuConfigs = allExuParams.map(_.fuConfigs).flatten.toSet.toSeq
  val sortedFuConfigs = allFuConfigs.sortBy(_.fuType.id)
  println(s"[NewDispatch] ${allExuParams.map(_.name)}")
  println(s"[NewDispatch] ${allFuConfigs.map(_.name)}")
  println(s"[NewDispatch] ${allFuConfigs.map(_.fuType.id)}")
  println(s"[NewDispatch] ${sortedFuConfigs.map(_.name)}")
  println(s"[NewDispatch] ${sortedFuConfigs.map(_.fuType.id)}")
  val fuConfigsInIssueParams = allIssueParams.map(_.allExuParams.map(_.fuConfigs).flatten.toSet.toSeq)
  val fuMapIQIdx = sortedFuConfigs.map( fu => {
    val fuInIQIdx = fuConfigsInIssueParams.zipWithIndex.filter { case (f, i) => f.contains(fu) }.map(_._2)
    (fu -> fuInIQIdx)
   }
  )
  fuMapIQIdx.map { case (fu, iqidx) =>
    println(s"[NewDispatch] ${fu.name} $iqidx")
  }
  val sameIQIdxFus = fuMapIQIdx.map{ case (fu, iqidx) =>
    fuMapIQIdx.filter(_._2 == iqidx).map(_._1) -> iqidx
  }.toSet.toSeq
  val needMultiIQ = sameIQIdxFus.sortBy(_._1.head.fuType.id).filter(_._2.size > 1)
  val needSingleIQ = sameIQIdxFus.sortBy(_._1.head.fuType.id).filter(_._2.size == 1)
  needMultiIQ.map { case (fus, iqidx) =>
    println(s"[NewDispatch] needMultiIQ: ${fus.map(_.name)} $iqidx")
  }
  needSingleIQ.map { case (fus, iqidx) =>
    println(s"[NewDispatch] needSingleIQ: ${fus.map(_.name)} $iqidx")
  }
  val fuConfigsInExuParams = allExuParams.map(_.fuConfigs)
  val fuMapExuIdx = sortedFuConfigs.map { case fu => {
    val fuInExuIdx = fuConfigsInExuParams.zipWithIndex.filter { case (f, i) => f.contains(fu) }.map(_._2)
    (fu -> fuInExuIdx)
    }
  }
  val sameExuIdxFus = fuMapExuIdx.map { case (fu, exuidx) =>
    fuMapExuIdx.filter(_._2 == exuidx).map(_._1) -> exuidx
  }.toSet.toSeq
  val needMultiExu = sameExuIdxFus.sortBy(_._1.head.fuType.id).filter(_._2.size > 1).filter{ x =>
    x._1.map(y => fuMapIQIdx.filter(_._1 == y).head._2.size > 1).reduce(_ && _)
  }

  val exuNum = allExuParams.size
  val maxIQSize = allIssueParams.map(_.numEntries).max
  val IQEnqSum = allIssueParams.map(_.numEnq).sum

  val io = IO(new Bundle {
    // from rename
    val renameIn = Vec(RenameWidth, Flipped(ValidIO(new DecodeOutUop)))
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new RenameOutUop)))
    val toRenameAllFire = Output(Bool())
    // enq Rob
    val enqRob = Flipped(new RobEnqIO)
    // IssueQueues
    val IQValidNumVec = Vec(exuNum, Input(UInt(maxIQSize.U.getWidth.W)))
    val toIssueQueues = Vec(IQEnqSum, DecoupledIO(new DispatchOutUop))
    // to busyTable
    // set preg state to ready (write back regfile)
    val wbPregsInt = Vec(backendParams.numPregWb(IntData()), Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val wbPregsFp = Vec(backendParams.numPregWb(FpData()), Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val wbPregsVec = Vec(backendParams.numPregWb(VecData()), Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val wbPregsV0 = Vec(backendParams.numPregWb(V0Data()), Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val wbPregsVl = Vec(backendParams.numPregWb(VlData()), Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val wakeUpAll = new Bundle {
      val wakeUpInt: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(backendParams.intSchdParams.get.genIQWakeUpOutValidBundle)
      val wakeUpFp: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(backendParams.fpSchdParams.get.genIQWakeUpOutValidBundle)
      val wakeUpVec: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(backendParams.vecSchdParams.get.genIQWakeUpOutValidBundle)
    }
    val og0Cancel = Input(ExuVec())
    val ldCancel = Vec(backendParams.LdExuCnt, Flipped(new LoadCancelIO))
    // to vlbusytable
    val vlWriteBackInfo = new Bundle {
      val vlFromIntIsZero  = Input(Bool())
      val vlFromIntIsVlmax = Input(Bool())
      val vlFromVfIsZero   = Input(Bool())
      val vlFromVfIsVlmax  = Input(Bool())
    }
    // from MemBlock
    val fromMem = new Bundle {
      val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
      val scommit = Input(UInt(log2Ceil(EnsbufferWidth + 1).W)) // connected to `memBlock.io.sqDeq` instead of ROB
      val lqDeqPtr = Input(new LqPtr)
      val sqDeqPtr = Input(new SqPtr)
      // from lsq
      val lqCancelCnt = Input(UInt(log2Up(VirtualLoadQueueSize + 1).W))
      val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))
    }
    //toMem
    val toMem = new Bundle {
      val lsqEnqIO = Flipped(new LsqEnqIO)
    }
    // redirect
    val redirect = Flipped(ValidIO(new Redirect))
    // singleStep
    val singleStep = Input(Bool())
    // lfst
    val lfst = new DispatchLFSTIO

    // perf only
    val robHead = Input(new DynInst)
    val stallReason = Flipped(new StallReasonIO(RenameWidth))
    val lqCanAccept = Input(Bool())
    val sqCanAccept = Input(Bool())
    val robHeadNotReady = Input(Bool())
    val robFull = Input(Bool())
    val debugTopDown = new Bundle {
      val fromRob = Flipped(new RobDispatchTopDownIO)
      val fromCore = new CoreDispatchTopDownIO
    }
  })
  // Deq for std's IQ is not assigned in Dispatch2Iq, so add one more src for it.
  val issueBlockParams = backendParams.allIssueParams
  val renameIn = io.renameIn
  val fromRename = io.fromRename
  io.toRenameAllFire := io.fromRename.map(x => !x.valid || x.fire).reduce(_ && _)
  val fromRenameUpdate = Wire(Vec(RenameWidth, Flipped(ValidIO(new DispatchUpdateUop))))

  // Update ftqidx to dispatch: Due to branch instructions/store compression, the required ftqidx should correspond to the ftqidx of the last instruction in the compressed robentry.
  // update isrvc to dispatch: branch need last isrvc, rob need first isrvc as rob should attach interrupt to first uop
  for (i <- 0 until RenameWidth) {
    fromRenameUpdate(i).valid := fromRename(i).valid
    // srcLoadDependency and srcState
    fromRenameUpdate(i).bits := 0.U.asTypeOf(fromRenameUpdate(i).bits)
    connectSamePort(fromRenameUpdate(i).bits, fromRename(i).bits)
    fromRenameUpdate(i).bits.debug.foreach(connectSamePort(_, fromRename(i).bits.debug.get))
    fromRenameUpdate(i).bits.ftqOffset := fromRename(i).bits.ftqLastOffset
    fromRenameUpdate(i).bits.ftqPtr := fromRename(i).bits.ftqPtr + fromRename(i).bits.crossFtq
    fromRenameUpdate(i).bits.preDecodeInfo.isRVC := fromRename(i).bits.lastIsRVC
  }

  val renameWidth = io.fromRename.size
  val issueQueueCount = io.IQValidNumVec
  val issueQueueNum = allIssueParams.size
  // int fp vec v0 vl
  val numRegType = 5
  val idxRegTypeInt = allFuConfigs.map(x => {
    x.srcData.map(xx => {
      xx.zipWithIndex.filter(y => IntRegSrcDataSet.contains(y._1)).map(_._2)
    }).flatten
  }).flatten.toSet.toSeq.sorted
  val idxRegTypeFp = allFuConfigs.map(x => {
    x.srcData.map(xx => {
      xx.zipWithIndex.filter(y => FpRegSrcDataSet.contains(y._1)).map(_._2)
    }).flatten
  }).flatten.toSet.toSeq.sorted
  val idxRegTypeVec = allFuConfigs.map(x => {
    x.srcData.map(xx => {
      xx.zipWithIndex.filter(y => VecRegSrcDataSet.contains(y._1)).map(_._2)
    }).flatten
  }).flatten.toSet.toSeq.sorted
  val idxRegTypeV0 = allFuConfigs.map(x => {
    x.srcData.map(xx => {
      xx.zipWithIndex.filter(y => V0RegSrcDataSet.contains(y._1)).map(_._2)
    }).flatten
  }).flatten.toSet.toSeq.sorted
  val idxRegTypeVl = allFuConfigs.map(x => {
    x.srcData.map(xx => {
      xx.zipWithIndex.filter(y => VlRegSrcDataSet.contains(y._1)).map(_._2)
    }).flatten
  }).flatten.toSet.toSeq.sorted
  println(s"[NewDispatch] idxRegTypeInt: $idxRegTypeInt")
  println(s"[NewDispatch] idxRegTypeFp: $idxRegTypeFp")
  println(s"[NewDispatch] idxRegTypeVec: $idxRegTypeVec")
  println(s"[NewDispatch] idxRegTypeV0: $idxRegTypeV0")
  println(s"[NewDispatch] idxRegTypeVl: $idxRegTypeVl")
  val numRegSrc: Int = issueBlockParams.map(_.exuBlockParams.map(
    x => if (x.hasStdFu) x.numRegSrc + 1 else x.numRegSrc
  ).max).max

  val numRegSrcInt: Int = issueBlockParams.map(_.exuBlockParams.map(
    x => if (x.hasStdFu) x.numIntSrc + 1 else x.numIntSrc
  ).max).max
  val numRegSrcFp: Int = issueBlockParams.map(_.exuBlockParams.map(
    x => if (x.hasStdFu) x.numFpSrc + 1 else x.numFpSrc
  ).max).max
  val numRegSrcVf: Int = issueBlockParams.map(_.exuBlockParams.map(
    x => x.numVecSrc
  ).max).max
  val numRegSrcV0: Int = issueBlockParams.map(_.exuBlockParams.map(
    x => x.numV0Src
  ).max).max
  val numRegSrcVl: Int = issueBlockParams.map(_.exuBlockParams.map(
    x => x.numVlSrc
  ).max).max

  println(s"[Dispatch2Iq] numRegSrc: ${numRegSrc}, numRegSrcInt: ${numRegSrcInt}, numRegSrcFp: ${numRegSrcFp}, " +
    s"numRegSrcVf: ${numRegSrcVf}, numRegSrcV0: ${numRegSrcV0}, numRegSrcVl: ${numRegSrcVl}")

  // RegCacheTagTable Module
  val rcTagTable = Module(new RegCacheTagTable(numRegSrcInt * renameWidth))
  // BusyTable Modules
  val intBusyTable = Module(new BusyTable(numRegSrcInt * renameWidth, backendParams.numPregWb(IntData()), IntPhyRegs, IntWB()))
  val fpBusyTable = Module(new BusyTable(numRegSrcFp * renameWidth, backendParams.numPregWb(FpData()), FpPhyRegs, FpWB()))
  val vecBusyTable = Module(new BusyTable(numRegSrcVf * renameWidth, backendParams.numPregWb(VecData()), VfPhyRegs, VfWB()))
  val v0BusyTable = Module(new BusyTable(numRegSrcV0 * renameWidth, backendParams.numPregWb(V0Data()), V0PhyRegs, V0WB()))
  val vlBusyTable = Module(new VlBusyTable(numRegSrcVl * renameWidth, backendParams.numPregWb(VlData()), VlPhyRegs, VlWB()))
  vlBusyTable.io_vl_Wb.vlWriteBackInfo := io.vlWriteBackInfo
  val busyTables = Seq(intBusyTable, fpBusyTable, vecBusyTable, v0BusyTable, vlBusyTable)
  val wbPregs = Seq(io.wbPregsInt, io.wbPregsFp, io.wbPregsVec, io.wbPregsV0, io.wbPregsVl)
  val idxRegType = Seq(idxRegTypeInt, idxRegTypeFp, idxRegTypeVec, idxRegTypeV0, idxRegTypeVl)
  val allocPregsValid = Wire(Vec(busyTables.size, Vec(RenameWidth, Bool())))
  allocPregsValid(0) := VecInit(fromRename.map(x => x.valid && x.bits.rfWen && !x.bits.isMove))
  allocPregsValid(1) := VecInit(fromRename.map(x => x.valid && x.bits.fpWen))
  allocPregsValid(2) := VecInit(fromRename.map(x => x.valid && x.bits.vecWen))
  allocPregsValid(3) := VecInit(fromRename.map(x => x.valid && x.bits.v0Wen))
  allocPregsValid(4) := VecInit(fromRename.map(x => x.valid && x.bits.vlWen))
  val allocPregs = Wire(Vec(busyTables.size, Vec(RenameWidth, ValidIO(UInt(PhyRegIdxWidth.W)))))
  allocPregs.zip(allocPregsValid).map(x =>{
    x._1.zip(x._2).zipWithIndex.map{case ((sink, source), i) => {
      sink.valid := source
      sink.bits := fromRename(i).bits.pdest
    }}
  })
  val wakeUp = io.wakeUpAll.wakeUpInt ++ io.wakeUpAll.wakeUpFp ++ io.wakeUpAll.wakeUpVec
  busyTables.zip(wbPregs).zip(allocPregs).map{ case ((b, w), a) => {
    b.io.wakeUpInt := io.wakeUpAll.wakeUpInt
    b.io.wakeUpFp  := io.wakeUpAll.wakeUpFp
    b.io.wakeUpVec := io.wakeUpAll.wakeUpVec
    b.io.og0Cancel := io.og0Cancel
    b.io.ldCancel := io.ldCancel
    b.io.wbPregs := w
    b.io.allocPregs := a
  }}
  rcTagTable.io.allocPregs.zip(allocPregs(0)).map(x => x._1 := x._2)
  println(s"rcTagTable.io.wakeupFromIQ.length: ${rcTagTable.io.wakeupFromIQ.length}")
  println(s"io.wakeUpAll.wakeUpInt.length: ${io.wakeUpAll.wakeUpInt.length}")
  println(s"rcTagTable.io.wakeupFromIQ.length: ${rcTagTable.io.wakeupFromIQ.size}")
  println(s"io.wakeUpAll.wakeUpInt.length: ${io.wakeUpAll.wakeUpInt.size}")
  rcTagTable.io.wakeupFromIQ := io.wakeUpAll.wakeUpInt
  rcTagTable.io.og0Cancel := io.og0Cancel
  rcTagTable.io.ldCancel := io.ldCancel
  busyTables.zip(idxRegType).zipWithIndex.map { case ((b, idxseq), i) => {
    val readAddr = VecInit(fromRename.map(x => x.bits.psrc.zipWithIndex.filter(xx => idxseq.contains(xx._2)).map(_._1)).flatten)
    val readValid = VecInit(fromRename.map(x => x.bits.psrc.zipWithIndex.filter(xx => idxseq.contains(xx._2)).map(y => x.valid && SrcType.isXp(x.bits.srcType(y._2)))).flatten)
    b.io.read.map(_.req).zip(readAddr).map(x => x._1 := x._2)
    // only int src need srcLoadDependency, src0 src1
    if (i == 0) {
      // int and fp idx 0 1 2(only fp)
      val srcLoadDependencyUpdate = fromRenameUpdate.map(x => x.bits.srcLoadDependency.zipWithIndex.filter(x => x._2 < 3).map(_._1))
      val srcType = fromRenameUpdate.map(x => x.bits.srcType.zipWithIndex.filter(x => x._2 < 3).map(_._1))
      // for int is 2 src, fp is 3 src
      srcLoadDependencyUpdate.zip(srcType).zipWithIndex.map{ case ((sinks, srctypes), idx) =>
        sinks.zip(srctypes).zipWithIndex.map{ case ((sink, srctype), srcidx) =>
          println(s"srcidx = ${srcidx}")
          val fpRead = busyTables(1).io.read(idx * 3 + srcidx).loadDependency
          if (srcidx < 2) {
            val intRead = busyTables(0).io.read(idx * 2 + srcidx).loadDependency
            sink := Mux1H(
              Seq(SrcType.isFp(srctype), SrcType.isXp(srctype), !SrcType.isFp(srctype) && !SrcType.isXp(srctype)),
              Seq(fpRead, intRead, 0.U.asTypeOf(sink))
            )
          }
          else {
            sink := Mux(SrcType.isFp(srctype), fpRead, 0.U.asTypeOf(sink))
          }
        }
      }
      // only int src need rcTag
      val rcTagUpdate = fromRenameUpdate.map(x => x.bits.regCacheIdx.zipWithIndex.filter(x => idxseq.contains(x._2)).map(_._1)).flatten
      rcTagUpdate.zip(rcTagTable.io.readPorts.map(_.addr)).map(x => x._1 := x._2)
      val useRegCacheUpdate = fromRenameUpdate.map(x => x.bits.useRegCache.zipWithIndex.filter(x => idxseq.contains(x._2)).map(_._1)).flatten
      useRegCacheUpdate.zip(rcTagTable.io.readPorts.map(_.valid)).map(x => x._1 := x._2)
      rcTagTable.io.readPorts.map(_.ren).zip(readValid).map(x => x._1 := x._2)
      rcTagTable.io.readPorts.map(_.tag).zip(readAddr).map(x => x._1 := x._2)
    }
  }}
  val allSrcState = Wire(Vec(renameWidth, Vec(numRegSrc, Vec(numRegType, Bool()))))
  for (i <- 0 until renameWidth){
    for (j <- 0 until numRegSrc){
      for (k <- 0 until numRegType){
        if (!idxRegType(k).contains(j)) {
          allSrcState(i)(j)(k) := false.B
        }
        else {
          val readidx = i * idxRegType(k).size + idxRegType(k).indexOf(j)
          val readEn = k match {
            case 0 => SrcType.isXp(fromRename(i).bits.srcType(j))
            case 1 => SrcType.isFp(fromRename(i).bits.srcType(j))
            case 2 => SrcType.isVp(fromRename(i).bits.srcType(j))
            case 3 => SrcType.isV0(fromRename(i).bits.srcType(j))
            case 4 => true.B
          }
          allSrcState(i)(j)(k) := readEn && busyTables(k).io.read(readidx).resp || SrcType.isImm(fromRename(i).bits.srcType(j))
        }
      }
    }
  }

  // eliminate old vd
  val ignoreOldVdVec = Wire(Vec(renameWidth, Bool()))
  for (i <- 0 until renameWidth){
    // numRegSrcVf - 1 is old vd
    var j = numRegSrcVf - 1
    // 2 is type of vec
    var k = 2
    val readidx = i * idxRegType(k).size + idxRegType(k).indexOf(j)
    val readEn = SrcType.isVp(fromRename(i).bits.srcType(j))
    val isDependOldVd = fromRename(i).bits.vpu.isDependOldVd
    val isWritePartVd = fromRename(i).bits.vpu.isWritePartVd
    val vta = fromRename(i).bits.vpu.vta
    val vma = fromRename(i).bits.vpu.vma
    val vm = fromRename(i).bits.vpu.vm
    val vlIsVlmax = vlBusyTable.io_vl_read.vlReadInfo(i).is_vlmax
    val vlIsNonZero = vlBusyTable.io_vl_read.vlReadInfo(i).is_nonzero
    val ignoreTail = vlIsVlmax && (vm =/= 0.U || vma) && !isWritePartVd
    val ignoreWhole = (vm =/= 0.U || vma) && vta
    val ignoreOldVd = vlBusyTable.io.read(i).resp && vlIsNonZero && !isDependOldVd && (ignoreTail || ignoreWhole)
    ignoreOldVdVec(i) := readEn && ignoreOldVd
    allSrcState(i)(j)(k) := readEn && (busyTables(k).io.read(readidx).resp || ignoreOldVd) || SrcType.isImm(fromRename(i).bits.srcType(j))
  }

  // Singlestep should only commit one machine instruction after dret, and then hart enter debugMode according to singlestep exception.
  val s_holdRobidx :: s_updateRobidx :: Nil = Enum(2)
  val singleStepState = RegInit(s_updateRobidx)

  val robidxStepHold  = WireInit(0.U.asTypeOf(fromRename(0).bits.robIdx))
  val robidxStepReg   = RegInit(0.U.asTypeOf(fromRename(0).bits.robIdx))
  val robidxCanCommitStepping = WireInit(0.U.asTypeOf(fromRename(0).bits.robIdx))
  robidxStepReg := robidxCanCommitStepping

  when(!io.singleStep) {
    singleStepState := s_updateRobidx
  }.elsewhen(io.singleStep && fromRename(0).fire && io.enqRob.req(0).valid) {
    singleStepState := s_holdRobidx
    robidxStepHold := fromRename(0).bits.robIdx
  }

  when(singleStepState === s_updateRobidx) {
    robidxCanCommitStepping := robidxStepHold
  }.elsewhen(singleStepState === s_holdRobidx) {
    when(io.redirect.valid){
      robidxCanCommitStepping.flag := !robidxStepReg.flag
    }.otherwise {
      robidxCanCommitStepping := robidxStepReg
    }
  }

  val minIQSelAll = Wire(Vec(needMultiExu.size, Vec(renameWidth, Vec(issueQueueNum, Bool()))))
  needMultiExu.zipWithIndex.map{ case ((fus, exuidx), needMultiExuidx) => {
    val suffix = fus.map(_.name).mkString("_")
    val iqNum = exuidx.size
    val iqidx = allIssueParams.map(_.exuBlockParams.map(_.fuConfigs).flatten.toSet.toSeq).zipWithIndex.filter{x => fus.toSet.subsetOf(x._1.toSet)}.map(_._2)
    println(s"[NewDispatch] ${fus.map(_.name)};iqidx:$iqidx;exuIdx:$exuidx")
    val compareMatrix = Wire(Vec(iqNum, Vec(iqNum, Bool()))).suggestName(s"compareMatrix_$suffix")
    for (i <- 0 until iqNum) {
      for (j <- 0 until iqNum) {
        if (i == j) compareMatrix(i)(j) := false.B
        else if (i < j) compareMatrix(i)(j) := issueQueueCount(exuidx(i)) < issueQueueCount(exuidx(j))
        else compareMatrix(i)(j) := !compareMatrix(j)(i)
      }
    }
    val IQSort = Reg(Vec(iqNum, Vec(iqNum, Bool()))).suggestName(s"IQSort_$suffix}")
    for (i <- 0 until iqNum){
      // i = 0 minimum iq, i = iqNum - 1 -> maximum iq
      IQSort(i) := compareMatrix.map(x => PopCount(x) === (iqNum - 1 - i).U)
    }
    val minIQSel = Wire(Vec(renameWidth, Vec(issueQueueNum, Bool()))).suggestName(s"minIQSel_$suffix")
    for (i <- 0 until renameWidth){
      val minIQSel_ith = IQSort(i % iqNum)
      println(s"minIQSel_${i}th_$suffix = IQSort(${i % iqNum})")
      for (j <- 0 until issueQueueNum){
        minIQSel(i)(j) := false.B
        if (iqidx.contains(j)){
          minIQSel(i)(j) := minIQSel_ith(iqidx.indexOf(j))
          println(s"minIQSel_${suffix}_${i}_${j} = minIQSel_ith(iqidx.indexOf(${j}))")
        }
      }
    }
    minIQSelAll(needMultiExuidx) := minIQSel
    if (backendParams.debugEn){
      dontTouch(compareMatrix)
      dontTouch(IQSort)
      dontTouch(minIQSel)
    }
  }
  }
  val fuConfigSeq = needMultiExu.map(_._1)
  val fuTypeOH = Wire(Vec(renameWidth, Vec(needMultiExu.size, Bool())))
  fuTypeOH.zip(renameIn).map{ case(oh, in) => {
    oh := fuConfigSeq.map(x => x.map(xx => in.bits.fuType(xx.fuType.id)).reduce(_ || _) && in.valid)
  }
  }
  // not count itself
  val popFuTypeOH = Wire(Vec(renameWidth, Vec(needMultiExu.size, UInt((renameWidth-1).U.getWidth.W))))
  popFuTypeOH.zipWithIndex.map{ case (pop, idx) => {
    if (idx == 0){
      pop := 0.U.asTypeOf(pop)
    }
    else {
      pop.zipWithIndex.map{ case (p, i) => {
        p := PopCount(fuTypeOH.take(idx).map(x => x(i)))
        }
      }
    }
  }}
  val uopSelIQ = Reg(Vec(renameWidth, Vec(issueQueueNum, Bool())))
  val fuTypeOHSingle = Wire(Vec(renameWidth, Vec(needSingleIQ.size, Bool())))
  fuTypeOHSingle.zip(renameIn).map{ case (oh, in) => {
    oh := needSingleIQ.map(_._1).map(x => x.map(xx => in.valid && in.bits.fuType(xx.fuType.id)).reduce(_ || _))
  }}
  val uopSelIQSingle = Wire(Vec(needSingleIQ.size, Vec(issueQueueNum, Bool())))
  uopSelIQSingle := VecInit(needSingleIQ.map(_._2).flatten.map(x => VecInit((1.U(issueQueueNum.W) << x)(issueQueueNum-1, 0).asBools)))
  uopSelIQ.zipWithIndex.map{ case (u, i) => {
    when(io.toRenameAllFire){
      u := Mux(renameIn(i).valid,
                Mux(fuTypeOH(i).asUInt.orR,
                  Mux1H(fuTypeOH(i), minIQSelAll)(Mux1H(fuTypeOH(i), popFuTypeOH(i))),
                  Mux1H(fuTypeOHSingle(i), uopSelIQSingle)),
                0.U.asTypeOf(u)
              )
    }.elsewhen(io.fromRename(i).fire){
      u := 0.U.asTypeOf(u)
    }
  }}
  val uopSelIQMatrix = Wire(Vec(renameWidth, Vec(issueQueueNum, UInt(renameWidth.U.getWidth.W))))
  uopSelIQMatrix.zipWithIndex.map{ case (u, i) => {
    u.zipWithIndex.map{ case (uu, j) => {
     uu := PopCount(uopSelIQ.take(i+1).map(x => x.zipWithIndex.filter(_._2 == j).map(_._1)).flatten)
    }}
  }}
  val IQSelUop = Wire(Vec(IQEnqSum, ValidIO(new DispatchOutUop)))
  val uopBlockByIQ = Wire(Vec(renameWidth, Bool()))
  val allowDispatch = Wire(Vec(renameWidth, Bool()))
  val thisCanActualOut = Wire(Vec(renameWidth, Bool()))
  val lsqCanAccept = Wire(Bool())
  for (i <- 0 until RenameWidth){
    // update valid logic
    fromRenameUpdate(i).valid := fromRename(i).valid && allowDispatch(i) && !uopBlockByIQ(i) && thisCanActualOut(i) &&
      lsqCanAccept && !fromRename(i).bits.isMove && !fromRename(i).bits.hasException && !fromRenameUpdate(i).bits.singleStep
    fromRename(i).ready := allowDispatch(i) && !uopBlockByIQ(i) && thisCanActualOut(i) && lsqCanAccept
    // update src type if eliminate old vd
    fromRenameUpdate(i).bits.srcType(numRegSrcVf - 1) := Mux(ignoreOldVdVec(i), SrcType.no, fromRename(i).bits.srcType(numRegSrcVf - 1))
  }
  val dispatchBlock = fromRename.map(_.valid).reduce(_ || _) && !io.toRenameAllFire
  XSPerfAccumulate(s"block_cycle", dispatchBlock)
  XSPerfAccumulate(s"block_iq", dispatchBlock && uopBlockByIQ.asUInt.orR)
  XSPerfAccumulate(s"block_allowDispatch", dispatchBlock && allowDispatch.asUInt.orR)
  XSPerfAccumulate(s"block_lsqFull", dispatchBlock && lsqCanAccept)
  for (i <- 0 until RenameWidth){
    // check is drop amocas sta
    fromRenameUpdate(i).bits.isDropAmocasSta := fromRename(i).bits.isAMOCAS && fromRename(i).bits.uopIdx(0) === 0.U
    // update singleStep
    fromRenameUpdate(i).bits.singleStep := io.singleStep && (fromRename(i).bits.robIdx =/= robidxCanCommitStepping)
  }
  var temp = 0
  allIssueParams.zipWithIndex.map{ case(issue, iqidx) => {
    for (i <- 0 until issue.numEnq){
      val oh = Wire(Vec(renameWidth, Bool())).suggestName(s"oh_IQSelUop_$temp")
      oh := uopSelIQMatrix.map(_(iqidx)).map(_ === (i+1).U)
      val updateUop = PriorityMux(oh, fromRenameUpdate)
      IQSelUop(temp).valid := updateUop.valid
      connectSamePort(IQSelUop(temp).bits, updateUop.bits)
      // there only assign valid not use PriorityMuxDefalut for better timing
      IQSelUop(temp).valid := PriorityMuxDefault(oh.zip(fromRenameUpdate.map(_.valid)), false.B)
      val allFuThisIQ = issue.exuBlockParams.map(_.fuConfigs).flatten.toSet.toSeq
      val hasStaFu = !allFuThisIQ.filter(_.name == "sta").isEmpty
      for (j <- 0 until numRegSrc){
        val maskForStd = hasStaFu && (j == 1)
        val thisSrcHasInt = allFuThisIQ.map(x => {x.srcData.map(xx => {if (j < xx.size) IntRegSrcDataSet.contains(xx(j)) else false}).reduce(_ || _)}).reduce(_ || _)
        val thisSrcHasFp  = allFuThisIQ.map(x => {x.srcData.map(xx => {if (j < xx.size) FpRegSrcDataSet.contains(xx(j))  else false}).reduce(_ || _)}).reduce(_ || _)
        val thisSrcHasVec = allFuThisIQ.map(x => {x.srcData.map(xx => {if (j < xx.size) VecRegSrcDataSet.contains(xx(j)) else false}).reduce(_ || _)}).reduce(_ || _)
        val thisSrcHasV0  = allFuThisIQ.map(x => {x.srcData.map(xx => {if (j < xx.size) V0RegSrcDataSet.contains(xx(j))  else false}).reduce(_ || _)}).reduce(_ || _)
        val thisSrcHasVl  = allFuThisIQ.map(x => {x.srcData.map(xx => {if (j < xx.size) VlRegSrcDataSet.contains(xx(j))  else false}).reduce(_ || _)}).reduce(_ || _)
        val selSrcState = Seq(thisSrcHasInt || maskForStd, thisSrcHasFp || maskForStd, thisSrcHasVec, thisSrcHasV0, thisSrcHasVl)
        IQSelUop(temp).bits.srcState(j) := PriorityMux(oh, allSrcState)(j).zip(selSrcState).filter(_._2 == true).map(_._1).foldLeft(false.B)(_ || _).asUInt
      }
      temp = temp + 1
      if (backendParams.debugEn){
        dontTouch(oh)
      }
    }
  }}
  temp = 0
  val uopBlockMatrix = Wire(Vec(renameWidth, Vec(issueQueueNum, Bool())))
  val uopBlockMatrixForAssign = allIssueParams.zipWithIndex.map { case (issue, iqidx) => {
    val result = uopSelIQMatrix.map(_(iqidx)).map(x => Mux(io.toIssueQueues(temp).ready, x > issue.numEnq.U, x.orR))
    temp = temp + issue.numEnq
    result
  }}.transpose
  uopBlockMatrix.zip(uopBlockMatrixForAssign).map(x => x._1 := VecInit(x._2))
  uopBlockByIQ := uopBlockMatrix.map(_.reduce(_ || _))
  io.toIssueQueues.zip(IQSelUop).map(x => {
    x._1.valid := x._2.valid
    x._1.bits := x._2.bits
  })
  if (backendParams.debugEn){
    dontTouch(uopSelIQMatrix)
    dontTouch(IQSelUop)
    dontTouch(fromRenameUpdate)
    dontTouch(uopBlockByIQ)
    dontTouch(allowDispatch)
    dontTouch(thisCanActualOut)
    dontTouch(popFuTypeOH)
    dontTouch(fuTypeOH)
    dontTouch(fuTypeOHSingle)
    dontTouch(minIQSelAll)
  }
  ///////////////////////////////////////////////////////////

  val lsqEnqCtrl = Module(new LsqEnqCtrl)

  // TODO: check lsqEnqCtrl redirect logic
  // here is RegNext because dispatch2iq use s2_s4_redirect, newDispatch use s1_s3_redirect
  lsqEnqCtrl.io.redirect := RegNext(io.redirect)
  lsqEnqCtrl.io.lcommit := io.fromMem.lcommit
  lsqEnqCtrl.io.scommit := io.fromMem.scommit
  lsqEnqCtrl.io.lqCancelCnt := io.fromMem.lqCancelCnt
  lsqEnqCtrl.io.sqCancelCnt := io.fromMem.sqCancelCnt
  lsqEnqCtrl.io.enq.iqAccept := io.fromRename.map(x => !x.valid || x.fire)
  io.toMem.lsqEnqIO <> lsqEnqCtrl.io.enqLsq

  private val enqLsqIO = lsqEnqCtrl.io.enq
  private val lqFreeCount = lsqEnqCtrl.io.lqFreeCount
  private val sqFreeCount = lsqEnqCtrl.io.sqFreeCount

  private val numLoadDeq = LSQLdEnqWidth
  private val numStoreAMODeq = LSQStEnqWidth
  private val numVLoadDeq = LoadPipelineWidth
  private val numDeq = enqLsqIO.req.size
  lsqCanAccept := enqLsqIO.canAccept

  private val isLoadVec = VecInit(fromRename.map(x => x.valid && FuType.isLoad(x.bits.fuType)))
  private val isStoreVec = VecInit(fromRename.map(x => x.valid && FuType.isStore(x.bits.fuType)))
  private val isAMOVec = fromRename.map(x => x.valid && FuType.isAMO(x.bits.fuType))
  private val isStoreAMOVec = fromRename.map(x => x.valid && (FuType.isStore(x.bits.fuType) || FuType.isAMO(x.bits.fuType)))
  private val isVLoadVec = VecInit(fromRename.map(x => x.valid && FuType.isVLoad(x.bits.fuType)))
  private val isVStoreVec = VecInit(fromRename.map(x => x.valid && FuType.isVStore(x.bits.fuType)))

  private val loadCntVec = VecInit(isLoadVec.indices.map(x => PopCount(isLoadVec.slice(0, x + 1))))
  private val storeAMOCntVec = VecInit(isStoreAMOVec.indices.map(x => PopCount(isStoreAMOVec.slice(0, x + 1))))
  private val vloadCntVec = VecInit(isVLoadVec.indices.map(x => PopCount(isVLoadVec.slice(0, x + 1))))

  private val s0_enqLsq_resp = Wire(enqLsqIO.resp.cloneType)
  for (i <- 0 until RenameWidth) {
    // update lqIdx sqIdx
    fromRenameUpdate(i).bits.lqIdx := s0_enqLsq_resp(i).lqIdx
    fromRenameUpdate(i).bits.sqIdx := s0_enqLsq_resp(i).sqIdx
  }

  val loadBlockVec = VecInit(loadCntVec.map(_ > numLoadDeq.U))
  val storeAMOBlockVec = VecInit(storeAMOCntVec.map(_ > numStoreAMODeq.U))
  val vloadBlockVec = VecInit(vloadCntVec.map(_ > numVLoadDeq.U))
  val lsStructBlockVec = VecInit((loadBlockVec.zip(storeAMOBlockVec)).zip(vloadBlockVec).map(x => x._1._1 || x._1._2 || x._2))
  if (backendParams.debugEn) {
    dontTouch(loadBlockVec)
    dontTouch(storeAMOBlockVec)
    dontTouch(lsStructBlockVec)
    dontTouch(vloadBlockVec)
    dontTouch(isLoadVec)
    dontTouch(isVLoadVec)
    dontTouch(loadCntVec)
  }

  private val uop = fromRename.map(_.bits)
  private val fuType = uop.map(_.fuType)
  private val fuOpType = uop.map(_.fuOpType)
  private val vtype = uop.map(_.vpu.vtype)
  private val sew = vtype.map(_.vsew)
  private val lmul = vtype.map(_.vlmul)
  private val eew = uop.map(_.vpu.veew)
  private val mop = fuOpType.map(fuOpTypeItem => LSUOpType.getVecLSMop(fuOpTypeItem))
  private val nf = fuOpType.zip(uop.map(_.vpu.nf)).map { case (fuOpTypeItem, nfItem) => Mux(LSUOpType.isWhole(fuOpTypeItem), 0.U, nfItem) }
  private val emul = fuOpType.zipWithIndex.map { case (fuOpTypeItem, index) =>
    Mux(
      LSUOpType.isWhole(fuOpTypeItem),
      GenUSWholeEmul(nf(index)),
      Mux(
        LSUOpType.isMasked(fuOpTypeItem),
        0.U(mulBits.W),
        EewLog2(eew(index)) - sew(index) + lmul(index)
      )
    )
  }

  private val isVlsType = fuType.map(fuTypeItem => FuType.isVls(fuTypeItem)).zip(fromRename.map(_.valid)).map(x => x._1 && x._2)
  private val isLSType = fuType.map(fuTypeItem => FuType.isLoad(fuTypeItem) || FuType.isStore(fuTypeItem)).zip(fromRename.map(_.valid)).map(x => x._1 && x._2)
  private val isSegment = fuType.map(fuTypeItem => FuType.isVsegls(fuTypeItem)).zip(fromRename.map(_.valid)).map(x => x._1 && x._2)
  // TODO
  private val isUnitStride = fuOpType.map(fuOpTypeItem => LSUOpType.isAllUS(fuOpTypeItem))
  private val isVecUnitType = isVlsType.zip(isUnitStride).map { case (isVlsTypeItme, isUnitStrideItem) =>
    isVlsTypeItme && isUnitStrideItem
  }
  private val isfofFixVlUop = uop.map { x => x.vpu.isVleff && x.lastUop }
  private val instType = isSegment.zip(mop).map { case (isSegementItem, mopItem) => Cat(isSegementItem, mopItem) }
  // There is no way to calculate the 'flow' for 'unit-stride' exactly:
  //  Whether 'unit-stride' needs to be split can only be known after obtaining the address.
  // For scalar instructions, this is not handled here, and different assignments are done later according to the situation.
  private val numLsElem = VecInit(uop.map(_.numLsElem))

  // The maximum 'numLsElem' number that can be emitted per port is:
  //    16 2 2 2 2 2.
  // The 'allowDispatch' calculations are done conservatively for timing purposes:
  //   The Flow of scalar instructions is considered 1,
  //   The flow of vector 'unit-stride' instructions is considered 2, and the flow of other vector instructions is considered 16.
  private val conserveFlows = VecInit(isVlsType.zip(isLSType).zipWithIndex.map { case ((isVlsTyepItem, isLSTypeItem), index) =>
    Mux(
      isVlsTyepItem,
      Mux(isUnitStride(index), VecMemUnitStrideMaxFlowNum.U, 16.U),
      Mux(isLSTypeItem, 1.U, 0.U)
    )
  })

  private val conserveFlowsIs16 = VecInit(isVlsType.zipWithIndex.map { case (isVlsTyepItem, index) =>
    isVlsTyepItem && !isUnitStride(index)
  })
  private val conserveFlowsIs2 = VecInit(isVlsType.zipWithIndex.map { case (isVlsTyepItem, index) =>
    isVlsTyepItem && isUnitStride(index)
  })
  private val conserveFlowsIs1 = VecInit(isLSType.zipWithIndex.map { case (isLSTyepItem, index) =>
    isLSTyepItem
  })
  private val flowTotalWidth = (VecMemLSQEnqIteratorNumberSeq.max * RenameWidth).U.getWidth
  private val conserveFlowTotalDispatch = Wire(Vec(RenameWidth, UInt(flowTotalWidth.W)))
  private val lowCountMaxWidth = (2 * RenameWidth).U.getWidth
  conserveFlowTotalDispatch.zipWithIndex.map{ case (flowTotal, idx) =>
    val highCount = PopCount(conserveFlowsIs16.take(idx + 1))
    val conserveFlowsIs2Or1 = VecInit(conserveFlowsIs2.zip(conserveFlowsIs1).map(x => Cat(x._1, x._2)))
    val lowCount = conserveFlowsIs2Or1.take(idx + 1).reduce(_ +& _).asTypeOf(0.U(lowCountMaxWidth.W))
    flowTotal := (if (RenameWidth == 6) Cat(highCount, lowCount) else ((highCount << 4).asUInt + lowCount))
  }
  // renameIn
  private val isVlsTypeRename = io.renameIn.map(x => x.valid && FuType.isVls(x.bits.fuType))
  private val isLSTypeRename = io.renameIn.map(x => x.valid && (FuType.isLoad(x.bits.fuType)) || FuType.isStore(x.bits.fuType))
  private val isUnitStrideRename = io.renameIn.map(x => LSUOpType.isAllUS(x.bits.fuOpType))
  private val conserveFlowsIs16Rename = VecInit(isVlsTypeRename.zipWithIndex.map { case (isVlsTyepItem, index) =>
    isVlsTyepItem && !isUnitStrideRename(index)
  })
  private val conserveFlowsIs2Rename = VecInit(isVlsTypeRename.zipWithIndex.map { case (isVlsTyepItem, index) =>
    isVlsTyepItem && isUnitStrideRename(index)
  })
  private val conserveFlowsIs1Rename = VecInit(isLSTypeRename.zipWithIndex.map { case (isLSTyepItem, index) =>
    isLSTyepItem
  })
  private val conserveFlowTotalRename = Wire(Vec(RenameWidth, UInt(flowTotalWidth.W)))
  conserveFlowTotalRename.zipWithIndex.map { case (flowTotal, idx) =>
    val highCount = PopCount(conserveFlowsIs16Rename.take(idx + 1))
    val conserveFlowsIs2Or1 = VecInit(conserveFlowsIs2Rename.zip(conserveFlowsIs1Rename).map(x => Cat(x._1, x._2)))
    val lowCount = conserveFlowsIs2Or1.take(idx + 1).reduce(_ +& _).asTypeOf(0.U(lowCountMaxWidth.W))
    flowTotal := (if (RenameWidth == 6) Cat(highCount, lowCount) else ((highCount << 4).asUInt + lowCount))
  }


  private val conserveFlowTotal = Reg(Vec(RenameWidth, UInt(flowTotalWidth.W)))
  when(io.toRenameAllFire){
    conserveFlowTotal := conserveFlowTotalRename
  }.otherwise(
    conserveFlowTotal := conserveFlowTotalDispatch
  )
  // A conservative allocation strategy is adopted here.
  // Vector 'unit-stride' instructions and scalar instructions can be issued from all six ports,
  // while other vector instructions can only be issued from the first port
  // if is segment instruction, need disptch it to Vldst_RS0, so, except port 0, stall other.
  // The allocation needs to meet a few conditions:
  //  1) The lsq has enough entris.
  //  2) The number of flows accumulated does not exceed VecMemDispatchMaxNumber.
  //  3) Vector instructions other than 'unit-stride' can only be issued on the first port.


  for (index <- allowDispatch.indices) {
    val flowTotal = conserveFlowTotal(index)
    val allowDispatchPrevious = if (index == 0) true.B else allowDispatch(index - 1)
    when(isStoreVec(index) || isVStoreVec(index)) {
      allowDispatch(index) := (sqFreeCount > flowTotal) && allowDispatchPrevious
    }.elsewhen(isLoadVec(index) || isVLoadVec(index)) {
      allowDispatch(index) := (lqFreeCount > flowTotal) && allowDispatchPrevious
    }.elsewhen(isAMOVec(index)) {
      allowDispatch(index) := allowDispatchPrevious
    }.otherwise {
      allowDispatch(index) := allowDispatchPrevious
    }
  }


  // enqLsq io
  require(enqLsqIO.req.size == enqLsqIO.resp.size)
  for (i <- enqLsqIO.req.indices) {
    when(!io.fromRename(i).fire) {
      enqLsqIO.needAlloc(i) := 0.U
    }.elsewhen(isStoreVec(i) || isVStoreVec(i)) {
      enqLsqIO.needAlloc(i) := 2.U // store | vstore
    }.elsewhen(isLoadVec(i) || isVLoadVec(i)){
      enqLsqIO.needAlloc(i) := 1.U // load | vload
    }.otherwise {
      enqLsqIO.needAlloc(i) := 0.U
    }
    enqLsqIO.req(i).valid := io.fromRename(i).fire && !isAMOVec(i) && !isSegment(i) && !isfofFixVlUop(i)
    enqLsqIO.req(i).bits.connectRenameOutUop(io.fromRename(i).bits)

    // This is to make it easier to calculate in LSQ.
    // Both scalar instructions and vector instructions with FLOW equal to 1 have a NUM value of 1.”
    // But, the 'numLsElem' that is not a vector is set to 0 when passed to IQ
    enqLsqIO.req(i).bits.numLsElem := Mux(isVlsType(i), numLsElem(i), 1.U)
    s0_enqLsq_resp(i) := enqLsqIO.resp(i)
  }

  val isFp = VecInit(fromRename.map(req => FuType.isFArith(req.bits.fuType)))
  val isVec     = VecInit(fromRename.map(req => FuType.isVArith (req.bits.fuType) ||
                                                  FuType.isVsetRvfWvf(req.bits.fuType)))
  val isMem    = VecInit(fromRename.map(req => FuType.isMem(req.bits.fuType) ||
                                                  FuType.isVls (req.bits.fuType)))
  val isLs     = VecInit(fromRename.map(req => FuType.isLoadStore(req.bits.fuType)))
  val isVls    = VecInit(fromRename.map(req => FuType.isVls (req.bits.fuType)))
  val isStore  = VecInit(fromRename.map(req => FuType.isStore(req.bits.fuType)))
  val isVStore = VecInit(fromRename.map(req => FuType.isVStore(req.bits.fuType)))
  val isAMO    = VecInit(fromRename.map(req => FuType.isAMO(req.bits.fuType)))
  val isBlockBackward  = VecInit(fromRename.map(x => x.valid && x.bits.blockBackward))
  val isWaitForward    = VecInit(fromRename.map(x => x.valid && x.bits.waitForward))

  val updatedUop = Wire(Vec(RenameWidth, new DynInst))
  val checkpoint_id = RegInit(0.U(64.W))
  checkpoint_id := checkpoint_id + PopCount((0 until RenameWidth).map(i =>
    fromRename(i).fire
  ))


  for (i <- 0 until RenameWidth) {

    updatedUop(i).connectRenameOutUop(fromRename(i).bits)
    updatedUop(i).debugInfo.eliminatedMove := fromRename(i).bits.isMove
    // For the LUI instruction: psrc(0) is from register file and should always be zero.
    when (fromRename(i).bits.isLUI) {
      updatedUop(i).psrc(0) := 0.U
    }
    //TODO: vec ls mdp
    io.lfst.req(i).valid := fromRename(i).fire && updatedUop(i).storeSetHit
    io.lfst.req(i).bits.isstore := isStore(i)
    io.lfst.req(i).bits.ssid := updatedUop(i).ssid
    io.lfst.req(i).bits.robIdx := updatedUop(i).robIdx // speculatively assigned in rename

    // override load delay ctrl signal with store set result
    if(StoreSetEnable) {
      updatedUop(i).loadWaitBit := io.lfst.resp(i).bits.shouldWait
      updatedUop(i).waitForRobIdx := io.lfst.resp(i).bits.robIdx
    } else {
      updatedUop(i).loadWaitBit := isLs(i) && !isStore(i) && fromRename(i).bits.loadWaitBit
    }
    // // update singleStep, singleStep exception only enable in next machine instruction.
    updatedUop(i).singleStep := io.singleStep && (fromRename(i).bits.robIdx =/= robidxCanCommitStepping)
    XSDebug(
      fromRename(i).fire &&
        (TriggerAction.isDmode(updatedUop(i).trigger) || updatedUop(i).exceptionVec(breakPoint)), s"Debug Mode: inst ${i} has frontend trigger exception\n")
    XSDebug(fromRename(i).fire && updatedUop(i).singleStep, s"Debug Mode: inst ${i} has single step exception\n")
    if (env.EnableDifftest) {
      // debug runahead hint
      val debug_runahead_checkpoint_id = Wire(checkpoint_id.cloneType)
      if(i == 0){
        debug_runahead_checkpoint_id := checkpoint_id
      } else {
        debug_runahead_checkpoint_id := checkpoint_id + PopCount((0 until i).map(i =>
          fromRename(i).fire
        ))
      }
    }
  }

  // store set perf count
  XSPerfAccumulate("waittable_load_wait", PopCount((0 until RenameWidth).map(i =>
    fromRename(i).fire && fromRename(i).bits.loadWaitBit && !isStore(i) && isLs(i)
  )))
  XSPerfAccumulate("storeset_load_wait", PopCount((0 until RenameWidth).map(i =>
    fromRename(i).fire && updatedUop(i).loadWaitBit && !isStore(i) && isLs(i)
  )))
  XSPerfAccumulate("storeset_load_strict_wait", PopCount((0 until RenameWidth).map(i =>
    fromRename(i).fire && updatedUop(i).loadWaitBit && updatedUop(i).loadWaitStrict && !isStore(i) && isLs(i)
  )))
  XSPerfAccumulate("storeset_store_wait", PopCount((0 until RenameWidth).map(i =>
    fromRename(i).fire && updatedUop(i).loadWaitBit && isStore(i)
  )))

  val allResourceReady = io.enqRob.canAccept

  // Instructions should enter issue queues in order.
  // blockedByWaitForward: this instruction is blocked by itself (based on waitForward)
  // nextCanOut: next instructions can out (based on blockBackward)
  // notBlockedByPrevious: previous instructions can enqueue
  val hasException = VecInit(fromRename.zip(updatedUop).map {
    case (fromRename, uop) =>
      fromRename.bits.hasException || uop.singleStep
  })

  private val blockedByWaitForward = Wire(Vec(RenameWidth, Bool()))
  blockedByWaitForward(0) := !io.enqRob.isEmpty && isWaitForward(0)
  for (i <- 1 until RenameWidth) {
    blockedByWaitForward(i) := blockedByWaitForward(i - 1) || (!io.enqRob.isEmpty || Cat(fromRename.take(i).map(_.valid)).orR) && isWaitForward(i)
  }
  if(backendParams.debugEn){
    dontTouch(blockedByWaitForward)
    dontTouch(conserveFlows)
  }

  // Only the uop with block backward flag will block the next uop
  val nextCanOut = VecInit((0 until RenameWidth).map(i =>
    !isBlockBackward(i)
  ))
  val notBlockedByPrevious = VecInit((0 until RenameWidth).map(i =>
    if (i == 0) true.B
    else Cat((0 until i).map(j => nextCanOut(j))).andR
  ))

  // for noSpecExec: (robEmpty || !this.noSpecExec) && !previous.noSpecExec
  // For blockBackward:
  // this instruction can actually dequeue: 3 conditions
  // (1) resources are ready
  // (2) previous instructions are ready
  thisCanActualOut := VecInit((0 until RenameWidth).map(i => !blockedByWaitForward(i) && notBlockedByPrevious(i) && io.enqRob.canAccept))
  val thisActualOut = (0 until RenameWidth).map(i => io.enqRob.req(i).valid && io.enqRob.canAccept)

  // input for ROB, LSQ
  for (i <- 0 until RenameWidth) {
    // needAlloc no use, need deleted
    io.enqRob.needAlloc(i) := fromRename(i).valid
    io.enqRob.req(i).valid := fromRename(i).fire
    io.enqRob.req(i).bits := updatedUop(i)
    io.enqRob.req(i).bits.hasException := updatedUop(i).hasException || updatedUop(i).singleStep
    io.enqRob.req(i).bits.numWB := Mux(updatedUop(i).singleStep, 0.U, updatedUop(i).numWB)
    io.enqRob.req(i).bits.isXSTrap := FuType.isAlu(updatedUop(i).fuType) && (updatedUop(i).fuOpType === ALUOpType.xstrap)
    io.enqRob.req(i).bits.stdwriteNeed := FuType.isStore(updatedUop(i).fuType)
  }
  val hasValidInstr = VecInit(fromRename.map(_.valid)).asUInt.orR
  val hasSpecialInstr = Cat((0 until RenameWidth).map(i => isBlockBackward(i))).orR

  private val canAccept = !hasValidInstr || !hasSpecialInstr && io.enqRob.canAccept

  val isWaitForwardOrBlockBackward = isWaitForward.asUInt.orR || isBlockBackward.asUInt.orR
  val renameFireCnt = PopCount(fromRename.map(_.fire))

  val stall_rob = hasValidInstr && !io.enqRob.canAccept
  val stall_int_dq = hasValidInstr && io.enqRob.canAccept
  val stall_int_dq0 = hasValidInstr && io.enqRob.canAccept
  val stall_int_dq1 = hasValidInstr && io.enqRob.canAccept
  val stall_fp_dq = hasValidInstr && io.enqRob.canAccept
  val stall_ls_dq = hasValidInstr && io.enqRob.canAccept

  XSPerfAccumulate("in_valid_count", PopCount(fromRename.map(_.valid)))
  XSPerfAccumulate("in_fire_count", PopCount(fromRename.map(_.fire)))
  XSPerfAccumulate("in_valid_not_ready_count", PopCount(fromRename.map(x => x.valid && !x.ready)))
  XSPerfAccumulate("wait_cycle", !fromRename.head.valid && allResourceReady)

  XSPerfAccumulate("stall_cycle_rob", stall_rob)
  XSPerfAccumulate("stall_cycle_int_dq0", stall_int_dq0)
  XSPerfAccumulate("stall_cycle_int_dq1", stall_int_dq1)
  XSPerfAccumulate("stall_cycle_fp_dq", stall_fp_dq)
  XSPerfAccumulate("stall_cycle_ls_dq", stall_ls_dq)

  val notIssue = !io.debugTopDown.fromRob.robHeadLsIssue
  val tlbReplay = io.debugTopDown.fromCore.fromMem.robHeadTlbReplay
  val tlbMiss = io.debugTopDown.fromCore.fromMem.robHeadTlbMiss
  val vioReplay = io.debugTopDown.fromCore.fromMem.robHeadLoadVio
  val mshrReplay = io.debugTopDown.fromCore.fromMem.robHeadLoadMSHR
  val l1Miss = io.debugTopDown.fromCore.fromMem.robHeadMissInDCache
  val l2Miss = io.debugTopDown.fromCore.l2MissMatch
  val l3Miss = io.debugTopDown.fromCore.l3MissMatch

  val ldReason = Mux(l3Miss, TopDownCounters.LoadMemStall.id.U,
  Mux(l2Miss, TopDownCounters.LoadL3Stall.id.U,
  Mux(l1Miss, TopDownCounters.LoadL2Stall.id.U,
  Mux(notIssue, TopDownCounters.MemNotReadyStall.id.U,
  Mux(tlbMiss, TopDownCounters.LoadTLBStall.id.U,
  Mux(tlbReplay, TopDownCounters.LoadTLBStall.id.U,
  Mux(mshrReplay, TopDownCounters.LoadMSHRReplayStall.id.U,
  Mux(vioReplay, TopDownCounters.LoadVioReplayStall.id.U,
  TopDownCounters.LoadL1Stall.id.U))))))))

  val fusedVec = (0 until RenameWidth).map{ case i =>
    if (i == 0 || !backendParams.debugEn) false.B
    else (io.fromRename(i-1).fire && !io.fromRename(i).valid && io.fromRename(i-1).bits.debug.get.fusionNum =/= 0.U)
  }

  val decodeReason = RegNextN(io.stallReason.reason, 2)
  val renameReason = RegNext(io.stallReason.reason)

  val stallReason = Wire(chiselTypeOf(io.stallReason.reason))
  val firedVec = fromRename.map(_.fire)
  io.stallReason.backReason.valid := !canAccept
  io.stallReason.backReason.bits := TopDownCounters.OtherCoreStall.id.U
  stallReason.zip(io.stallReason.reason).zip(firedVec).zipWithIndex.zip(fusedVec).map { case ((((update, in), fire), idx), fused) =>
    val headIsInt = FuType.isInt(io.robHead.getDebugFuType)  && io.robHeadNotReady
    val headIsFp  = FuType.isFArith(io.robHead.getDebugFuType)   && io.robHeadNotReady
    val headIsDiv = FuType.isDivSqrt(io.robHead.getDebugFuType) && io.robHeadNotReady
    val headIsLd  = io.robHead.getDebugFuType === FuType.ldu.U && io.robHeadNotReady || !io.lqCanAccept
    val headIsSt  = io.robHead.getDebugFuType === FuType.stu.U && io.robHeadNotReady || !io.sqCanAccept
    val headIsAmo = io.robHead.getDebugFuType === FuType.mou.U && io.robHeadNotReady
    val headIsLs  = headIsLd || headIsSt
    val robLsFull = io.robFull || !io.lqCanAccept || !io.sqCanAccept

    import TopDownCounters._
    update := MuxCase(OtherCoreStall.id.U, Seq(
      // fire
      (fire || fused                                     ) -> NoStall.id.U          ,
      // dispatch not stall / core stall from decode or rename
      (in =/= OtherCoreStall.id.U && in =/= NoStall.id.U ) -> in                    ,
      // rob stall
      (headIsAmo                                         ) -> AtomicStall.id.U      ,
      (headIsSt                                          ) -> StoreStall.id.U       ,
      (headIsLd                                          ) -> ldReason              ,
      (headIsDiv                                         ) -> DivStall.id.U         ,
      (headIsInt                                         ) -> IntNotReadyStall.id.U ,
      (headIsFp                                          ) -> FPNotReadyStall.id.U  ,
      (renameReason(idx) =/= NoStall.id.U                ) -> renameReason(idx)     ,
      (decodeReason(idx) =/= NoStall.id.U                ) -> decodeReason(idx)     ,
    ))
  }

  TopDownCounters.values.foreach(ctr => XSPerfAccumulate(ctr.toString(), PopCount(stallReason.map(_ === ctr.id.U)), XSPerfLevel.CRITICAL))

  val robTrueCommit = io.debugTopDown.fromRob.robTrueCommit
  TopDownCounters.values.foreach(ctr => XSPerfRolling("td_"+ctr.toString(), PopCount(stallReason.map(_ === ctr.id.U)),
                                                      robTrueCommit, 1000, clock, reset))

  XSPerfHistogram("slots_fire", PopCount(thisActualOut), true.B, 0, RenameWidth+1, 1)
  // Explaination: when out(0) not fire, PopCount(valid) is not meaningfull
  XSPerfHistogram("slots_valid_pure", PopCount(io.enqRob.req.map(_.valid)), thisActualOut(0), 0, RenameWidth+1, 1)
  XSPerfHistogram("slots_valid_rough", PopCount(io.enqRob.req.map(_.valid)), true.B, 0, RenameWidth+1, 1)

  val perfEvents = Seq(
    ("dispatch_in",                 PopCount(fromRename.map(_.valid && fromRename(0).ready))                       ),
    ("dispatch_empty",              !hasValidInstr                                                                 ),
    ("dispatch_utili",              PopCount(fromRename.map(_.valid))                                              ),
    ("dispatch_waitinstr",          PopCount(fromRename.map(!_.valid && canAccept))                                ),
    ("dispatch_stall_cycle_lsq",    false.B                                                                        ),
    ("dispatch_stall_cycle_rob",    stall_rob                                                                      ),
    ("dispatch_stall_cycle_int_dq", stall_int_dq                                                                   ),
    ("dispatch_stall_cycle_fp_dq",  stall_fp_dq                                                                    ),
    ("dispatch_stall_cycle_ls_dq",  stall_ls_dq                                                                    )
  )
  generatePerfEvent()
}
