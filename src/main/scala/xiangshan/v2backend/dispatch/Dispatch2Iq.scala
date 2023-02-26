package xiangshan.v2backend.dispatch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.SelectOne
import xiangshan._
import xiangshan.backend.rename.BusyTableReadIO
import xiangshan.mem.LsqEnqIO
import xiangshan.v2backend.Bundles.DynInst
import xiangshan.v2backend.{AluCfg, FuConfig, IntScheduler, LduCfg, MemScheduler, SchdBlockParams, StaCfg, StdCfg, VfScheduler}

class Dispatch2Iq(val schdBlockParams : SchdBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val issueBlockParams = schdBlockParams.issueBlockParams

  val numIn = schdBlockParams.numUopIn
  require(issueBlockParams.size > 0 && issueBlockParams.forall(_.numEnq == issueBlockParams.head.numEnq), "issueBlock is null or the enq size of all issueBlock not be the same all\n")
  val numOut = issueBlockParams.head.numEnq
  val numIntSrc = issueBlockParams.map(_.exuBlockParams.map(_.numIntSrc).max)
  val numFpSrc = issueBlockParams.map(_.exuBlockParams.map(_.numFpSrc).max)

  val isMem = schdBlockParams.schdType match {
    case MemScheduler() => true
    case _ => false
  }
  val loadPortSize = issueBlockParams.filter(_.getFuCfgs.contains(LduCfg)).map(_.numEnq).sum
  val storePortSize = issueBlockParams.filter(_.getFuCfgs.contains(StaCfg)).map(_.numEnq).sum

  val numIntStateRead = if (isMem) numIntSrc.zip(issueBlockParams).map(x => x._1 * x._2.numEnq).sum else numIntSrc.max * numIn
  val numFpStateRead  = if (isMem)  numFpSrc.zip(issueBlockParams).map(x => x._1 * x._2.numEnq).sum else  numFpSrc.max * numIn

  val fuConfigsAll = issueBlockParams.flatMap(x => x.getFuCfgs).distinct
  val fuConfigsIdxInIB = fuConfigsAll.map(fuConfig => issueBlockParams.zipWithIndex.filter { case (issueBlockParam, idx) => issueBlockParam.getFuCfgs.contains(fuConfig) }.map(_._2))
  val fuConfigsIBNumber = fuConfigsAll.map(fuConfig => issueBlockParams.zipWithIndex.filter { case (issueBlockParam, idx) => issueBlockParam.getFuCfgs.contains(fuConfig) }.map(_._2).length)

  val portMatrix = fuConfigsIdxInIB.zip(fuConfigsIBNumber).map { case (blockIdxs, length) =>
    blockIdxs.zipWithIndex.map { case (blockIdx, idx) => (blockIdx, (idx until numIn by length).map(x => x)) }.toMap
  }
  val fuConfigsMap = fuConfigsAll.zip(portMatrix).toMap

  //  lazy val module = new Dispatch2IqImp(this)(p)
  lazy val module = schdBlockParams.schdType match {
    case IntScheduler() | VfScheduler() => new CommonDispatch2IqImp(this)(p)
    case MemScheduler() => new MemDispatch2IqImp(this)(p)

    case _ => new Dispatch2IqImp(this)
  }
}

class Dispatch2IqImp(outer: Dispatch2Iq)(implicit p: Parameters) extends LazyModuleImp(outer) with HasXSParameter {
  val numIntSrc = outer.numIntSrc.max
  val numIntStateRead = outer.numIntStateRead
  val numFpSrc = outer.numIntSrc.max
  val numFpStateRead = outer.numFpStateRead

  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val in = Flipped(Vec(outer.numIn, DecoupledIO(new DynInst)))
    val readIntState = if (numIntStateRead > 0) Some(Vec(numIntStateRead, Flipped(new BusyTableReadIO))) else None
    val readFpState = if (numFpStateRead > 0) Some(Vec(numFpStateRead, Flipped(new BusyTableReadIO))) else None
    val out = Vec(outer.issueBlockParams.size, Vec(outer.numOut, DecoupledIO(new DynInst)))
    val enqLsq = if (outer.isMem) Some(Flipped(new LsqEnqIO)) else None
  })

  val in = Wire(Vec(outer.numIn, DecoupledIO(new DynInst)))
  in <> io.in
  in.map(_.ready := false.B)

  def filterCanAccept(fuType: UInt, inPortNum: Int, fuConfigs: Seq[FuConfig], blockIdx: Int): Bool = {
    Cat(fuConfigs.map(fuConfig => if (outer.fuConfigsMap(fuConfig)(blockIdx).contains(inPortNum)) fuConfig.fuType.U === fuType else false.B)).orR
  }
}

class CommonDispatch2IqImp(outer: Dispatch2Iq)(implicit p: Parameters) extends Dispatch2IqImp(outer) with HasXSParameter {

  // We always read physical register states when in gives the instructions.
  // This usually brings better timing.
  if (io.readIntState.isDefined) {
    val req = io.in.flatMap(in => in.bits.psrc.take(numIntSrc))
    io.readIntState.get.map(_.req).zip(req).foreach(x => x._1 := x._2)
  }
  if (io.readFpState.isDefined) {
    val req = io.in.flatMap(in => in.bits.psrc.take(numFpSrc))
    io.readFpState.get.map(_.req).zip(req).foreach(x => x._1 := x._2)
  }

  // srcState is read from outside and connected directly
  if (io.readIntState.isDefined) {
    val intSrcStateVec = in.flatMap(_.bits.srcState.take(numIntSrc))
    io.readIntState.get.map(_.resp).zip(intSrcStateVec).foreach(x => x._2 := x._1)
  }
  if (io.readFpState.isDefined) {
    val fpSrcStateVec = in.flatMap(_.bits.srcState.take(numFpSrc))
    io.readFpState.get.map(_.resp).zip(fpSrcStateVec).foreach(x => x._2 := x._1)
  }

  // arbiter
  for (((outs, issueBlockParam), blockIdx) <- io.out.zip(outer.issueBlockParams).zipWithIndex) {

    val canAccept = in.zipWithIndex.map { case (in, inPortNum) => filterCanAccept(in.bits.fuType, inPortNum, issueBlockParam.getFuCfgs, blockIdx) }

    val select = SelectOne("naive", canAccept, issueBlockParam.numEnq)
    for (j <- 0 until issueBlockParam.numEnq) {
      val (selectValid, selectIdxOH) = select.getNthOH(j + 1)
      outs(j).valid := selectValid
      outs(j).bits := Mux1H(selectIdxOH, io.in.map(_.bits))

      in.zip(selectIdxOH).foreach { case (in, v) =>
        when(v) {
          in.ready := outs(j).ready
        }
      }
    }
  }
}

class MemDispatch2IqImp(outer: Dispatch2Iq)(implicit p: Parameters) extends Dispatch2IqImp(outer) with HasXSParameter {

  val is_blocked = WireDefault(VecInit(Seq.fill(io.in.length)(false.B)))

  if (io.enqLsq.isDefined) {

    val enqLsq = io.enqLsq.get
    val fuType = io.in.map(_.bits.fuType)
    val isLs = fuType.map(f => FuType.isLoadStore(f))
    val isStore = fuType.map(f => FuType.isStoreExu(f))
    val isAMO = fuType.map(f => FuType.isAMO(f))

    val isLoadArrays = Seq.tabulate(io.in.length)(Seq.tabulate(_)(i => io.in(i).valid && !isStore(i)))
    val isStoreArrays = Seq.tabulate(io.in.length)(Seq.tabulate(_)(i => io.in(i).valid && isStore(i)))
    val blockLoads = isLoadArrays.map(PopCount(_) >= outer.loadPortSize.U)
    val blockStores = isStoreArrays.map(PopCount(_) >= outer.storePortSize.U)

    for (i <- io.in.indices) {
      is_blocked(i) := (
        // Q: Why " || is_blocked(i - 1) " ?
        // A: Lsq needs instructions enq in order.
        if (i >= LoadPipelineWidth) Mux(isStore(i), blockStores(i), blockLoads(i)) || is_blocked(i - 1)
        else false.B
        )
      in(i).valid := io.in(i).valid && !is_blocked(i)
      io.in(i).ready := in(i).ready && !is_blocked(i)

      if (i < enqLsq.req.length) {
        enqLsq.needAlloc(i) := Mux(io.in(i).valid && isLs(i), Mux(isStore(i) && !isAMO(i), 2.U, 1.U), 0.U)
        enqLsq.req(i).bits := io.in(i).bits
        in(i).bits.lqIdx := enqLsq.resp(i).lqIdx
        in(i).bits.sqIdx := enqLsq.resp(i).sqIdx

        enqLsq.req(i).valid := in(i).valid && VecInit(io.out.map(_.map(_.ready)).flatten).asUInt.andR
      }
    }
  }

  // arbiter
  for (((outs, issueBlockParam), blockIdx) <- io.out.zip(outer.issueBlockParams).zipWithIndex) {

    val canAccept = in.zipWithIndex.map { case (in, inPortNum) => filterCanAccept(in.bits.fuType, inPortNum, issueBlockParam.getFuCfgs, blockIdx) }

    val select = SelectOne("naive", canAccept, issueBlockParam.numEnq)
    for (j <- 0 until issueBlockParam.numEnq) {
      val (selectValid, selectIdxOH) = select.getNthOH(j + 1)
      outs(j).valid := selectValid
      outs(j).bits := Mux1H(selectIdxOH, io.in.map(_.bits))
      // Special case for STD
      if (issueBlockParam.getFuCfgs.contains(StdCfg)) {
        outs(j).bits.srcType(0) := outs(j).bits.srcType(1)
        outs(j).bits.psrc(0) := outs(j).bits.psrc(1)
      }
      else {
        in.zip(selectIdxOH).foreach { case (in, v) =>
          when(v) {
            in.ready := outs(j).ready
          }
        }
      }
    }
  }

  if (io.enqLsq.isDefined) {
    when(!VecInit(io.out.flatMap(_.map(_.ready))).asUInt.andR || !io.enqLsq.get.canAccept) {
      in.foreach(_.ready := false.B)
      io.out.foreach(_.map(_.valid := false.B))
    }
  }

  if (io.readIntState.isDefined) {
    val stateReadReq = io.out.zip(outer.numIntSrc).flatMap{ case (ports, numIntSrc) => ports.flatMap(port => port.bits.psrc.take(numIntSrc))}
    io.readIntState.get.map(_.req).zip(stateReadReq).foreach(x => x._1 := x._2)
    val stateReadResp = io.out.zip(outer.numIntSrc).flatMap{ case (ports, numIntSrc) => ports.flatMap(port => port.bits.srcState.take(numIntSrc))}
    io.readIntState.get.map(_.resp).zip(stateReadResp).foreach(x => x._2 := x._1)
  }

  if (io.readFpState.isDefined) {
    val stateReadReq = io.out.zip(outer.numFpSrc).flatMap{ case (ports, numFpSrc) => ports.flatMap(port => port.bits.psrc.take(numFpSrc))}
    io.readFpState.get.map(_.req).zip(stateReadReq).foreach(x => x._1 := x._2)
    println("stateReadReq length: " + stateReadReq.length)
    println("io.readFpState.get length: " + io.readFpState.get.length)
    val stateReadResp = io.out.zip(outer.numFpSrc).flatMap{ case (ports, numFpSrc) => ports.flatMap(port => port.bits.srcState.take(numFpSrc))}
    val srcTypeOut = io.out.zip(outer.numFpSrc).flatMap{ case (ports, numFpSrc) => ports.flatMap(port => port.bits.srcType.take(numFpSrc))}
    // When both int and fp are needed, need Mux
    io.readFpState.get.map(_.resp).zip(stateReadResp).zip(srcTypeOut).foreach {
      case ((resp, state), srcType) =>
        when(!io.readIntState.isDefined.B || SrcType.isFp(srcType)) {
          state := resp
        }
    }
  }
}
