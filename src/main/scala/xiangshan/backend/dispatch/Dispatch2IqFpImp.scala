package xiangshan.backend.dispatch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.{Constantin, PriorityMuxDefault, SelectOne}
import utils._
import xiangshan._
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.rename.BusyTableReadIO
import xiangshan.mem.LsqEnqIO
import xiangshan.backend.Bundles.{DynInst, ExuOH}
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.fu.FuType.{FuTypeOrR, falu}
import xiangshan.backend.issue._

import scala.collection._

class Dispatch2IqFpImp(override val wrapper: Dispatch2Iq)(implicit p: Parameters, params: SchdBlockParams)
  extends Dispatch2IqImp(wrapper)
    with HasXSParameter {

  val fpIQValidNumVec = io.fpIQValidNumVec.get
  private val numEnq = io.in.size
  private val numIQ = io.out.size

  val uopsIn = Wire(Vec(wrapper.numIn, DecoupledIO(new DynInst)))
  val numInPorts = io.in.size
  val outs = io.out.flatten
  uopsIn <> io.in
  val exeUnitParms = params.issueBlockParams.map(_.exuBlockParams)
  val exeFuConfigs = params.issueBlockParams.map(_.exuBlockParams.map(_.fuConfigs)).flatten
  val IQFuConfigs = params.issueBlockParams.map(_.exuBlockParams.map(_.fuConfigs).flatten)
  val allFuConfigs = params.issueBlockParams.map(_.exuBlockParams.map(_.fuConfigs)).flatten.flatten.distinct
  val fuConfigMapIQ = allFuConfigs.map{ case x =>
    x -> IQFuConfigs.map(_.contains(x))
  }
  val fuConfigOnlyOneIQ = fuConfigMapIQ.filter(x => x._2.count(_ == true) == 1).map(_._1)
  val fuConfigMultiIQ = fuConfigMapIQ.filter(x => x._2.count(_ == true) > 1).map(_._1)
  println(s"[Dispatch2IqFpImp] IQFuConfigs: ${IQFuConfigs.map(_.map(_.name))}")
  println(s"[Dispatch2IqFpImp] allFuConfigs: ${allFuConfigs.map(_.name)}")
  println(s"[Dispatch2IqFpImp] fuConfigMapIQ: ${fuConfigMapIQ.map(x => (x._1.name, x._2))}")
  println(s"[Dispatch2IqFpImp] fuConfigOnlyOneIQ: ${fuConfigOnlyOneIQ.map(_.name)}")
  println(s"[Dispatch2IqFpImp] fuConfigMultiIQ: ${fuConfigMultiIQ.map(_.name)}")
  val uopsInFuType = VecInit(uopsIn.map(x => Mux(x.valid, x.bits.fuType, 0.U.asTypeOf(x.bits.fuType))))
  val inFuTypeIsMultiIQ = VecInit(uopsInFuType.map{ case x =>
    VecInit(fuConfigMultiIQ.map{ case y => x(y.fuType.id)})
  })
  val inFuTypeIsOnlyOneIQ  = VecInit(uopsInFuType.map { case x =>
    VecInit(fuConfigOnlyOneIQ.map { case y => x(y.fuType.id) })
  })
  val popFuType = Wire(Vec(numEnq, Vec(fuConfigMultiIQ.size, UInt(numEnq.U.getWidth.W))))
  popFuType.zipWithIndex.map{ case (p, i) =>
    p.zipWithIndex.map{ case (pp, j) =>
      pp := PopCount(inFuTypeIsMultiIQ.take(i + 1).map(_(j)))
    }
  }

  // sort the UInt, result[0] is minimum OH, result[1] is second minimum OH...
  def sortUInt(input: Vec[UInt]): Vec[Vec[Bool]] = {
    val inputSize = input.size
    val inputWithIndex = input.zipWithIndex
    val compareResult = inputWithIndex.map{ case (input, i) =>
      inputWithIndex.filter(_._2 != i).map{ case (input2, i2) =>
        if (i < i2) input <= input2 else input < input2
      }
    }
    VecInit((0 until inputSize).map{ i =>
      VecInit(compareResult.map{ c =>
        PopCount(c) === (inputSize - 1 - i).U
      })
    })
  }
  def shiftLeftVecBool(input: Vec[Vec[Bool]], shiftNum: UInt): Vec[Vec[Bool]] = {
    VecInit(input.zipWithIndex.map{ case (in, i) =>
      Mux(i.U < shiftNum, input((i+input.size).U - shiftNum), input(i.U - shiftNum))
    })
  }
  val minIQSel = VecInit(fuConfigMultiIQ.map{ case fuConfig =>
    val thisFuValidNumSeq = VecInit(exeFuConfigs.zip(io.fpIQValidNumVec.get.flatten).filter{case (a,b) => a.contains(fuConfig)}.map(_._2))
    // here use RegNext for better timing
    val sortResult = RegNext(sortUInt(thisFuValidNumSeq))
    val selIQOHSeq = Wire(Vec(numEnq, Vec(numIQ, Bool())))
    selIQOHSeq.zipWithIndex.map{ case (s, i) =>
      s.zipWithIndex.map{ case (ss, j) =>
        if (IQFuConfigs(j).contains(fuConfig)) ss := sortResult(i % sortResult.size)(j)
        else ss := false.B
      }
    }
    if (backendParams.debugEn){
      dontTouch(thisFuValidNumSeq)
      dontTouch(sortResult)
    }
    selIQOHSeq
  })
  val lastIQSel = Reg(chiselTypeOf(minIQSel))
  val lastDispatchNum = Reg(Vec(fuConfigMultiIQ.size, UInt(numEnq.W)))
  val robbSel = VecInit(lastIQSel.zip(lastDispatchNum).map{ case(s, n) =>
    shiftLeftVecBool(s,n)
  })
  val useMinIQSel = Wire(Vec(fuConfigMultiIQ.size, Bool()))
  // set all useMinIQSel is true
  useMinIQSel.map(_ := true.B)
  val finalMultiIQSel = VecInit(minIQSel.zip(robbSel).zip(useMinIQSel).map{ case ((m , r), u) =>
    Mux(u, m, r)
  })
  lastIQSel := finalMultiIQSel
  val onlyOneIQSel = Wire(Vec(fuConfigOnlyOneIQ.size, Vec(numIQ, Bool())))
  onlyOneIQSel := VecInit(fuConfigOnlyOneIQ.map{ case fu =>
    val index = fuConfigMapIQ.map(_._1).indexOf(fu)
    VecInit(fuConfigMapIQ(index)._2.map(_.B))
  })
  val multiSelIQOH = Wire(Vec(numEnq, Vec(numIQ, Bool())))
  multiSelIQOH.zip(inFuTypeIsMultiIQ.zip(popFuType)).map{ case (sel, (fu, popFuType)) =>
    // popFuType need - 1.U because first instr index is 0 popFuType is 1
    sel := Mux1H(fu, finalMultiIQSel)(Mux1H(fu, popFuType.map(_ - 1.U )))
  }
  val onlyOneSelIQOH = Wire(Vec(numEnq, Vec(numIQ, Bool())))
  onlyOneSelIQOH.zip(inFuTypeIsOnlyOneIQ).map { case (sel, fu) =>
    sel := Mux1H(fu, onlyOneIQSel)
  }
  val finalSelIQOH = VecInit(multiSelIQOH.zip(onlyOneSelIQOH).map{ case (m ,o) =>
    VecInit(m.zip(o).map{ case (mm, oo) =>
      mm || oo
    })
  })
  // popSelIQOH width is numEnq.U.getWidth.W because of all uopsIn can select same IQ
  val popSelIQOH = Wire(Vec(numEnq, Vec(numIQ, UInt(numEnq.U.getWidth.W))))
  popSelIQOH.zipWithIndex.map { case (p, i) =>
    p.zipWithIndex.map { case (pp, j) =>
      pp := PopCount(finalSelIQOH.take(i + 1).map(_(j)))
    }
  }
  val IQIsReadyMapEnqNum = io.out.map(x => x.size -> x.head.ready)
  val uopsInReady = popSelIQOH.zipWithIndex.map{ case (p, uopinIndex) =>
    p.zip(IQIsReadyMapEnqNum).zipWithIndex.map{ case ((pp, mapEnqNum), iqIndex) =>
      mapEnqNum._2 && (pp < (mapEnqNum._1 + 1).U) && finalSelIQOH(uopinIndex)(iqIndex) || !finalSelIQOH(uopinIndex)(iqIndex)
    }.reduce(_ && _)
  }
  val isMultiIQDispatch = VecInit(inFuTypeIsMultiIQ.zip(uopsInReady).map{ case (i, u) =>
    VecInit(i.map{_ && u})
  })
  lastDispatchNum.zipWithIndex.map{ case (l,i) =>
    // isMultiIQDispatch index (uopsIn)(fuConfig)
    // lastDispatchNum index (fuConfig)
    l := Mux(isMultiIQDispatch.map(_(i)).reduce(_ || _), PopCount(isMultiIQDispatch.map(_(i))), 0.U)
  }
  uopsIn.zip(uopsInReady).map{ case (u, r) =>
    u.ready := r
  }
  val popSelIQOHIsOne = VecInit(popSelIQOH.map { case p =>
    VecInit(p.zip(IQIsReadyMapEnqNum).map { case (pp, i) =>
      i._2 && pp === 1.U
    })
  })
  val popSelIQOHIsTwo = VecInit(popSelIQOH.map { case p =>
    VecInit(p.zip(IQIsReadyMapEnqNum).map { case (pp, i) =>
      i._2 && pp === 2.U
    })
  })
  io.out.zipWithIndex.map{ case (iq, iqIndex) =>
    iq.zipWithIndex.map{ case (enq, i) =>
      if (i == 0) {
        enq.valid := PriorityMuxDefault(popSelIQOHIsOne.map(_(iqIndex)).zip(uopsIn.map(_.valid)), false.B)
        enq.bits := PriorityMuxDefault(popSelIQOHIsOne.map(_(iqIndex)).zip(uopsIn.map(_.bits)), 0.U.asTypeOf(enq.bits))
      }
      else if (i == 1) {
        enq.valid := PriorityMuxDefault(popSelIQOHIsTwo.map(_(iqIndex)).zip(uopsIn.map(_.valid)), false.B)
        enq.bits := PriorityMuxDefault(popSelIQOHIsTwo.map(_(iqIndex)).zip(uopsIn.map(_.bits)), 0.U.asTypeOf(enq.bits))
      }
      else {
        assert(false, "IQ enqNum must <= 2")
      }
    }
  }
  if(backendParams.debugEn){
    dontTouch(io.fpIQValidNumVec.get)
    dontTouch(inFuTypeIsOnlyOneIQ)
    dontTouch(inFuTypeIsMultiIQ)
    dontTouch(uopsInFuType)
    dontTouch(popFuType)
    dontTouch(minIQSel)
    dontTouch(lastIQSel)
    dontTouch(lastDispatchNum)
    dontTouch(robbSel)
    dontTouch(useMinIQSel)
    dontTouch(finalMultiIQSel)
    dontTouch(onlyOneIQSel)
    dontTouch(multiSelIQOH)
    dontTouch(onlyOneSelIQOH)
    dontTouch(finalSelIQOH)
    dontTouch(popSelIQOH)
    dontTouch(isMultiIQDispatch)
    dontTouch(popSelIQOHIsOne)
    dontTouch(popSelIQOHIsTwo)
  }






  private val reqPsrcVec: IndexedSeq[UInt] = uopsIn.flatMap(in => in.bits.psrc.take(numRegSrc))
  private val intSrcStateVec = if (io.readIntState.isDefined) Some(Wire(Vec(numEnq * numRegSrc, SrcState()))) else None
  private val fpSrcStateVec = if (io.readFpState.isDefined) Some(Wire(Vec(numEnq * numRegSrc, SrcState()))) else None
  private val vfSrcStateVec = if (io.readVfState.isDefined) Some(Wire(Vec(numEnq * numRegSrc, SrcState()))) else None
  private val intSrcLoadDependency = OptionWrapper(io.readIntState.isDefined, Wire(Vec(numEnq * numRegSrc, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))))
  private val fpSrcLoadDependency = OptionWrapper(io.readFpState.isDefined, Wire(Vec(numEnq * numRegSrc, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))))
  private val vfSrcLoadDependency = OptionWrapper(io.readVfState.isDefined, Wire(Vec(numEnq * numRegSrc, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))))

  // We always read physical register states when in gives the instructions.
  // This usually brings better timing.
  if (io.readIntState.isDefined) {
    require(io.readIntState.get.size >= reqPsrcVec.size,
      s"[Dispatch2IqArithImp] io.readIntState.get.size: ${io.readIntState.get.size}, psrc size: ${reqPsrcVec.size}")
    io.readIntState.get.map(_.req).zip(reqPsrcVec).foreach(x => x._1 := x._2)
    io.readIntState.get.map(_.resp).zip(intSrcStateVec.get).foreach(x => x._2 := x._1)
    io.readIntState.get.map(_.loadDependency).zip(intSrcLoadDependency.get).foreach(x => x._2 := x._1)
  }
  if (io.readFpState.isDefined) {
    require(io.readFpState.get.size >= reqPsrcVec.size,
      s"[Dispatch2IqArithImp] io.readFpState.get.size: ${io.readFpState.get.size}, psrc size: ${reqPsrcVec.size}")
    io.readFpState.get.map(_.req).zip(reqPsrcVec).foreach(x => x._1 := x._2)
    io.readFpState.get.map(_.resp).zip(fpSrcStateVec.get).foreach(x => x._2 := x._1)
    io.readFpState.get.map(_.loadDependency).zip(fpSrcLoadDependency.get).foreach(x => x._2 := x._1)
  }
  if (io.readVfState.isDefined) {
    require(io.readVfState.get.size >= reqPsrcVec.size,
      s"[Dispatch2IqArithImp] io.readVfState.get.size: ${io.readVfState.get.size}, psrc size: ${reqPsrcVec.size}")
    io.readVfState.get.map(_.req).zip(reqPsrcVec).foreach(x => x._1 := x._2)
    io.readVfState.get.map(_.resp).zip(vfSrcStateVec.get).foreach(x => x._2 := x._1)
    io.readVfState.get.map(_.loadDependency).zip(vfSrcLoadDependency.get).foreach(x => x._2 := x._1)
  }


  uopsIn
    .flatMap(x => x.bits.srcState.take(numRegSrc) zip x.bits.srcType.take(numRegSrc))
    .zip(
      intSrcStateVec.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(SrcState.busy).toSeq))
        zip fpSrcStateVec.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(SrcState.busy).toSeq))
        zip vfSrcStateVec.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(SrcState.busy).toSeq))
    )
    .foreach {
      case ((state: UInt, srcType), ((intState, fpState), vfState)) =>
        state := Mux1H(Seq(
          SrcType.isXp(srcType) -> intState,
          SrcType.isFp(srcType) -> fpState,
          SrcType.isVp(srcType) -> vfState,
          SrcType.isNotReg(srcType) -> true.B,
        ))
    }
  uopsIn
    .flatMap(x => x.bits.srcLoadDependency.take(numRegSrc) zip x.bits.srcType.take(numRegSrc))
    .zip(
      intSrcLoadDependency.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(0.U.asTypeOf(Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))).toSeq))
        zip fpSrcLoadDependency.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(0.U.asTypeOf(Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))).toSeq))
        zip vfSrcLoadDependency.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(0.U.asTypeOf(Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))).toSeq))
    )
    .foreach {
      case ((ldp, srcType), ((intLdp, fpLdq), vfLdp)) =>
        when(SrcType.isXp(srcType)) {
          ldp := intLdp
        }.elsewhen(SrcType.isFp(srcType)) {
          ldp := fpLdq
        }.elsewhen(SrcType.isVp(srcType)) {
          ldp := vfLdp
        }.otherwise {
          ldp := 0.U.asTypeOf(ldp)
        }
    }


  XSPerfAccumulate("in_valid", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("in_fire", PopCount(io.in.map(_.fire)))
  XSPerfAccumulate("out_valid", PopCount(io.out.flatMap(_.map(_.valid))))
  XSPerfAccumulate("out_fire", PopCount(io.out.flatMap(_.map(_.fire))))
}
