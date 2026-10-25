package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import chisel3.reflect.DataMirror
import chisel3.simulator.scalatest.ChiselSim
import freechips.rocketchip.diplomacy.LazyModule
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.fu.FuType
import scala.util.Random

object RobTestArtifacts {
  private val directory = java.nio.file.Paths.get(
    sys.props.getOrElse("rob.test.outputDir", "build/rob-equivalence")).toAbsolutePath.normalize
  java.nio.file.Files.createDirectories(directory)

  def path(name: String): java.nio.file.Path = directory.resolve(name)

  def topdownObject(): String = {
    val root = java.nio.file.Paths.get("").toAbsolutePath.normalize
    val output = path("RobTopdownDpi.o")
    val command = Seq(
      sys.env.getOrElse("CXX", "c++"), "-std=c++17", "-O2", "-c",
      root.resolve("src/test/csrc/rob/RobTopdownDpi.cpp").toString,
      "-I", root.resolve("difftest/src/test/csrc/plugin/topdown").toString,
      "-o", output.toString)
    require(scala.sys.process.Process(command).! == 0, "Failed to compile the real TopDown DPI adapter")
    output.toString
  }
}

object RobTestParameters {
  def apply(): Parameters = {
    val base = new DefaultConfig
    val fullDebug = java.lang.Boolean.getBoolean("rob.test.fullDebug")
    val core = base(XSTileKey).head.copy(RobSize=32, RabSize=80, VTypeBufferSize=32)
    val debug = base(DebugOptionsKey).copy(
      FPGAPlatform=true, EnableDifftest=fullDebug, AlwaysBasicDiff=false,
      FullBasicDiff=false, EnablePerfDebug=false, EnableChiselDB=false, AlwaysBasicDB=false)
    base.alterPartial {
      case XSCoreParamsKey => core
      case DebugOptionsKey => debug
      case LogUtilsOptionsKey => LogUtilsOptions(false,false,true,false)
      case PerfCounterOptionsKey => PerfCounterOptions(
        enablePerfPrint=false,enablePerfDB=false,perfLevel=XSPerfLevel.VERBOSE,perfDBHartID=0)
    }
  }
  def bind(params: BackendParams): Unit = {
    params.allExuParams.zipWithIndex.foreach { case (exu, index) =>
      exu.bindBackendParam(params)
      exu.updateIQWakeUpConfigs(params.iqWakeUpParams)
      exu.updateExuIdx(index)
    }
    params.allSchdParams.foreach(_.bindBackendParam(params))
    params.allIssueParams.foreach { iq =>
      iq.bindBackendParam(params)
      iq.exuBlockParams.foreach(_.bindIssueBlockParam(iq))
    }
  }
}

/** Complete independent ROB instances, each with its own RAB and VTB. */
class RobEquivalenceHarness(implicit val p: Parameters) extends Module with HasXSParameter {
  if (java.lang.Boolean.getBoolean("rob.test.fullDebug")) {
    difftest.DifftestModule.parseArgs(Array("--difftest-exclude","Difftest"))
  }
  private val params=p(XSCoreParamsKey).backendParams
  RobTestParameters.bind(params)
  val io=IO(new RobIO(params))
  val checkEnable=IO(Input(Bool()))
  val checkStoreQueries=IO(Input(Bool()))
  val retiredEntries=IO(Output(UInt(32.W)))
  val candidate=Module(LazyModule(new Rob(params)).module)
  private val referenceCore = p(XSCoreParamsKey).copy(HartId=1)
  private val referenceParameters = p.alterPartial {
    case XSCoreParamsKey => referenceCore
  }
  val reference=Module(LazyModule(new OldRob(params)(referenceParameters)).module)
  def ioShape(data: Data, path: String): Vector[String] = data match {
    case record: Record => record.elements.toVector.flatMap { case (name, value) =>
      ioShape(value, if (path.isEmpty) name else path + "." + name)
    }
    case vector: Vec[_] => vector.zipWithIndex.toVector.flatMap { case (value, index) =>
      ioShape(value, path + "[" + index + "]")
    }
    case leaf => Vector(path + "\t" + DataMirror.directionOf(leaf) + "\t" + leaf.getWidth)
  }
  private val candidateShape = ioShape(candidate.io, "io")
  private val referenceShape = ioShape(reference.io, "io")
  require(candidateShape == referenceShape, "ROB IO leaf names, directions or widths differ")
  private val shapePath = RobTestArtifacts.path(
      (if (java.lang.Boolean.getBoolean("rob.test.fullDebug")) "unit-debug-io-shape.tsv" else "unit-io-shape.tsv"))
  java.nio.file.Files.write(shapePath,
    ("path\tdirection\twidth\n" + candidateShape.mkString("\n") + "\n")
      .getBytes(java.nio.charset.StandardCharsets.UTF_8))
  println("ROB_IO_SHAPE_PASS leaves=" + candidateShape.size)
  io <> candidate.io
  def connectInputs(old: Data, external: Data): Unit = old match {
    case record: Record =>
      val other=external.asInstanceOf[Record]
      record.elements.foreach { case (name,value) => connectInputs(value,other.elements(name)) }
    case vector: Vec[_] =>
      vector.zip(external.asInstanceOf[Vec[Data]]).foreach { case (a,b) => connectInputs(a,b) }
    case leaf if DataMirror.directionOf(leaf)==ActualDirection.Input => leaf := external
    case _ =>
  }
  connectInputs(reference.io,io)
  private val a=candidate.io
  private val b=reference.io
  private def leafPaths(data: Data,path: String): Vector[(Data,String)] = data match {
    case record: Record => record.elements.toVector.flatMap { case(name,value)=>
      leafPaths(value,path + "." + name)
    }
    case vector: Vec[_] => vector.zipWithIndex.toVector.flatMap { case(value,index)=>
      leafPaths(value,path + "[" + index + "]")
    }
    case leaf => Vector(leaf -> path)
  }
  private val candidatePaths = new java.util.IdentityHashMap[Data,String]()
  leafPaths(a,"io").foreach { case(leaf,path)=>candidatePaths.put(leaf,path) }
  private val comparisons=scala.collection.mutable.LinkedHashMap.empty[String,Vector[(String,String)]]
  def same(x: Data,y: Data,name: String,valid: Bool=true.B): Unit = {
    val guard=if(valid.litOption.contains(BigInt(1))) "checkEnable" else "checkEnable && eventGuard"
    leafPaths(x,"").foreach { case(leaf,_)=>
      val path=candidatePaths.get(leaf)
      require(path!=null,"Compared data is outside candidate ROB IO")
      comparisons.update(path,comparisons.getOrElse(path,Vector.empty) :+ (name -> guard))
    }
    when(checkEnable && valid) { assert(x.asUInt===y.asUInt,s"ROB equivalence: $name") }
  }
  Seq("canAccept","canAcceptForDispatch","isEmpty","resp").foreach { key =>
    same(a.enq.elements(key),b.enq.elements(key),key)
  }
  same(a.robFull,b.robFull,"robFull")
  same(a.robDeqPtr,b.robDeqPtr,"robDeqPtr")
  same(a.cpu_wfi,b.cpu_wfi,"cpu_wfi")
  same(a.wfi.wfiReq,b.wfi.wfiReq,"wfiReq")
  same(a.commits.isCommit,b.commits.isCommit,"commit mode")
  same(a.commits.isWalk,b.commits.isWalk,"walk mode")
  same(a.commits.commitValid,b.commits.commitValid,"commit candidates")
  same(a.commits.walkValid,b.commits.walkValid,"walk mask")
  a.commits.info.indices.foreach { i =>
    a.commits.info(i).elements.foreach { case(name,value)=>
      // connectCommitEntry never assigns this legacy register in either DUT.
      if(name!="debug_otherPdest")
        same(value,b.commits.info(i).elements(name),s"commit info $i.$name",
          a.commits.isCommit && a.commits.commitValid(i))
    }
    same(a.commits.robIdx(i),b.commits.robIdx(i),s"commit pointer $i",a.commits.isCommit && a.commits.commitValid(i))
  }
  same(a.flushOut.valid,b.flushOut.valid,"flush valid")
  Seq("robIdx","ftqIdx","ftqOffset","isRVC","level","interrupt","satpFlush").foreach { key =>
    same(a.flushOut.bits.elements(key),b.flushOut.bits.elements(key),key,a.flushOut.valid)
  }
  same(a.flushPcInfo,b.flushPcInfo,"flush PC",a.flushOut.valid)
  same(a.exception.valid,b.exception.valid,"exception valid")
  same(a.exception.bits,b.exception.bits,"exception payload",a.exception.valid)
  same(a.readGPAMemAddr.valid,b.readGPAMemAddr.valid,"GPA valid")
  same(a.readGPAMemAddr.bits,b.readGPAMemAddr.bits,"GPA payload",a.readGPAMemAddr.valid)
  Seq("lcommit","scommit","commit","pendingPtr","pendingPtrNext").foreach { key =>
    same(a.lsq.elements(key),b.lsq.elements(key),key)
  }
  Seq("fflags","vxsat","dirty_fs","dirty_vs").foreach { key =>
    same(a.csr.elements(key),b.csr.elements(key),key)
  }
  same(a.csr.perfinfo,b.csr.perfinfo,"retired count")
  same(a.csr.vstart.valid,b.csr.vstart.valid,"vstart valid")
  same(a.csr.vstart.bits,b.csr.vstart.bits,"vstart",a.csr.vstart.valid)
  same(a.rabCommits.isCommit,b.rabCommits.isCommit,"RAB commit mode")
  same(a.rabCommits.isWalk,b.rabCommits.isWalk,"RAB walk mode")
  same(a.rabCommits.commitValid,b.rabCommits.commitValid,"RAB commit mask")
  same(a.rabCommits.walkValid,b.rabCommits.walkValid,"RAB walk mask")
  a.rabCommits.info.indices.foreach { i =>
    same(a.rabCommits.info(i),b.rabCommits.info(i),s"RAB info $i",
      a.rabCommits.isCommit && a.rabCommits.commitValid(i) || a.rabCommits.isWalk && a.rabCommits.walkValid(i))
  }
  same(a.vlCommits.isCommit,b.vlCommits.isCommit,"VL commit mode")
  same(a.vlCommits.isWalk,b.vlCommits.isWalk,"VL walk mode")
  same(a.vlCommits.commitValid,b.vlCommits.commitValid,"VL commit mask")
  same(a.vlCommits.walkValid,b.vlCommits.walkValid,"VL walk mask")
  a.vlCommits.pdestVl.indices.foreach { i =>
    same(a.vlCommits.pdestVl(i),b.vlCommits.pdestVl(i),s"VL info $i",
      a.vlCommits.isCommit && a.vlCommits.commitValid(i) || a.vlCommits.isWalk && a.vlCommits.walkValid(i))
  }
  same(a.toDecode.isResumeVType,b.toDecode.isResumeVType,"vtype resume")
  same(a.toDecode.walkToArchVType,b.toDecode.walkToArchVType,"vtype arch")
  same(a.toDecode.walkVType.valid,b.toDecode.walkVType.valid,"vtype walk valid")
  same(a.toDecode.walkVType.bits,b.toDecode.walkVType.bits,"vtype walk",a.toDecode.walkVType.valid)
  same(a.toDecode.commitVType.vtype.valid,b.toDecode.commitVType.vtype.valid,"vtype commit valid")
  same(a.toDecode.commitVType.hasVsetvl,b.toDecode.commitVType.hasVsetvl,"vsetvl commit")
  same(a.toDecode.commitVType,b.toDecode.commitVType,"vtype commit",a.toDecode.commitVType.vtype.valid)
  same(a.isVsetFlushPipe,b.isVsetFlushPipe,"vset flush")
  same(a.toVecExcpMod.excpInfo.valid,b.toVecExcpMod.excpInfo.valid,"vector exception valid")
  same(a.toVecExcpMod.excpInfo.bits,b.toVecExcpMod.excpInfo.bits,"vector exception",a.toVecExcpMod.excpInfo.valid)
  a.toVecExcpMod.logicPhyRegMap.indices.foreach { i =>
    val x=a.toVecExcpMod.logicPhyRegMap(i); val y=b.toVecExcpMod.logicPhyRegMap(i)
    same(x.valid,y.valid,s"map valid $i"); same(x.bits,y.bits,s"map $i",x.valid)
  }
  a.trace.traceCommitInfo.blocks.indices.foreach { i =>
    val x=a.trace.traceCommitInfo.blocks(i); val y=b.trace.traceCommitInfo.blocks(i)
    same(x.valid,y.valid,s"trace valid $i"); same(x.bits,y.bits,s"trace $i",x.valid)
  }
  same(a.debugRolling,b.debugRolling,"rolling count")
  same(a.debugTopDown.toDispatch,b.debugTopDown.toDispatch,"dispatch TopDown")
  Seq(
    (a.debugTopDown.toCore.robHeadVaddr,b.debugTopDown.toCore.robHeadVaddr,"head virtual address"),
    (a.debugTopDown.toCore.robHeadPaddr,b.debugTopDown.toCore.robHeadPaddr,"head physical address"),
    (a.debugTopDown.robHeadLqIdx,b.debugTopDown.robHeadLqIdx,"head load queue index")
  ).foreach { case(x,y,name)=>
    same(x.valid,y.valid,name + " valid")
    same(x.bits,y.bits,name,x.valid)
  }
  a.debugRobHeadStall.zip(b.debugRobHeadStall).foreach { case(x,y)=>
    same(x.valid,y.valid,"head stall valid")
    same(x.bits,y.bits,"head stall reason",x.valid)
  }
  a.diffCommitForTrap.zip(b.diffCommitForTrap).foreach { case(x,y)=>same(x,y,"trap observation release") }
  a.diffRatCommitRobIdx.zip(b.diffRatCommitRobIdx).foreach { case(x,y)=>
    same(x.valid,y.valid,"diff RAT valid");same(x.bits,y.bits,"diff RAT pointer",x.valid)
  }
  a.diffRatCommitRobIdxVec.zip(b.diffRatCommitRobIdxVec).foreach { case(x,y)=>
    x.zip(y).zipWithIndex.foreach { case((vx,vy),index)=>
      same(vx.valid,vy.valid,s"diff RAT valid $index")
      same(vx.bits,vy.bits,s"diff RAT pointer $index",vx.valid)
    }
  }
  a.debugBlockBackward.zip(b.debugBlockBackward).foreach { case(x,y)=>same(x,y,"block backward") }
  a.debugWaitForward.zip(b.debugWaitForward).foreach { case(x,y)=>same(x,y,"wait forward") }
  if (p(DebugOptionsKey).EnableDifftest || p(DebugOptionsKey).FullBasicDiff) {
    a.storeDebugInfo.zip(b.storeDebugInfo).zipWithIndex.foreach { case((x,y),index)=>
      same(x.pc,y.pc,s"store PC $index",checkStoreQueries)
      x.storeOffset.zip(y.storeOffset).foreach { case(vx,vy)=>same(vx,vy,s"store offset $index",checkStoreQueries) }
      x.preCommit.zip(y.preCommit).foreach { case(vx,vy)=>same(vx,vy,s"store precommit $index",checkStoreQueries) }
    }
  }
  private val definedFlushFields=Set("robIdx","ftqIdx","ftqOffset","isRVC","level","interrupt","satpFlush")
  private val outputComparisons=leafPaths(a,"io").collect {
    case(leaf,path) if DataMirror.directionOf(leaf)==ActualDirection.Output =>
      val checks=comparisons.getOrElse(path,Vector.empty)
      val exemption=if(checks.nonEmpty) ""
        else if(path.contains(".debug_otherPdest[")) "Reference register is never assigned"
        else if(path.startsWith("io.flushOut.bits.") &&
          !definedFlushFields(path.stripPrefix("io.flushOut.bits.").takeWhile(_!='.')))
          "Reference flush payload field is DontCare"
        else if(path.startsWith("io.storeDebugInfo[") && path.endsWith(".pc") &&
          !p(DebugOptionsKey).EnableDifftest && !p(DebugOptionsKey).FullBasicDiff)
          "Reference store PC is undefined in this configuration"
        else throw new IllegalArgumentException("ROB output leaf has no comparison or exemption: " + path)
      ujson.Obj("path"->path,"width"->leaf.getWidth,"compared"->checks.nonEmpty,
        "checks"->ujson.Arr.from(checks.map { case(name,guard)=>ujson.Obj("name"->name,"guard"->guard) }),
        "exemption"->exemption)
  }
  private val comparisonPath=RobTestArtifacts.path(
      (if(java.lang.Boolean.getBoolean("rob.test.fullDebug")) "unit-debug-output-comparisons.json" else "unit-output-comparisons.json"))
  java.nio.file.Files.write(comparisonPath,
    ujson.write(ujson.Obj("scope"->"Elaboration-time comparison census; dynamic execution recorded separately",
      "outputs"->ujson.Arr.from(outputComparisons)),indent=2).getBytes(java.nio.charset.StandardCharsets.UTF_8))
  println("ROB_OUTPUT_COMPARISON_CENSUS_PASS outputs="+outputComparisons.size)
  val retired=RegInit(0.U(32.W))
  retired := retired+Mux(a.commits.isCommit,PopCount(a.commits.commitValid),0.U)
  retiredEntries := retired
}

class RobEquivalenceTest extends AnyFlatSpec with ChiselSim {
  behavior of "Refactored ROB"
  it should "match the independent OldRob for legal allocation and completion traffic" in {
    implicit val simulator: chisel3.simulator.HasSimulator =
      chisel3.simulator.HasSimulator.simulators.verilator(
        svsim.CommonCompilationSettings(linkLibraryPaths =
          if(java.lang.Boolean.getBoolean("rob.test.fullDebug"))
            Seq(RobTestArtifacts.topdownObject())
          else Seq.empty))
    implicit val p: Parameters=RobTestParameters()
    simulate(new RobEquivalenceHarness) { dut =>
      var trapReleases=0
      def step(cycles: Int=1): Unit = for(_<-0 until cycles) {
        dut.io.diffArchEvent.foreach { event=>
          val release=dut.io.diffCommitForTrap.get.peek().litToBoolean
          event.valid.poke(release.B);event.bits.poke(release.B)
          if(release) trapReleases+=1
        }
        dut.clock.step()
      }
      def zeroInputs(data: Data): Unit = data match {
        case record: Record => record.elements.values.foreach(zeroInputs)
        case vector: Vec[_] => vector.foreach(zeroInputs)
        case leaf if DataMirror.directionOf(leaf)==ActualDirection.Input => leaf match {
          case value: Bool => value.poke(false.B)
          case value: UInt if value.getWidth > 0 => value.poke(0.U)
          case value: SInt if value.getWidth > 0 => value.poke(0.S)
          case _ =>
        }
        case _ =>
      }
      def clearCycle(): Unit = {
        dut.io.enq.req.foreach(_.valid.poke(false.B))
        dut.io.writeback.foreach(_.valid.poke(false.B))
        dut.io.exuWriteback.foreach(_.valid.poke(false.B))
        dut.io.writebackNeedFlush.foreach(_.poke(false.B))
        dut.io.redirect.valid.poke(false.B)
        dut.io.csr.wfiEvent.poke(false.B)
        dut.io.csr.intrBitSet.poke(false.B)
        dut.io.csr.criticalErrorState.poke(false.B)
        dut.io.trace.blockCommit.poke(false.B)
        dut.io.fromVecExcpMod.busy.poke(false.B)
        dut.io.snpt.useSnpt.poke(false.B)
        dut.io.snpt.snptDeq.poke(false.B)
        dut.io.snpt.flushVec.foreach(_.poke(false.B))
        dut.io.wfi_enable.poke(true.B); dut.io.vstartIsZero.poke(true.B)
        dut.io.wfi.safeFromMem.poke(true.B); dut.io.wfi.safeFromFrontend.poke(true.B)
      }
      zeroInputs(dut.io)
      dut.checkStoreQueries.poke(false.B)
      clearCycle(); dut.checkEnable.poke(false.B); dut.reset.poke(true.B)
      step(5); dut.reset.poke(false.B); step(8); dut.checkEnable.poke(true.B)
      val random=new Random(0x524f42L)
      case class Pending(flag: Boolean,index: Int,former: Boolean,due: Int)
      var pending=Vector.empty[Pending]
      var allocated=0
      val ports=dut.io.writeback.indices.filter(i=>dut.io.writeback(i).bits.params.hasAluFu).toVector
      require(ports.nonEmpty)
      def pointer(ptr: RobPtr,flag: Boolean,index: Int,former: Boolean): Unit = {
        ptr.flag.poke(flag.B);ptr.value.poke(index.U);ptr.isFormer.poke(former.B)
      }
      def enqueue(lane: Int,pair: Int,former: Boolean,first: Boolean,
        flag: Boolean,index: Int,formerReports: Int,latterReports: Int): Unit = {
        val r=dut.io.enq.req(lane)
        r.valid.poke(true.B);r.bits.firstUop.poke(first.B);r.bits.lastUop.poke(true.B)
        r.bits.entryPairType.poke(pair.U);r.bits.formerNumWB.poke(formerReports.U)
        r.bits.latterNumWB.poke(latterReports.U);r.bits.numWB.poke((formerReports+latterReports).U)
        r.bits.fuType.poke(FuType.alu.U);r.bits.commitType.poke(CommitType.NORMAL)
        r.bits.traceBlockInPipe.iretire.poke((if(pair==0)2 else 5).U)
        r.bits.formerTraceIretire.poke(2.U);r.bits.interruptSafe.poke(true.B)
        pointer(r.bits.robIdx,flag,index,former)
      }
      for(cycle<-0 until 2500) {
        clearCycle()
        dut.io.trace.blockCommit.poke((random.nextInt(7)==0).B)
        dut.io.fromVecExcpMod.busy.poke((random.nextInt(31)==0).B)
        val completing=pending.filter(_.due<=cycle).take(ports.size)
        completing.zip(ports).foreach { case(e,port)=>
          dut.io.writeback(port).valid.poke(true.B)
          pointer(dut.io.writeback(port).bits.robIdx,e.flag,e.index,e.former)
          dut.io.exuWriteback(port).valid.poke(true.B)
          pointer(dut.io.exuWriteback(port).bits.robIdx,e.flag,e.index,e.former)
        }
        completing.foreach(e=>pending=pending.patch(pending.indexOf(e),Nil,1))
        if(cycle<2200 && dut.io.enq.canAccept.peek().litToBoolean && random.nextInt(4)!=0) {
          val entryCount=1+random.nextInt(3)
          var lane=0
          for(_<-0 until entryCount if lane<7) {
            val pair=random.nextInt(4);val index=allocated%32;val flag=(allocated/32)%2!=0
            val formerReports=1+random.nextInt(2);val latterReports=if(pair==0)0 else 1+random.nextInt(2)
            enqueue(lane,pair,true,true,flag,index,formerReports,latterReports);lane+=1
            if(pair!=0) {enqueue(lane,pair,false,false,flag,index,formerReports,latterReports);lane+=1}
            for(_<-0 until formerReports)pending :+= Pending(flag,index,true,cycle+2+random.nextInt(13))
            for(_<-0 until latterReports)pending :+= Pending(flag,index,false,cycle+2+random.nextInt(13))
            allocated+=1
          }
        }
        step()
      }
      assert(pending.isEmpty,"Outstanding completion reports remain")
      assert(dut.retiredEntries.peek().litValue==allocated,"Accepted entry retirement count differs")
      assert(allocated>100,"Insufficient test traffic")
      clearCycle();step(10)
      println("ROB_EQUIVALENCE_PASS entries="+allocated+" cycles=2510 seed=0x524f42")

      def restart(): Unit = {
        trapReleases=0;dut.checkStoreQueries.poke(false.B)
        dut.checkEnable.poke(false.B);zeroInputs(dut.io);clearCycle()
        dut.reset.poke(true.B);step(5);dut.reset.poke(false.B)
        step(8);dut.checkEnable.poke(true.B)
      }
      def complete(index: Int,former: Boolean,port: Int=ports.head): Unit = {
        dut.io.writeback(port).valid.poke(true.B)
        pointer(dut.io.writeback(port).bits.robIdx,false,index,former)
        dut.io.exuWriteback(port).valid.poke(true.B)
        pointer(dut.io.exuWriteback(port).bits.robIdx,false,index,former)
      }
      def drain(cycles: Int): Unit = for(_<-0 until cycles) {clearCycle();step()}
      for(cutFormer<-Seq(true,false);self<-Seq(true,false)) {
        restart();dut.io.trace.blockCommit.poke(true.B)
        for(entry<-0 until 3) {
          enqueue(2*entry,3,true,true,false,entry,1,1)
          enqueue(2*entry+1,3,false,false,false,entry,1,1)
        }
        step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
        dut.io.redirect.valid.poke(true.B)
        pointer(dut.io.redirect.bits.robIdx,false,1,cutFormer)
        dut.io.redirect.bits.level.poke(self.B)
        step();clearCycle();step()
        clearCycle();complete(0,true);complete(0,false,ports(1));step()
        clearCycle()
        if(!(cutFormer && self)) {
          complete(1,true)
          if(!cutFormer && !self) complete(1,false,ports(1))
        }
        step();drain(60)
        val expected=if(cutFormer && self)1 else 2
        assert(dut.retiredEntries.peek().litValue==expected,"Recovery surviving entry count differs")
      }
      println("ROB_RECOVERY_PASS cuts=4")

      restart();dut.io.trace.blockCommit.poke(true.B)
      enqueue(0,3,true,true,false,0,1,1);enqueue(1,3,false,false,false,0,1,1)
      step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
      complete(0,true);step()
      for(_<-0 until 4){clearCycle();dut.io.trace.blockCommit.poke(true.B);step()}
      assert(!dut.io.lsq.pendingPtr.isFormer.peek().litToBoolean,"Pending slot did not advance to latter")
      clearCycle();dut.io.redirect.valid.poke(true.B)
      pointer(dut.io.redirect.bits.robIdx,false,0,false);dut.io.redirect.bits.level.poke(true.B)
      assert(dut.io.lsq.pendingPtr.isFormer.peek().litToBoolean,"Redirect pending bypass was delayed")
      step();drain(60)
      assert(dut.retiredEntries.peek().litValue==1,"Surviving former did not retire")
      println("ROB_PENDING_PASS")

      restart()
      enqueue(0,0,true,true,false,0,0,0)
      dut.io.enq.req(0).bits.fuType.poke(FuType.csr.U)
      dut.io.enq.req(0).bits.fuOpType.poke(CSROpType.wfi)
      step();clearCycle()
      assert(dut.io.wfi.wfiReq.peek().litToBoolean,"WFI request missing")
      for(mem<-Seq(false,true);frontend<-Seq(false,true)) {
        dut.io.wfi.safeFromMem.poke(mem.B);dut.io.wfi.safeFromFrontend.poke(frontend.B)
        assert(dut.io.cpu_wfi.peek().litToBoolean==(mem && frontend),"WFI safety handshake differs")
        assert(dut.io.wfi.wfiReq.peek().litToBoolean,"Safety handshake released WFI")
      }
      dut.io.csr.wfiEvent.poke(true.B);step()
      assert(dut.io.wfi.wfiReq.peek().litToBoolean,"WFI wake was one cycle early")
      clearCycle();step()
      assert(dut.io.wfi.wfiReq.peek().litToBoolean,"WFI wake was two cycles early")
      step()
      assert(!dut.io.wfi.wfiReq.peek().litToBoolean,"WFI wake event failed")
      drain(30);assert(dut.retiredEntries.peek().litValue==1,"WFI did not retire after wake")
      println("ROB_WFI_PASS")

      for(disableAtAccept<-Seq(false,true)) {
        restart()
        enqueue(0,0,true,true,false,0,0,0)
        dut.io.enq.req(0).bits.fuType.poke(FuType.csr.U)
        dut.io.enq.req(0).bits.fuOpType.poke(CSROpType.wfi)
        dut.io.wfi_enable.poke((!disableAtAccept).B)
        step();clearCycle()
        if(disableAtAccept) {
          assert(!dut.io.wfi.wfiReq.peek().litToBoolean,"Disabled WFI admission set waiting state")
        } else {
          assert(dut.io.wfi.wfiReq.peek().litToBoolean,"WFI waiting state missing before disable")
          dut.io.wfi_enable.poke(false.B);step();clearCycle()
          assert(!dut.io.wfi.wfiReq.peek().litToBoolean,"WFI disable failed to release waiting state")
        }
        drain(30)
        assert(dut.retiredEntries.peek().litValue==1,"Disabled WFI failed to retire")
      }
      println("ROB_WFI_DISABLE_PASS cases=2 safe=4 wakeCycles=3")

      restart();dut.io.trace.blockCommit.poke(true.B)
      for(i<-0 until 4){
        enqueue(i,0,true,true,false,i,1,0)
        dut.io.enq.req(i).bits.rfWen.poke(true.B)
        dut.io.enq.req(i).bits.ldest.poke((i+1).U)
        dut.io.enq.req(i).bits.pdest.poke((i+40).U)
        dut.io.enq.req(i).bits.dirtyFs.poke(true.B)
      }
      step();clearCycle()
      for(i<-0 until 4){
        dut.io.trace.blockCommit.poke(true.B)
        complete(i,true);step();clearCycle()
      }
      var mappings=0;var sawDirty=false
      for(_<-0 until 40){
        if(dut.io.rabCommits.isCommit.peek().litToBoolean)
          mappings+=dut.io.rabCommits.commitValid.count(_.peek().litToBoolean)
        sawDirty=sawDirty||dut.io.csr.dirty_fs.peek().litToBoolean
        step();clearCycle()
      }
      assert(dut.retiredEntries.peek().litValue==4,"Mapping scenario retire count differs")
      assert(mappings==4,"Mapping quota was lost or duplicated")
      assert(sawDirty,"Retired dirty FS side effect missing")
      println("ROB_MAPPING_CSR_PASS maps="+mappings)

      restart();dut.io.trace.blockCommit.poke(true.B)
      enqueue(0,3,true,true,false,0,1,1);enqueue(1,3,false,false,false,0,1,1)
      dut.io.enq.req(0).bits.commitType.poke(CommitType.LOAD)
      dut.io.enq.req(1).bits.commitType.poke(CommitType.LOAD)
      step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
      complete(0,true);complete(0,false,ports(1));step();clearCycle()
      var loads=0
      for(_<-0 until 30){
        loads+=dut.io.lsq.lcommit.peek().litValue.toInt
        step();clearCycle()
      }
      assert(loads==2,"Dual-load retirement count differs")
      println("ROB_LSQ_LOAD_PASS loads="+loads)

      for(useSnapshot <- Seq(false,true)) {
        restart();dut.io.trace.blockCommit.poke(true.B)
        for(i <- 0 until 5) enqueue(i,0,true,true,false,i,1,0)
        dut.io.enq.req(0).bits.snapshot.poke(useSnapshot.B)
        step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
        dut.io.redirect.valid.poke(true.B)
        pointer(dut.io.redirect.bits.robIdx,false,3,true)
        dut.io.redirect.bits.level.poke(false.B)
        dut.io.snpt.useSnpt.poke(useSnapshot.B);dut.io.snpt.snptSelect.poke(0.U)
        assert(!dut.io.commits.walkValid.exists(_.peek().litToBoolean),"Redirect setup emitted walk")
        step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
        dut.io.redirect.valid.poke(true.B)
        pointer(dut.io.redirect.bits.robIdx,false,2,true)
        dut.io.redirect.bits.level.poke(false.B)
        dut.io.snpt.useSnpt.poke(useSnapshot.B)
        assert(!dut.io.commits.walkValid.exists(_.peek().litToBoolean),"Repeated redirect emitted walk")
        step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
        assert(!dut.io.commits.walkValid.exists(_.peek().litToBoolean),"Delayed setup emitted walk")
        step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
        for(i <- 0 until 3) {
          complete(i,true);step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
        }
        clearCycle();drain(70)
        assert(dut.retiredEntries.peek().litValue==3,"Repeated recovery surviving count differs")
      }
      println("ROB_SNAPSHOT_RESTART_PASS modes=2")

      restart();dut.io.trace.blockCommit.poke(true.B)
      enqueue(0,0,true,true,false,0,1,0);enqueue(1,0,true,true,false,1,1,0)
      dut.io.enq.req(0).bits.fflagsWen.poke(true.B)
      dut.io.enq.req(1).bits.vxsatWen.poke(true.B)
      step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
      val flagsPort=dut.io.writeback.indices.find(i=>dut.io.writeback(i).bits.fflags.nonEmpty).get
      val satPort=dut.io.writeback.indices.find(i=>dut.io.writeback(i).bits.vxsat.nonEmpty).get
      complete(0,true,flagsPort)
      dut.io.writeback(flagsPort).bits.fflags.get.poke(0x15.U)
      dut.io.writeback(flagsPort).bits.fflagsWen.get.poke(true.B)
      step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
      complete(1,true,satPort);dut.io.writeback(satPort).bits.vxsat.get.poke(true.B)
      step();clearCycle()
      var flags=0;var saturated=false
      for(_<-0 until 40) {
        flags |= dut.io.csr.fflags.peek().litValue.toInt
        saturated ||= dut.io.csr.vxsat.peek().litToBoolean
        step();clearCycle()
      }
      assert(flags==0x15 && saturated,"Retired sticky flag increments differ")
      assert(dut.retiredEntries.peek().litValue==2,"Flag scenario did not retire")
      println("ROB_FLAGS_PASS fflags=21 vxsat=1")

      restart();dut.io.trace.blockCommit.poke(true.B)
      enqueue(0,0,true,true,false,0,1,0);enqueue(1,0,true,true,false,1,2,0)
      dut.io.enq.req(1).bits.fflagsWen.poke(true.B)
      dut.io.enq.req(1).bits.vxsatWen.poke(true.B)
      dut.io.enq.req(1).bits.dirtyFs.poke(true.B)
      step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
      complete(1,true,flagsPort)
      dut.io.writeback(flagsPort).bits.fflags.get.poke(0x1f.U)
      dut.io.writeback(flagsPort).bits.fflagsWen.get.poke(true.B)
      step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
      complete(1,true,satPort);dut.io.writeback(satPort).bits.vxsat.get.poke(true.B)
      step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
      dut.io.redirect.valid.poke(true.B);pointer(dut.io.redirect.bits.robIdx,false,0,true)
      dut.io.redirect.bits.level.poke(false.B)
      step();clearCycle()
      def noKilledFlags(): Unit = {
        assert(dut.io.csr.fflags.peek().litValue==0,"Killed setter leaked floating flags")
        assert(!dut.io.csr.vxsat.peek().litToBoolean,"Killed setter leaked saturation flag")
        assert(!dut.io.csr.dirty_fs.peek().litToBoolean,"Killed setter leaked dirty FS")
      }
      for(_<-0 until 50) {noKilledFlags();step();clearCycle()}
      complete(0,true);step();clearCycle()
      for(_<-0 until 30) {noKilledFlags();step();clearCycle()}
      assert(dut.retiredEntries.peek().litValue==1,"Flag recovery surviving count differs")
      dut.io.trace.blockCommit.poke(true.B)
      enqueue(0,0,true,true,false,1,1,0)
      step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
      complete(1,true);step();clearCycle()
      for(_<-0 until 30) {noKilledFlags();step();clearCycle()}
      assert(dut.retiredEntries.peek().litValue==2,"Reused entry failed to retire")
      println("ROB_KILLED_FLAGS_PASS reusedEntry=1")

      for(kind <- Seq("former-trap","latter-trap","flush-pipe")) {
        restart();dut.io.trace.blockCommit.poke(true.B)
        val latter=kind=="latter-trap";val trap=kind!="flush-pipe"
        enqueue(0,if(latter)3 else 0,true,true,false,0,0,0)
        if(latter) enqueue(1,3,false,false,false,0,0,0)
        val fault=dut.io.enq.req(if(latter)1 else 0).bits
        fault.hasException.poke(trap.B);fault.flushPipe.poke((!trap).B)
        if(trap) fault.exceptionVec(ExceptionNO.EX_IPF).poke(true.B)
        fault.ftqPtr.value.poke(3.U);fault.ftqOffset.poke(2.U);fault.isRVC.poke(true.B)
        dut.io.enq.req(0).bits.slotNeedFlushMask.poke((if(latter)2 else 1).U)
        dut.io.enq.req(0).bits.slotHeadRvcMask.poke(3.U)
        step();clearCycle()
        var waited=0
        while(!dut.io.flushOut.valid.peek().litToBoolean && waited<40) {
          step();clearCycle();waited+=1
        }
        assert(dut.io.flushOut.valid.peek().litToBoolean,"Head flush did not occur: "+kind)
        assert(dut.io.readGPAMemAddr.valid.peek().litToBoolean==trap,"Trap GPA request differs")
        assert(dut.io.flushOut.bits.robIdx.isFormer.peek().litToBoolean==(!latter),"Flush slot differs")
        assert(dut.io.flushOut.bits.level.peek().litValue==(if(trap)1 else 0),"Flush cut differs")
        assert(!dut.io.exception.valid.peek().litToBoolean,"Exception was not delayed")
        step();clearCycle()
        dut.io.readGPAMemData.gpaddr.poke(0x81234500L.U)
        dut.io.readGPAMemData.isForVSnonLeafPTE.poke(true.B)
        assert(dut.io.exception.valid.peek().litToBoolean==trap,"Exception/GPA cycle differs")
        if(trap) {
          assert(dut.io.exception.bits.gpaddr.peek().litValue==0x81234500L,"GPA data misaligned")
          assert(dut.io.exception.bits.isFormer.peek().litToBoolean==(!latter),"Trap slot differs")
          assert(!dut.io.exception.bits.isInterrupt.peek().litToBoolean,"Trap misclassified as interrupt")
        }
        dut.io.redirect.valid.poke(true.B)
        pointer(dut.io.redirect.bits.robIdx,false,0,!latter)
        dut.io.redirect.bits.level.poke(trap.B)
        step();drain(70)
        assert(dut.retiredEntries.peek().litValue==(if(latter || !trap)1 else 0),
          "Post-flush retirement count differs: "+kind)
        if(latter && dut.io.diffCommitForTrap.nonEmpty)
          assert(trapReleases==1,"Latter trap observation release was lost or duplicated")
      }
      println("ROB_TRAP_GPA_FLUSH_PASS cases=3")

      restart();dut.io.trace.blockCommit.poke(true.B)
      enqueue(0,0,true,true,false,0,1,0)
      step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
      dut.io.exuWriteback(ports.head).valid.poke(true.B)
      pointer(dut.io.exuWriteback(ports.head).bits.robIdx,false,0,true)
      step();clearCycle();drain(12)
      assert(dut.retiredEntries.peek().litValue==0,"Raw-only WB changed functional completion")
      complete(0,true);step();drain(20)
      assert(dut.retiredEntries.peek().litValue==1,"Functional WB did not complete raw-only scenario")
      println("ROB_RAW_ONLY_PASS")

      val replaySupported=dut.io.writeback.exists(_.bits.replay.nonEmpty)
      for(replay <- Seq(false) ++ (if(replaySupported) Seq(true) else Seq.empty)) {
        restart();dut.io.trace.blockCommit.poke(true.B)
        enqueue(0,0,true,true,false,0,1,0)
        step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
        if(replay) {
          val replayPort=dut.io.writeback.indices.find(i=>dut.io.writeback(i).bits.replay.nonEmpty).get
          complete(0,true,replayPort);dut.io.writeback(replayPort).bits.replay.get.poke(true.B)
          val exceptionPorts=dut.io.writeback.indices.filter(i=>dut.io.writeback(i).bits.params.needExceptionGen)
          dut.io.writebackNeedFlush(exceptionPorts.indexOf(replayPort)).poke(true.B)
        } else {
          dut.io.csr.intrBitSet.poke(true.B)
        }
        step();clearCycle()
        var waited=0
        while(!dut.io.flushOut.valid.peek().litToBoolean && waited<40) {
          step();clearCycle();waited+=1
        }
        assert(dut.io.flushOut.valid.peek().litToBoolean,"Interrupt/replay flush missing")
        assert(dut.io.flushOut.bits.level.peek().litValue==1,"Interrupt/replay did not self-flush")
        assert(dut.io.readGPAMemAddr.valid.peek().litToBoolean==(!replay),"Replay was classified as trap")
        step();clearCycle()
        assert(dut.io.exception.valid.peek().litToBoolean==(!replay),"Interrupt/replay exception differs")
        if(!replay) assert(dut.io.exception.bits.isInterrupt.peek().litToBoolean,"Interrupt type missing")
        dut.io.redirect.valid.poke(true.B);pointer(dut.io.redirect.bits.robIdx,false,0,true)
        dut.io.redirect.bits.level.poke(true.B);step();drain(50)
        assert(dut.retiredEntries.peek().litValue==0,"Self-flushed instruction retired")
      }
      println("ROB_INTERRUPT_PASS")
      println(if(replaySupported) "ROB_REPLAY_PASS" else "ROB_REPLAY_NOT_GENERATED")

      restart();dut.io.trace.blockCommit.poke(true.B)
      enqueue(0,3,true,true,false,0,1,1);enqueue(1,3,false,false,false,0,1,1)
      dut.io.enq.req(0).bits.entryHasStore.poke(true.B)
      dut.io.enq.req(0).bits.commitType.poke(CommitType.STORE)
      dut.io.enq.req(1).bits.commitType.poke(CommitType.STORE)
      step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
      complete(0,true);step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
      var partialNotifications=0
      for(_<-0 until 8) {
        if(dut.io.lsq.scommit.peek().litValue==1) partialNotifications+=1
        step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
      }
      assert(partialNotifications>=3,"CC partial store notification was incorrectly deduplicated")
      complete(0,false);step();clearCycle();drain(25)
      assert(dut.retiredEntries.peek().litValue==1,"CC store entry failed to retire")
      println("ROB_CC_STORE_PASS partial="+partialNotifications)

      restart();dut.io.trace.blockCommit.poke(true.B);dut.io.vstartIsZero.poke(false.B)
      enqueue(0,0,true,true,false,0,1,0);dut.io.enq.req(0).bits.dirtyVs.poke(true.B)
      step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
      complete(0,true);step();clearCycle()
      var sawVstart=false
      for(_<-0 until 30) {
        dut.io.vstartIsZero.poke(false.B)
        if(dut.io.csr.vstart.valid.peek().litToBoolean) {
          assert(dut.io.csr.vstart.bits.peek().litValue==0,"Dirty VS failed to clear vstart")
          sawVstart=true
        }
        step();clearCycle()
      }
      assert(sawVstart,"Dirty VS vstart clear was not observed")
      println("ROB_VSTART_CLEAR_PASS")

      restart();dut.io.trace.blockCommit.poke(true.B)
      enqueue(0,0,true,true,false,0,1,0)
      val vset=dut.io.enq.req(0).bits
      vset.isVset.poke(true.B);vset.vlWen.poke(true.B);vset.pdestVl.poke(3.U)
      vset.fuOpType.poke(VSETOpType.uvsetvcfg_ii)
      vset.vtype.vta.poke(true.B);vset.vtype.vma.poke(true.B)
      vset.vtype.vsew.poke(2.U);vset.vtype.vlmul.poke(1.U)
      step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
      complete(0,true);step();clearCycle()
      var vlMappings=0;var sawVtype=false
      for(_<-0 until 40) {
        if(dut.io.vlCommits.isCommit.peek().litToBoolean) {
          dut.io.vlCommits.commitValid.zip(dut.io.vlCommits.pdestVl).foreach { case(valid,pdest)=>
            if(valid.peek().litToBoolean) {
              assert(pdest.peek().litValue==3,"VL mapping payload differs")
              vlMappings+=1
            }
          }
        }
        if(dut.io.toDecode.commitVType.vtype.valid.peek().litToBoolean) {
          assert(dut.io.toDecode.commitVType.vtype.bits.vsew.peek().litValue==2,"Committed vtype differs")
          assert(dut.io.toDecode.commitVType.vtype.bits.vlmul.peek().litValue==1,"Committed LMUL differs")
          sawVtype=true
        }
        step();clearCycle()
      }
      assert(vlMappings==1 && sawVtype,"VTB retirement effects missing")
      println("ROB_VTYPE_VL_PASS mappings="+vlMappings)

      restart();dut.io.trace.blockCommit.poke(true.B)
      enqueue(0,0,true,true,false,0,1,0)
      dut.io.enq.req(0).bits.commitType.poke(CommitType.LOAD)
      step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
      val history=dut.io.lsTopdownInfo.head
      history.s1.robIdx.poke(0.U);history.s1.vaddr_valid.poke(true.B)
      history.s1.vaddr_bits.poke(0x12345678L.U)
      history.s2.robIdx.poke(0.U);history.s2.paddr_valid.poke(true.B)
      history.s2.paddr_bits.poke(0x81234568L.U)
      dut.io.debugEnqLsq.canAccept.poke(true.B)
      dut.io.debugEnqLsq.needAlloc(0).poke(1.U)
      dut.io.debugEnqLsq.req(0).valid.poke(true.B)
      pointer(dut.io.debugEnqLsq.req(0).bits.uop.robIdx,false,0,true)
      dut.io.debugEnqLsq.req(0).bits.uop.lqIdx.flag.poke(false.B)
      dut.io.debugEnqLsq.req(0).bits.uop.lqIdx.value.poke(7.U)
      dut.io.debugHeadLsIssue.poke(true.B)
      step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
      zeroInputs(history);zeroInputs(dut.io.debugEnqLsq)
      assert(dut.io.debugTopDown.toCore.robHeadVaddr.valid.peek().litToBoolean,"Head virtual history missing")
      assert(dut.io.debugTopDown.toCore.robHeadVaddr.bits.peek().litValue==0x12345678L,"Head virtual history differs")
      assert(dut.io.debugTopDown.toCore.robHeadPaddr.valid.peek().litToBoolean,"Head physical history missing")
      assert(dut.io.debugTopDown.toCore.robHeadPaddr.bits.peek().litValue==0x81234568L,"Head physical history differs")
      assert(dut.io.debugTopDown.robHeadLqIdx.valid.peek().litToBoolean,"Head load queue history missing")
      assert(dut.io.debugTopDown.robHeadLqIdx.bits.value.peek().litValue==7,"Head load queue history differs")
      dut.io.debugHeadLsIssue.poke(false.B)
      assert(!dut.io.debugTopDown.toDispatch.robHeadLsIssue.peek().litToBoolean,"Head issue bypass did not follow current input")
      complete(0,true);step();clearCycle();drain(30)
      assert(dut.retiredEntries.peek().litValue==1,"TopDown history entry failed to retire")
      println("ROB_TOPDOWN_HISTORY_PASS vaddr=0x12345678 paddr=0x81234568 lq=7")

      restart();dut.io.trace.blockCommit.poke(true.B)
      enqueue(0,0,true,true,false,0,0,0)
      val vsetvl=dut.io.enq.req(0).bits
      vsetvl.isVset.poke(true.B);vsetvl.vlWen.poke(true.B);vsetvl.pdestVl.poke(5.U)
      vsetvl.fuType.poke(FuType.vset.U)
      vsetvl.fuOpType.poke(xiangshan.backend.decode.opcode.Opcode.VSetOpcodes.uvset_vtypex_vlx.encode.value.U)
      vsetvl.flushPipe.poke(true.B);vsetvl.slotNeedFlushMask.poke(1.U)
      step();clearCycle()
      var vsetFlushWait=0
      while(!dut.io.flushOut.valid.peek().litToBoolean && vsetFlushWait<40) {
        step();clearCycle();vsetFlushWait+=1
      }
      assert(dut.io.flushOut.valid.peek().litToBoolean,"Vsetvl flush missing")
      assert(dut.io.flushOut.bits.level.peek().litValue==0,"Vsetvl did not flush younger entries")
      step();clearCycle()
      dut.io.redirect.valid.poke(true.B);pointer(dut.io.redirect.bits.robIdx,false,0,true)
      dut.io.redirect.bits.level.poke(false.B)
      step();clearCycle()
      var vsetvlEvents=0;var vsetvlMappings=0
      for(_<-0 until 80) {
        if(dut.io.toDecode.commitVType.hasVsetvl.peek().litToBoolean) {
          assert(dut.io.toDecode.commitVType.vtype.valid.peek().litToBoolean,"VTB vsetvl event lacked a committed entry")
          vsetvlEvents+=1
        }
        if(dut.io.vlCommits.isCommit.peek().litToBoolean) {
          dut.io.vlCommits.commitValid.zip(dut.io.vlCommits.pdestVl).foreach { case(valid,pdest)=>
            if(valid.peek().litToBoolean) {
              assert(pdest.peek().litValue==5,"Vsetvl mapping differs")
              vsetvlMappings+=1
            }
          }
        }
        step();clearCycle()
      }
      assert(vsetvlEvents==1 && vsetvlMappings==1,"Vsetvl event was lost or duplicated")
      assert(dut.retiredEntries.peek().litValue==1,"Vsetvl failed to retire after recovery")
      println("ROB_VSETVL_EVENT_PASS events=1 mappings=1")

      if(p(DebugOptionsKey).EnableDifftest) {
        restart();dut.io.trace.blockCommit.poke(true.B)
        enqueue(0,3,true,true,false,0,1,1);enqueue(1,3,false,false,false,0,1,1)
        dut.io.enq.req(0).bits.debug.get.pc.poke(0x80001000L.U)
        dut.io.enq.req(1).bits.debug.get.pc.poke(0x80001004L.U)
        step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
        dut.io.storeDebugInfo.zipWithIndex.foreach { case(query,index)=>
          pointer(query.robidx,false,0,index==0)
        }
        dut.checkStoreQueries.poke(true.B)
        assert(dut.io.storeDebugInfo(0).pc.peek().litValue==0x80001000L,"Former store PC differs")
        if(dut.io.storeDebugInfo.size>1)
          assert(dut.io.storeDebugInfo(1).pc.peek().litValue==0x80001004L,"Latter store PC differs")
        step();clearCycle();dut.io.trace.blockCommit.poke(true.B)
        complete(0,true);complete(0,false,ports(1));step();clearCycle()
        var queryDrain=0
        while(dut.retiredEntries.peek().litValue==0 && queryDrain<40) {
          step();clearCycle();queryDrain+=1
        }
        assert(dut.retiredEntries.peek().litValue==1,"Store query entry did not retire")
        dut.checkStoreQueries.poke(false.B);drain(10)
        println("ROB_DEBUG_STORE_PASS")
        println("ROB_DEBUG_TRAP_RELEASE_PASS")
      }
    }
  }
}
