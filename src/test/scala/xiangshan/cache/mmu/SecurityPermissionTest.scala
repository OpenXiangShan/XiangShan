package xiangshan.cache.mmu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._
import xiangshan.backend.fu.{PMPCheckMethod, PMPConfig, PMPEntry, PMPRespBundle}

private class PmpSecurityProbe(implicit val p: Parameters) extends Module with PMPCheckMethod {
  val io = IO(new Bundle {
    val cmd = Input(TlbCmd())
    val read = Input(Bool())
    val write = Input(Bool())
    val execute = Input(Bool())
    val shadow = Input(Bool())
    val mml = Input(Bool())
    val mmwp = Input(Bool())
    val faults = Output(new PMPRespBundle)
    val unmatchedExecute = Output(Bool())
    val unmatchedRead = Output(Bool())
  })
  val cfg = WireInit(0.U.asTypeOf(new PMPConfig))
  cfg.r := io.read
  cfg.w := io.write
  cfg.x := io.execute
  io.faults := pmp_check(io.cmd, cfg, Some(io.shadow))
  val entries = WireInit(0.U.asTypeOf(Vec(NumPMPReal, new PMPEntry)))
  val unmatched = pmp_match_res()(0x80000000L.U(PMPAddrBits.W), 3.U, entries,
    3.U, false.B, 3, io.mml, io.mmwp, io.cmd)
  io.unmatchedExecute := unmatched.cfg.x
  io.unmatchedRead := unmatched.cfg.r
}

class SecurityPermissionTest extends AnyFlatSpec with ChiselSim {
  import chisel3.simulator.HasSimulator.simulators.verilator

  private def config(enabled: Boolean): Parameters = {
    val base = top.ArgParser.parse(Array.empty[String])._1
    base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasShadowStack = enabled, HasEpmp = enabled)
    }
  }

  private def zero(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(zero)
    case vector: Vec[_] => vector.foreach(zero)
    case value: Bool => value.poke(false.B)
    case value: UInt => value.poke(0.U)
    case value: SInt => value.poke(0.S)
  }

  private def initialize(dut: TlbAccessCheck): Unit = {
    zero(dut.io.in)
    dut.io.in.info.mode.poke(1.U)
    dut.io.in.info.translateEnable.poke(true.B)
    dut.io.in.info.sse.foreach(_.poke(true.B))
    dut.io.in.perm.v.get.poke(true.B)
    dut.io.in.perm.a.poke(true.B)
    dut.io.in.perm.d.poke(true.B)
    dut.io.in.gPerm.a.poke(true.B)
    dut.io.in.gPerm.d.poke(true.B)
    dut.io.in.gPerm.r.poke(true.B)
    dut.io.in.gPerm.w.poke(true.B)
  }

  behavior of "Security permission integration"

  it should "preserve shadow-stack fault types and privilege-check priority" in {
    simulate(new TlbAccessCheck()(config(true))) { dut =>
      initialize(dut)
      dut.io.in.info.isShadowStack.get.poke(true.B)
      dut.io.in.perm.w.poke(true.B)
      dut.io.out.excp.pf.st.expect(false.B)
      dut.io.out.excp.af.st.expect(false.B)
      dut.io.out.excp.pf.ld.expect(false.B)

      dut.io.in.info.sse.get.poke(false.B)
      dut.io.out.excp.pf.st.expect(true.B)
      dut.io.out.excp.af.st.expect(false.B)
      dut.io.in.info.sse.get.poke(true.B)

      dut.io.in.perm.r.poke(true.B)
      dut.io.in.perm.w.poke(false.B)
      dut.io.out.excp.pf.st.expect(true.B)
      dut.io.out.excp.af.st.expect(false.B)

      dut.io.in.perm.w.poke(true.B)
      dut.io.out.excp.af.st.expect(true.B)
      dut.io.out.excp.pf.st.expect(false.B)
      dut.io.in.perm.u.poke(true.B) // SUM=0 must fault before classifying the page.
      dut.io.out.excp.pf.st.expect(true.B)
      dut.io.out.excp.af.st.expect(false.B)

      dut.io.in.perm.u.poke(false.B)
      dut.io.in.perm.v.get.poke(false.B)
      dut.io.in.perm.pf.poke(true.B)
      dut.io.out.excp.pf.st.expect(true.B)
      dut.io.out.excp.af.st.expect(false.B)
      dut.io.out.excp.pf.ld.expect(false.B)

      dut.io.in.info.translateEnable.poke(false.B)
      dut.io.out.excp.af.st.expect(true.B)
      dut.io.out.excp.af.ld.expect(false.B)
    }
  }

  it should "reject CBO and ordinary stores to shadow-stack pages and require G-stage read-write permission" in {
    simulate(new TlbAccessCheck()(config(true))) { dut =>
      initialize(dut)
      dut.io.in.perm.w.poke(true.B)
      dut.io.out.excp.pf.ld.expect(false.B)
      dut.io.out.excp.af.ld.expect(false.B)
      dut.io.in.info.isCbo.get.poke(true.B)
      dut.io.out.excp.af.st.expect(true.B)
      dut.io.in.info.isCbo.get.poke(false.B)
      dut.io.in.info.cmd.poke(TlbCmd.write)
      dut.io.out.excp.af.st.expect(true.B)
      dut.io.in.info.cmd.poke(TlbCmd.exec)
      dut.io.out.excp.af.instr.expect(true.B)

      dut.io.in.info.cmd.poke(TlbCmd.read)
      dut.io.in.info.isShadowStack.get.poke(true.B)
      dut.io.in.info.s2xlate.poke(3.U)
      dut.io.in.gPerm.w.poke(false.B)
      dut.io.out.excp.gpf.st.expect(true.B)
      dut.io.out.excp.gpf.ld.expect(false.B)
      dut.io.in.gPerm.w.poke(true.B)
      dut.io.in.gPerm.r.poke(false.B)
      dut.io.out.excp.gpf.st.expect(true.B)
      dut.io.in.gPerm.r.poke(true.B)
      dut.io.out.excp.gpf.st.expect(false.B)
    }
  }

  it should "retain HLVX execute permission and normal behavior without security ports" in {
    simulate(new TlbAccessCheck()(config(false))) { dut =>
      initialize(dut)
      dut.io.in.info.hlvx.poke(true.B)
      dut.io.in.perm.r.poke(true.B)
      dut.io.out.excp.pf.ld.expect(true.B)
      dut.io.in.perm.x.poke(true.B)
      dut.io.out.excp.pf.ld.expect(false.B)
    }
  }

  for (enabled <- Seq(false, true)) {
    it should s"lower TLB request plumbing with security enabled=$enabled" in {
      val rtl = circt.stage.ChiselStage.emitSystemVerilog(
        new TLB(1, 1, TLBParameters(name = "security-test"), hasCboAccess = enabled)(config(enabled))
      )
      assert(rtl.contains("module TLB"))
    }

    it should s"combine HLVX and shadow-stack PMP checks with Smepmp enabled=$enabled" in {
      simulate(new PmpSecurityProbe()(config(enabled))) { dut =>
        dut.io.cmd.poke(TlbCmd.read_exec)
        dut.io.read.poke(true.B)
        dut.io.write.poke(false.B)
        dut.io.execute.poke(false.B)
        dut.io.shadow.poke(false.B)
        dut.io.mml.poke(false.B)
        dut.io.mmwp.poke(false.B)
        dut.io.faults.ld.expect(false.B)
        dut.io.faults.instr.expect(true.B)
        dut.io.unmatchedExecute.expect(true.B)
        dut.io.mml.poke(true.B)
        dut.io.unmatchedExecute.expect((!enabled).B)
        dut.io.cmd.poke(TlbCmd.read)
        dut.io.unmatchedRead.expect(true.B)
        dut.io.mmwp.poke(true.B)
        dut.io.unmatchedRead.expect((!enabled).B)
        dut.io.shadow.poke(true.B)
        dut.io.faults.ld.expect(true.B)
        dut.io.write.poke(true.B)
        dut.io.faults.ld.expect(false.B)
      }
    }
  }
}
