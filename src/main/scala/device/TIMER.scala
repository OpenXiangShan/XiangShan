/***************************************************************************************
 * Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
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

package device

import chisel3._
import chisel3.util._
import chisel3.util.ShiftRegister
import chisel3.util.ValidIO
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Field
import org.chipsalliance.cde.config.Parameters

object TIMERConsts {
  def msipOffset(hart:    Int) = hart * msipBytes
  def timecmpOffset(hart: Int) = 0x4000 + hart * timecmpBytes
  def timeOffset   = 0xbff8
  def msipBytes    = 4
  def timecmpBytes = 8
  def size         = 0x10000
  def timeWidth    = 64
  def ipiWidth     = 32
  def ints         = 2
}

case class TIMERParams(IsSelfTest: Boolean = false, baseAddress: BigInt = 0x02000000, intStages: Int = 0) {
  def address = AddressSet(baseAddress, TIMERConsts.size - 1)
}

case object CLINTKey extends Field[Option[TIMERParams]](None)

case class CLINTAttachParams(
    slaveWhere: TLBusWrapperLocation = CBUS
)

case object CLINTAttachKey extends Field(CLINTAttachParams())

class TIMER(params: TIMERParams, beatBytes: Int)(implicit p: Parameters) extends LazyModule {
  import TIMERConsts._

  // clint0 => at most 4095 devices
  val device = new SimpleDevice("clint", Seq("riscv,clint0")) {
    override val alwaysExtended = true
  }

  val node: TLRegisterNode = TLRegisterNode(
    address = Seq(params.address),
    device = device,
    beatBytes = beatBytes
  )

  val intnode: IntNexusNode = IntNexusNode(
    sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(ints, Seq(Resource(device, "int"))))) },
    sinkFn = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    outputRequiresInput = false
  )

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    Annotated.params(this, params)
    require(intnode.edges.in.size == 0, "CLINT only produces interrupts; it does not accept them")

    val io = IO(new Bundle {
      val time   = Input(ValidIO(UInt(timeWidth.W)))
      val hartId = Input(UInt(p(MaxHartIdBits).W))
    })

    val time = RegInit(0.U(timeWidth.W))
    when(io.time.valid)(time := io.time.bits)

//    val nTiles = intnode.out.size
    val NumsHart = 1 << p(MaxHartIdBits)
    val nTiles   = NumsHart // def the max hart satisfing kmhv2 spec
    val timecmp  = Seq.fill(nTiles)(RegInit((BigInt(2).pow(timeWidth) - 1).asUInt(timeWidth.W)))
    val ipi      = Seq.fill(nTiles)(RegInit(0.U(1.W)))

    // only get interrupt from the current hart
    val (intnode_out, _) = intnode.out.unzip

    intnode_out.zipWithIndex.foreach { case (int, i) =>
      if (params.IsSelfTest) {
        int(0) := ShiftRegister(ipi(i)(0), params.intStages)                        // msip
        int(1) := ShiftRegister(time.asUInt >= timecmp(i).asUInt, params.intStages) // mtip
      } else {
        int(0) := ShiftRegister(ipi(io.hartId)(0), params.intStages)                        // msip
        int(1) := ShiftRegister(time.asUInt >= timecmp(io.hartId).asUInt, params.intStages) // mtip
      }
    }

    // io.time.valid := RegNext(io.rtcTick)
    // io.time.bits := time

    /* 0000 msip hart 0
     * 0004 msip hart 1
     * 4000 mtimecmp hart 0 lo
     * 4004 mtimecmp hart 0 hi
     * 4008 mtimecmp hart 1 lo
     * 400c mtimecmp hart 1 hi
     * bff8 mtime lo
     * bffc mtime hi
     */
    node.regmap(
      0 -> RegFieldGroup(
        "msip",
        Some("MSIP Bits"),
        ipi.zipWithIndex.flatMap { case (r, i) =>
          RegField(1, r, RegFieldDesc(s"msip_$i", s"MSIP bit for Hart $i", reset = Some(0))) :: RegField(
            ipiWidth - 1
          ) :: Nil
        }
      ),
      timecmpOffset(0) -> timecmp.zipWithIndex.flatMap { case (t, i) =>
        RegFieldGroup(
          s"mtimecmp_$i",
          Some(s"MTIMECMP for hart $i"),
          RegField.bytes(t, Some(RegFieldDesc(s"mtimecmp_$i", "", reset = None)))
        )
      },
      timeOffset -> Seq(RegField.r(64, time, RegFieldDesc("mtime", "", volatile = true)))
    )
  }
}

/** Trait that will connect a CLINT to a subsystem */
trait CanHavePeripheryCLINT { this: BaseSubsystem =>
  val clintOpt = p(CLINTKey).map { params =>
    val tlbus = locateTLBusWrapper(p(CLINTAttachKey).slaveWhere)
    val clint = LazyModule(new TIMER(params, cbus.beatBytes))
    clint.node := tlbus.coupleTo("clint")(TLFragmenter(tlbus) := _)

    // Override the implicit clock and reset -- could instead include a clockNode in the clint, and make it a RawModuleImp?
    InModuleBody {
      clint.module.clock := tlbus.module.clock
      clint.module.reset := tlbus.module.reset
    }
    clint

  }
}
