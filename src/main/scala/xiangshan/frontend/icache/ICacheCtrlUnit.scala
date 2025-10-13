// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.icache

import annotation.unused
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.diplomacy.SimpleDevice
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.regmapper.RegFieldDesc
import freechips.rocketchip.regmapper.RegReadFn
import freechips.rocketchip.regmapper.RegWriteFn
import freechips.rocketchip.tilelink.TLRegisterNode
import org.chipsalliance.cde.config.Parameters
import utils.EnumUInt

// currently for ECC control only
class ICacheCtrlUnit(implicit p: Parameters) extends LazyModule
    with HasICacheCtrlUnitParameters {
  lazy val module = new ICacheCtrlUnitImp(this)

  // register tilelink node
  val device: SimpleDevice = new SimpleDevice("L1ICacheCtrl", Seq("xiangshan,l1icache_ctrl"))

  val node: TLRegisterNode = TLRegisterNode(
    address = Seq(Address),
    device = device,
    beatBytes = BeatBytes,
    concurrency = 1
  )

  class ICacheCtrlUnitImp(wrapper: LazyModule) extends LazyModuleImp(wrapper)
      with HasICacheCtrlUnitParameters
      with ICacheMetaHelper {
    class ICacheCtrlUnitIO(implicit p: Parameters) extends ICacheBundle {
      // ecc control
      val eccEnable: Bool = Output(Bool())
      // ecc inject
      val injecting: Bool            = Output(Bool())
      val metaRead:  MetaReadBundle  = new MetaReadBundle
      val metaWrite: MetaWriteBundle = new MetaWriteBundle
      val dataWrite: DataWriteBundle = new DataWriteBundle
    }

    val io: ICacheCtrlUnitIO = IO(new ICacheCtrlUnitIO)

    // eccCtrl.ierror: inject error code
    private def nInjError: Int = 8
    private object EccCtrlInjError extends EnumUInt(nInjError) {
      def NotEnabled:    UInt = 0.U(width.W) // try to inject when ECC check is not enabled
      def TargetInvalid: UInt = 1.U(width.W) // try to inject to invalid(rsvd) eccCtrl.itarget
      def NotFound:      UInt = 2.U(width.W) // try to inject to eccIAddr.pAddr does not exist in ICache
      @unused
      def Rsvd3: UInt = 3.U(width.W)
      @unused
      def Rsvd4: UInt = 4.U(width.W)
      @unused
      def Rsvd5: UInt = 5.U(width.W)
      @unused
      def Rsvd6: UInt = 6.U(width.W)
      @unused
      def Rsvd7: UInt = 7.U(width.W)
    }
    // eccCtrl.istatus: inject status
    private def nInjStatus: Int = 8
    private object EccCtrlInjStatus extends EnumUInt(nInjStatus) {
      def Idle:     UInt = 0.U(width.W)
      def Working:  UInt = 1.U(width.W)
      def Injected: UInt = 2.U(width.W)
      def Error:    UInt = 7.U(width.W)
      @unused
      def Rsvd3: UInt = 3.U(width.W)
      @unused
      def Rsvd4: UInt = 4.U(width.W)
      @unused
      def Rsvd5: UInt = 5.U(width.W)
      @unused
      def Rsvd6: UInt = 6.U(width.W)
    }
    // eccCtrl.itarget: inject target
    private def nInjTarget: Int = 4
    private object EccCtrlInjTarget extends EnumUInt(nInjTarget) {
      def MetaArray: UInt = 0.U(width.W)
      def DataArray: UInt = 2.U(width.W)
      @unused
      def Rsvd1: UInt = 1.U(width.W)
      @unused
      def Rsvd3: UInt = 3.U(width.W)
    }
    private class EccCtrlBundle extends Bundle {
      val iError:  UInt = EccCtrlInjError()  // inject error code, read-only, valid only when istatus === error
      val iStatus: UInt = EccCtrlInjStatus() // inject status, read-only
      val iTarget: UInt = EccCtrlInjTarget() // inject target
      val inject:  Bool = Bool()             // request to inject, write-only, read 0
      val enable:  Bool = Bool()             // enable ECC
    }
    private object EccCtrlBundle {
      def default: EccCtrlBundle = {
        val x = Wire(new EccCtrlBundle)
        x.iError  := EccCtrlInjError.NotEnabled
        x.iStatus := EccCtrlInjStatus.Idle
        x.iTarget := EccCtrlInjTarget.MetaArray
        x.inject  := false.B
        x.enable  := true.B
        x
      }
    }

    private class EccIAddrBundle extends Bundle {
      val pAddr: UInt = UInt(PAddrBits.W) // inject position physical address
    }
    private object EccIAddrBundle {
      def default: EccIAddrBundle = {
        val x = Wire(new EccIAddrBundle)
        x.pAddr := 0.U
        x
      }
    }

    private val eccCtrl  = RegInit(EccCtrlBundle.default)
    private val eccIAddr = RegInit(EccIAddrBundle.default)

    // sanity check
    require(RegWidth >= eccCtrl.asUInt.getWidth)
    require(RegWidth >= eccIAddr.asUInt.getWidth)

    // control signal
    io.eccEnable := eccCtrl.enable
    io.injecting := eccCtrl.iStatus === EccCtrlInjStatus.Working

    // inject position
    private val iVSetIdx = get_idx(eccIAddr.pAddr)
    private val iPAddr   = eccIAddr.pAddr
    private val iPTag    = get_phy_tag(iPAddr)
    // read from metaArray, valid after iState === InjectFsmState.readMetaResp
    private val iWaymask = RegInit(0.U(nWays.W))

    // inject FSM
    private def nInjectFsmState: Int = 5
    private object InjectFsmState extends EnumUInt(nInjectFsmState) {
      // scala ask identifier that begins with uppercase cannot be used in pattern matching like `X :: Nil = Enum()`
      // but we want UpperCamelCase for constants for better readability, so we dont use Enum() here
      def Idle:         UInt = 0.U(width.W)
      def ReadMetaReq:  UInt = 1.U(width.W)
      def ReadMetaResp: UInt = 2.U(width.W)
      def WriteMeta:    UInt = 3.U(width.W)
      def WriteData:    UInt = 4.U(width.W)
    }
    private val iState = RegInit(InjectFsmState.Idle)

    io.metaRead.req.valid             := iState === InjectFsmState.ReadMetaReq
    io.metaRead.req.bits.isDoubleLine := false.B // we inject into first cacheline and ignore the rest port
    io.metaRead.req.bits.vSetIdx      := VecInit(Seq.fill(PortNumber)(iVSetIdx))

    io.metaWrite.req.valid := iState === InjectFsmState.WriteMeta
    io.metaWrite.req.bits.generate(
      phyTag = get_phy_tag(iPAddr),
      maybeRvcMap = 0.U,
      vSetIdx = iVSetIdx,
      waymask = iWaymask,
      bankIdx = iVSetIdx(0),
      poison = true.B
    )

    io.dataWrite.req.valid := iState === InjectFsmState.WriteData
    io.dataWrite.req.bits.generate(
      data = 0.U, // inject poisoned data, don't care actual data
      vSetIdx = iVSetIdx,
      waymask = iWaymask,
      bankIdx = iVSetIdx(0),
      poison = true.B
    )

    switch(iState) {
      is(InjectFsmState.Idle) {
        when(eccCtrl.iStatus === EccCtrlInjStatus.Working) {
          // we need to read meta first to get waymask, whether itarget is metaArray or dataArray
          iState := InjectFsmState.ReadMetaReq
        }
      }
      is(InjectFsmState.ReadMetaReq) {
        when(io.metaRead.req.fire) {
          iState := InjectFsmState.ReadMetaResp
        }
      }
      is(InjectFsmState.ReadMetaResp) {
        // metaArray ensures resp is valid one cycle after req
        val waymask = getWaymask(iPTag, io.metaRead.resp.tags.head, io.metaRead.resp.entryValid.head)
        iWaymask := waymask
        when(!waymask.orR) {
          // not hit, refuse to inject
          iState          := InjectFsmState.Idle
          eccCtrl.iStatus := EccCtrlInjStatus.Error
          eccCtrl.iError  := EccCtrlInjError.NotFound
        }.otherwise {
          iState := Mux(
            eccCtrl.iTarget === EccCtrlInjTarget.MetaArray,
            InjectFsmState.WriteMeta,
            InjectFsmState.WriteData
          )
        }
      }
      is(InjectFsmState.WriteMeta) {
        when(io.metaWrite.req.fire) {
          iState          := InjectFsmState.Idle
          eccCtrl.iStatus := EccCtrlInjStatus.Injected
        }
      }
      is(InjectFsmState.WriteData) {
        when(io.dataWrite.req.fire) {
          iState          := InjectFsmState.Idle
          eccCtrl.iStatus := EccCtrlInjStatus.Injected
        }
      }
    }

    private def eccctrlRegDesc: RegFieldDesc =
      RegFieldDesc(
        name = s"ecc_control",
        desc = s"ECC control",
        group = Option(s"ecc_control"),
        groupDesc = Option(s"ECC Control"),
        reset = Option(0)
      )

    private def ecciaddrRegDesc: RegFieldDesc =
      RegFieldDesc(
        name = s"ecc_iaddr",
        desc = s"ECC Inject Address",
        group = Option(s"ecc_iaddr"),
        groupDesc = Option(s"ECC Inject Address"),
        reset = Option(0)
      )

    private def eccctrlRegField(x: EccCtrlBundle): RegField =
      RegField(
        RegWidth,
        RegReadFn { ready =>
          val res = WireInit(x)
          res.inject := false.B // read always 0
          when(ready) {
            // if istatus is injected or error, clear it after read
            when(x.iStatus === EccCtrlInjStatus.Injected || x.iStatus === EccCtrlInjStatus.Error) {
              x.iStatus := EccCtrlInjStatus.Idle
              x.iError  := EccCtrlInjError.NotEnabled
            }
          }
          // always read valid
          (true.B, res.asUInt)
        },
        RegWriteFn { (valid, data) =>
          when(valid) {
            val req = data.asTypeOf(new EccCtrlBundle)
            x.enable := req.enable
            when(req.inject && x.iStatus === EccCtrlInjStatus.Idle) {
              // if istatus is not idle, ignore the inject request
              when(req.enable === false.B) {
                // check if enable is not valid
                x.iStatus := EccCtrlInjStatus.Error
                x.iError  := EccCtrlInjError.NotEnabled
              }.elsewhen(req.iTarget =/= EccCtrlInjTarget.MetaArray && req.iTarget =/= EccCtrlInjTarget.DataArray) {
                // check if itarget is not valid
                x.iStatus := EccCtrlInjStatus.Error
                x.iError  := EccCtrlInjError.TargetInvalid
              }.otherwise {
                x.iStatus := EccCtrlInjStatus.Working
              }
            }
            x.iTarget := req.iTarget
            // istatus is read-only, ignore req.istatus
            // ierror is read-only, ignore req.ierror
          }
          // always ready to write
          true.B
        },
        eccctrlRegDesc
      )

    private def ecciaddrRegField(x: EccIAddrBundle): RegField =
      RegField(RegWidth, x.asUInt, ecciaddrRegDesc)

    node.regmap(
      EccCtrlOffset  -> Seq(eccctrlRegField(eccCtrl)),
      EccIAddrOffset -> Seq(ecciaddrRegField(eccIAddr))
    )
  }
}
