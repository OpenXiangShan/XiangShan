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
***************************************************************************************/

package xiangshan.frontend.icache

import annotation.unused
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import utils._

case class L1ICacheCtrlParams(
    address:   AddressSet,
    regWidth:  Int,
    beatBytes: Int = 8
) {
  def regBytes: Int = regWidth / 8

  def eccctrlOffset:  Int = 0
  def ecciaddrOffset: Int = eccctrlOffset + regBytes
}

class ICacheCtrlUnitIO(implicit p: Parameters) extends ICacheBundle {
  // ecc control
  val ecc_enable: Bool = Output(Bool())
  // ecc inject
  val injecting:    Bool                               = Output(Bool())
  val metaRead:     DecoupledIO[ICacheReadBundle]      = DecoupledIO(new ICacheReadBundle)
  val metaReadResp: ICacheMetaRespBundle               = Input(new ICacheMetaRespBundle)
  val metaWrite:    DecoupledIO[ICacheMetaWriteBundle] = DecoupledIO(new ICacheMetaWriteBundle)
  val dataWrite:    DecoupledIO[ICacheDataWriteBundle] = DecoupledIO(new ICacheDataWriteBundle)
}

// currently for ECC control only
class ICacheCtrlUnit(params: L1ICacheCtrlParams)(implicit p: Parameters) extends LazyModule {
  lazy val module = new ICacheCtrlUnitImp(this)

  // register tilelink node
  val device: SimpleDevice = new SimpleDevice("L1ICacheCtrl", Seq("xiangshan,l1icache_ctrl"))

  val node: TLRegisterNode = TLRegisterNode(
    address = Seq(params.address),
    device = device,
    beatBytes = params.beatBytes,
    concurrency = 1
  )

  class ICacheCtrlUnitImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) with HasICacheParameters {
    val io: ICacheCtrlUnitIO = IO(new ICacheCtrlUnitIO)

    // eccctrl.ierror: inject error code
    private def nInjError: Int = 8
    private object eccctrlInjError extends NamedUInt(log2Up(nInjError)) {
      def notEnabled:    UInt = 0.U(width.W) // try to inject when ECC check is not enabled
      def targetInvalid: UInt = 1.U(width.W) // try to inject to invalid(rsvd) eccctrl.itarget
      def notFound:      UInt = 2.U(width.W) // try to inject to ecciaddr.paddr does not exist in ICache
      @unused
      def rsvd3: UInt = 3.U(width.W)
      @unused
      def rsvd4: UInt = 4.U(width.W)
      @unused
      def rsvd5: UInt = 5.U(width.W)
      @unused
      def rsvd6: UInt = 6.U(width.W)
      @unused
      def rsvd7: UInt = 7.U(width.W)
    }
    // eccctrl.istatus: inject status
    private def nInjStatus: Int = 8
    private object eccctrlInjStatus extends NamedUInt(log2Up(nInjStatus)) {
      def idle:     UInt = 0.U(width.W)
      def working:  UInt = 1.U(width.W)
      def injected: UInt = 2.U(width.W)
      def error:    UInt = 7.U(width.W)
      @unused
      def rsvd3: UInt = 3.U(width.W)
      @unused
      def rsvd4: UInt = 4.U(width.W)
      @unused
      def rsvd5: UInt = 5.U(width.W)
      @unused
      def rsvd6: UInt = 6.U(width.W)
    }
    // eccctrl.itarget: inject target
    private def nInjTarget: Int = 4
    private object eccctrlInjTarget extends NamedUInt(log2Up(nInjTarget)) {
      def metaArray: UInt = 0.U(width.W)
      def dataArray: UInt = 2.U(width.W)
      @unused
      def rsvd1: UInt = 1.U(width.W)
      @unused
      def rsvd3: UInt = 3.U(width.W)
    }
    private class eccctrlBundle extends Bundle {
      val ierror:  UInt = eccctrlInjError()  // inject error code, read-only, valid only when istatus === error
      val istatus: UInt = eccctrlInjStatus() // inject status, read-only
      val itarget: UInt = eccctrlInjTarget() // inject target
      val inject:  Bool = Bool()             // request to inject, write-only, read 0
      val enable:  Bool = Bool()             // enable ECC
    }
    private object eccctrlBundle {
      def default: eccctrlBundle = {
        val x = Wire(new eccctrlBundle)
        x.ierror  := eccctrlInjError.notEnabled
        x.istatus := eccctrlInjStatus.idle
        x.itarget := eccctrlInjTarget.metaArray
        x.inject  := false.B
        x.enable  := true.B
        x
      }
    }

    private class ecciaddrBundle extends Bundle {
      val paddr: UInt = UInt(PAddrBits.W) // inject position physical address
    }
    private object ecciaddrBundle {
      def default: ecciaddrBundle = {
        val x = Wire(new ecciaddrBundle)
        x.paddr := 0.U
        x
      }
    }

    private val eccctrl  = RegInit(eccctrlBundle.default)
    private val ecciaddr = RegInit(ecciaddrBundle.default)

    // sanity check
    require(params.regWidth >= eccctrl.asUInt.getWidth)
    require(params.regWidth >= ecciaddr.asUInt.getWidth)

    // control signal
    io.ecc_enable := eccctrl.enable
    io.injecting  := eccctrl.istatus === eccctrlInjStatus.working

    // inject position
    private val ivirIdx  = get_idx(ecciaddr.paddr)
    private val iphyTag  = get_tag(ecciaddr.paddr)
    private val iwaymask = RegInit(0.U(nWays.W)) // read from metaArray, valid after istate === is_readMetaResp

    // inject FSM
    private val is_idle :: is_readMetaReq :: is_readMetaResp :: is_writeMeta :: is_writeData :: Nil =
      Enum(5)
    private val istate = RegInit(is_idle)

    io.metaRead.valid             := istate === is_readMetaReq
    io.metaRead.bits.isDoubleLine := false.B // we inject into first cacheline and ignore the rest port
    io.metaRead.bits.vSetIdx      := VecInit(Seq.fill(PortNumber)(ivirIdx))
    io.metaRead.bits.waymask   := VecInit(Seq.fill(PortNumber)(VecInit(Seq.fill(nWays)(false.B)))) // dontcare
    io.metaRead.bits.blkOffset := 0.U(blockBits.W)                                                 // dontcare

    io.metaWrite.valid := istate === is_writeMeta
    io.metaWrite.bits.generate(
      tag = iphyTag,
      idx = ivirIdx,
      waymask = iwaymask,
      bankIdx = ivirIdx(0),
      poison = true.B
    )

    io.dataWrite.valid := istate === is_writeData
    io.dataWrite.bits.generate(
      data = 0.U, // inject poisoned data, don't care actual data
      idx = ivirIdx,
      waymask = iwaymask,
      bankIdx = ivirIdx(0),
      poison = true.B
    )

    switch(istate) {
      is(is_idle) {
        when(eccctrl.istatus === eccctrlInjStatus.working) {
          // we need to read meta first to get waymask, whether itarget is metaArray or dataArray
          istate := is_readMetaReq
        }
      }
      is(is_readMetaReq) {
        when(io.metaRead.fire) {
          istate := is_readMetaResp
        }
      }
      is(is_readMetaResp) {
        // metaArray ensures resp is valid one cycle after req
        val waymask = VecInit((0 until nWays).map { w =>
          io.metaReadResp.entryValid.head(w) && io.metaReadResp.tags.head(w) === iphyTag
        }).asUInt
        iwaymask := waymask
        when(!waymask.orR) {
          // not hit, refuse to inject
          istate          := is_idle
          eccctrl.istatus := eccctrlInjStatus.error
          eccctrl.ierror  := eccctrlInjError.notFound
        }.otherwise {
          istate := Mux(eccctrl.itarget === eccctrlInjTarget.metaArray, is_writeMeta, is_writeData)
        }
      }
      is(is_writeMeta) {
        when(io.metaWrite.fire) {
          istate          := is_idle
          eccctrl.istatus := eccctrlInjStatus.injected
        }
      }
      is(is_writeData) {
        when(io.dataWrite.fire) {
          istate          := is_idle
          eccctrl.istatus := eccctrlInjStatus.injected
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

    private def eccctrlRegField(x: eccctrlBundle): RegField =
      RegField(
        params.regWidth,
        RegReadFn { ready =>
          val res = WireInit(x)
          res.inject := false.B // read always 0
          when(ready) {
            // if istatus is injected or error, clear it after read
            when(x.istatus === eccctrlInjStatus.injected || x.istatus === eccctrlInjStatus.error) {
              x.istatus := eccctrlInjStatus.idle
              x.ierror  := eccctrlInjError.notEnabled
            }
          }
          // always read valid
          (true.B, res.asUInt)
        },
        RegWriteFn { (valid, data) =>
          when(valid) {
            val req = data.asTypeOf(new eccctrlBundle)
            x.enable := req.enable
            when(req.inject && x.istatus === eccctrlInjStatus.idle) {
              // if istatus is not idle, ignore the inject request
              when(req.enable === false.B) {
                // check if enable is not valid
                x.istatus := eccctrlInjStatus.error
                x.ierror  := eccctrlInjError.notEnabled
              }.elsewhen(req.itarget =/= eccctrlInjTarget.metaArray && req.itarget =/= eccctrlInjTarget.dataArray) {
                // check if itarget is not valid
                x.istatus := eccctrlInjStatus.error
                x.ierror  := eccctrlInjError.targetInvalid
              }.otherwise {
                x.istatus := eccctrlInjStatus.working
              }
            }
            x.itarget := req.itarget
            // istatus is read-only, ignore req.istatus
            // ierror is read-only, ignore req.ierror
          }
          // always ready to write
          true.B
        },
        eccctrlRegDesc
      )

    private def ecciaddrRegField(x: ecciaddrBundle): RegField =
      RegField(params.regWidth, x.asUInt, ecciaddrRegDesc)

    node.regmap(
      params.eccctrlOffset  -> Seq(eccctrlRegField(eccctrl)),
      params.ecciaddrOffset -> Seq(ecciaddrRegField(ecciaddr))
    )
  }
}
