package xiangshan.backend.ctrlblock

import chipsalliance.rocketchip.config.Parameters
import chisel3.util.ValidIO
import chisel3._
import xiangshan.{CustomCSRCtrlIO, MemPredUpdateReq, Redirect, XSBundle, XSModule}
import xiangshan.mem.mdp.{DispatchLFSTIO, LFST, SSIT, SSITEntry, WaitTable}
import xiangshan.v2backend.BackendParams
import xiangshan.v2backend.Bundles.DynInst

class MemCtrl(params: BackendParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new MemCtrlIO(params))

  private val ssit = Module(new SSIT)
  private val waittable = Module(new WaitTable)
  private val lfst = Module(new LFST)
  ssit.io.update <> RegNext(io.memPredUpdate)
  waittable.io.update <> RegNext(io.memPredUpdate)
  ssit.io.csrCtrl := RegNext(io.csrCtrl)
  waittable.io.csrCtrl := RegNext(io.csrCtrl)

  for (i <- 0 until RenameWidth) {
    ssit.io.raddr(i) := io.mdpFlodPcVec(i)
    waittable.io.raddr(i) := io.mdpFlodPcVec(i)
  }
  lfst.io.redirect <> RegNext(io.redirect)
  lfst.io.storeIssue <> RegNext(io.stIn)
  lfst.io.csrCtrl <> RegNext(io.csrCtrl)
  lfst.io.dispatch <> io.dispatchLFSTio

  io.waitTable2Rename := waittable.io.rdata
  io.ssit2Rename := ssit.io.rdata
}

class MemCtrlIO(params: BackendParams)(implicit p: Parameters) extends XSBundle {
  val redirect = Flipped(ValidIO(new Redirect))
  val csrCtrl = Input(new CustomCSRCtrlIO)
  val stIn = Vec(params.StaCnt, Flipped(ValidIO(new DynInst))) // use storeSetHit, ssid, robIdx
  val memPredUpdate = Input(new MemPredUpdateReq)
  val mdpFlodPcVec = Input(Vec(DecodeWidth, UInt(MemPredPCWidth.W)))
  val dispatchLFSTio = Flipped(new DispatchLFSTIO)
  val waitTable2Rename = Vec(DecodeWidth, Output(Bool()))   // loadWaitBit
  val ssit2Rename = Vec(RenameWidth, Output(new SSITEntry)) // ssit read result
}
