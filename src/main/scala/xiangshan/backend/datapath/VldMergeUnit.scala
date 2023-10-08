package xiangshan.backend.datapath

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.Bundles.ExuOutput
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.vector.Mgu

class VldMergeUnit(val params: ExeUnitParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new VldMergeUnitIO(params))

  io.writeback.ready := io.writebackAfterMerge.ready
  io.oldVdReadAddr := 0.U //io.writeback.bits.oldVdPdest    todo add this in ExuOutput
  val wbReg = Reg(Valid(new ExuOutput(params)))
  val mgu = Module(new Mgu(VLEN))
  val vdAfterMerge = Wire(UInt(VLEN.W))

  wbReg.bits := io.writeback.bits
  wbReg.valid := io.writeback.bits.robIdx.needFlush(io.flush) && io.writeback.valid
  mgu.io.in.vd := wbReg.bits.data
  mgu.io.in.oldVd := io.oldVdReadData
  mgu.io.in.mask := 0.U // wbReg.bits.mask   todo add this in ExuOutput
  mgu.io.in.info := 0.U.asTypeOf(mgu.io.in.info) // todo add this in ExuOutput
  vdAfterMerge := mgu.io.out.vd

  io.writebackAfterMerge.valid := wbReg.valid
  io.writebackAfterMerge.bits := wbReg.bits
  io.writebackAfterMerge.bits.data := vdAfterMerge

}

class VldMergeUnitIO(val params: ExeUnitParams)(implicit p: Parameters) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect))
  val writeback = Flipped(DecoupledIO(new ExuOutput(params)))
  val oldVdReadData = Input(UInt(VLEN.W))
  val oldVdReadAddr = Output(UInt(PhyRegIdxWidth.W))
  val writebackAfterMerge = DecoupledIO(new ExuOutput(params))
}