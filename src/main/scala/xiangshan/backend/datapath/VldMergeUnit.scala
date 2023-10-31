package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.Bundles.{ExuOutput, MemExuOutput}
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.vector.{ByteMaskTailGen, Mgu, VecInfo}
import yunsuan.vector.SewOH

class VldMergeUnit(val params: ExeUnitParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new VldMergeUnitIO(params))

  io.writeback.ready := io.writebackAfterMerge.ready
  io.oldVdReadAddr := io.writeback.bits.vls.get.oldVdPsrc
  val wbReg = Reg(Valid(new ExuOutput(params)))
  val mgu = Module(new Mgu(VLEN))
  val vdAfterMerge = Wire(UInt(VLEN.W))

  val wbFire = !io.writeback.bits.robIdx.needFlush(io.flush) && io.writeback.fire
  wbReg.bits := Mux(wbFire, io.writeback.bits, wbReg.bits)
  wbReg.valid := wbFire
  mgu.io.in.vd := wbReg.bits.data
  mgu.io.in.oldVd := io.oldVdReadData
  mgu.io.in.mask := io.writeback.bits.vls.get.vpu.vmask
  mgu.io.in.info.valid := io.writeback.valid
  mgu.io.in.info.ta := io.writeback.bits.vls.get.vpu.vta
  mgu.io.in.info.ma := io.writeback.bits.vls.get.vpu.vma
  mgu.io.in.info.vl := io.writeback.bits.vls.get.vpu.vl
  mgu.io.in.info.vstart := io.writeback.bits.vls.get.vpu.vstart
  mgu.io.in.info.eew := io.writeback.bits.vls.get.vpu.veew
  mgu.io.in.info.vsew := io.writeback.bits.vls.get.vpu.vsew
  mgu.io.in.info.vdIdx := io.writeback.bits.vls.get.vdIdx
  mgu.io.in.info.vlmul := io.writeback.bits.vls.get.vpu.vlmul
  mgu.io.in.info.narrow := false.B  // never narrow
  mgu.io.in.info.dstMask := false.B // vlm need not mask

  vdAfterMerge := mgu.io.out.vd

  io.writebackAfterMerge.valid := wbReg.valid
  io.writebackAfterMerge.bits := wbReg.bits
}

class VldMergeUnitIO(param: ExeUnitParams)(implicit p: Parameters) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect))
  val writeback = Flipped(DecoupledIO(new ExuOutput(param)))
  val oldVdReadData = Input(UInt(VLEN.W))
  val oldVdReadAddr = Output(UInt(PhyRegIdxWidth.W))
  val writebackAfterMerge = DecoupledIO(new ExuOutput(param))
}