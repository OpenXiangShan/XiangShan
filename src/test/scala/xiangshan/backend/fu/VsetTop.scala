package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.wrapper.{VSetRiWi, VSetRiWvf, VSetRvfWvf}
import xiangshan._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.fu.vector.Bundles.{VConfig, VType, Vl}

class VsetTop(implicit p: Parameters) extends XSModule {

  val io = IO(new XSBundle{
    val in = new XSBundle {
      val avl = Input(UInt(XLEN.W))
      val vtype = Input(VType())
      val func  = Input(FuOpType())
      val oldVl = Input(UInt(8.W))
    }

    val out = Output(new XSBundle {
      val vconfig = VConfig()
    })
    val debug = new XSBundle() {
      val fuOpType0 = Output(FuOpType())
      val src0      = Output(Vec(2, UInt(64.W)))
      val fuOpType1 = Output(FuOpType())
      val src1      = Output(Vec(2, UInt(64.W)))
      val fuOpType2 = Output(FuOpType())
      val src2      = Output(Vec(2, UInt(64.W)))

    }
  })

  val vtypeStruct = WireInit(0.U.asTypeOf(new VtypeStruct()))
  vtypeStruct := VType.toVtypeStruct(io.in.vtype)
  vtypeStruct.reserved := io.in.avl
  when(!VSETOpType.isVsetvl(io.in.func)){
    vtypeStruct.vill := false.B
  }

  val vsetRiWi = Module(new VSetRiWi(FuConfig.VSetRiWiCfg)(p))
  val vsetRiWvf = Module(new VSetRiWvf(FuConfig.VSetRiWvfCfg)(p))
  val vsetRvfWvf = Module(new VSetRvfWvf(FuConfig.VSetRvfWvfCfg)(p))

  vsetRiWi.io.flush := 0.U.asTypeOf(vsetRiWi.io.flush)
  vsetRiWi.io.in.valid := true.B
  vsetRiWi.io.out.ready := true.B
  vsetRiWi.io.in.bits := 0.U.asTypeOf(vsetRiWi.io.in.bits.cloneType)
  vsetRiWi.io.in.bits.ctrl.fuOpType := io.in.func
  vsetRiWi.io.in.bits.data.src(0) := io.in.avl
  vsetRiWi.io.in.bits.data.src(1) := vtypeStruct.asUInt

  vsetRiWvf.io.flush := 0.U.asTypeOf(vsetRiWvf.io.flush)
  vsetRiWvf.io.in.valid := true.B
  vsetRiWvf.io.out.ready := true.B
  vsetRiWvf.io.in.bits := 0.U.asTypeOf(vsetRiWvf.io.in.bits.cloneType)
  vsetRiWvf.io.in.bits.ctrl.fuOpType := io.in.func
  vsetRiWvf.io.in.bits.data.src(0) := io.in.avl
  vsetRiWvf.io.in.bits.data.src(1) := vtypeStruct.asUInt

  val vconfig = WireInit(0.U.asTypeOf(VConfig()))
  vconfig.vl := io.in.oldVl
  vsetRvfWvf.io.flush := 0.U.asTypeOf(vsetRvfWvf.io.flush)
  vsetRvfWvf.io.in.valid := true.B
  vsetRvfWvf.io.out.ready := true.B
  vsetRvfWvf.io.in.bits := 0.U.asTypeOf(vsetRvfWvf.io.in.bits.cloneType)
  vsetRvfWvf.io.in.bits.ctrl.fuOpType := io.in.func
  vsetRvfWvf.io.in.bits.data.src(0) := vconfig.asUInt
  vsetRvfWvf.io.in.bits.data.src(1) := Mux(VSETOpType.isVsetvl(io.in.func), vtypeStruct.asUInt, vtypeStruct.asUInt(7, 0))

  val selVsetIVL: Bool =  io.in.func === VSETOpType.uvsetrd_ii ||
                          io.in.func === VSETOpType.uvsetrd_xi ||
                          io.in.func === VSETOpType.uvsetrd_xx ||
                          io.in.func === VSETOpType.uvsetrd_vlmax_i ||
                          io.in.func === VSETOpType.uvsetrd_vlmax_x

  val selVsetIVConfig: Bool = io.in.func === VSETOpType.uvsetvcfg_ii ||
                              io.in.func === VSETOpType.uvsetvcfg_xi ||
                              io.in.func === VSETOpType.uvsetvcfg_xx ||
                              io.in.func === VSETOpType.uvsetvcfg_vlmax_i ||
                              io.in.func === VSETOpType.uvsetvcfg_vlmax_x

  val selVsetFVConfig: Bool = io.in.func === VSETOpType.uvsetvcfg_vv ||
                              io.in.func === VSETOpType.uvsetvcfg_keep_v

  io.out.vconfig := MuxCase(0.U.asTypeOf(VConfig()), Seq(
    selVsetIVL -> vsetRiWi.debugIO.vconfig,
    selVsetIVConfig -> vsetRiWvf.debugIO.vconfig,
    selVsetFVConfig -> vsetRvfWvf.debugIO.vconfig,
  ))

  io.debug.fuOpType0 := vsetRiWi.io.in.bits.ctrl.fuOpType
  io.debug.src0(0) := vsetRiWi.io.in.bits.data.src(0)
  io.debug.src0(1) := vsetRiWi.io.in.bits.data.src(1)

  io.debug.fuOpType1 := vsetRiWvf.io.in.bits.ctrl.fuOpType
  io.debug.src1(0) := vsetRiWvf.io.in.bits.data.src(0)
  io.debug.src1(1) := vsetRiWvf.io.in.bits.data.src(1)

  io.debug.fuOpType2 := vsetRvfWvf.io.in.bits.ctrl.fuOpType
  io.debug.src2(0) := vsetRvfWvf.io.in.bits.data.src(0)
  io.debug.src2(1) := vsetRvfWvf.io.in.bits.data.src(1)
}
