package xiangshan.cache.dcache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.cache.{HasDCacheParameters, HasL1CacheParameters, L1CacheParameters, Meta}

trait HasWPUParameters extends HasDCacheParameters
abstract class WPUBuddle(implicit P: Parameters) extends XSBundle with HasWPUParameters
abstract class WPUModule(implicit P: Parameters) extends XSModule with HasWPUParameters

class WPUReq(implicit p: Parameters) extends WPUBuddle {
  val idx = UInt(idxBits.W)
}

class WPUResp(implicit p:Parameters) extends WPUBuddle{
  val predict_way = UInt(wayBits.W)
  val predict_way_oh = UInt(nWays.W)
}

class WPUUpdate(implicit p:Parameters) extends WPUBuddle{
  val idx = UInt(idxBits.W)
  val cache_miss =  Bool()
  val correct_way = UInt(wayBits.W)
}

class WPUIO(implicit p:Parameters) extends WPUBuddle{
  // FIXME: no need to be wrapped by Decoupled ?
  val req = Decoupled(new WPUReq)
  val resp = Flipped(Decoupled(new WPUResp))
  val update = Decoupled(new WPUUpdate)
}

// TODO: mode design
// validaty
class DCacheWPU (implicit p:Parameters) extends WPUModule{
  val io = IO(new WPUIO)

  // data
  val predict_regs = RegInit(VecInit(Seq.fill(nSets)(0.U(wayBits.W))))

  // predict in load s0
  io.resp.bits.predict_way := predict_regs(io.req.bits.idx)

  // update in load s1
  when (io.update.valid) {
    when(!io.update.bits.cache_miss){
      predict_regs(io.update.bits.idx) := io.update.bits.correct_way
    }
  }

}



/** IdealWPU:
  * req in s0 and resp in s1
  * (but the recognised WPU uses reg and can get predict way in the same stage. So req in s0, resp in s0, and update in s1
  */
class IdealIfIO(implicit p:Parameters) extends WPUBuddle{
  // get the real way
  val all_tags = Input(Vec(nWays, UInt(encTagBits.W)))
  val all_metas = Input(Vec(nWays, new Meta))
  val real_tag = Input(UInt(encTagBits.W))
}
class IdealWPUIO(implicit p:Parameters) extends WPUBuddle{
  val req = Flipped(ValidIO(new WPUReq))
  val resp = ValidIO(new WPUResp)
  val idealIf = new IdealIfIO()
}
class IdealWPU(implicit p:Parameters) extends WPUModule{
  val io = IO(new IdealWPUIO)

  val pred_way = MuxCase(
    nWays.U,
    io.idealIf.all_tags.zipWithIndex.map {
      case (x, i) => (x === io.idealIf.real_tag && io.idealIf.all_metas(i.U).coh.isValid(), i.U)
    }
  )

  io.resp.valid := true.B
  io.resp.bits.predict_way := pred_way
  io.resp.bits.predict_way_oh := VecInit((0 until nWays).map(x => x.U === pred_way)).asUInt
  
  /* 
  val predict_way_oh = VecInit((0 until nWays).map(x =>
    io.idealIf.all_tags(x.U) === io.idealIf.real_tag && io.idealIf.all_metas(x.U).coh.isValid()
  )).asUInt
  io.resp.valid := predict_way_oh.orR
  io.resp.bits.predict_way_oh := predict_way_oh
  io.resp.bits.predict_way := OHToUInt(predict_way_oh)
   */
}
