package device

import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, SimpleDevice}
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import freechips.rocketchip.regmapper.RegFieldGroup
import freechips.rocketchip.tilelink.TLRegisterNode
import xiangshan.backend.fu.{MMPMAMethod, PMAConst, PMPChecker, PMPReqBundle, PMPRespBundle}

class TLPMAIO(implicit val p: Parameters) extends Bundle with PMAConst {
  val req = Vec(mmpma.num, Flipped(Valid(new PMPReqBundle(mmpma.lgMaxSize))))
  val resp = Vec(mmpma.num, new PMPRespBundle())
}

class TLPMA(implicit p: Parameters) extends LazyModule with PMAConst with MMPMAMethod{
  val node = TLRegisterNode(
    address = Seq(AddressSet(mmpma.address/*pmaParam.address*/, mmpma.mask)),
    device = new SimpleDevice("mmpma", Nil),
    concurrency = 1,
    beatBytes = 8
  )

  lazy val module = new LazyModuleImp(this) {

    val io = IO(new TLPMAIO)
    val req = io.req
    val resp = io.resp

    val (cfg_map, addr_map, pma) = gen_mmpma_mapping(NumPMA)
    node.regmap(
      0x0000 -> RegFieldGroup(
        "MMPMA_Config_Register", desc = Some("MMPMA configuation register"),
        regs = cfg_map
      ),
      // still blank space here, fix it
      0x0100 -> RegFieldGroup(
        "MMPMA_Address_Register", desc = Some("MMPMA Address register"),
        regs = addr_map
      )
    )

    val pma_check = VecInit(Seq.fill(mmpma.num)(
      Module(new PMPChecker(
        mmpma.lgMaxSize/*pmaParam.lgMaxSize*/,
        mmpma.sameCycle/* pmaParam.sameCycle*/,
        false)).io
    ))
    pma_check.map(_.check_env.apply(mmpma.lgMaxSize.U, pma/*placeHolder*/, pma))
    for (i <- 0 until mmpma.num) {
      pma_check(i).req_apply(req(i).valid, req(i).bits.addr)
      resp(i) := pma_check(i).resp
    }
  }

}
