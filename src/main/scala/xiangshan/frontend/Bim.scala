package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.ALUOpType
import utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap

trait BimParams extends HasXSParameter {
  val BimBanks = PredictWidth
  val BimSize = 4096
  val nRows = BimSize / BimBanks
}

class BIM extends BasePredictor with BimParams{
  class BIMResp extends Resp {
    val ctrs = Vec(PredictWidth, ValidUndirectioned(UInt(2.W)))
  }
  class BIMMeta extends Meta {
    val ctrs = Vec(PredictWidth, UInt(2.W))
  }
  class BIMFromOthers extends FromOthers {}

  class BIMIO extends DefaultBasePredictorIO {
    val resp = new BIMResp
    val meta = new BIMMeta
  }

  override val io = IO(new BIMIO)
  // Update logic
  // 1 calculate new 2-bit saturated counter value
  def satUpdate(old: UInt, len: Int, taken: Bool): UInt = {
    val oldSatTaken = old === ((1 << len)-1).U
    val oldSatNotTaken = old === 0.U
    Mux(oldSatTaken && taken, ((1 << len)-1).U,
      Mux(oldSatNotTaken && !taken, 0.U,
        Mux(taken, old + 1.U, old - 1.U)))
  }

  val bimAddr = new TableAddr(log2Up(BimSize), BimBanks)

  val pcLatch = RegEnable(io.pc.bits, io.pc.valid)

  val bim = List.fill(BimBanks) {
    Module(new SRAMTemplate(UInt(2.W), set = nRows, shouldReset = true, holdRead = true))
  }

  val baseBank = bimAddr.getBank(io.pc.bits)

  val realMask = circularShiftRight(io.inMask, BimBanks, baseBank)
  
  // those banks whose indexes are less than baseBank are in the next row
  val isInNextRow = VecInit((0 until BtbBanks).map(_.U < baseBank))

  val baseRow = bimAddr.getBankIdx(io.pc.bits)

  val realRow = VecInit((0 until BimBanks).map(b => Mux(isInNextRow(b.U), (baseRow+1.U)(log2Up(nRows)-1, 0), baseRow)))

  val realRowLatch = VecInit(realRow.map(RegEnable(_, enable=io.pc.valid)))

  for (b <- 0 until BimBanks) {
    bim(b).reset                := reset.asBool
    bim(b).io.r.req.valid       := realMask(b) && io.pc.valid
    bim(b).io.r.req.bits.setIdx := realRow(b)
  }

  val bimRead = VecInit(bim.map(_.io.r.resp.data(0)))

  val baseBankLatch = bimAddr.getBank(pcLatch)
  
  // e.g: baseBank == 5 => (5, 6,..., 15, 0, 1, 2, 3, 4)
  val bankIdxInOrder = VecInit((0 until BimBanks).map(b => (baseBankLatch +& b.U)(log2Up(BimBanks)-1, 0)))

  for (b <- 0 until BimBanks) {
    val ctr = bimRead(bankIdxInOrder(b))
    io.resp.ctrs(b).valid := RegNext(io.pc.valid) // Does not need the valid bit
    io.resp.ctrs(b).bits  := ctr
    io.meta.ctrs(b)       := ctr
  }

  val u = io.update.bits

  val updateBank = bimAddr.getBank(u.pc)
  val updateRow = bimAddr.getBankIdx(u.pc)

  val oldCtr = u.brInfo.bimCtr
  val newTaken = u.taken
  val oldSaturated = u.taken && oldCtr === 3.U || !u.taken && oldCtr === 0.U
  
  val needToUpdate = io.update.valid && !oldSaturated && u.pd.isBr

  for (b <- 0 until BimBanks) {
    bim(b).io.w.req.valid := needToUpdate && b.U === updateBank
    bim(b).io.w.req.bits.setIdx := updateRow
    bim(b).io.w.req.bits.data := satUpdate(oldCtr, 2, newTaken)
  }
}