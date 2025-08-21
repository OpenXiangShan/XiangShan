package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.CSRs
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.fu.PMAConfigEntry
import xiangshan.backend.fu.util.CSRConst
import xiangshan.{HasPMParameters, PMParameKey}
import CSRConfig._

trait PMAConst extends PMPConst

abstract class PMABundle(implicit val p: Parameters) extends Bundle with PMAConst
abstract class PMAModule(implicit val p: Parameters) extends Module with PMAConst

class PMAEntryHandleModule(implicit p: Parameters) extends PMAModule with PMAInit with PMAReadWrite {
  val io = IO(new PMAEntryHandleIOBundle)

  val pmaCfg  = io.in.pmaCfg

  val ren   = io.in.ren
  val wen   = io.in.wen
  val addr  = io.in.addr
  val wdata = io.in.wdata

  require(NumPMA >= 16, "The number of PMA should be greater than or equal to 16.")

  val pmaAddrInit = VecInit(Seq.fill(p(PMParameKey).NumPMA)(0.U((PMPAddrWidth-PMPOffBits).W)))
  val pmaMaskInit = VecInit(Seq.fill(p(PMParameKey).NumPMA)(0.U(PMPAddrWidth.W)))

  pmaAddrInit.zip(pmaMaskInit).zip(pmaInit).foreach { case ((addr, mask), init) =>
    addr := genAddr(init).U((PMPAddrWidth-PMPOffBits).W)
    mask := genMask(init.a, genAddr(init))
  }

  val pmaAddr = RegInit(pmaAddrInit)
  val pmaMask = RegInit(pmaMaskInit)

  val pmaEntry = Wire(Vec(p(PMParameKey).NumPMA, new PMAEntry))

  for (i <- pmaEntry.indices) {
    pmaEntry(i).gen(pmaCfg(i), pmaAddr(i), pmaMask(i))
  }

  // write pmaCfg
  val cfgVec = WireInit(VecInit(Seq.fill(8)(0.U.asTypeOf(new PMACfgBundle))))
  for (i <- 0 until (p(PMParameKey).NumPMA/8+1) by 2) {
    when (wen && (addr === (CSRConst.PmacfgBase + i).U)) {
      for (j <- cfgVec.indices) {
        val cfgOldTmp = pmaEntry(8*i/2+j).cfg
        val cfgNewTmp = Wire(new PMACfgBundle)
        cfgNewTmp := wdata(8*(j+1)-1, 8*j)
        cfgVec(j) := cfgOldTmp
        when (!cfgOldTmp.L.asBool) {
          cfgVec(j) := cfgNewTmp
          cfgVec(j).W := cfgNewTmp.W.asBool && cfgNewTmp.R.asBool
          if (CoarserGrain) {
            cfgVec(j).A := Cat(cfgNewTmp.A.asUInt(1), cfgNewTmp.A.asUInt.orR)
          }
          when (PMPCfgAField.isNa4OrNapot(cfgVec(j))) {
            pmaMask(8*i/2+j) := pmaEntry(8*i/2+j).matchMask(cfgVec(j), pmaEntry(8*i/2+j).addr)
          }
        }
      }
    }
  }

  io.out.pmaCfgWdata := Cat(cfgVec.map(_.asUInt).reverse)


  val pmaAddrR = Wire(Vec(p(PMParameKey).NumPMA, UInt(64.W)))

  for (i <- 0 until p(PMParameKey).NumPMA) {
    pmaAddrR(i) := pmaEntry(i).addr
    // write pmaAddr
    when (wen && (addr === (CSRConst.PmaaddrBase + i).U)) {
      if (i != (p(PMParameKey).NumPMA - 1)) {
        val addrNextLocked: Bool = PMPCfgLField.addrLocked(pmaEntry(i).cfg, pmaEntry(i + 1).cfg)
        pmaMask(i) := Mux(!addrNextLocked, pmaEntry(i).matchMask(wdata), pmaEntry(i).mask)
        pmaAddr(i) := Mux(!addrNextLocked, wdata, pmaEntry(i).addr)
      } else {
        val addrLocked: Bool = PMPCfgLField.addrLocked(pmaEntry(i).cfg)
        pmaMask(i) := Mux(!addrLocked, pmaEntry(i).matchMask(wdata), pmaEntry(i).mask)
        pmaAddr(i) := Mux(!addrLocked, wdata, pmaEntry(i).addr)
      }
    }
    // read pmaAddr
    when (ren && (addr === (CSRConst.PmaaddrBase + i).U)) {
      pmaAddrR(i) := pmaEntry(i).readAddr(pmaEntry(i).cfg, pmaEntry(i).addr)
    }
  }

  io.out.pmaAddrRData := pmaAddrR
  io.out.pmaAddrRegOut := pmaAddr

}

class PMAEntryHandleIOBundle(implicit p: Parameters) extends PMABundle {
  val in = Input(new Bundle {
    val wen = Bool()
    val ren = Bool()
    val addr = UInt(12.W)
    val wdata = UInt(64.W)
    val pmaCfg = Vec(NumPMA, new PMACfgBundle)
  })

  val out = Output(new Bundle {
    val pmaCfgWdata = UInt(PMXLEN.W)
    val pmaAddrRData = Vec(NumPMA, UInt(64.W))
    val pmaAddrRegOut = Vec(NumPMA, UInt(64.W))
  })
}

trait PMAReadWrite extends PMAConst with PMPReadWrite {
  def shift_addr(addr: BigInt): BigInt = {
    addr >> 2
  }

  def get_napot(base: BigInt, range: BigInt): BigInt = {
    val PlatformGrainBytes = 1 << PlatformGrain
    if ((base % PlatformGrainBytes) != 0) {
      println("base: %x", base)
    }
    if ((range % PlatformGrainBytes) != 0) {
      println("range: %x", range)
    }
    require((base % PlatformGrainBytes) == 0)
    require((range % PlatformGrainBytes) == 0)

    (base + (range/2 - 1)) >> PMPOffBits
  }

  def matchMask(cfgA0: Bool, paddr: UInt): UInt = {
    val matchMaskCAddr = Cat(paddr, cfgA0) | (((1 << PlatformGrain) - 1) >> PMPOffBits).U((paddr.getWidth + 1).W)
    Cat(matchMaskCAddr & (~(matchMaskCAddr + 1.U)).asUInt, ((1 << PMPOffBits) - 1).U(PMPOffBits.W))
  }

  def readAddr(cfg: PMACfgBundle, addr: UInt): UInt = {
    val G = PlatformGrain - PMPOffBits
    require(G >= 0)
    if (G == 0) {
      addr
    } else if (G >= 2) {
      Mux(PMPCfgAField.isNa4OrNapot(cfg), setLowBits(addr, G-1), clearLowBits(addr, G))
    } else {
      Mux(PMPCfgAField.isOffOrTor(cfg), clearLowBits(addr, G), addr)
    }
  }

  def genAddr(init: PMAConfigEntry): BigInt = {
    if (init.a < 2) {
      shift_addr(init.base_addr)
    } else {
      get_napot(init.base_addr, init.range)
    }
  }

  def genMask(a: BigInt, initAddr: BigInt) = {
    val matchMaskAddr = (initAddr << 1) | (a & 0x1) | (((1 << PlatformGrain) - 1) >> PMPOffBits)
    val mask = ((matchMaskAddr & ~(matchMaskAddr + 1)) << PMPOffBits) | ((1 << PMPOffBits) - 1)
    mask.U(PMPAddrWidth.W)
  }
}

class PMAEntry(implicit p: Parameters) extends PMABundle with PMAReadWrite {
  val cfg  = new PMACfgBundle
  val addr = UInt((PMPAddrWidth-PMPOffBits).W)
  val mask = UInt(PMPAddrBits.W)

  def gen(cfg: PMACfgBundle, addr: UInt, mask: UInt) = {
    this.cfg := cfg
    this.addr := addr
    this.mask := mask
  }

  def matchMask(paddr: UInt): UInt = {
    matchMask(cfg.A.asUInt(0), paddr)
  }
}
