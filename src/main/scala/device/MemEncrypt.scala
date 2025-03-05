/***************************************************************************************
* Copyright (c) 2024-2025 Institute of Information Engineering, Chinese Academy of Sciences
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

package device

import chisel3._
import chisel3.util._
import chisel3.util.HasBlackBoxResource
import org.chipsalliance.cde.config.Field
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.tilelink.AXI4TLState
import javax.xml.crypto.dsig.keyinfo.KeyInfo
import system._

case object MemcEdgeInKey extends Field[AXI4EdgeParameters]
case object MemcEdgeOutKey extends Field[AXI4EdgeParameters]

trait Memconsts {
  val p: Parameters
  val cvm = p(CVMParamskey)
  val soc = p(SoCParamsKey)
  val PAddrBits= soc.PAddrBits
  val KeyIDBits= cvm.KeyIDBits
  val MemencPipes = cvm.MemencPipes
  lazy val MemcedgeIn = p(MemcEdgeInKey)
  lazy val MemcedgeOut = p(MemcEdgeOutKey)
  require (isPow2(MemencPipes), s"AXI4MemEncrypt: MemencPipes must be a power of two, not $MemencPipes")
  require (PAddrBits > KeyIDBits, s"AXI4MemEncrypt: PAddrBits must be greater than KeyIDBits")
  def HasDelayNoencryption = cvm.HasDelayNoencryption
}


abstract class MemEncryptModule(implicit val p: Parameters) extends Module with Memconsts

class TweakEncrptyQueue(implicit p: Parameters) extends MemEncryptModule
{
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(new Bundle {
      val addr = UInt(PAddrBits.W)
      val len  = UInt(MemcedgeIn.bundle.lenBits.W)  // number of beats - 1
    }))
    val deq = DecoupledIO(new Bundle {
      val keyid = UInt(KeyIDBits.W)
      val tweak = UInt(MemcedgeIn.bundle.dataBits.W)
      val addr = UInt(MemcedgeIn.bundle.addrBits.W)
    })
    val tweak_round_keys = Input(Vec(32, UInt(32.W)))
  })
    val tweak_in = Cat(0.U((128 - PAddrBits).W), Cat(io.enq.bits.addr(PAddrBits - 1, 6), 0.U(6.W)))

    val tweak_enc_module = Module(new TweakEncrypt(opt = true))
    val tweakgf128_module = Module(new TweakGF128())

    tweak_enc_module.io.tweak_enc_req.valid           := io.enq.valid
    tweak_enc_module.io.tweak_enc_resp.ready          := tweakgf128_module.io.req.ready
    tweak_enc_module.io.tweak_enc_req.bits.tweak             := tweak_in
    tweak_enc_module.io.tweak_enc_req.bits.addr_in           := io.enq.bits.addr
    tweak_enc_module.io.tweak_enc_req.bits.len_in            := io.enq.bits.len
    tweak_enc_module.io.tweak_enc_req.bits.id_in             := 0.U
    tweak_enc_module.io.tweak_enc_req.bits.tweak_round_keys  := io.tweak_round_keys

    io.enq.ready := tweak_enc_module.io.tweak_enc_req.ready
 
    tweakgf128_module.io.req.bits.len      := tweak_enc_module.io.tweak_enc_resp.bits.len_out
    tweakgf128_module.io.req.bits.addr     := tweak_enc_module.io.tweak_enc_resp.bits.addr_out
    tweakgf128_module.io.req.bits.tweak_in := tweak_enc_module.io.tweak_enc_resp.bits.tweak_encrpty
    tweakgf128_module.io.req.valid         := tweak_enc_module.io.tweak_enc_resp.valid
    tweakgf128_module.io.resp.ready        := io.deq.ready

    io.deq.bits.keyid := tweakgf128_module.io.resp.bits.keyid_out
    io.deq.bits.tweak := tweakgf128_module.io.resp.bits.tweak_out
    io.deq.bits.addr  := tweakgf128_module.io.resp.bits.addr_out
    io.deq.valid      := tweakgf128_module.io.resp.valid
}

class AXI4W_KT(opt:Boolean)(implicit val p: Parameters) extends Bundle with Memconsts
{
  val edgeUse = if (opt) MemcedgeIn else MemcedgeOut
  val axi4 = new AXI4BundleW(edgeUse.bundle)
  val keyid = UInt(KeyIDBits.W)
  val tweak = UInt(edgeUse.bundle.dataBits.W)
}

// Used to indicate the source of the req (L1I/L1D/PTW)
case object ReqSourceKey extends ControlKey[UInt]("reqSource")

class AXI4WriteMachine(implicit p: Parameters) extends MemEncryptModule
{
  val io = IO(new Bundle {
    val in_w = Flipped(Irrevocable(new AXI4BundleW(MemcedgeOut.bundle)))
    val in_kt = Flipped(DecoupledIO(new Bundle {
      val keyid = UInt(KeyIDBits.W)
      val tweak = UInt(MemcedgeOut.bundle.dataBits.W)
      val addr = UInt(MemcedgeOut.bundle.addrBits.W)
    }))
    val out_ar = Irrevocable(new AXI4BundleAR(MemcedgeOut.bundle))
    val in_r = Flipped(Irrevocable(new AXI4BundleR(MemcedgeOut.bundle)))
    val out_w = DecoupledIO(new AXI4W_KT(true))
    val uncache_en = Output(Bool())
    val uncache_commit = Input(Bool())
  })
  // ----------------
  // s0 stage
  // ----------------
  val w_cacheable = io.in_w.bits.strb.andR

  // ----------------
  // s1 stage
  // ----------------
  val in_w_v   = RegInit(false.B)
  val in_kt_v  = RegInit(false.B)

  val in_w_req  = RegEnable(io.in_w.bits, io.in_w.fire)
  val in_kt_req = RegEnable(io.in_kt.bits, io.in_kt.fire)
  io.in_w.ready := !in_w_v || io.out_w.fire
  io.in_kt.ready := !in_kt_v || io.out_w.fire

  when(io.in_w.fire) {
    in_w_v := true.B
  }.elsewhen(io.out_w.fire) {
    in_w_v := false.B
  }.otherwise {
    in_w_v := in_w_v
  }

  when(io.in_kt.fire) {
    in_kt_v := true.B
  }.elsewhen(io.out_w.fire) {
    in_kt_v := false.B
  }.otherwise {
    in_kt_v := in_kt_v
  }

  // -----------------------------
  // s2 stage only uncacheable use
  // -----------------------------
  val out_ar_v = RegInit(false.B)
  val out_ar_mask = RegInit(false.B)
  val in_r_v   = RegInit(false.B)
  val r_uncache_en = RegInit(false.B)
  when(io.in_r.fire) {
    in_r_v := true.B
  }.elsewhen(io.out_w.fire) {
    in_r_v := false.B
  }.otherwise {
    in_r_v := in_r_v
  }

  when(io.in_r.fire) {
    r_uncache_en := true.B
  }.elsewhen(io.uncache_commit) {
    r_uncache_en := false.B
  }.otherwise {
    r_uncache_en := r_uncache_en
  }

  io.in_r.ready := !r_uncache_en || io.uncache_commit
  io.uncache_en := r_uncache_en

  val s1_w_cacheable = RegEnable(w_cacheable, io.in_w.fire)

  when(in_w_v && in_kt_v && !s1_w_cacheable && !out_ar_mask) {
    out_ar_v := true.B
  }.elsewhen(io.out_ar.fire) {
    out_ar_v := false.B
  }.otherwise {
    out_ar_v := out_ar_v
  }

  when(in_w_v && in_kt_v && !s1_w_cacheable && !out_ar_mask) {
    out_ar_mask := true.B
  }.elsewhen(io.out_w.fire) {
    out_ar_mask := false.B
  }.otherwise {
    out_ar_mask := out_ar_mask
  }

  io.out_ar.valid := out_ar_v
  val ar = io.out_ar.bits
  ar.id    := 1.U << (MemcedgeOut.bundle.idBits - 1)
  ar.addr  := (in_kt_req.addr >> log2Ceil(MemcedgeOut.bundle.dataBits/8)) << log2Ceil(MemcedgeOut.bundle.dataBits/8)
  ar.len   := 0.U
  ar.size  := log2Ceil(MemcedgeOut.bundle.dataBits/8).U
  ar.burst := AXI4Parameters.BURST_INCR
  ar.lock  := 0.U // not exclusive (LR/SC unsupported b/c no forward progress guarantee)
  ar.cache := 0.U // do not allow AXI to modify our transactions
  ar.prot  := AXI4Parameters.PROT_PRIVILEGED
  ar.qos   := 0.U // no QoS
  if (MemcedgeOut.bundle.echoFields != Nil) {
    val ar_extra = ar.echo(AXI4TLState)
      ar_extra.source := 0.U
      ar_extra.size   := 0.U
  }
  if (MemcedgeOut.bundle.requestFields != Nil) {
    val ar_user = ar.user(ReqSourceKey)
      ar_user := 0.U
  }

  def gen_wmask(strb: UInt): UInt = {
    val extendedBits = VecInit((0 until MemcedgeOut.bundle.dataBits/8).map(i => Cat(Fill(7, strb((MemcedgeOut.bundle.dataBits/8)-1-i)), strb((MemcedgeOut.bundle.dataBits/8)-1-i))))
    extendedBits.reduce(_ ## _)
  }

  val new_data = Reg(UInt(MemcedgeOut.bundle.dataBits.W))
  val new_strb = ~0.U((MemcedgeOut.bundle.dataBits/8).W)
  val wmask = gen_wmask(in_w_req.strb)

  when(io.in_r.fire) {
    new_data := (io.in_r.bits.data & ~wmask) | (in_w_req.data & wmask)
  }

  when(s1_w_cacheable) {
    io.out_w.valid := in_w_v && in_kt_v
    io.out_w.bits.axi4 := in_w_req
    io.out_w.bits.keyid := in_kt_req.keyid
    io.out_w.bits.tweak := in_kt_req.tweak
  }.otherwise {
    io.out_w.valid := in_w_v && in_kt_v && in_r_v
    io.out_w.bits.axi4 := in_w_req
    io.out_w.bits.axi4.data := new_data
    io.out_w.bits.axi4.strb := new_strb
    io.out_w.bits.keyid := in_kt_req.keyid
    io.out_w.bits.tweak := in_kt_req.tweak
  }

}

class WdataEncrptyPipe(implicit p: Parameters) extends MemEncryptModule
{
  val io = IO(new Bundle {
    val in_w = Flipped(DecoupledIO(new AXI4W_KT(true)))
    val out_w = Irrevocable(new AXI4BundleW(MemcedgeIn.bundle))
    val enc_keyids = Output(Vec(MemencPipes, UInt(KeyIDBits.W)))
    val enc_round_keys = Input(Vec(MemencPipes, UInt((32*32/MemencPipes).W)))
  })
  val reg_encdec_result_0 = Reg(Vec(MemencPipes, UInt(128.W)))
  val reg_encdec_result_1 = Reg(Vec(MemencPipes, UInt(128.W)))
  val reg_axi4_other_result = Reg(Vec(MemencPipes, new AXI4BundleWWithoutData(MemcedgeIn.bundle)))
  val reg_tweak_result_0 = Reg(Vec(MemencPipes, UInt(128.W)))
  val reg_tweak_result_1 = Reg(Vec(MemencPipes, UInt(128.W)))
  val reg_keyid = Reg(Vec(MemencPipes, UInt(KeyIDBits.W)))
  val reg_encdec_valid = RegInit(VecInit(Seq.fill(MemencPipes)(false.B)))
  val wire_ready_result = WireInit(VecInit(Seq.fill(MemencPipes)(false.B)))

  val wire_axi4_other = Wire(new AXI4BundleWWithoutData(MemcedgeIn.bundle))
  wire_axi4_other.strb := io.in_w.bits.axi4.strb
  wire_axi4_other.last := io.in_w.bits.axi4.last
  wire_axi4_other.user := io.in_w.bits.axi4.user


  val pipes_first_data_0 = Wire(UInt(128.W))
  val pipes_first_data_1 = Wire(UInt(128.W))
  if (HasDelayNoencryption) {
    pipes_first_data_0 := io.in_w.bits.axi4.data(127,0)
    pipes_first_data_1 := io.in_w.bits.axi4.data(255,128)
  } else {
    pipes_first_data_0 := io.in_w.bits.axi4.data(127,0) ^ io.in_w.bits.tweak(127, 0)
    pipes_first_data_1 := io.in_w.bits.axi4.data(255,128) ^ io.in_w.bits.tweak(255,128)
  }

  def configureModule(flag: Boolean, i: Int, keyId: UInt, dataIn: UInt, tweakIn: UInt, axi4In: AXI4BundleWWithoutData, roundKeys: UInt): OnePipeEncBase = {
      when(wire_ready_result(i) && (if (i == 0) io.in_w.valid else reg_encdec_valid(i-1))) {
        reg_encdec_valid(i) := true.B
      }.elsewhen(reg_encdec_valid(i) && (if (i == MemencPipes - 1) io.out_w.ready else wire_ready_result(i + 1))) {
        reg_encdec_valid(i) := false.B
      }.otherwise {
        reg_encdec_valid(i) := reg_encdec_valid(i)
      }

      wire_ready_result(i) := !reg_encdec_valid(i) || (reg_encdec_valid(i) && (if (i == MemencPipes - 1) io.out_w.ready else wire_ready_result(i+1)))

      val module: OnePipeEncBase = if (HasDelayNoencryption) Module(new OnePipeForEncNoEnc()) else Module(new OnePipeForEnc())
      module.io.onepipe_in.keyid := keyId
      module.io.onepipe_in.data_in := dataIn
      module.io.onepipe_in.tweak_in := tweakIn
      module.io.onepipe_in.axi4_other := axi4In
      for (i <- 0 until 32/MemencPipes) {
        module.io.onepipe_in.round_key_in(i) := roundKeys(i * 32 + 31, i * 32)
      }
      when((if (i == 0) io.in_w.valid else reg_encdec_valid(i-1)) && wire_ready_result(i)) {
        if (flag) {
          reg_encdec_result_0(i) := module.io.onepipe_out.result_out
          reg_tweak_result_0(i) := module.io.onepipe_out.tweak_out
          reg_axi4_other_result(i) := module.io.onepipe_out.axi4_other_out
          reg_keyid(i) := module.io.onepipe_out.keyid_out
        } else {
          reg_encdec_result_1(i) := module.io.onepipe_out.result_out
          reg_tweak_result_1(i) := module.io.onepipe_out.tweak_out
        }
      }
      io.enc_keyids(i) := module.io.onepipe_out.keyid_out
      module
  }
  val modules_0 = (0 until MemencPipes).map { i =>
    if (i == 0) {
      configureModule(true, i, io.in_w.bits.keyid, pipes_first_data_0, io.in_w.bits.tweak(127, 0), wire_axi4_other, io.enc_round_keys(i))
    } else {
      configureModule(true, i, reg_keyid(i-1), reg_encdec_result_0(i-1), reg_tweak_result_0(i-1), reg_axi4_other_result(i-1), io.enc_round_keys(i))
    }
  }
  val modules_1 = (0 until MemencPipes).map { i =>
    if (i == 0) {
      configureModule(false, i, io.in_w.bits.keyid, pipes_first_data_1, io.in_w.bits.tweak(255,128), wire_axi4_other, io.enc_round_keys(i))
    } else {
      configureModule(false, i, reg_keyid(i-1), reg_encdec_result_1(i-1), reg_tweak_result_1(i-1), reg_axi4_other_result(i-1), io.enc_round_keys(i))
    }
  }
  if (HasDelayNoencryption) {
    io.out_w.bits.data := Cat(reg_encdec_result_1.last, reg_encdec_result_0.last)
  } else {
    val enc_0_out = Cat(
      reg_encdec_result_0.last(31, 0),
      reg_encdec_result_0.last(63, 32),
      reg_encdec_result_0.last(95, 64),
      reg_encdec_result_0.last(127, 96)
    )
    val enc_1_out = Cat(
      reg_encdec_result_1.last(31, 0),
      reg_encdec_result_1.last(63, 32),
      reg_encdec_result_1.last(95, 64),
      reg_encdec_result_1.last(127, 96)
    )
    io.out_w.bits.data := Cat(enc_1_out ^ reg_tweak_result_1.last, enc_0_out ^ reg_tweak_result_0.last)
  }

  io.out_w.bits.strb := reg_axi4_other_result.last.strb
  io.out_w.bits.last := reg_axi4_other_result.last.last
  io.out_w.bits.user := reg_axi4_other_result.last.user
  io.out_w.valid := reg_encdec_valid.last
  io.in_w.ready := wire_ready_result(0)
}

class TweakEncrptyTable(implicit p: Parameters) extends MemEncryptModule
{
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(new Bundle {
      val addr = UInt(PAddrBits.W)
      val len  = UInt(MemcedgeOut.bundle.lenBits.W)  // number of beats - 1
      val id  = UInt(MemcedgeOut.bundle.idBits.W)    // 7 bits
    }))
    val req = Flipped(DecoupledIO(new Bundle {
      val id  = UInt(MemcedgeOut.bundle.idBits.W)
    }))
    val resp = DecoupledIO(new Bundle {
      val keyid = UInt(KeyIDBits.W)
      val tweak = UInt(MemcedgeOut.bundle.dataBits.W)
    })
    val dec_r = new Bundle {
      val id = Input(UInt(MemcedgeOut.bundle.idBits.W))
      val mode = Output(Bool())
    }
    val dec_keyid = Output(UInt(KeyIDBits.W))
    val dec_mode = Input(Bool())
    val tweak_round_keys = Input(Vec(32, UInt(32.W)))
    val memenc_enable = Input(Bool())
  })

  val tweak_in = Cat(0.U((128 - PAddrBits).W), Cat(io.enq.bits.addr(PAddrBits-1, 6), 0.U(6.W)))
  // query the dec_mode from the round key
  io.dec_keyid := io.enq.bits.addr(PAddrBits - 1, PAddrBits - KeyIDBits)

  val tweak_enc_module = Module(new TweakEncrypt(opt = false))
  val tweak_table = Module(new TweakTable())
  val tweak_gf128 = Module(new GF128())

  // updata mode table
  tweak_table.io.w_mode.bits.id       := io.enq.bits.id
  tweak_table.io.w_mode.bits.dec_mode := io.dec_mode && io.memenc_enable
  tweak_table.io.w_mode.valid         := io.enq.fire

  tweak_enc_module.io.tweak_enc_resp.ready          := tweak_table.io.write.ready   // always true
  tweak_enc_module.io.tweak_enc_req.bits.tweak             := tweak_in
  tweak_enc_module.io.tweak_enc_req.bits.addr_in           := io.enq.bits.addr
  tweak_enc_module.io.tweak_enc_req.bits.len_in            := io.enq.bits.len
  tweak_enc_module.io.tweak_enc_req.bits.id_in             := io.enq.bits.id
  tweak_enc_module.io.tweak_enc_req.bits.tweak_round_keys  := io.tweak_round_keys
  tweak_enc_module.io.tweak_enc_req.valid                  := io.enq.valid && io.dec_mode && io.memenc_enable

  io.enq.ready := tweak_enc_module.io.tweak_enc_req.ready

  // write signal in tweak table
  tweak_table.io.write.valid       := tweak_enc_module.io.tweak_enc_resp.valid
  tweak_table.io.write.bits.id            := tweak_enc_module.io.tweak_enc_resp.bits.id_out
  tweak_table.io.write.bits.addr          := tweak_enc_module.io.tweak_enc_resp.bits.addr_out
  tweak_table.io.write.bits.len           := tweak_enc_module.io.tweak_enc_resp.bits.len_out
  tweak_table.io.write.bits.tweak_encrpty := tweak_enc_module.io.tweak_enc_resp.bits.tweak_encrpty

  // read signal in tweak table
  tweak_table.io.req.valid   := io.req.valid
  tweak_table.io.resp.ready  := io.resp.ready

  tweak_table.io.req.bits.read_id := io.req.bits.id

  val tweak_encrpty = tweak_table.io.resp.bits.read_tweak
  val tweak_counter = tweak_table.io.resp.bits.read_sel_counter
  val keyid         = tweak_table.io.resp.bits.read_keyid

  tweak_table.io.r_mode.id := io.dec_r.id
  val mode = tweak_table.io.r_mode.dec_mode
  io.dec_r.mode        := mode

  tweak_gf128.io.tweak_in := tweak_encrpty
  io.resp.bits.tweak := Mux(tweak_counter, tweak_gf128.io.tweak_out(511, 256), tweak_gf128.io.tweak_out(255, 0))
  io.resp.bits.keyid := keyid
  io.resp.valid      := tweak_table.io.resp.valid
  io.req.ready       := tweak_table.io.req.ready

}

class AXI4R_KT(opt:Boolean)(implicit val p: Parameters) extends Bundle with Memconsts
{
  val edgeUse = if (opt) MemcedgeIn else MemcedgeOut
  val axi4 = new AXI4BundleR(edgeUse.bundle)
  val keyid = UInt(KeyIDBits.W)
  val tweak = UInt(edgeUse.bundle.dataBits.W)
}

class AXI4ReadMachine(implicit p: Parameters) extends MemEncryptModule
{
  val io = IO(new Bundle {
    val in_r = Flipped(Irrevocable(new AXI4BundleR(MemcedgeOut.bundle)))
    val kt_req = DecoupledIO(new Bundle {
      val id  = UInt(MemcedgeOut.bundle.idBits.W)
    })
    val in_kt = Flipped(DecoupledIO(new Bundle {
      val keyid = UInt(KeyIDBits.W)
      val tweak = UInt(MemcedgeOut.bundle.dataBits.W)
    }))
    val out_r = DecoupledIO(new AXI4R_KT(false))
  })
  val s1_r_val = RegInit(false.B)
  val s1_r_req = RegEnable(io.in_r.bits, io.in_r.fire)
  val s1_r_out_rdy = Wire(Bool())

  val s2_r_val = RegInit(false.B)
  val s2_r_in_rdy = Wire(Bool())
  val s2_r_req = RegEnable(s1_r_req, s1_r_val && s2_r_in_rdy)

  // ----------------
  // s0 stage
  // ----------------
  io.in_r.ready := !s1_r_val || (s1_r_val && s1_r_out_rdy)

  // ----------------
  // s1 stage
  // ----------------
  when(io.in_r.fire) {
    s1_r_val := true.B
  }.elsewhen(s1_r_val && s1_r_out_rdy) {
    s1_r_val := false.B
  }.otherwise {
    s1_r_val := s1_r_val
  }

  s1_r_out_rdy := s2_r_in_rdy && io.kt_req.ready
  io.kt_req.valid := s1_r_val && s2_r_in_rdy
  io.kt_req.bits.id := s1_r_req.id

  // ----------------
  // s2 stage
  // ----------------
  when(s1_r_val && s1_r_out_rdy) {
    s2_r_val := true.B
  }.elsewhen(s2_r_val && io.out_r.fire) {
    s2_r_val := false.B
  }.otherwise {
    s2_r_val := s2_r_val
  }
  s2_r_in_rdy := !s2_r_val || io.out_r.fire

  io.in_kt.ready := io.out_r.fire

  io.out_r.valid := s2_r_val && io.in_kt.valid
  io.out_r.bits.axi4 := s2_r_req
  io.out_r.bits.keyid := io.in_kt.bits.keyid
  io.out_r.bits.tweak := io.in_kt.bits.tweak
}

class RdataDecrptyPipe(implicit p: Parameters) extends MemEncryptModule
{
  val io = IO(new Bundle {
    val in_r = Flipped(DecoupledIO(new AXI4R_KT(false)))
    val out_r = Irrevocable(new AXI4BundleR(MemcedgeOut.bundle))
    val dec_keyids = Output(Vec(MemencPipes, UInt(KeyIDBits.W)))
    val dec_round_keys = Input(Vec(MemencPipes, UInt((32*32/MemencPipes).W)))
  })

  val reg_encdec_result_0 = Reg(Vec(MemencPipes, UInt(128.W)))
  val reg_encdec_result_1 = Reg(Vec(MemencPipes, UInt(128.W)))
  val reg_axi4_other_result = Reg(Vec(MemencPipes, new AXI4BundleRWithoutData(MemcedgeOut.bundle)))
  val reg_tweak_result_0 = Reg(Vec(MemencPipes, UInt(128.W)))
  val reg_tweak_result_1 = Reg(Vec(MemencPipes, UInt(128.W)))
  val reg_keyid = Reg(Vec(MemencPipes, UInt(KeyIDBits.W)))
  val reg_encdec_valid = RegInit(VecInit(Seq.fill(MemencPipes)(false.B)))
  val wire_ready_result = WireInit(VecInit(Seq.fill(MemencPipes)(false.B)))


  val wire_axi4_other = Wire(new AXI4BundleRWithoutData(MemcedgeOut.bundle))
  wire_axi4_other.id := io.in_r.bits.axi4.id
  wire_axi4_other.resp := io.in_r.bits.axi4.resp
  wire_axi4_other.user := io.in_r.bits.axi4.user
  wire_axi4_other.echo := io.in_r.bits.axi4.echo
  wire_axi4_other.last := io.in_r.bits.axi4.last

  val pipes_first_data_0 = Wire(UInt(128.W))
  val pipes_first_data_1 = Wire(UInt(128.W))

  if (HasDelayNoencryption) {
    pipes_first_data_0 := io.in_r.bits.axi4.data(127,0)
    pipes_first_data_1 := io.in_r.bits.axi4.data(255,128)
  } else {
    pipes_first_data_0 := io.in_r.bits.axi4.data(127,0) ^ io.in_r.bits.tweak(127, 0)
    pipes_first_data_1 := io.in_r.bits.axi4.data(255,128) ^ io.in_r.bits.tweak(255,128)
  }
  def configureModule(flag: Boolean, i: Int, keyId: UInt, dataIn: UInt, tweakIn: UInt, axi4In: AXI4BundleRWithoutData, roundKeys: UInt): OnePipeDecBase = {

    when(wire_ready_result(i) && (if (i == 0) io.in_r.valid else reg_encdec_valid(i-1))) {
      reg_encdec_valid(i) := true.B
    }.elsewhen(reg_encdec_valid(i) && (if (i == MemencPipes - 1) io.out_r.ready else wire_ready_result(i+1))) {
      reg_encdec_valid(i) := false.B
    }.otherwise {
      reg_encdec_valid(i) := reg_encdec_valid(i)
    }

    wire_ready_result(i) := !reg_encdec_valid(i) || (reg_encdec_valid(i) && (if (i == MemencPipes - 1) io.out_r.ready else wire_ready_result(i+1)))

    val module: OnePipeDecBase = if (HasDelayNoencryption) Module(new OnePipeForDecNoDec()) else Module(new OnePipeForDec())
    module.io.onepipe_in.keyid := keyId
    module.io.onepipe_in.data_in := dataIn
    module.io.onepipe_in.tweak_in := tweakIn
    module.io.onepipe_in.axi4_other := axi4In
    for (i <- 0 until 32/MemencPipes) {
      module.io.onepipe_in.round_key_in(i) := roundKeys(i * 32 + 31, i * 32)
    }
    when((if (i == 0) io.in_r.valid else reg_encdec_valid(i-1)) && wire_ready_result(i)) {
      if (flag) {
        reg_encdec_result_0(i) := module.io.onepipe_out.result_out
        reg_tweak_result_0(i) := module.io.onepipe_out.tweak_out
        reg_axi4_other_result(i) := module.io.onepipe_out.axi4_other_out
        reg_keyid(i) := module.io.onepipe_out.keyid_out
      } else {
        reg_encdec_result_1(i) := module.io.onepipe_out.result_out
        reg_tweak_result_1(i) := module.io.onepipe_out.tweak_out
      }
    }
    io.dec_keyids(i)  := module.io.onepipe_out.keyid_out
    module
  }
  val modules_0 = (0 until MemencPipes).map { i =>
    if (i == 0) {
      configureModule(true, i, io.in_r.bits.keyid, pipes_first_data_0, io.in_r.bits.tweak(127, 0), wire_axi4_other, io.dec_round_keys(i))
    } else {
      configureModule(true, i, reg_keyid(i-1), reg_encdec_result_0(i-1), reg_tweak_result_0(i-1), reg_axi4_other_result(i-1), io.dec_round_keys(i))
    }
  }

  val modules_1 = (0 until MemencPipes).map { i =>
    if (i == 0) {
      configureModule(false, i, io.in_r.bits.keyid, pipes_first_data_1, io.in_r.bits.tweak(255,128), wire_axi4_other,  io.dec_round_keys(i))
    } else {
      configureModule(false, i, reg_keyid(i-1),reg_encdec_result_1(i-1), reg_tweak_result_1(i-1), reg_axi4_other_result(i-1), io.dec_round_keys(i))
    }
  }
  if (HasDelayNoencryption) {
    io.out_r.bits.data := Cat(reg_encdec_result_1.last, reg_encdec_result_0.last)
  } else {
    val enc_0_out = Cat(
      reg_encdec_result_0.last(31, 0),
      reg_encdec_result_0.last(63, 32),
      reg_encdec_result_0.last(95, 64),
      reg_encdec_result_0.last(127, 96)
    )
    val enc_1_out = Cat(
      reg_encdec_result_1.last(31, 0),
      reg_encdec_result_1.last(63, 32),
      reg_encdec_result_1.last(95, 64),
      reg_encdec_result_1.last(127, 96)
    )
    io.out_r.bits.data := Cat(enc_1_out ^ reg_tweak_result_1.last, enc_0_out ^ reg_tweak_result_0.last)
  }

  io.out_r.bits.id := reg_axi4_other_result.last.id
  io.out_r.bits.resp := reg_axi4_other_result.last.resp
  io.out_r.bits.user := reg_axi4_other_result.last.user
  io.out_r.bits.echo := reg_axi4_other_result.last.echo
  io.out_r.bits.last := reg_axi4_other_result.last.last
  io.out_r.valid := reg_encdec_valid.last
  io.in_r.ready := wire_ready_result(0)

}

class RdataRoute(implicit p: Parameters) extends MemEncryptModule
{
  val io = IO(new Bundle {
    val in_r = Flipped(Irrevocable(new AXI4BundleR(MemcedgeOut.bundle)))
    val out_r0 = Irrevocable(new AXI4BundleR(MemcedgeIn.bundle))
    val out_r1 = Irrevocable(new AXI4BundleR(MemcedgeIn.bundle))
  })

  val r_sel = io.in_r.bits.id(MemcedgeOut.bundle.idBits - 1).asBool

  io.out_r0.bits <> io.in_r.bits
  io.out_r1.bits <> io.in_r.bits

  io.out_r0.valid := io.in_r.valid && !r_sel
  io.out_r1.valid := io.in_r.valid &&  r_sel
  io.in_r.ready := Mux(r_sel, io.out_r1.ready, io.out_r0.ready)
}

class MemEncryptCSR(implicit p: Parameters) extends MemEncryptModule
{
  val io = IO(new Bundle {
      val en    = Input(Bool())
      val wmode = Input(Bool())
      val addr  = Input(UInt(12.W))
      val wdata = Input(UInt(64.W))
      val wmask = Input(UInt(8.W))
      val rdata = Output(UInt(64.W))  // get rdata next cycle after en
      val memenc_enable = Output(Bool())
      val keyextend_req = DecoupledIO(new Bundle {
        val key = UInt(128.W)
        val keyid = UInt(KeyIDBits.W)
        val enc_mode = Bool()         // 1:this keyid open enc  0:this keyid close enc
        val tweak_flage = Bool()      // 1:extend tweak key  0:extend keyid key
      })
      val randomio = new Bundle {
        val random_req = Output(Bool())
        val random_val = Input(Bool())
        val random_data = Input(Bool())
      }
  })
  // CSR
  val key_id             = RegInit(0.U(5.W)) // [4:0]
  val mode               = RegInit(0.U(2.W)) // [6:5]
  val tweak_flage        = RegInit(0.U(1.W)) // [7]
  val memenc_enable      = if (HasDelayNoencryption) RegInit(true.B) else RegInit(false.B) // [8]
  val memenc_enable_lock = RegInit(false.B)
  val random_ready_flag  = Wire(Bool()) // [32]
  val key_expansion_idle = Wire(Bool()) // [33]
  val last_req_accepted  = RegInit(false.B) // [34]
  val cfg_succesd        = Wire(Bool()) // [35]
  val key_init_req       = RegInit(false.B) // [63]
  // KEY0&1
  val key0               = RegInit(0.U(64.W))
  val key1               = RegInit(0.U(64.W))
  // RelPaddrBitsMap
  val relpaddrbitsmap    = ~0.U((PAddrBits - KeyIDBits).W)
  // KeyIDBitsMap
  val keyidbitsmap       = ~0.U(PAddrBits.W) - ~0.U((PAddrBits - KeyIDBits).W)
  // Version
  val memenc_version_p0  = (0x0001).U(16.W)
  val memenc_version_p1  = (0x0001).U(16.W)
  val memenc_version_p2  = (0x00000002).U(32.W)
  val memenc_version     = Cat(memenc_version_p0, memenc_version_p1, memenc_version_p2)

  // READ
  val rdata_reg = RegInit(0.U(64.W))
  when(io.en && !io.wmode && (io.addr(11,3) === 0.U)) {
    rdata_reg := Cat(0.U(28.W), cfg_succesd, last_req_accepted, key_expansion_idle, random_ready_flag, 0.U(23.W), memenc_enable, tweak_flage, mode, key_id)
  }.elsewhen(io.en && !io.wmode && (io.addr(11,3) === 3.U)) {
    rdata_reg := relpaddrbitsmap
  }.elsewhen(io.en && !io.wmode && (io.addr(11,3) === 4.U)) {
    rdata_reg := keyidbitsmap
  }.elsewhen(io.en && !io.wmode && (io.addr(11,3) === 5.U)) {
    rdata_reg := memenc_version
  }.otherwise {
    rdata_reg := 0.U
  }

  io.rdata := rdata_reg

  // WRITE
  val wmask_legal = (io.wmask === (0xff).U)

  when(io.en && io.wmode && wmask_legal && (io.addr(11,3) === 0.U)) {
    key_id := io.wdata(4,0)
    mode := io.wdata(6,5)
    tweak_flage := io.wdata(7)
    key_init_req := io.wdata(63).asBool
  }.otherwise {
    key_init_req := false.B
  }
  when(io.en && io.wmode && wmask_legal && (io.addr(11,3) === 0.U) && (!memenc_enable_lock)) {
    memenc_enable := io.wdata(8)
    memenc_enable_lock := true.B
  }
  when(io.en && io.wmode && wmask_legal && (io.addr(11,3) === 1.U)) {
    key0 := io.wdata
  }
  when(io.en && io.wmode && wmask_legal && (io.addr(11,3) === 2.U)) {
    key1 := io.wdata
  }
  io.memenc_enable := memenc_enable

  // RANDOM COLLECT
  val random_vec_data = RegInit(0.U(128.W))
  val random_cnt = RegInit(0.U(8.W))
  val random_key_init_done = Wire(Bool())
  io.randomio.random_req := random_cnt =/= 128.U(8.W)
  random_ready_flag := random_cnt === 128.U(8.W)

  when(io.randomio.random_req && io.randomio.random_val) {
    random_vec_data := Cat(random_vec_data(127,1), io.randomio.random_data)
  }

  when(random_ready_flag && random_key_init_done) {
    random_cnt := 0.U
  }.elsewhen(io.randomio.random_req && io.randomio.random_val) {
    random_cnt := random_cnt + 1.U
  }

  // KEY Extend Req
  key_expansion_idle := io.keyextend_req.ready
  cfg_succesd := io.keyextend_req.ready

  val keyextend_req_valid = RegInit(false.B)
  val req_leagl = Wire(Bool())
  req_leagl := (mode =/= 3.U(2.W)) && key_expansion_idle && ((mode =/= 2.U(2.W)) || random_ready_flag)

  when(key_init_req && req_leagl) {
    keyextend_req_valid := true.B
  }.elsewhen(io.keyextend_req.fire) {
    keyextend_req_valid := false.B
  }.otherwise {
    keyextend_req_valid := keyextend_req_valid
  }

  when(key_init_req && req_leagl) {
    last_req_accepted := true.B
  }.elsewhen(key_init_req) {
    last_req_accepted := false.B
  }.otherwise {
    last_req_accepted := last_req_accepted
  }

  random_key_init_done := io.keyextend_req.fire && (mode === 2.U(2.W))

  io.keyextend_req.valid := keyextend_req_valid
  io.keyextend_req.bits.key := Mux(mode === 1.U(2.W), Cat(key1, key0), random_vec_data)
  io.keyextend_req.bits.keyid := key_id
  io.keyextend_req.bits.enc_mode := mode =/= 0.U(2.W)
  io.keyextend_req.bits.tweak_flage := tweak_flage.asBool
}

class KeyTableEntry extends Bundle {
  val round_key_data = Vec(32, UInt(32.W))
  val encdec_mode = Bool()
}
class KeyTable(implicit p: Parameters) extends MemEncryptModule {
  val io = IO(new Bundle {
    val write_req = Input(new Bundle {
      val keyid = UInt(KeyIDBits.W)
      val keyid_valid = Input(Bool())
      val enc_mode = Input(Bool())        // 1: this keyid open enc, 0: this keyid close enc
      val round_id = UInt(5.W)
      val data = Input(UInt(32.W))
    })

  val enc_keyids = Input(Vec(MemencPipes, UInt(KeyIDBits.W)))
  val enc_round_keys = Output(Vec(MemencPipes, UInt((32*32/MemencPipes).W)))
  val dec_keyids = Input(Vec(MemencPipes, UInt(KeyIDBits.W)))
  val dec_round_keys = Output(Vec(MemencPipes, UInt((32*32/MemencPipes).W)))
  val dec = new Bundle {
    val keyid = Input(UInt(KeyIDBits.W))           // query dec_mode in advance in the AR channel
    val mode = Output(Bool())
  }
  val enc = new Bundle {
    val keyid = Input(UInt(KeyIDBits.W))           // query enc_mode in advance in the AW channel
    val mode = Output(Bool())
  }
})

  val init_entry = Wire(new KeyTableEntry)
  init_entry.round_key_data := DontCare // Keep round_key_data as default (uninitialized)
  if (HasDelayNoencryption) {
    init_entry.encdec_mode := true.B
  } else {
    init_entry.encdec_mode := false.B
  }
  val table = RegInit(VecInit(Seq.fill(1 << KeyIDBits)(init_entry)))
  val wire_enc_round_keys = Wire(Vec(MemencPipes, UInt((32*32/MemencPipes).W)))
  val wire_dec_round_keys = Wire(Vec(MemencPipes, UInt((32*32/MemencPipes).W)))

  // write and updata mode
  when(io.write_req.keyid_valid && io.write_req.enc_mode) {
    val entry = table(io.write_req.keyid)
    entry.encdec_mode := io.write_req.enc_mode
    entry.round_key_data(io.write_req.round_id) := io.write_req.data
  }
  when(io.write_req.keyid_valid && !io.write_req.enc_mode) {
    val entry = table(io.write_req.keyid)
    entry.encdec_mode := io.write_req.enc_mode
  }

// read logic
  for (i <- 0 until MemencPipes) {
    val enc_entry = table(io.enc_keyids(i))
    val enc_round_key_parts = VecInit(Seq.fill(32 / MemencPipes)(0.U(32.W)))
    for (j <- 0 until (32 / MemencPipes)) {
      enc_round_key_parts((32 / MemencPipes) - 1 - j) := enc_entry.round_key_data(i.U * (32 / MemencPipes).U + j.U)
    }
    wire_enc_round_keys(i) := enc_round_key_parts.reduce(Cat(_, _))

    val dec_entry = table(io.dec_keyids(i))
    val dec_round_key_parts = VecInit(Seq.fill(32 / MemencPipes)(0.U(32.W)))
    for (j <- 0 until (32 / MemencPipes)) {
      dec_round_key_parts((32 / MemencPipes) - 1 - j) := dec_entry.round_key_data(31.U - (i.U * (32 / MemencPipes).U + j.U))
    }
    wire_dec_round_keys(i) := dec_round_key_parts.reduce(Cat(_, _))
  }
  // output read data(round keys, enc/dec_mode, ar_mode, aw_mode)
  val dec_mode_entry = table(io.dec.keyid)
  io.dec.mode := dec_mode_entry.encdec_mode

  val enc_mode_entry = table(io.enc.keyid)
  io.enc.mode := enc_mode_entry.encdec_mode

  io.enc_round_keys := wire_enc_round_keys
  io.dec_round_keys := wire_dec_round_keys

}

class KeyExtender(implicit p: Parameters) extends MemEncryptModule{
  val io = IO(new Bundle {
      val keyextend_req = Flipped(DecoupledIO(new Bundle {
        val key = UInt(128.W)
        val keyid = UInt(KeyIDBits.W)
        val enc_mode = Bool()         // 1:this keyid open enc  0:this keyid close enc
        val tweak_flage = Bool()      // 1:extend tweak key  0:extend keyid key
      }))
      val tweak_round_keys = Output(Vec(32, UInt(32.W)))
      val enc_keyids = Input(Vec(MemencPipes, UInt(KeyIDBits.W)))
      val enc_round_keys = Output(Vec(MemencPipes, UInt((32*32/MemencPipes).W)))
      val dec_keyids = Input(Vec(MemencPipes, UInt(KeyIDBits.W)))
      val dec_round_keys = Output(Vec(MemencPipes, UInt((32*32/MemencPipes).W)))
      val dec = new Bundle {
        val keyid = Input(UInt(KeyIDBits.W))           // query dec_mode in advance in the AR channel
        val mode = Output(Bool())
      }
      val enc = new Bundle {
        val keyid = Input(UInt(KeyIDBits.W))           // query enc_mode in advance in the AW channel
        val mode = Output(Bool())
      }
  })

  val idle :: keyExpansion :: Nil = Enum(2)
  val current = RegInit(idle)
  val next = WireDefault(idle)
  current := next

  val count_round = RegInit(0.U(5.W))
  val reg_count_round = RegNext(count_round)
  val reg_user_key = RegInit(0.U(128.W))
  val data_for_round = Wire(UInt(128.W))
  val data_after_round = Wire(UInt(128.W))
  val reg_data_after_round = RegInit(0.U(128.W))
  val key_exp_finished_out = RegInit(1.U)
  val reg_key_valid = RegNext(io.keyextend_req.valid, false.B)
  val reg_tweak_round_keys = Reg(Vec(32, UInt(32.W)))


  switch(current) {
    is(idle) {
      when(!reg_key_valid && io.keyextend_req.valid && io.keyextend_req.bits.enc_mode) {
        next := keyExpansion
      }
    }
    is(keyExpansion) {
      when(reg_count_round === 31.U) {
        next := idle
      }.otherwise {
        next := keyExpansion
      }
    }
  }
    
  when(next === keyExpansion) {
    count_round := count_round + 1.U
  }.otherwise {
    count_round := 0.U
  }

  when(!reg_key_valid && io.keyextend_req.valid && io.keyextend_req.bits.enc_mode) {
    reg_user_key := io.keyextend_req.bits.key
  }

  when(current === keyExpansion && next === idle) {
    key_exp_finished_out := true.B
  }.elsewhen(io.keyextend_req.valid && io.keyextend_req.bits.enc_mode) {
    key_exp_finished_out := false.B
  } 
  io.keyextend_req.ready := key_exp_finished_out

  // Data for round calculation
  data_for_round := Mux(reg_count_round =/= 0.U, reg_data_after_round, reg_user_key)
  val cki = Module(new GetCKI)
  cki.io.countRoundIn := count_round
  val one_round = Module(new OneRoundForKeyExp)
  one_round.io.countRoundIn := reg_count_round
  one_round.io.dataIn := data_for_round
  one_round.io.ckParameterIn := cki.io.ckiOut
  data_after_round := one_round.io.resultOut

  when(current === keyExpansion) {
    reg_data_after_round := data_after_round
  }

  val keyTable = Module(new KeyTable())
    keyTable.io.write_req.keyid := io.keyextend_req.bits.keyid
    keyTable.io.write_req.enc_mode := io.keyextend_req.bits.enc_mode
    keyTable.io.write_req.round_id := reg_count_round
    keyTable.io.write_req.data := data_after_round(31, 0)

    keyTable.io.enc_keyids := io.enc_keyids
    keyTable.io.dec_keyids := io.dec_keyids
    keyTable.io.dec.keyid := io.dec.keyid
    keyTable.io.enc.keyid := io.enc.keyid
    io.dec.mode := keyTable.io.dec.mode
    io.enc.mode := keyTable.io.enc.mode
    io.enc_round_keys := keyTable.io.enc_round_keys
    io.dec_round_keys := keyTable.io.dec_round_keys


  when(io.keyextend_req.bits.tweak_flage) {
    reg_tweak_round_keys(reg_count_round) := data_after_round(31, 0)
    keyTable.io.write_req.keyid_valid := false.B
  }.otherwise {
    keyTable.io.write_req.keyid_valid := current
  }
  io.tweak_round_keys := reg_tweak_round_keys
}

class AXI4MemEncrypt(address: AddressSet)(implicit p: Parameters) extends LazyModule with Memconsts
{
  require (isPow2(MemencPipes), s"AXI4MemEncrypt: MemencPipes must be a power of two, not $MemencPipes")
  require (PAddrBits > KeyIDBits, s"AXI4MemEncrypt: PAddrBits must be greater than KeyIDBits")

  val node = AXI4AdapterNode(
    masterFn = { mp => 
      val new_idbits = log2Ceil(mp.endId) + 1
      // Create one new "master" per ID
      val masters = Array.tabulate(1 << new_idbits) { i => AXI4MasterParameters(
         name      = "",
         id        = IdRange(i, i+1),
         aligned   = true,
         maxFlight = Some(0))
      }
      // Accumulate the names of masters we squish
      val names = Array.fill(1 << new_idbits) { new scala.collection.mutable.HashSet[String]() }
      // Squash the information from original masters into new ID masters
      mp.masters.foreach { m =>
        for (i <- 0 until (1 << new_idbits)) {
          val accumulated = masters(i)
          names(i) += m.name
          masters(i) = accumulated.copy(
            aligned   = accumulated.aligned && m.aligned,
            maxFlight = accumulated.maxFlight.flatMap { o => m.maxFlight.map { n => o+n } })
        }
      }
      val finalNameStrings = names.map { n => if (n.isEmpty) "(unused)" else n.toList.mkString(", ") }
      mp.copy(masters = masters.toIndexedSeq.zip(finalNameStrings.toIndexedSeq).map { case (m, n) => m.copy(name = n) })
    },
    slaveFn  = { sp => sp })

  val device = new SimpleDevice("mem-encrypt-unit", Seq("iie,memencrypt0"))
  val ctrl_node = APBSlaveNode(Seq(APBSlavePortParameters(
    Seq(APBSlaveParameters(
      address       = List(address),
      resources     = device.reg,
      device        = Some(device),
      regionType    = RegionType.IDEMPOTENT)),
    beatBytes     = 8)))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val random_req = Output(Bool())
      val random_val = Input(Bool())
      val random_data = Input(Bool())
    })

    val en    = Wire(Bool())
    val wmode = Wire(Bool())
    val addr  = Wire(UInt(12.W))
    val wdata = Wire(UInt(64.W))
    val wmask = Wire(UInt(8.W))
    val rdata = Wire(UInt(64.W))  // get rdata next cycle after en

    (ctrl_node.in) foreach { case (ctrl_in, _) =>
      en    := ctrl_in.psel && !ctrl_in.penable
      wmode := ctrl_in.pwrite
      addr  := ctrl_in.paddr(11, 0)
      wdata := ctrl_in.pwdata
      wmask := ctrl_in.pstrb
      ctrl_in.pready  := true.B
      ctrl_in.pslverr := false.B
      ctrl_in.prdata  := rdata
    }

    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      require (edgeIn.bundle.dataBits == 256, s"AXI4MemEncrypt: edgeIn dataBits must be 256")
      require (edgeOut.bundle.dataBits == 256, s"AXI4MemEncrypt: edgeOut dataBits must be 256")
      
      val memencParams: Parameters = p.alterPartial {
        case MemcEdgeInKey => edgeIn
        case MemcEdgeOutKey => edgeOut
      }
      // -------------------------------------
      // MemEncrypt Config and State Registers
      // -------------------------------------
      val memenc_enable = Wire(Bool())
      val memencrypt_csr = Module(new MemEncryptCSR()(memencParams))
      memencrypt_csr.io.en    := en
      memencrypt_csr.io.wmode := wmode
      memencrypt_csr.io.addr  := addr
      memencrypt_csr.io.wdata := wdata
      memencrypt_csr.io.wmask := wmask
      memenc_enable := memencrypt_csr.io.memenc_enable
      rdata := memencrypt_csr.io.rdata

      io.random_req := memencrypt_csr.io.randomio.random_req
      memencrypt_csr.io.randomio.random_val := io.random_val
      memencrypt_csr.io.randomio.random_data := io.random_data

      // -------------------------------------
      // Key Extender & Round Key Lookup Table
      // -------------------------------------
      val key_extender = Module(new KeyExtender()(memencParams))
      key_extender.io.keyextend_req :<>= memencrypt_csr.io.keyextend_req

      // -------------------
      // AXI4 chanel B 
      // -------------------
      Connectable.waiveUnmatched(in.b, out.b) match {
        case (lhs, rhs) => lhs.squeezeAll :<>= rhs.squeezeAll
      }

      val write_route = Module(new WriteChanelRoute()(memencParams))
      val aw_tweakenc = Module(new TweakEncrptyQueue()(memencParams))
      val waddr_q = Module(new IrrevocableQueue(chiselTypeOf(in.aw.bits), entries = MemencPipes+1))
      val wdata_q = Module(new IrrevocableQueue(chiselTypeOf(in.w.bits), entries = MemencPipes+1))
      val write_machine = Module(new AXI4WriteMachine()(memencParams))
      val axi4w_kt_q = Module(new Queue(new AXI4W_KT(false)(memencParams), entries = 2, flow = true))
      val wdata_encpipe = Module(new WdataEncrptyPipe()(memencParams))
      val write_arb = Module(new WriteChanelArbiter()(memencParams))

      // -------------------
      // AXI4 Write Route
      // Unencrypt & Encrypt
      // -------------------
      write_route.io.memenc_enable := memenc_enable
      key_extender.io.enc.keyid := write_route.io.enc_keyid
      write_route.io.enc_mode := key_extender.io.enc.mode

      write_route.io.in.aw :<>= in.aw
      write_route.io.in.w :<>= in.w

      val unenc_aw = write_route.io.out0.aw
      val unenc_w = write_route.io.out0.w
      val pre_enc_aw = write_route.io.out1.aw
      val pre_enc_w = write_route.io.out1.w

      // -------------------
      // AXI4 chanel AW
      // -------------------
      pre_enc_aw.ready := waddr_q.io.enq.ready && aw_tweakenc.io.enq.ready
      waddr_q.io.enq.valid := pre_enc_aw.valid && aw_tweakenc.io.enq.ready
      aw_tweakenc.io.enq.valid := pre_enc_aw.valid && waddr_q.io.enq.ready

      waddr_q.io.enq.bits := pre_enc_aw.bits
      waddr_q.io.enq.bits.addr := pre_enc_aw.bits.addr(PAddrBits-KeyIDBits-1, 0)
      aw_tweakenc.io.enq.bits.addr := pre_enc_aw.bits.addr
      aw_tweakenc.io.enq.bits.len := pre_enc_aw.bits.len
      aw_tweakenc.io.tweak_round_keys := key_extender.io.tweak_round_keys

      // -------------------
      // AXI4 chanel W
      // -------------------
      wdata_q.io.enq :<>= pre_enc_w
      write_machine.io.in_w :<>= wdata_q.io.deq
      write_machine.io.in_kt :<>= aw_tweakenc.io.deq
      axi4w_kt_q.io.enq :<>= write_machine.io.out_w
      wdata_encpipe.io.in_w :<>= axi4w_kt_q.io.deq
      key_extender.io.enc_keyids := wdata_encpipe.io.enc_keyids
      wdata_encpipe.io.enc_round_keys := key_extender.io.enc_round_keys

      // -------------------
      // AXI4 Write Arbiter
      // Unencrypt & Encrypt
      // -------------------
      write_arb.io.in0.aw :<>= unenc_aw
      write_arb.io.in0.aw.bits.addr := unenc_aw.bits.addr(PAddrBits-KeyIDBits-1, 0)
      write_arb.io.in0.w :<>= unenc_w

      write_arb.io.in1.aw.valid := waddr_q.io.deq.valid && (waddr_q.io.deq.bits.len =/=0.U || write_machine.io.uncache_en)
      waddr_q.io.deq.ready := write_arb.io.in1.aw.ready && (waddr_q.io.deq.bits.len =/=0.U || write_machine.io.uncache_en)
      write_machine.io.uncache_commit := write_arb.io.in1.aw.fire
      write_arb.io.in1.aw.bits := waddr_q.io.deq.bits
      write_arb.io.in1.w :<>= wdata_encpipe.io.out_w

      out.aw :<>= write_arb.io.out.aw
      out.w  :<>= write_arb.io.out.w

      val ar_arb = Module(new IrrevocableArbiter(chiselTypeOf(out.ar.bits), 2))
      val ar_tweakenc = Module(new TweakEncrptyTable()(memencParams))
      val read_machine = Module(new AXI4ReadMachine()(memencParams))
      val axi4r_kt_q = Module(new Queue(new AXI4R_KT(false)(memencParams), entries = 2, flow = true))
      val pre_dec_rdata_route = Module(new RdataChanelRoute()(memencParams))
      val rdata_decpipe = Module(new RdataDecrptyPipe()(memencParams))
      val r_arb = Module(new IrrevocableArbiter(chiselTypeOf(out.r.bits), 2))
      val post_dec_rdata_route = Module(new RdataRoute()(memencParams))

      // -------------------
      // AXI4 chanel AR
      // -------------------
      ar_arb.io.in(0) :<>= write_machine.io.out_ar
      // DecoupledIO connect IrrevocableIO
      ar_arb.io.in(1).valid := in.ar.valid
      ar_arb.io.in(1).bits := in.ar.bits
      in.ar.ready := ar_arb.io.in(1).ready

      ar_arb.io.out.ready := out.ar.ready && ar_tweakenc.io.enq.ready

      ar_tweakenc.io.enq.valid := ar_arb.io.out.valid && out.ar.ready
      ar_tweakenc.io.enq.bits.addr := ar_arb.io.out.bits.addr
      ar_tweakenc.io.enq.bits.len := ar_arb.io.out.bits.len
      ar_tweakenc.io.enq.bits.id := ar_arb.io.out.bits.id
      ar_tweakenc.io.tweak_round_keys := key_extender.io.tweak_round_keys
      ar_tweakenc.io.memenc_enable := memenc_enable
      key_extender.io.dec.keyid := ar_tweakenc.io.dec_keyid
      ar_tweakenc.io.dec_mode := key_extender.io.dec.mode

      out.ar.valid := ar_arb.io.out.valid && ar_tweakenc.io.enq.ready
      out.ar.bits := ar_arb.io.out.bits
      out.ar.bits.addr := ar_arb.io.out.bits.addr(PAddrBits-KeyIDBits-1, 0)

      // -------------------
      // AXI4 Rdata Route
      // Unencrypt & Encrypt
      // -------------------
      pre_dec_rdata_route.io.in_r :<>= out.r
      ar_tweakenc.io.dec_r.id := pre_dec_rdata_route.io.dec_rid
      pre_dec_rdata_route.io.dec_mode := ar_tweakenc.io.dec_r.mode

      val undec_r = pre_dec_rdata_route.io.out_r0
      val pre_dec_r = pre_dec_rdata_route.io.out_r1

      // -------------------
      // AXI4 chanel R
      // -------------------
      read_machine.io.in_r :<>= pre_dec_r
      ar_tweakenc.io.req :<>= read_machine.io.kt_req
      read_machine.io.in_kt :<>= ar_tweakenc.io.resp
      axi4r_kt_q.io.enq :<>= read_machine.io.out_r
      rdata_decpipe.io.in_r :<>= axi4r_kt_q.io.deq
      key_extender.io.dec_keyids := rdata_decpipe.io.dec_keyids
      rdata_decpipe.io.dec_round_keys := key_extender.io.dec_round_keys

      // -------------------
      // AXI4 Rdata Arbiter
      // Unencrypt & Encrypt
      // -------------------
      r_arb.io.in(0) :<>= undec_r
      r_arb.io.in(1) :<>= rdata_decpipe.io.out_r

      post_dec_rdata_route.io.in_r :<>= r_arb.io.out
      write_machine.io.in_r :<>= post_dec_rdata_route.io.out_r1
      in.r :<>= post_dec_rdata_route.io.out_r0
    }
  }
}

object AXI4MemEncrypt
{
  def apply(address: AddressSet)(implicit p: Parameters): AXI4Node =
  {
    val axi4memenc = LazyModule(new AXI4MemEncrypt(address))
    axi4memenc.node
  }
}
