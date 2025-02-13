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
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.tilelink.AXI4TLState

class IrrevocableQueue[T <: Data](gen: T, entries: Int, flow: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Irrevocable(gen))
    val deq = Irrevocable(gen)
  })
  val queue = Module(new Queue(gen, entries = entries, flow = flow))

  queue.io.enq.valid := io.enq.valid
  queue.io.enq.bits  := io.enq.bits
  io.enq.ready := queue.io.enq.ready

  io.deq.valid := queue.io.deq.valid
  io.deq.bits  := queue.io.deq.bits
  queue.io.deq.ready := io.deq.ready
}

class IrrevocableArbiter[T <: Data](gen: T, n: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(n, Irrevocable(gen)))
    val out = Irrevocable(gen)
  })

  val decoupledIn = io.in.map { irrevocable =>
    val decoupled = Wire(Decoupled(gen))
    decoupled.valid := irrevocable.valid
    decoupled.bits  := irrevocable.bits
    irrevocable.ready := decoupled.ready
    decoupled
  }

  val arbiter = Module(new Arbiter(gen, n))
  arbiter.io.in <> decoupledIn

  io.out.valid := arbiter.io.out.valid
  io.out.bits  := arbiter.io.out.bits
  arbiter.io.out.ready := io.out.ready
}

// CKI (Cipher Key Input) is a constant input used in the SM4 encryption algorithm.
// It is part of the key expansion process and participates in generating subkeys.
// During each round of the key expansion, the CKI value is mixed with other constants and
// the initial key to enhance the security of the encryption algorithm.
class GetCKI extends Module {
  val io = IO(new Bundle {
    val countRoundIn = Input(UInt(5.W))
    val ckiOut = Output(UInt(32.W))
  })
  val ckiOutReg= RegInit(0.U(32.W))
  // 32 32-bit CKI constant values
  val ckiValuesVec = VecInit(Seq(
    "h00070e15".U, "h1c232a31".U, "h383f464d".U, "h545b6269".U,
    "h70777e85".U, "h8c939aa1".U, "ha8afb6bd".U, "hc4cbd2d9".U,
    "he0e7eef5".U, "hfc030a11".U, "h181f262d".U, "h343b4249".U,
    "h50575e65".U, "h6c737a81".U, "h888f969d".U, "ha4abb2b9".U,
    "hc0c7ced5".U, "hdce3eaf1".U, "hf8ff060d".U, "h141b2229".U,
    "h30373e45".U, "h4c535a61".U, "h686f767d".U, "h848b9299".U,
    "ha0a7aeb5".U, "hbcc3cad1".U, "hd8dfe6ed".U, "hf4fb0209".U,
    "h10171e25".U, "h2c333a41".U, "h484f565d".U, "h646b7279".U
  ))
  when(io.countRoundIn < 32.U) {
    ckiOutReg := ckiValuesVec(io.countRoundIn)
  }.otherwise {
    ckiOutReg := 0.U
  }
  io.ckiOut := ckiOutReg
}


// S-box is used in SM4 for nonlinear transformations during encryption processes. 
// SM4 uses a fixed 256 byte S-box for byte replacement. 
// This replacement process is achieved by replacing the input 8-bit data
// with the corresponding values in the S-box lookup table.
class SboxReplace extends Module {
  val io = IO(new Bundle {
    val dataIn = Input(UInt(8.W))
    val resultOut = Output(UInt(8.W))
  })
  // A 256 element S-box lookup table, where each element is an 8-bit hexadecimal constant
  val sbox = VecInit(Seq(
    0xd6.U, 0x90.U, 0xe9.U, 0xfe.U, 0xcc.U, 0xe1.U, 0x3d.U, 0xb7.U, 0x16.U, 0xb6.U, 0x14.U, 0xc2.U, 0x28.U, 0xfb.U, 0x2c.U, 0x05.U,
    0x2b.U, 0x67.U, 0x9a.U, 0x76.U, 0x2a.U, 0xbe.U, 0x04.U, 0xc3.U, 0xaa.U, 0x44.U, 0x13.U, 0x26.U, 0x49.U, 0x86.U, 0x06.U, 0x99.U,
    0x9c.U, 0x42.U, 0x50.U, 0xf4.U, 0x91.U, 0xef.U, 0x98.U, 0x7a.U, 0x33.U, 0x54.U, 0x0b.U, 0x43.U, 0xed.U, 0xcf.U, 0xac.U, 0x62.U,
    0xe4.U, 0xb3.U, 0x1c.U, 0xa9.U, 0xc9.U, 0x08.U, 0xe8.U, 0x95.U, 0x80.U, 0xdf.U, 0x94.U, 0xfa.U, 0x75.U, 0x8f.U, 0x3f.U, 0xa6.U,
    0x47.U, 0x07.U, 0xa7.U, 0xfc.U, 0xf3.U, 0x73.U, 0x17.U, 0xba.U, 0x83.U, 0x59.U, 0x3c.U, 0x19.U, 0xe6.U, 0x85.U, 0x4f.U, 0xa8.U,
    0x68.U, 0x6b.U, 0x81.U, 0xb2.U, 0x71.U, 0x64.U, 0xda.U, 0x8b.U, 0xf8.U, 0xeb.U, 0x0f.U, 0x4b.U, 0x70.U, 0x56.U, 0x9d.U, 0x35.U,
    0x1e.U, 0x24.U, 0x0e.U, 0x5e.U, 0x63.U, 0x58.U, 0xd1.U, 0xa2.U, 0x25.U, 0x22.U, 0x7c.U, 0x3b.U, 0x01.U, 0x21.U, 0x78.U, 0x87.U,
    0xd4.U, 0x00.U, 0x46.U, 0x57.U, 0x9f.U, 0xd3.U, 0x27.U, 0x52.U, 0x4c.U, 0x36.U, 0x02.U, 0xe7.U, 0xa0.U, 0xc4.U, 0xc8.U, 0x9e.U,
    0xea.U, 0xbf.U, 0x8a.U, 0xd2.U, 0x40.U, 0xc7.U, 0x38.U, 0xb5.U, 0xa3.U, 0xf7.U, 0xf2.U, 0xce.U, 0xf9.U, 0x61.U, 0x15.U, 0xa1.U,
    0xe0.U, 0xae.U, 0x5d.U, 0xa4.U, 0x9b.U, 0x34.U, 0x1a.U, 0x55.U, 0xad.U, 0x93.U, 0x32.U, 0x30.U, 0xf5.U, 0x8c.U, 0xb1.U, 0xe3.U,
    0x1d.U, 0xf6.U, 0xe2.U, 0x2e.U, 0x82.U, 0x66.U, 0xca.U, 0x60.U, 0xc0.U, 0x29.U, 0x23.U, 0xab.U, 0x0d.U, 0x53.U, 0x4e.U, 0x6f.U,
    0xd5.U, 0xdb.U, 0x37.U, 0x45.U, 0xde.U, 0xfd.U, 0x8e.U, 0x2f.U, 0x03.U, 0xff.U, 0x6a.U, 0x72.U, 0x6d.U, 0x6c.U, 0x5b.U, 0x51.U,
    0x8d.U, 0x1b.U, 0xaf.U, 0x92.U, 0xbb.U, 0xdd.U, 0xbc.U, 0x7f.U, 0x11.U, 0xd9.U, 0x5c.U, 0x41.U, 0x1f.U, 0x10.U, 0x5a.U, 0xd8.U,
    0x0a.U, 0xc1.U, 0x31.U, 0x88.U, 0xa5.U, 0xcd.U, 0x7b.U, 0xbd.U, 0x2d.U, 0x74.U, 0xd0.U, 0x12.U, 0xb8.U, 0xe5.U, 0xb4.U, 0xb0.U,
    0x89.U, 0x69.U, 0x97.U, 0x4a.U, 0x0c.U, 0x96.U, 0x77.U, 0x7e.U, 0x65.U, 0xb9.U, 0xf1.U, 0x09.U, 0xc5.U, 0x6e.U, 0xc6.U, 0x84.U,
    0x18.U, 0xf0.U, 0x7d.U, 0xec.U, 0x3a.U, 0xdc.U, 0x4d.U, 0x20.U, 0x79.U, 0xee.U, 0x5f.U, 0x3e.U, 0xd7.U, 0xcb.U, 0x39.U, 0x48.U
  ))

  io.resultOut := sbox(io.dataIn)
}

// Nonlinear Transformation in Data Encryption Process
class TransformForEncDec extends Module {
  val io = IO(new Bundle {
    val data_in = Input(UInt(32.W))
    val result_out = Output(UInt(32.W))
  })

  val bytes_in = VecInit(Seq(io.data_in(7, 0), io.data_in(15, 8), io.data_in(23, 16), io.data_in(31, 24)))
  val bytes_replaced = Wire(Vec(4, UInt(8.W)))
  val word_replaced = Wire(UInt(32.W))

  val sbox_replace_modules = VecInit(Seq.fill(4)(Module(new SboxReplace).io))
  for (i <- 0 until 4) {
    sbox_replace_modules(i).dataIn := bytes_in(i)
    bytes_replaced(i) := sbox_replace_modules(i).resultOut
  }

  word_replaced := Cat(bytes_replaced.reverse)

  io.result_out := ((word_replaced ^ Cat(word_replaced(29, 0), word_replaced(31, 30))) ^
                      (Cat(word_replaced(21, 0), word_replaced(31, 22)) ^ Cat(word_replaced(13, 0), word_replaced(31, 14)))) ^
                     Cat(word_replaced(7, 0), word_replaced(31, 8))
}


// Nonlinear Transformation in Key Expansion Process
class TransformForKeyExp extends Module {
  val io = IO(new Bundle {
    val data_in = Input(UInt(32.W))
    val data_after_linear_key_out = Output(UInt(32.W))
  })
  val bytes_in = VecInit(Seq(io.data_in(7, 0), io.data_in(15, 8), io.data_in(23, 16), io.data_in(31, 24)))
  val bytes_replaced = Wire(Vec(4, UInt(8.W)))
  val word_replaced = Wire(UInt(32.W))

  val sbox_replace_modules = VecInit(Seq.fill(4)(Module(new SboxReplace).io))
  for (i <- 0 until 4) {
    sbox_replace_modules(i).dataIn := bytes_in(i)
    bytes_replaced(i) := sbox_replace_modules(i).resultOut
  }

  word_replaced := Cat(bytes_replaced.reverse)

  io.data_after_linear_key_out := (word_replaced ^ Cat(word_replaced(18, 0), word_replaced(31, 19))) ^ Cat(word_replaced(8, 0), word_replaced(31, 9))
}

// The key expansion algorithm requires a total of 32 rounds of operations, including one round of operation
class OneRoundForKeyExp extends Module {
  val io = IO(new Bundle {
    val countRoundIn = Input(UInt(5.W))
    val dataIn = Input(UInt(128.W))
    val ckParameterIn = Input(UInt(32.W))
    val resultOut = Output(UInt(128.W))
  })
  // In key expansion, the first step is to XOR each word of the original key with the system parameter to obtain four new words.
  // system parameter: FK0, FK1, FK2, FK3.
  val FK0 = "ha3b1bac6".U
  val FK1 = "h56aa3350".U
  val FK2 = "h677d9197".U
  val FK3 = "hb27022dc".U

  val word = VecInit(Seq(io.dataIn(127, 96), io.dataIn(95, 64), io.dataIn(63, 32), io.dataIn(31, 0)))

  val k0 = word(0) ^ FK0
  val k1 = word(1) ^ FK1
  val k2 = word(2) ^ FK2
  val k3 = word(3) ^ FK3


  val dataForXor = io.ckParameterIn
  val tmp0 = Mux(io.countRoundIn === 0.U, k1 ^ k2, word(1) ^ word(2))
  val tmp1 = Mux(io.countRoundIn === 0.U, k3 ^ dataForXor, word(3) ^ dataForXor)
  val dataForTransform = tmp0 ^ tmp1

  val transformKey = Module(new TransformForKeyExp)
  transformKey.io.data_in := dataForTransform

  io.resultOut := Mux(io.countRoundIn === 0.U,
                      Cat(k1, k2, k3, transformKey.io.data_after_linear_key_out ^ k0),
                      Cat(word(1), word(2), word(3), transformKey.io.data_after_linear_key_out ^ word(0)))
}

// The SM4 encryption algorithm requires a total of 32 rounds of operations, including one round of operation
class OneRoundForEncDec  extends Module {
  val io = IO(new Bundle {
    val data_in = Input(UInt(128.W))
    val round_key_in = Input(UInt(32.W))
    val result_out = Output(UInt(128.W))
  })

  val word = VecInit(Seq(io.data_in(127, 96), io.data_in(95, 64), io.data_in(63, 32), io.data_in(31, 0)))

  val tmp0 = word(1) ^ word(2)
  val tmp1 = word(3) ^ io.round_key_in
  val data_for_transform = tmp0 ^ tmp1

  val transform_encdec = Module(new TransformForEncDec)
  transform_encdec.io.data_in := data_for_transform

  io.result_out := Cat(word(1), word(2), word(3), transform_encdec.io.result_out ^ word(0))
}



class AXI4BundleWWithoutData(params: AXI4BundleParameters) extends Bundle {
  val strb = UInt((params.dataBits/8).W)
  val last = Bool()
  val user = BundleMap(params.requestFields.filter(_.key.isData))
}

class AXI4BundleRWithoutData(params: AXI4BundleParameters) extends Bundle {
  val id   = UInt(params.idBits.W)
  val resp = UInt(params.respBits.W)
  val user = BundleMap(params.responseFields) 
  val echo = BundleMap(params.echoFields)
  val last = Bool()
}

// OnePipeEncBase is an abstract class that defines the structure of a single-pipe encryption module.
// The main purpose of this class is to standardize the input and output interfaces for encryption modules.
abstract class OnePipeEncBase(implicit p: Parameters) extends MemEncryptModule {
  val io = IO(new Bundle {
    val onepipe_in = new Bundle {
      val keyid = Input(UInt(KeyIDBits.W))
      val data_in = Input(UInt(128.W))
      val tweak_in = Input(UInt(128.W))
      val axi4_other = Input(new AXI4BundleWWithoutData(MemcedgeIn.bundle))
      val round_key_in = Input(Vec(32/MemencPipes, UInt(32.W))) 
    }
    val onepipe_out = new Bundle {
      val result_out = Output(UInt(128.W))
      val axi4_other_out = Output(new AXI4BundleWWithoutData(MemcedgeIn.bundle))
      val tweak_out = Output(UInt(128.W))
      val keyid_out = Output(UInt(5.W))
    }
  })
}

// The OnePipeForEnc module needs to actually perform the encryption process for each 
// level of the pipeline in the encryption pipeline. 
// The flow level can be customized and configured.
class OnePipeForEnc(implicit p: Parameters) extends OnePipeEncBase {

  val OneRoundForEncDecs = Seq.fill(32/MemencPipes)(Module(new OneRoundForEncDec))

  for (i <- 0 until 32/MemencPipes) {
    val mod = OneRoundForEncDecs(i)
    mod.io.round_key_in := io.onepipe_in.round_key_in(i)
    if (i == 0) mod.io.data_in := io.onepipe_in.data_in else mod.io.data_in := OneRoundForEncDecs(i - 1).io.result_out
  }
  
  io.onepipe_out.result_out := OneRoundForEncDecs.last.io.result_out
  io.onepipe_out.keyid_out := io.onepipe_in.keyid
  io.onepipe_out.axi4_other_out := io.onepipe_in.axi4_other
  io.onepipe_out.tweak_out := io.onepipe_in.tweak_in

}
// The encryption process of each stage in the encryption pipeline does not require
// the OnePipeForEnc module to actually execute the encryption process. 
// Test usage
class OnePipeForEncNoEnc(implicit p: Parameters) extends OnePipeEncBase {
  io.onepipe_out.result_out := io.onepipe_in.data_in
  io.onepipe_out.keyid_out := io.onepipe_in.keyid
  io.onepipe_out.tweak_out := io.onepipe_in.tweak_in
  io.onepipe_out.axi4_other_out := io.onepipe_in.axi4_other
}

abstract class OnePipeDecBase(implicit p: Parameters) extends MemEncryptModule {
  val io = IO(new Bundle {
    val onepipe_in = new Bundle {
      val keyid = Input(UInt(KeyIDBits.W))
      val data_in = Input(UInt(128.W))
      val tweak_in = Input(UInt(128.W))
      val axi4_other = Input(new AXI4BundleRWithoutData(MemcedgeOut.bundle))
      val round_key_in = Input(Vec(32/MemencPipes, UInt(32.W))) 
    }
    val onepipe_out = new Bundle {
      val result_out = Output(UInt(128.W))
      val axi4_other_out = Output(new AXI4BundleRWithoutData(MemcedgeOut.bundle))
      val tweak_out = Output(UInt(128.W))
      val keyid_out = Output(UInt(5.W))
    }
  })
}
class OnePipeForDec(implicit p: Parameters) extends OnePipeDecBase {

  val OneRoundForEncDecs = Seq.fill(32/MemencPipes)(Module(new OneRoundForEncDec))

  for (i <- 0 until 32/MemencPipes) {
    val mod = OneRoundForEncDecs(i)
    mod.io.round_key_in := io.onepipe_in.round_key_in(i)
    if (i == 0) mod.io.data_in := io.onepipe_in.data_in else mod.io.data_in := OneRoundForEncDecs(i - 1).io.result_out
  }
  
  io.onepipe_out.result_out := OneRoundForEncDecs.last.io.result_out
  io.onepipe_out.keyid_out := io.onepipe_in.keyid
  io.onepipe_out.axi4_other_out := io.onepipe_in.axi4_other
  io.onepipe_out.tweak_out := io.onepipe_in.tweak_in

}
class OnePipeForDecNoDec(implicit p: Parameters) extends OnePipeDecBase {
  io.onepipe_out.result_out := io.onepipe_in.data_in
  io.onepipe_out.keyid_out := io.onepipe_in.keyid
  io.onepipe_out.axi4_other_out := io.onepipe_in.axi4_other
  io.onepipe_out.tweak_out := io.onepipe_in.tweak_in
}

// Finite field operations after encrypting tweak (encryption adjustment value) in XTS confidential mode
// Encryption adjustment value (tweak), This encryption adjustment utilizes finite fields and XOR operations to 
// ensure security by preventing the same ciphertext from being obtained even if the packet is identical each time.

// Calculation process: 
// Move the logic one bit to the left. If the highest bit that is moved out is 1, XOR the lower 8 bits 0x87 three times, 
// generating four different sets of data for XOR before and after data encryption;
class GF128 extends Module{
  val io = IO(new Bundle {
    val tweak_in = Input(UInt(128.W))
    val tweak_out = Output(UInt(512.W))
  })

  val gf_128_fdbk = "h87".U(8.W)
  val tweak_1_isgf = io.tweak_in(127)
  val tweak_2_isgf = io.tweak_in(126)
  val tweak_3_isgf = io.tweak_in(125)
  val tweak_1_shifted = Wire(UInt(128.W))
  val tweak_2_shifted = Wire(UInt(128.W))
  val tweak_3_shifted = Wire(UInt(128.W))
  val tweak_1_out = Wire(UInt(128.W))
  val tweak_2_out = Wire(UInt(128.W))
  val tweak_3_out = Wire(UInt(128.W))

  tweak_1_shifted := io.tweak_in << 1
  tweak_2_shifted := tweak_1_out << 1
  tweak_3_shifted := tweak_2_out << 1

  tweak_1_out := Mux(tweak_1_isgf, tweak_1_shifted ^ gf_128_fdbk, tweak_1_shifted)
  tweak_2_out := Mux(tweak_2_isgf, tweak_2_shifted ^ gf_128_fdbk, tweak_2_shifted)
  tweak_3_out := Mux(tweak_3_isgf, tweak_3_shifted ^ gf_128_fdbk, tweak_3_shifted)

  io.tweak_out := Cat(tweak_3_out, tweak_2_out, tweak_1_out, io.tweak_in)
}

// Perform finite field operations on the initial tweak during the request sending process, 
// and output according to the requirements (aw. len)
class TweakGF128(implicit p: Parameters) extends MemEncryptModule{
    val io = IO(new Bundle {
      val req = Flipped(DecoupledIO(new Bundle {
        val len = UInt(MemcedgeIn.bundle.lenBits.W)
        val addr = UInt(PAddrBits.W)
        val tweak_in = UInt(128.W)
      }))
      val resp = DecoupledIO(new Bundle {
        val tweak_out = UInt(256.W)
        val keyid_out = UInt(KeyIDBits.W)
        val addr_out = UInt(PAddrBits.W)
      })
    })
    val tweak_gf128 = Module(new GF128())
    tweak_gf128.io.tweak_in := io.req.bits.tweak_in
    
    val reg_valid = RegInit(false.B)
    val reg_counter = RegInit(0.U(2.W))
    val reg_len = RegInit(0.U(MemcedgeIn.bundle.lenBits.W))
    val reg_addr = RegInit(0.U(PAddrBits.W))
    val reg_tweak_result = RegInit(0.U(512.W))

    io.req.ready := !reg_valid || (reg_valid && io.resp.ready && (reg_len === 0.U || reg_counter =/= 0.U))

    when(io.req.fire) {
      reg_tweak_result := tweak_gf128.io.tweak_out
      reg_len := io.req.bits.len
      reg_addr := io.req.bits.addr
      reg_valid := true.B
      reg_counter := 0.U
    }.elsewhen(reg_valid && io.resp.ready) {
      when(reg_len === 0.U) {
        reg_valid := false.B
        reg_counter := 0.U
      }.otherwise {
        when(reg_counter === 0.U) {
          reg_counter := reg_counter + 1.U
        }.otherwise {
          reg_valid := false.B
          reg_counter := 0.U
        }
      }
    }.otherwise {
      reg_valid := reg_valid
      reg_counter := reg_counter
    }


    io.resp.bits.addr_out  := reg_addr
    io.resp.bits.keyid_out := reg_addr(PAddrBits - 1, PAddrBits - KeyIDBits)
    io.resp.bits.tweak_out := Mux(reg_len === 0.U, Mux(reg_addr(5) === 0.U, reg_tweak_result(255, 0), reg_tweak_result(511, 256)),
                                  Mux(reg_counter === 0.U, reg_tweak_result(255, 0), reg_tweak_result(511, 256)))
    io.resp.valid          := reg_valid
}

// The encryption process in each stage of the pipeline during the initial tweak encryption process
class OnePipeForTweakEnc(implicit p: Parameters) extends MemEncryptModule {
    val io = IO(new Bundle {
      val in = new Bundle {
        val data_in = Input(UInt(128.W))
        val addr_in = Input(UInt(PAddrBits.W))
        val len_in = Input(UInt(MemcedgeOut.bundle.lenBits.W))
        val id_in = Input(UInt(MemcedgeOut.bundle.idBits.W))
        val round_key_in = Input(Vec(32/MemencPipes, UInt(32.W)))
      }
      val out = new Bundle {
        val result_out = Output(UInt(128.W))
        val addr_out = Output(UInt(PAddrBits.W))
        val len_out = Output(UInt(MemcedgeOut.bundle.lenBits.W))
        val id_out = Output(UInt(MemcedgeOut.bundle.idBits.W))
      }
  })

  val OneRoundForEncDecs = Seq.fill(32/MemencPipes)(Module(new OneRoundForEncDec))
  for (i <- 0 until 32/MemencPipes) {
    val mod = OneRoundForEncDecs(i)
    mod.io.round_key_in := io.in.round_key_in(i)
    if (i == 0) mod.io.data_in := io.in.data_in else mod.io.data_in := OneRoundForEncDecs(i - 1).io.result_out
  }
  
  io.out.result_out := OneRoundForEncDecs.last.io.result_out
  io.out.addr_out   := io.in.addr_in
  io.out.len_out    := io.in.len_in
  io.out.id_out     := io.in.id_in
}

// Initial TWEAK encryption module.
// The pipeline configuration is determined by the MemencPipes parameter
class TweakEncrypt(opt: Boolean)(implicit p: Parameters) extends MemEncryptModule{
  val edgeUse = if (opt) MemcedgeIn else MemcedgeOut
  val io = IO(new Bundle {
    val tweak_enc_req = Flipped(DecoupledIO(new Bundle {
      val tweak = UInt(128.W)
      val addr_in = UInt(PAddrBits.W)
      val len_in = UInt(edgeUse.bundle.lenBits.W)   // 6 bit
      val id_in = UInt(edgeUse.bundle.idBits.W)
      val tweak_round_keys = Vec(32, UInt(32.W))
    }))
    val tweak_enc_resp = DecoupledIO(new Bundle {
      val tweak_encrpty = UInt(128.W)
      val addr_out = UInt(PAddrBits.W)
      val len_out = UInt(edgeUse.bundle.lenBits.W)
      val id_out = UInt(edgeUse.bundle.idBits.W)
    })
  })

  val reg_tweak = Reg(Vec(MemencPipes, UInt(128.W)))
  val reg_addr = Reg(Vec(MemencPipes, UInt(PAddrBits.W)))
  val reg_len = Reg(Vec(MemencPipes, UInt(edgeUse.bundle.lenBits.W)))
  val reg_id = Reg(Vec(MemencPipes, UInt(edgeUse.bundle.idBits.W)))
  val reg_tweak_valid = RegInit(VecInit(Seq.fill(MemencPipes)(false.B)))
  // TWEAK encryption requires 32 rounds of encryption keys, grouped by pipeline level
  val wire_round_key = Wire(Vec(MemencPipes, UInt((32 * 32 / MemencPipes).W)))

  val keysPerPipe = 32 / MemencPipes
  for (i <- 0 until MemencPipes) {
    val keySegment = VecInit((0 until keysPerPipe).map(j => io.tweak_enc_req.bits.tweak_round_keys(i * keysPerPipe + j)))
    wire_round_key(i) := Cat(keySegment.asUInt)
  }
    
  val wire_ready_result = WireInit(VecInit(Seq.fill(MemencPipes)(false.B)))
  // The configuration method for each level of encryption module in tweak
  def configureModule(i: Int, dataIn: UInt, addrIn: UInt, lenIn: UInt, idIn: UInt, roundKeys: UInt): OnePipeForTweakEnc = {

    when(wire_ready_result(i) && (if (i == 0) io.tweak_enc_req.valid else reg_tweak_valid(i-1))) {
      reg_tweak_valid(i) := true.B
    }.elsewhen(reg_tweak_valid(i) && (if (i == MemencPipes - 1) io.tweak_enc_resp.ready else wire_ready_result(i+1))) {
      reg_tweak_valid(i) := false.B
    }.otherwise {
      reg_tweak_valid(i) := reg_tweak_valid(i)
    }
    wire_ready_result(i) := !reg_tweak_valid(i) || (reg_tweak_valid(i) && (if (i == MemencPipes - 1) io.tweak_enc_resp.ready else wire_ready_result(i+1)))

    val module = Module(new OnePipeForTweakEnc())
    module.io.in.data_in := dataIn
    module.io.in.addr_in := addrIn
    module.io.in.len_in  := lenIn
    module.io.in.id_in   := idIn
    for (j <- 0 until 32/MemencPipes) {
      module.io.in.round_key_in(j) := roundKeys(j * 32 + 31, j * 32)
    }
    when(wire_ready_result(i) && (if (i == 0) io.tweak_enc_req.valid else reg_tweak_valid(i-1))) {
      reg_tweak(i) := module.io.out.result_out
      reg_addr(i)  := module.io.out.addr_out
      reg_len(i)   := module.io.out.len_out
      reg_id(i)    := module.io.out.id_out
    }
    module
  }
  // Instantiate the tweak encryption module for each pipeline level
  val tweak_enc_modules = (0 until MemencPipes).map { i =>
      if (i == 0) {
        configureModule(i, io.tweak_enc_req.bits.tweak, io.tweak_enc_req.bits.addr_in, io.tweak_enc_req.bits.len_in, io.tweak_enc_req.bits.id_in, wire_round_key(i))
      } else {
        configureModule(i, reg_tweak(i-1), reg_addr(i-1), reg_len(i-1), reg_id(i-1), wire_round_key(i))
      }
  }
  val result_out = Cat(
    reg_tweak.last(31, 0),
    reg_tweak.last(63, 32),
    reg_tweak.last(95, 64),
    reg_tweak.last(127, 96)
  )
    io.tweak_enc_resp.bits.tweak_encrpty  := result_out
    io.tweak_enc_resp.bits.addr_out       := reg_addr.last
    io.tweak_enc_resp.bits.len_out        := reg_len.last
    io.tweak_enc_resp.bits.id_out         := reg_id.last
    io.tweak_enc_resp.valid           := reg_tweak_valid.last
    io.tweak_enc_req.ready            := wire_ready_result(0)

}


// tweak table entry in AR Channel
class TweakTableEntry(implicit val p: Parameters) extends Bundle with Memconsts {
  val v_flag        = Bool()
  val keyid         = UInt(KeyIDBits.W)
  val len           = UInt(MemcedgeOut.bundle.lenBits.W)
  val tweak_encrpty = UInt(128.W)
  val sel_counter   = Bool()
}
class TweakTableModeEntry extends Bundle {
  val dec_mode       = Bool()
}
// tweak table in AR Channel
class TweakTable(implicit p: Parameters) extends MemEncryptModule {
  val io = IO(new Bundle {
    // Write to tweak table
    val write = Flipped(DecoupledIO(new Bundle {
      val id      = UInt(MemcedgeOut.bundle.idBits.W)
      val len     = UInt(MemcedgeOut.bundle.lenBits.W)
      val addr    = UInt(PAddrBits.W)
      val tweak_encrpty = UInt(128.W)
    }))
    // Read from the tweak table with the ID of channel R in AXI4 as the index
    val req = Flipped(DecoupledIO(new Bundle {
      val read_id = UInt(MemcedgeOut.bundle.idBits.W)
    }))
    // Tweak table read response
    val resp = DecoupledIO(new Bundle {
      val read_tweak       = UInt(128.W)
      val read_keyid       = UInt(KeyIDBits.W)
      val read_sel_counter = Bool()
    })
    val w_mode = Flipped(DecoupledIO(new Bundle {
      val id       = UInt(MemcedgeOut.bundle.idBits.W)
      val dec_mode = Input(Bool())
    }))
    val r_mode = new Bundle {
      val id = Input(UInt(MemcedgeOut.bundle.idBits.W))
      val dec_mode = Output(Bool())
    }
  })

  val init_tweak_entry = Wire(new TweakTableEntry())
    init_tweak_entry.v_flag := false.B
    init_tweak_entry.keyid := DontCare
    init_tweak_entry.len := DontCare
    init_tweak_entry.tweak_encrpty := DontCare
    init_tweak_entry.sel_counter := DontCare
  val init_mode_entry = Wire(new TweakTableModeEntry)
    init_mode_entry.dec_mode := false.B
  val tweak_table = RegInit(VecInit(Seq.fill((1 << (MemcedgeOut.bundle.idBits - 1)) + 1)(init_tweak_entry)))
  val tweak_mode_table = RegInit(VecInit(Seq.fill((1 << (MemcedgeOut.bundle.idBits - 1)) + 1)(init_mode_entry)))

  // write tweak table entry logic
  when(io.write.valid) {
      val write_entry = tweak_table(io.write.bits.id)
      write_entry.tweak_encrpty    := io.write.bits.tweak_encrpty
      write_entry.keyid            := io.write.bits.addr(PAddrBits-1, PAddrBits-KeyIDBits)
      write_entry.len              := io.write.bits.len
      write_entry.v_flag           := true.B

      when(io.write.bits.len === 1.U) {
          write_entry.sel_counter := false.B
      }.otherwise {
          write_entry.sel_counter := Mux(io.write.bits.addr(5) === 0.U, false.B, true.B)
      }
  }
  io.write.ready := true.B

  // write mode table entry logic
  when(io.w_mode.valid) {
    val write_mode_entry = tweak_mode_table(io.w_mode.bits.id)
    write_mode_entry.dec_mode := io.w_mode.bits.dec_mode
  }
  io.w_mode.ready := true.B

  // Tweak table read response logic 
  val reg_read_valid    = RegInit(false.B)
  val reg_tweak_encrpty = RegInit(0.U(128.W))
  val reg_keyid = RegInit(0.U(KeyIDBits.W))
  val reg_sel_counter = RegInit(false.B)

  val read_entry = tweak_table(io.req.bits.read_id)
  val read_mode_entry = tweak_mode_table(io.r_mode.id)
  io.r_mode.dec_mode := read_mode_entry.dec_mode

  io.req.ready := (!reg_read_valid || (reg_read_valid && io.resp.ready)) && read_entry.v_flag
  when(io.req.fire) {
      reg_read_valid          := true.B
      reg_tweak_encrpty       := read_entry.tweak_encrpty
      reg_keyid               := read_entry.keyid
      reg_sel_counter         := read_entry.sel_counter
      when(read_entry.len === 0.U) {
          read_entry.v_flag := false.B
      }.otherwise {
          when(!read_entry.sel_counter) {
              read_entry.sel_counter := true.B
          }.otherwise {
              read_entry.v_flag := false.B
          }
      }
  }.elsewhen(reg_read_valid && io.resp.ready) {
      reg_read_valid          := false.B
  }.otherwise {
      reg_read_valid          := reg_read_valid
  }

  io.resp.bits.read_tweak       := reg_tweak_encrpty
  io.resp.bits.read_keyid       := reg_keyid
  io.resp.bits.read_sel_counter := reg_sel_counter
  io.resp.valid     := reg_read_valid

}



// AXI4Util
// Bypass routing, Determine the encryption mode in the key expansion module.
// write requests need to be encrypted ->io.out1; 
// Writing requests does not require encryption --->io.out0.
class WriteChanelRoute(implicit p: Parameters) extends MemEncryptModule
{
  val io = IO(new Bundle {
    val in = new Bundle {
      val aw = Flipped(Irrevocable(new AXI4BundleAW(MemcedgeIn.bundle)))
      val w = Flipped(Irrevocable(new AXI4BundleW(MemcedgeIn.bundle)))
    }
    // Unencrypt Chanel
    val out0 = new Bundle {
      val aw = Irrevocable(new AXI4BundleAW(MemcedgeIn.bundle))
      val w = Irrevocable(new AXI4BundleW(MemcedgeIn.bundle))
    }
    // Encrypt Chanel
    val out1 = new Bundle {
      val aw = Irrevocable(new AXI4BundleAW(MemcedgeIn.bundle))
      val w = Irrevocable(new AXI4BundleW(MemcedgeIn.bundle))
    }
    val enc_keyid = Output(UInt(KeyIDBits.W))
    val enc_mode = Input(Bool())
    val memenc_enable = Input(Bool())
  })
  io.enc_keyid := io.in.aw.bits.addr(PAddrBits-1, PAddrBits-KeyIDBits)

  val reg_idle = RegInit(true.B)
  val reg_enc_mode = RegInit(false.B)

  when(io.in.aw.fire) {
    reg_idle := false.B
    reg_enc_mode := io.enc_mode && io.memenc_enable
  }
  when(io.in.w.fire && io.in.w.bits.last) {
    reg_idle := true.B
  }

  val used_enc_mode = Mux(io.in.aw.fire, io.enc_mode && io.memenc_enable, reg_enc_mode)

  // Cut aw_queue.io.enq.ready from io.out*.awready
  val aw_queue = Module(new IrrevocableQueue(chiselTypeOf(io.in.aw.bits), 1, flow = true))

  io.in.aw.ready := reg_idle && aw_queue.io.enq.ready
  aw_queue.io.enq.valid := io.in.aw.valid && reg_idle
  aw_queue.io.enq.bits := io.in.aw.bits

  val unencrypt_aw_queue = Module(new IrrevocableQueue(chiselTypeOf(io.in.aw.bits), MemencPipes+1, flow = true))
  val unencrypt_w_queue = Module(new IrrevocableQueue(chiselTypeOf(io.in.w.bits), (MemencPipes+1)*2, flow = true))

  aw_queue.io.deq.ready := Mux(used_enc_mode, io.out1.aw.ready, unencrypt_aw_queue.io.enq.ready)
  io.in.w.ready := (io.in.aw.fire || !reg_idle) && Mux(used_enc_mode, io.out1.w.ready, unencrypt_w_queue.io.enq.ready)

  unencrypt_aw_queue.io.enq.valid := !used_enc_mode && aw_queue.io.deq.valid
  unencrypt_w_queue.io.enq.valid  := !used_enc_mode && io.in.w.valid  && (io.in.aw.fire || !reg_idle)

  unencrypt_aw_queue.io.enq.bits := aw_queue.io.deq.bits
  unencrypt_w_queue.io.enq.bits  := io.in.w.bits

  io.out0.aw.valid := unencrypt_aw_queue.io.deq.valid
  io.out0.w.valid := unencrypt_w_queue.io.deq.valid

  io.out0.aw.bits := unencrypt_aw_queue.io.deq.bits
  io.out0.w.bits  := unencrypt_w_queue.io.deq.bits

  unencrypt_aw_queue.io.deq.ready := io.out0.aw.ready
  unencrypt_w_queue.io.deq.ready  := io.out0.w.ready

  io.out1.aw.valid :=  used_enc_mode && aw_queue.io.deq.valid
  io.out1.w.valid  :=  used_enc_mode && io.in.w.valid  && (io.in.aw.fire || !reg_idle)

  io.out1.aw.bits := aw_queue.io.deq.bits
  io.out1.w.bits  := io.in.w.bits
}

class WriteChanelArbiter(implicit p: Parameters) extends MemEncryptModule
{
  val io = IO(new Bundle {
    // Unencrypt Chanel
    val in0 = new Bundle {
      val aw = Flipped(Irrevocable(new AXI4BundleAW(MemcedgeOut.bundle)))
      val w = Flipped(Irrevocable(new AXI4BundleW(MemcedgeOut.bundle)))
    }
    // Encrypt Chanel
    val in1 = new Bundle {
      val aw = Flipped(Irrevocable(new AXI4BundleAW(MemcedgeOut.bundle)))
      val w = Flipped(Irrevocable(new AXI4BundleW(MemcedgeOut.bundle)))
    }
    val out = new Bundle {
      val aw = Irrevocable(new AXI4BundleAW(MemcedgeOut.bundle))
      val w = Irrevocable(new AXI4BundleW(MemcedgeOut.bundle))
    }
  })

  val validMask = RegInit(false.B) // 1:last send write req from Encrypt Chanel
                                   // 0:last send write req from Unencrypt Chanel
  val aw_choice = Wire(Bool())     // 1:Encrypt Chanel 0:Unencrypt Chanel
  val w_choice = RegInit(false.B)  // 1:Encrypt Chanel 0:Unencrypt Chanel
  val reg_idle = RegInit(true.B)
  // Cut aw_queue.io.enq.ready from io.out*.awready
  val aw_queue = Module(new IrrevocableQueue(chiselTypeOf(io.in0.aw.bits), 1, flow = true))

  when(io.in1.aw.fire) {
    validMask := true.B
  }.elsewhen(io.in0.aw.fire) {
    validMask := false.B
  }.otherwise {
    validMask := validMask
  }

  // --------------------------[Unencrypt pref]  [Encrypt pref]
  aw_choice := Mux(validMask, !io.in0.aw.valid, io.in1.aw.valid)

  when(aw_queue.io.enq.fire) {
    reg_idle := false.B
    w_choice := aw_choice
  }
  when(io.out.w.fire && io.out.w.bits.last) {
    reg_idle := true.B
  }

  val used_w_choice = Mux(aw_queue.io.enq.fire, aw_choice, w_choice)

  io.in0.aw.ready := reg_idle && !aw_choice && aw_queue.io.enq.ready
  io.in1.aw.ready := reg_idle &&  aw_choice && aw_queue.io.enq.ready
  aw_queue.io.enq.valid := (io.in0.aw.valid || io.in1.aw.valid) && reg_idle
  aw_queue.io.enq.bits := Mux(aw_choice, io.in1.aw.bits, io.in0.aw.bits)

  // DecoupledIO connect IrrevocableIO
  io.out.aw.valid := aw_queue.io.deq.valid
  io.out.aw.bits := aw_queue.io.deq.bits
  aw_queue.io.deq.ready := io.out.aw.ready

  io.in0.w.ready := (aw_queue.io.enq.fire || !reg_idle) && !used_w_choice && io.out.w.ready
  io.in1.w.ready := (aw_queue.io.enq.fire || !reg_idle) &&  used_w_choice && io.out.w.ready

  io.out.w.valid := (aw_queue.io.enq.fire || !reg_idle) && Mux(used_w_choice, io.in1.w.valid, io.in0.w.valid)
  io.out.w.bits  := Mux(used_w_choice, io.in1.w.bits, io.in0.w.bits)
}

class RdataChanelRoute(implicit p: Parameters) extends MemEncryptModule
{
  val io = IO(new Bundle {
    val in_r = Flipped(Irrevocable(new AXI4BundleR(MemcedgeOut.bundle)))
    // Unencrypt Chanel
    val out_r0 = Irrevocable(new AXI4BundleR(MemcedgeOut.bundle))
    // Encrypt Chanel
    val out_r1 = Irrevocable(new AXI4BundleR(MemcedgeOut.bundle))
    val dec_rid = Output(UInt(MemcedgeOut.bundle.idBits.W))
    val dec_mode = Input(Bool())
  })
  io.dec_rid := io.in_r.bits.id

  val r_sel = io.dec_mode

  io.out_r0.bits <> io.in_r.bits
  io.out_r1.bits <> io.in_r.bits

  io.out_r0.valid := io.in_r.valid && !r_sel
  io.out_r1.valid := io.in_r.valid &&  r_sel
  io.in_r.ready := Mux(r_sel, io.out_r1.ready, io.out_r0.ready)
}