package gpu

import chisel3._
import chisel3.util._

import bus.axi4._
import device.AXI4SlaveModule
import utils._

class PixelBundle extends Bundle {
  val a = UInt(8.W)
  val r = UInt(8.W)
  val g = UInt(8.W)
  val b = UInt(8.W)
}

/* struct texture {
 *   uint32_t pixels[TextureW * TextureH];
 * } __attribute__((packed));
 */
class TextureLineBundle extends Bundle {
  val pixels = Vec(8, new PixelBundle)
}

/* struct sprite {
 *   uint16_t texture, x, y;
 *   uint32_t display : 4;
 *   uint32_t z : 12;
 * } __attribute__((packed));
 */
class SpriteBundle extends Bundle {
  val z = UInt(12.W)
  val display = UInt(4.W)
  val y = UInt(16.W)
  val x = UInt(16.W)
  val texture = UInt(16.W)
}

trait GPUConst {
  val TextureW = 8
  val TextureH = 8
  val ColorBytes = 4

  val TextureLineBytes = TextureW * ColorBytes
  val TextureLineShift = log2Up(TextureLineBytes)
  val TextureBytes = TextureLineBytes * TextureH
  val TextureShift = log2Up(TextureBytes)
  val TextureMaxNum = 65536 // 0 indicate the end
  val TextureIdBits = log2Up(TextureMaxNum)
  val TextureArrayBytes = TextureMaxNum * TextureBytes
  val TextureBase = 0x50000000L - TextureArrayBytes * 2

  def textureLineAddr(idx: UInt, line: UInt): UInt = TextureBase.U |
    (idx(TextureIdBits - 1, 0) << TextureShift.U) |
    (line(2, 0) << TextureLineShift.U)

  val SpriteBase = TextureBase + TextureArrayBytes
  val SpriteBytes = 8
  val SpriteShift = log2Up(SpriteBytes)
  def spriteAddr(idx: UInt): UInt = SpriteBase.U | (idx << SpriteShift.U)

  val ScreenW = 400
  val ScreenH = 300
  val FrameBufBase = 0x40000000L
  def fbAddr(x: UInt, y: UInt): UInt = {
    assert(x < ScreenW.U && y < ScreenH.U)
    FrameBufBase.U + ((y * ScreenW.U + x) << 2)
  }
}

class AXI4GPU extends AXI4SlaveModule(new AXI4Lite, Some(new AXI4(dataBits = 256))) with GPUConst {
  val out = io.extra.get

  // control registers
  //val regs = Mem(4, UInt(32.W))
  def index(addr: UInt) = (addr & 0xf.U) >> 2
  val statIdx = 0
  val ctrlIdx = 1

  val statReg = Reg(UInt(32.W))
  val ctrlReg = Reg(UInt(32.W))

  def readReg(addr: UInt) = LookupTree(addr, List(
    statIdx.U -> statReg,
    ctrlIdx.U -> ctrlReg
  ))
  in.r.bits.data := RegEnable(readReg(in.ar.bits.addr), in.ar.fire())

  val waddr = index(in.aw.bits.addr)
  val wdata = genWdata(readReg(in.aw.bits.addr))
  when (in.aw.fire()) {
    when (waddr === ctrlIdx.U) { ctrlReg := wdata }
  }

  val startCmd = ctrlReg(0) && !RegNext(ctrlReg(0))

  val s_idle :: s_sprite_read :: s_texture_read :: s_render_line :: s_render_align :: s_render_unalign :: Nil = Enum(6)
  val state = RegInit(s_idle)
  statReg := (state =/= s_idle)

  out := DontCare
  out.ar.bits.prot  := AXI4Parameters.PROT_PRIVILEDGED
  out.ar.bits.id    := 0.U
  out.ar.bits.len   := 0.U  // single beat
  out.ar.bits.burst := AXI4Parameters.BURST_INCR
  out.ar.bits.lock  := false.B
  out.ar.bits.cache := 0.U
  out.ar.bits.qos   := 0.U
  out.ar.bits.user  := 0.U
  out.w.bits.last   := true.B
  out.aw.bits := out.ar.bits

  val spriteIdx = Counter(65536)
  when (state === s_idle && startCmd) {
    printf("GPU start!!!!\n");
    state := s_sprite_read
    spriteIdx.value := 0.U
  }

  val spriteBuf = Reg(new SpriteBundle)
  val textureLineCnt = Counter(TextureH)
  when (state === s_sprite_read) {
    out.ar.bits.addr := spriteAddr(spriteIdx.value)
    out.ar.bits.size := log2Up(SpriteBytes).U

    when (out.r.fire()) {
      //val rdata = out.r.bits.data.asTypeOf(new SpriteBundle)
      val numOfSpritePerAccess = 256 / (SpriteBytes * 8)
      val spriteIdxOffset = spriteIdx.value(log2Up(numOfSpritePerAccess) - 1, 0)
      val spriteRead = out.r.bits.data.asTypeOf(Vec(numOfSpritePerAccess, new SpriteBundle))(spriteIdxOffset)
      spriteBuf := spriteRead
      textureLineCnt.value := 0.U

      val isEnd = spriteRead.texture === 0.U
      state := Mux(isEnd, s_idle, s_texture_read)
    }
  }

  val textureLineBuf = Reg(UInt((TextureLineBytes * 8).W))
  when (state === s_texture_read) {
    out.ar.bits.addr := textureLineAddr(spriteBuf.texture, textureLineCnt.value)
    out.ar.bits.size := log2Up(TextureLineBytes).U

    when (out.r.fire()) {
      textureLineBuf := out.r.bits.data
      state := s_render_line
    }
  }

  when (state === s_render_line) {
    val renderAddr = fbAddr(x = spriteBuf.x, y = spriteBuf.y + textureLineCnt.value)
    // FIXME: check the result of renderLineMask
    //val renderLineMask = Cat(textureLineBuf.asTypeOf(new TextureLineBundle).pixels.map(
    //  c => Mux(c.a === 0.U, 0.U(4.W), 0xf.U(4.W))))

    // should handle sprite accross a tile
    assert((renderAddr & (TextureLineBytes - 1).U) === 0.U)

    out.aw.bits.addr := renderAddr
    out.aw.bits.size := log2Up(TextureLineBytes).U
    out.w.bits.data := textureLineBuf
    out.w.bits.strb := 0xffffffffL.U

    when (out.b.fire()) {
      val finishOneTexture = textureLineCnt.inc()
      when (finishOneTexture) { spriteIdx.inc() }
      state := Mux(finishOneTexture, s_sprite_read, s_texture_read)
    }
  }

  val rWait = BoolStopWatch(out.ar.fire(), out.r.fire())
  out.ar.valid := BoolStopWatch((state === s_sprite_read || state === s_texture_read) && !rWait, out.ar.fire())
  out.r.ready := rWait

  val wSend = Wire(Bool())
  val awAck = BoolStopWatch(out.aw.fire(), wSend)
  val wAck = BoolStopWatch(out.w.fire(), wSend)
  wSend := (out.aw.fire() && out.w.fire()) || (awAck && wAck)

  val bWait = BoolStopWatch(wSend, out.b.fire())
  val wInflight = BoolStopWatch((state === s_render_line) && !bWait, wSend)
  out.aw.valid := wInflight && !awAck
  out.w .valid := wInflight && !wAck
  out.b.ready := bWait
}
