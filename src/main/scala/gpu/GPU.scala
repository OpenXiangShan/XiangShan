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
  val TextureBase = 0x60000000L - TextureArrayBytes * 2

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

class GPUOutBundle extends Bundle {
  // can use 32 bit after implementing burst
  val metaData = new AXI4(dataBits = 256)
  val fb = new AXI4(dataBits = 256)
}

class AXI4GPU extends AXI4SlaveModule(new AXI4Lite, Some(new GPUOutBundle)) with GPUConst {
  val out = io.extra.get

  // control registers
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
  out.metaData.ar.bits.prot  := AXI4Parameters.PROT_PRIVILEDGED
  out.metaData.ar.bits.id    := 0.U
  out.metaData.ar.bits.len   := 0.U  // single beat
  out.metaData.ar.bits.burst := AXI4Parameters.BURST_INCR
  out.metaData.ar.bits.lock  := false.B
  out.metaData.ar.bits.cache := 0.U
  out.metaData.ar.bits.qos   := 0.U
  out.metaData.ar.bits.user  := 0.U
  out.fb.w.bits.last   := true.B
  out.fb.aw.bits := out.metaData.ar.bits

  val spriteIdx = Counter(65536)
  when (state === s_idle && startCmd) {
    printf("GPU start!!!!\n");
    state := s_sprite_read
    spriteIdx.value := 0.U
  }

  val spriteBuf = Reg(new SpriteBundle)
  val textureLineCnt = Counter(TextureH)
  when (state === s_sprite_read) {
    out.metaData.ar.bits.addr := spriteAddr(spriteIdx.value)
    out.metaData.ar.bits.size := log2Up(SpriteBytes).U

    when (out.metaData.r.fire()) {
      //val rdata = out.r.bits.data.asTypeOf(new SpriteBundle)
      val numOfSpritePerAccess = 256 / (SpriteBytes * 8)
      val spriteIdxOffset = spriteIdx.value(log2Up(numOfSpritePerAccess) - 1, 0)
      val spriteRead = out.metaData.r.bits.data.asTypeOf(
        Vec(numOfSpritePerAccess, new SpriteBundle))(spriteIdxOffset)
      spriteBuf := spriteRead
      textureLineCnt.value := 0.U

      val isEnd = spriteRead.texture === 0.U
      state := Mux(isEnd, s_idle, s_texture_read)
    }
  }

  val textureLineBuf = Reg(UInt((TextureLineBytes * 8).W))
  when (state === s_texture_read) {
    out.metaData.ar.bits.addr := textureLineAddr(spriteBuf.texture, textureLineCnt.value)
    out.metaData.ar.bits.size := log2Up(TextureLineBytes).U

    when (out.metaData.r.fire()) {
      textureLineBuf := out.metaData.r.bits.data
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

    out.fb.aw.bits.addr := renderAddr
    out.fb.aw.bits.size := log2Up(TextureLineBytes).U
    out.fb.w.bits.data := textureLineBuf
    out.fb.w.bits.strb := 0xffffffffL.U

    when (out.fb.b.fire()) {
      val finishOneTexture = textureLineCnt.inc()
      when (finishOneTexture) { spriteIdx.inc() }
      state := Mux(finishOneTexture, s_sprite_read, s_texture_read)
    }
  }

  val rWait = BoolStopWatch(out.metaData.ar.fire(), out.metaData.r.fire())
  out.metaData.ar.valid := BoolStopWatch(
    (state === s_sprite_read || state === s_texture_read) && !rWait, out.metaData.ar.fire())
  out.metaData.r.ready := rWait
  out.metaData.aw.valid := false.B
  out.metaData.w.valid := false.B
  out.metaData.b.ready := true.B

  val wSend = Wire(Bool())
  val awAck = BoolStopWatch(out.fb.aw.fire(), wSend)
  val wAck = BoolStopWatch(out.fb.w.fire(), wSend)
  wSend := (out.fb.aw.fire() && out.fb.w.fire()) || (awAck && wAck)

  val bWait = BoolStopWatch(wSend, out.fb.b.fire())
  val wInflight = BoolStopWatch((state === s_render_line) && !bWait, wSend)
  out.fb.aw.valid := wInflight && !awAck
  out.fb.w .valid := wInflight && !wAck
  out.fb.b.ready := bWait
  out.fb.ar.valid := false.B
  out.fb.r.ready := true.B
}
