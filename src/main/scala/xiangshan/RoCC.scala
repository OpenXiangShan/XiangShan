/**************************************************************************************
 * Copyright (c) 2020 Institute of Computing Technology, CAS
 * Copyright (c) 2020 University of Chinese Academy of Sciences
 *
 * NutShell is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *             http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/
//all by LBR

package xiangshan


import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils._
import xiangshan._
import xiangshan.backend.fu.{FunctionUnit, HasRedirectOut}


class RoCCInstruction extends Bundle {
  val funct = Bits(7.W)
  val rs2 = Bits(5.W)
  val rs1 = Bits(5.W)
  val xd = Bool()
  val xs1 = Bool()
  val xs2 = Bool()
  val rd = Bits(5.W)
  val opcode = Bits(7.W)
}

class MstatusStruct4ROCC extends Bundle {
  val sd = Output(UInt(1.W))

  //val pad1 = if (XLEN == 64) Output(UInt(27.W)) else null
  //val sxl  = if (XLEN == 64) Output(UInt(2.W))  else null
  //val uxl  = if (XLEN == 64) Output(UInt(2.W))  else null
  //val pad0 = if (XLEN == 64) Output(UInt(9.W))  else Output(UInt(8.W))
  val pad0 = Output(UInt(8.W))

  val tsr = Output(UInt(1.W))
  val tw = Output(UInt(1.W))
  val tvm = Output(UInt(1.W))
  val mxr = Output(UInt(1.W))
  val sum = Output(UInt(1.W))
  val mprv = Output(UInt(1.W))
  val xs = Output(UInt(2.W))
  val fs = Output(UInt(2.W))
  val mpp = Output(UInt(2.W))
  val hpp = Output(UInt(2.W))
  val spp = Output(UInt(1.W))
  //val pie = new Priv
  //val ie = new Priv
}//MstatusStruct from Nutshell design.

class RoCCCommand(implicit p: Parameters) extends XSBundle {
  val inst = new RoCCInstruction
  val rs1 = Bits(XLEN.W)
  val rs2 = Bits(XLEN.W)
  //val status = new MstatusStruct4ROCC
}

class RoCCResponse(implicit p: Parameters) extends XSBundle {
  val rd = Bits(5.W)
  val data = Bits(XLEN.W)
}

class RoCCCoreIO(implicit p: Parameters) extends XSBundle {
  val cmd = Flipped(Decoupled(new RoCCCommand))
  val resp = Decoupled(new RoCCResponse)
  //val mem = new SimpleBusUC()                    //not clarified yet!
  val busy = Output(Bool())
  val interrupt = Output(Bool())
  val exception = Input(Bool())
}

/* To Be Implemented
class RoCCIO extends RoCCCoreIO {
  val ptw = Vec(nPTWPorts, new TLBPTWIO)
  val fpu_req = Decoupled(new FPInput)
  val fpu_resp = Flipped(Decoupled(new FPResult))
}
 */
class OpcodeSet(val opcodes: Seq[UInt]) {
  def |(set: OpcodeSet) =
    new OpcodeSet(this.opcodes ++ set.opcodes)

  def matches(oc: UInt) = opcodes.map(_ === oc).reduce(_ || _)
}

object OpcodeSet {
  def custom0 = new OpcodeSet(Seq("b0001011".U))
  def custom1 = new OpcodeSet(Seq("b0101011".U))
  def custom2 = new OpcodeSet(Seq("b1011011".U))
  def custom3 = new OpcodeSet(Seq("b1111011".U))
  def all = custom0 | custom1 | custom2 | custom3
}

//class RoccCommandRouter(opcodes: Seq[OpcodeSet])

class RoccCommandRouter(opcode: OpcodeSet)(implicit p: Parameters) extends XSModule {
  val io = IO( new Bundle {
    val in = Flipped(Decoupled(new RoCCCommand))
    val out = Decoupled(new RoCCCommand)
//    val out = Vec(opcodes.size, Decoupled(new RoCCCommand))
    val busy = Output(Bool())
  })

  val cmd = Queue(io.in)
  //val cmdReadys = io.out.zip(opcodes).map { case (out, opcode) =>
    val me = opcode.matches(cmd.bits.inst.opcode)
    io.out.valid := cmd.valid && me
    io.out.bits := cmd.bits

  /*
  Debug("[Acc-CC] cmdrouter in: funct = %x, rs2 = %x, rs1 = %x, xdxs1xs2=%b%b%b, rd = %x, opcode = %x\n",
    cmd.bits.inst.funct, cmd.bits.inst.rs2, cmd.bits.inst.rs1, cmd.bits.inst.xd, cmd.bits.inst.xs1, cmd.bits.inst.xs2, cmd.bits.inst.rd, cmd.bits.inst.opcode)
  Debug("[Acc-cc] cmdtouter in: cmd.rs1 = %x, cmd.rs2 = %x\n", cmd.bits.rs1, cmd.bits.rs2)
  Debug("[Acc-CC] cmdrouter out: funct = %x, rs2 = %x, rs1 = %x, xdxs1xs2=%b%b%b, rd = %x, opcode = %x\n",
    io.out.bits.inst.funct, io.out.bits.inst.rs2, io.out.bits.inst.rs1, io.out.bits.inst.xd, io.out.bits.inst.xs1, io.out.bits.inst.xs2, io.out.bits.inst.rd, io.out.bits.inst.opcode)
  Debug("[Acc-cc] cmdrouter out: cmd.rs1 = %x, cmd.rs2 = %x\n", io.out.bits.rs1, io.out.bits.rs2)
  */

  //  io.out.ready && me
    val cmdReady = io.out.ready && me
  //}
  cmd.ready := cmdReady
  io.busy := cmd.valid

  assert(PopCount(cmdReady) <= 1.U,
    "Custom opcode matched for more than one accelerator")
}
//wrap rocc module with
class XsCCIfInExeUnit(implicit p: Parameters) extends FunctionUnit{
  val roccModule = Module(new RoCCModule())

  io.in := DontCare
  io.in.ready := roccModule.io.cmd.ready
  roccModule.io.cmd.bits.rs1 := io.in.bits.src(0)
  roccModule.io.cmd.bits.rs2 := io.in.bits.src(1)
  roccModule.io.cmd.bits.inst := io.in.bits.uop.cf.instr.asTypeOf(new RoCCInstruction)
  roccModule.io.cmd.valid := io.in.valid

  io.out.valid := roccModule.io.resp.valid
  io.out.bits.data := roccModule.io.resp.bits.data
  io.out.bits.uop := io.in.bits.uop
  roccModule.io.resp.ready := io.out.ready

  roccModule.io.exception := DontCare
  roccModule.io.interrupt := DontCare

}

class RoCCModule(implicit p: Parameters) extends XSModule{
  val io = IO(new RoCCCoreIO)

  val cmdRouter = Module(new RoccCommandRouter(OpcodeSet.custom0))
  //val respArb = Module(new RRArbiter(new RoCCResponse(), 1))

  val Accum = Module(new AccumulatorExampleModule)

  io.cmd <> cmdRouter.io.in
  cmdRouter.io.out <> Accum.io.cmd

  /*
  Debug("[RoCC] cmdRouter.in: rs1 = %x, rs2 = %x\n", cmdRouter.io.in.bits.rs1, cmdRouter.io.in.bits.rs2)
  Debug("[RoCC] cmdRouter.out: rs1 = %x, rs2 = %x\n", cmdRouter.io.out.bits.rs1, cmdRouter.io.out.bits.rs2)
  Debug("[RoCC] acc.io.cmd: rs1 = %x, rs2 = %x\n", Accum.io.cmd.bits.rs1, Accum.io.cmd.bits.rs2)

  */

  Accum.io.resp <> io.resp
  /*
  Accum.io.resp <> respArb.io.in
  respArb.io.out<> io.resp
  */


  //io.mem <> Accum.io.mem

  io.busy <> (cmdRouter.io.busy || Accum.io.busy)
  io.interrupt <> Accum.io.interrupt
  io.exception <> Accum.io.exception

}


class AccumulatorExampleModule(implicit p: Parameters) extends XSModule {
  val io = IO(new RoCCCoreIO)

  val regfile = Mem(4, UInt(XLEN.W))
  //val regfile = Mem(4, UInt(XLEN.W))
  val busy = RegInit(VecInit(Seq.fill(4){false.B}))

  val cmd = Queue(io.cmd)
  val funct = cmd.bits.inst.funct
  val addr = cmd.bits.rs2(log2Up(4)-1,0)
  val doWrite = funct === 0.U
  val doRead = funct === 1.U
  val doLoad = funct === 2.U
  val doAccum = funct === 3.U
  //val memRespTag = addend(log2Up(4)-1,0)
  //val memRespTag = io.mem.resp.bits.id.get(log2Up(4)-1,0)// unclarified whether it is necessary on NutShell
  /*
  Debug("[Acc-CC] cmd: funct = %x, rs2 = %x, rs1 = %x, xdxs1xs2=%b%b%b, rd = %x, opcode = %x\n",
    io.cmd.bits.inst.funct, io.cmd.bits.inst.rs2, io.cmd.bits.inst.rs1, io.cmd.bits.inst.xd, io.cmd.bits.inst.xs1, io.cmd.bits.inst.xs2, io.cmd.bits.inst.rd, io.cmd.bits.inst.opcode)
  Debug("[Acc-cc] cmd in: cmd.rs1 = %x, cmd.rs2 = %x\n", io.cmd.bits.rs1, io.cmd.bits.rs2)
  Debug("[Acc-cc] cmd Q: cmd.rs1 = %x, cmd.rs2 = %x\n", cmd.bits.rs1, cmd.bits.rs2)
  */

  // datapath
  val addend = cmd.bits.rs1
  val accum = regfile(addr)
  val wdata = Mux(doWrite, addend, accum + addend)

  when (/*true.B*/cmd.fire() && (doWrite || doAccum)) {
    regfile(addr) := wdata
  }
  /*
  Debug("[CC-Acc] regfile[%d] = %x, wdata = %x\n", addr, regfile(addr), wdata)
  Debug(doWrite, "doWrite\n")
  Debug(doRead, "doRead\n")
  Debug(doAccum, "doAccum\n")
  */
  /*
  when (io.mem.resp.valid) {
    //regfile(memRespTag) := io.mem.resp.bits.rdata
    //busy(memRespTag) := false.B
    regfile(0) := io.mem.resp.bits.rdata
    busy(0) := false.B
  }

  // control
  when (io.mem.req.fire()) {
    busy(addr) := true.B
  }
  */

  val doResp = cmd.bits.inst.xd
  val stallReg = busy(addr)
  //val stallLoad = doLoad && !io.mem.req.ready
  val stallResp = doResp && !io.resp.ready
  /*
  Debug("cmd valid: %b, cmd ready: %b, stallReg: %b, stallLoad: %b, stallResp: %b, io.resp.ready: %b\n",
    cmd.valid, cmd.ready, stallReg, stallLoad, stallResp, io.resp.ready)
  */

  cmd.ready := !stallReg  && !stallResp //&& !stallLoad
  // command resolved if no stalls AND not issuing a load that will need a request

  // PROC RESPONSE INTERFACE
  io.resp.valid := cmd.valid && doResp && !stallReg //&& !stallLoad
  // valid response if valid command, need a response, and no stalls
  io.resp.bits.rd := cmd.bits.inst.rd
  // Must respond with the appropriate tag or undefined behavior
  io.resp.bits.data := accum
  // Semantics is to always send out prior accumulator register value

  io.busy := cmd.valid || busy.reduce(_||_)
  // Be busy when have pending memory requests or committed possibility of pending requests
  io.interrupt := false.B
  // Set this true to trigger an interrupt on the processor (please refer to supervisor documentation)
  /*
  // MEMORY REQUEST INTERFACE
  io.mem.req.valid := cmd.valid && doLoad && !stallReg && !stallResp
  io.mem.req.bits.addr := addend
  //io.mem.req.bits.id.get := addr
  io.mem.req.bits.cmd := SimpleBusCmd.read // perform a load (M_XWR for stores)
  io.mem.req.bits.size := log2Ceil(8).U
  //io.mem.mem.req.bits.signed := false.B
  io.mem.req.bits.wdata := DontCare // we're not performing any stores...
  io.mem.req.bits.wmask := DontCare
  io.mem.resp.ready := DontCare
  //io.mem.mem.req.bits.phys := false.B
  //io.mem.mem.req.bits.dprv := cmd.bits.status.dprv
  */
}