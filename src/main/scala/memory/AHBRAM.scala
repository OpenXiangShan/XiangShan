package memory

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

// This gets used everywhere, so make the smallest circuit possible ...
// Given an address and size, create a mask of beatBytes size
// eg: (0x3, 0, 4) => 0001, (0x3, 1, 4) => 0011, (0x3, 2, 4) => 1111
// groupBy applies an interleaved OR reduction; groupBy=2 take 0010 => 01
object MaskGen {
  def apply(addr_lo: UInt, lgSize: UInt, beatBytes: Int, groupBy: Int = 1): UInt = {
    require (groupBy >= 1 && beatBytes >= groupBy)
    require (isPow2(beatBytes) && isPow2(groupBy))
    val lgBytes = log2Ceil(beatBytes)
    val sizeOH = UIntToOH(lgSize | 0.U(log2Up(beatBytes).W), log2Up(beatBytes)) | (groupBy*2 - 1).U

    def helper(i: Int): Seq[(Bool, Bool)] = {
      if (i == 0) {
        Seq((lgSize >= lgBytes.U, true.B))
      } else {
        val sub = helper(i-1)
        val size = sizeOH(lgBytes - i)
        val bit = addr_lo(lgBytes - i)
        val nbit = !bit
        Seq.tabulate (1 << i) { j =>
          val (sub_acc, sub_eq) = sub(j/2)
          val eq = sub_eq && (if (j % 2 == 1) bit else nbit)
          val acc = sub_acc || (size && eq)
          (acc, eq)
        }
      }
    }

    if (groupBy == beatBytes) UInt(1) else
      Cat(helper(lgBytes-log2Ceil(groupBy)).map(_._1).reverse)
  }
}

class AHBRAM(memByte: Int, beatBytes: Int = 4, dataFile: String = "") extends Module {
  val io = IO(new Bundle{ 
    val in = Flipped(new AHBLiteIO)
  })

  val in = io.in
  //val mem = makeSinglePortedByteWriteSeqMem(1 << mask.filter(b=>b).size)
  val mem = SeqMem(memByte, Vec(beatBytes, UInt(8.W)))
  if (dataFile != "")
    loadMemoryFromFile(mem, dataFile)

  // The mask and address during the address phase
  val a_access    = in.htrans === AHBParameters.TRANS_NONSEQ || in.htrans === AHBParameters.TRANS_SEQ
  val a_request   = in.hready && in.hsel && a_access
  val a_mask      = MaskGen(in.haddr, in.hsize, beatBytes)
  val a_address   = in.haddr >> log2Ceil(beatBytes) //Cat((mask zip (in.haddr >> log2Ceil(beatBytes)).toBools).filter(_._1).map(_._2).reverse)
  val a_write     = in.hwrite
  val a_legal     = true.B

  // The data phase signals
  val d_wdata = VecInit.tabulate(beatBytes) { i => in.hwdata(8*(i+1)-1, 8*i) }

  // AHB writes must occur during the data phase; this poses a structural
  // hazard with reads which must occur during the address phase. To solve
  // this problem, we delay the writes until there is a free cycle.
  //
  // The idea is to record the address information from address phase and
  // then as soon as possible flush the pending write. This cannot be done
  // on a cycle when there is an address phase read, but on any other cycle
  // the write will execute. In the case of reads following a write, the
  // result must bypass data from the pending write into the read if they
  // happen to have matching address.

  def holdUnless[T <: Data](x: T, enable: Bool): T = Mux(enable, x, RegEnable(x, enable))

  // Pending write?
  val p_valid     = RegInit(false.B)
  val p_address   = RegInit(a_address)
  val p_mask      = RegInit(UInt(beatBytes.W), init = a_mask)
  val p_latch_d   = Reg(Bool())
  //val p_wdata     = d_wdata holdUnless p_latch_d
  val p_wdata     = holdUnless(d_wdata, p_latch_d)

  // Decide if the SRAM port is used for reading or (potentially) writing
  val read = a_request && !a_write
  // In case we choose to stall, we need to hold the read data
  //val d_rdata = mem.readAndHold(a_address, read)
  val d_rdata = holdUnless(mem.read(a_address, read), RegNext(read))
  val d_legal = RegEnable(a_legal, in.hreadyout)
  // Whenever the port is not needed for reading, execute pending writes
  when (!read && p_valid) {
    p_valid := false.B
    mem.write(p_address, p_wdata, p_mask.toBools)
  }

  // Record the request for later?
  p_latch_d := a_request && a_write
  when (a_request && a_write && a_legal) {
    p_valid   := true.B
    p_address := a_address
    p_mask    := a_mask
  }

  // Does the read need to be muxed with the previous write?
  val a_bypass = a_address === p_address && p_valid
  val d_bypass = RegEnable(a_bypass, a_request)

  // Mux in data from the pending write
  val muxdata = VecInit((p_mask.toBools zip (p_wdata zip d_rdata))
    map { case (m, (p, r)) => Mux(d_bypass && m, p, r) })

  // Don't fuzz hready when not in data phase
  val d_request = RegInit(false.B)
  when (in.hready) { d_request := false.B }
  when (a_request)  { d_request := true.B }

  // Finally, the outputs
  //in.hreadyout := (if(fuzzHreadyout) { !d_request || LFSRNoiseMaker(1)(0) }  else { Bool(true) })
  in.hreadyout := true.B
  in.hresp     := Mux(d_legal || !in.hreadyout, AHBParameters.RESP_OKAY, AHBParameters.RESP_ERROR)
  in.hrdata    := Mux(in.hreadyout, muxdata.asUInt, 0.U)
}
