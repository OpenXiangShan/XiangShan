package xiangshan.frontend

import chisel3._
import chiseltest._
import org.scalatest._
import xiangshan.testutils._


class RASTest extends FlatSpec 
with ChiselScalatestTester 
with Matchers 
with ParallelTestExecution
with HasPartialDecoupledDriver {
  it should "test RASTest" in {
    test(new RAS) { c =>
        def push(pc: Long,callIdx: Int){
            c.io.callIdx.valid.poke(true.B)
            c.io.callIdx.bits.poke(callIdx.U)
            c.io.pc.valid.poke(true.B)
            c.io.pc.bits.poke(pc.U)
            c.clock.step()
            c.io.callIdx.valid.poke(false.B)
        }

        def pop(rigth_target:Long){
            c.io.is_ret.poke(true.B)
            //c.io.out.bits.target.expect(rigth_target.U)
            c.clock.step()
        }

        push(pc=0x60002000,callIdx=8)
        push(pc=0x60002000,callIdx=8)
        push(pc=0x60002000,callIdx=8)
        pop(rigth_target=0x60002000+8*2+4)  
        pop(rigth_target=0x60002000+8*2+4)  
        pop(rigth_target=0x60002000+8*2+4)   
    }
  }
}