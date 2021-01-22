// package xiangshan.frontend

// import chisel3._
// import chiseltest._
// import org.scalatest._
// import org.scalatest.flatspec.AnyFlatSpec
// import org.scalatest.matchers.must.Matchers
// import xiangshan.testutils._


// class RASTest extends AnyFlatSpec
// with ChiselScalatestTester 
// with Matchers 
// with ParallelTestExecution
// with HasPartialDecoupledDriver {
//   it should "test RASTest" in {
//     test(new RAS) { c =>
//         def spec_push(pc: Long,callIdx: Int){
//             c.io.callIdx.valid.poke(true.B)
//             c.io.callIdx.bits.poke(callIdx.U)
//             c.io.pc.valid.poke(true.B)
//             c.io.pc.bits.poke(pc.U)
//             c.clock.step()
//             c.io.callIdx.valid.poke(false.B)
//         }

//         def spec_pop(){
//             c.io.is_ret.poke(true.B)
//             //c.io.out.bits.target.expect(rigth_target.U)
//             c.clock.step()
//             c.io.is_ret.poke(false.B)
//         }
        
//         def commit_push(pc: Long){
//           c.io.recover.valid.poke(true.B)
//           c.io.recover.bits.isMisPred.poke(false.B)
//           c.io.recover.bits.pd.isCall.poke(true.B)
//           c. io.recover.bits.pc.poke(pc.U)
//           c.clock.step()
//           c.io.recover.valid.poke(false.B)
//           c.io.recover.bits.pd.isCall.poke(false.B)
//         }

//         def commit_pop(){
//           c.io.recover.valid.poke(true.B)
//           c.io.recover.bits.pd.isRet.poke(true.B)
//           c.clock.step()
//           c.io.recover.valid.poke(false.B)
//           c.io.recover.bits.pd.isRet.poke(false.B)
//         }


//         // def update_pop(sp:Int,ctr:Int,addr:Long){
//         //   c.io.recover.valid.poke(true.B)
//         //   c.io.recover.bits.isMisPred.poke(true.B)
//         //   c.io.recover.bits.brInfo.rasToqAddr.poke(addr.U)
//         //   c.io.recover.bits.brInfo.rasTopCtr.poke(ctr.U)
//         //   c.io.recover.bits.brInfo.rasSp.poke(sp.U)
//         //   pop(right_target=addr)
//         //   c.io.out.bits.target.expect(addr.U)
//         // }

//         // def update_push(sp:Int,ctr:Int,addr:Long,pc: Long,callIdx: Int){
//         //   c.io.recover.valid.poke(true.B)
//         //   c.io.recover.bits.isMisPred.poke(true.B)
//         //   c.io.recover.bits.brInfo.rasToqAddr.poke(addr.U)
//         //   c.io.recover.bits.brInfo.rasTopCtr.poke(ctr.U)
//         //   c.io.recover.bits.brInfo.rasSp.poke(sp.U)
//         //   push(pc,callIdx)
//         // }

//         //update_pop(sp=2,ctr=1,addr=0x60002020+5*2+4)
//         spec_push(pc=0x60000010,callIdx=2)
//         spec_push(pc=0x60000014,callIdx=3)
//         commit_push(pc=0x60000028)
//         commit_push(pc=0x60000030)
//         spec_pop()
//         spec_pop()
//         c.io.recover.valid.poke(true.B)
//         c.io.recover.bits.isMisPred.poke(true.B)
//         commit_pop()
//         commit_pop()


//         // pop(rigth_target=0x60002000+8*2+4)  
//         // pop(rigth_target=0x60002000+8*2+4)  
//         // pop(rigth_target=0x60002000+8*2+4)   
//     }
//   }
// }