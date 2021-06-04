/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

package cache

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import xiangshan.testutils._
import utils._

import scala.util.Random

trait TestConst {
    val nWays = 4
    val nSets = 64
    val maxTag = 15
    val tagBits = log2Ceil(maxTag)  // 0-15
}

class ReqSignal extends Bundle with TestConst{
   val tag =  (UInt(tagBits.W))
}

class RespSignal extends Bundle {
    val hit = Bool()
    val miss = Bool()
}


class RepTestTop extends Module
    with TestConst
{
    val io = IO(new Bundle{
        val req = Flipped(ValidIO(new ReqSignal))
        val resp = ValidIO(new RespSignal)
    })

    val fakeCache = RegInit(VecInit( (0 until nWays).map{ w => w.U(tagBits.W) } ))

    val req = io.req
    val replacer = ReplacementPolicy.fromString("random",nWays)

    val hitVec = VecInit(fakeCache.map{w =>  req.valid && (req.bits.tag === w) }).asUInt
    val hit = hitVec.orR
    val victimWayMask = UIntToOH(replacer.way)

    when(hit && req.valid) { replacer.access(OHToUInt(hitVec))}
    .elsewhen(!hit && req.valid){       //refill
        fakeCache(replacer.way) := req.bits.tag
    }

    io.resp.valid := io.req.valid
    io.resp.bits.hit := io.req.valid && hit
    io.resp.bits.miss :=  io.req.valid && !hit

    when(io.req.valid){printf("tag:%d  hit :%d  miss:%d \n",req.bits.tag,hit,io.resp.bits.miss)}
    (0 until nWays).map{ w=>
        printf("(%d)  tag%d\n",w.U,fakeCache(w).asUInt)
    }

}

class ReplaceTest extends AnyFlatSpec
    with TestConst 
    with ChiselScalatestTester 
    with Matchers 
    with ParallelTestExecution
    with HasPartialDecoupledDriver {

    it should "run" in {
        test(new RepTestTop){ c =>

            val testnumber = 1000
            val randomGen = scala.util.Random
            var hitCounter = 0
            var missCounter = 0
            var tag = 0

            for(i <- 0 until testnumber){
                if(i%1 == 0){ tag = randomGen.nextInt(maxTag + 1)  }
                c.io.req.valid.poke(true.B)
                c.io.req.bits.tag.poke(tag.U)
                if(c.io.resp.bits.hit.peek().litToBoolean){ hitCounter = hitCounter + 1}
                if(c.io.resp.bits.miss.peek().litToBoolean){ missCounter = missCounter + 1}
                c.clock.step()
            }

            val missRate:Float = missCounter.toFloat/testnumber
            val hitRate:Float = hitCounter.toFloat/testnumber
            println("------Final Result-------\n")
            println("missRate",missRate)
            println("hitRate",hitRate)

        }
    }


}