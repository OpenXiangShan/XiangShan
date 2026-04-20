/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import xiangshan._
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.mem.StoreStage._
import xiangshan.cache._

class StoreUnitS0(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS0()
) extends StoreUnitStage(param) {

}

class StoreUnitS1(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS1()
) extends StoreUnitStage(param) {

}

class StoreUnitS2(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS2()
) extends StoreUnitStage(param) {

}

class StoreUnitS3(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS3()
) extends StoreUnitStage(param) {

}

class StoreUnitS4(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS4()
) extends StoreUnitStage(param) {
  
}

class StoreUnitIO(val param: ExeUnitParams)(implicit p: Parameters) extends XSBundle {
  
}

class NewStoreUnit(val param: ExeUnitParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new StoreUnitIO(param))
  
  val s0 = Module(new StoreUnitS0(param))
  val s1 = Module(new StoreUnitS1(param))
  val s2 = Module(new StoreUnitS2(param))
  val s3 = Module(new StoreUnitS3(param))
  val s4 = Module(new StoreUnitS4(param))
}

abstract class StoreUnitStage(val param: ExeUnitParams)(
  implicit p: Parameters,
  implicit val s: StoreStage
) extends XSModule with OnStoreStage {
  val io_pipeIn = if (afterS1) {
    Some(IO(Flipped(DecoupledIO(new StoreStageIO()(p, prevStage(s))))))
  } else None
  val io_pipeOut = if (!lastStage) {
    Some(IO(DecoupledIO(new StoreStageIO)))
  } else None

  def <>(that: StoreUnitStage): Unit = {
    this.io_pipeIn.foreach(_ <> that.io_pipeOut.get)
  }
}