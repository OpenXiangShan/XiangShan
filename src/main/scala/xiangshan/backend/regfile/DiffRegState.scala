/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.backend.regfile

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.issue._
import xiangshan.backend.datapath.DataConfig.VlData
import difftest._
import utility.RegNextN

class DiffRegState(implicit p: Parameters, params: BackendParams) extends XSModule {
  private val intSchdParams = params.schdParams(IntScheduler())
  private val fpSchdParams = params.schdParams(FpScheduler())
  private val vfSchdParams = params.schdParams(VfScheduler())
  private val memSchdParams = params.schdParams(MemScheduler())

  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))

    val intPRF = Input(Vec(intSchdParams.numPregs, UInt(intSchdParams.rfDataWidth.W)))
    val fpPRF  = Input(Vec(fpSchdParams.numPregs,  UInt(fpSchdParams.rfDataWidth.W)))
    val vfPRF  = Input(Vec(VLEN / XLEN, Vec(vfSchdParams.numPregs, UInt(XLEN.W))))
    val v0PRF  = Input(Vec(VLEN / XLEN, Vec(V0PhyRegs, UInt(XLEN.W))))
    val vlPRF  = Input(Vec(VlPhyRegs, UInt(VlData().dataWidth.W)))

    val intDiffTable = Input(Vec(32, UInt(PhyRegIdxWidth.W)))
    val fpDiffTable  = Input(Vec(32, UInt(PhyRegIdxWidth.W)))
    val vecDiffTable = Input(Vec(31, UInt(PhyRegIdxWidth.W)))
    val v0DiffTable  = Input(Vec(1,  UInt(PhyRegIdxWidth.W)))
    val vlDiffTable  = Input(Vec(1,  UInt(PhyRegIdxWidth.W)))

    val vecCSRState = Input(new Bundle {
      val vstart = UInt(64.W)
      val vxsat  = UInt(64.W)
      val vxrm   = UInt(64.W)
      val vcsr   = UInt(64.W)
      val vtype  = UInt(64.W)
      val vlenb  = UInt(64.W)
    })
  })

  private val v0PRFTrans = VecInit(io.v0PRF.transpose.map(x => VecInit(x)))
  private val v1to32PRFTrans = VecInit(io.vfPRF.transpose.map(x => VecInit(x)))
  private val v0DiffRegState = io.v0DiffTable.map(x => v0PRFTrans(x)).flatten
  private val v1to32DiffRegState = io.vecDiffTable.map(x => v1to32PRFTrans(x)).flatten

  private val intDiffRegState = io.intDiffTable.map(x => io.intPRF(x))
  private val fpDiffRegState  = io.fpDiffTable.map(x => io.fpPRF(x))
  private val vecDiffRegState = v0DiffRegState ++ v1to32DiffRegState
  private val vlDiffRegState  = io.vlDiffTable.map(x => io.vlPRF(x))

  private val difftestArchIntRegState = DifftestModule(new DiffArchIntRegState, delay = 2)
  difftestArchIntRegState.coreid := io.hartId
  difftestArchIntRegState.value := intDiffRegState

  private val difftestArchFpRegState = DifftestModule(new DiffArchFpRegState, delay = 2)
  difftestArchFpRegState.coreid := io.hartId
  difftestArchFpRegState.value := fpDiffRegState

  private val difftestArchVecRegState = DifftestModule(new DiffArchVecRegState, delay = 2)
  difftestArchVecRegState.coreid := io.hartId
  difftestArchVecRegState.value := vecDiffRegState

  private val diffVecCSRState = DifftestModule(new DiffVecCSRState)
  diffVecCSRState.coreid := io.hartId
  diffVecCSRState.vstart := io.vecCSRState.vstart
  diffVecCSRState.vxsat := io.vecCSRState.vxsat
  diffVecCSRState.vxrm := io.vecCSRState.vxrm
  diffVecCSRState.vcsr := io.vecCSRState.vcsr
  diffVecCSRState.vl := RegNextN(vlDiffRegState.head, 2)
  diffVecCSRState.vtype := io.vecCSRState.vtype
  diffVecCSRState.vlenb := io.vecCSRState.vlenb
}
