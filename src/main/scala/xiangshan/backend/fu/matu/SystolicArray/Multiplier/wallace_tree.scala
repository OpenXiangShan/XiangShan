package matu.SystolicArray.Multiplier

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Cat

class wallaceTree (val C_WIDTH: Int, val IN_CHANNEL: Int) extends Module{
  val io = IO(new Bundle {
    val data_i = Input(Vec(IN_CHANNEL, SInt(C_WIDTH.W)))
    val data_o = Output(SInt(C_WIDTH.W))
  })

  val rca = Module(new RCA(C_WIDTH))

  // remind: only support 1~7 layers csa. if layers > 7, please extend code to support.

  val L1_CSA_N = IN_CHANNEL/3
  val L1_INP_N = 3*L1_CSA_N
  val L1_RES_N = IN_CHANNEL-L1_INP_N
  val L1_OUT_N = 2*L1_CSA_N + L1_RES_N

  val L2_CSA_N = L1_OUT_N/3
  val L2_INP_N = 3*L2_CSA_N
  val L2_RES_N = L1_OUT_N - L2_INP_N
  val L2_OUT_N = 2*L2_CSA_N + L2_RES_N

  val L3_CSA_N = L2_OUT_N/3
  val L3_INP_N = 3*L3_CSA_N
  val L3_RES_N = L2_OUT_N - L3_INP_N
  val L3_OUT_N = 2*L3_CSA_N + L3_RES_N

  val L4_CSA_N = L3_OUT_N/3
  val L4_INP_N = 3*L4_CSA_N
  val L4_RES_N = L3_OUT_N - L4_INP_N
  val L4_OUT_N = 2*L4_CSA_N + L4_RES_N

  val L5_CSA_N = L4_OUT_N/3
  val L5_INP_N = 3*L5_CSA_N
  val L5_RES_N = L4_OUT_N - L5_INP_N
  val L5_OUT_N = 2*L5_CSA_N + L5_RES_N

  val L6_CSA_N = L5_OUT_N/3
  val L6_INP_N = 3*L6_CSA_N
  val L6_RES_N = L5_OUT_N - L6_INP_N
  val L6_OUT_N = 2*L6_CSA_N + L6_RES_N

  val L7_CSA_N = L6_OUT_N/3
  val L7_INP_N = 3*L7_CSA_N
  val L7_RES_N = L6_OUT_N - L7_INP_N
  val L7_OUT_N = 2*L7_CSA_N + L7_RES_N

  val rca_in = WireInit(VecInit(Seq.fill(2)(0.S(C_WIDTH.W))))

  // layer1
  val l1_csa_in  = WireInit(VecInit(Seq.fill(L1_INP_N)(0.S(C_WIDTH.W))))
  val l1_csa_os  = WireInit(VecInit(Seq.fill(L1_CSA_N)(0.S(C_WIDTH.W))))
  val l1_csa_oc  = WireInit(VecInit(Seq.fill(L1_CSA_N)(0.S(C_WIDTH.W))))
  val l1_csa_out = WireInit(VecInit(Seq.fill(L1_OUT_N)(0.S(C_WIDTH.W))))
  val layer1CSA = Seq.fill(L1_CSA_N)(Module(new pp_compressor3_2(C_WIDTH)))

  for(i <- 0 until L1_INP_N){l1_csa_in(i) := io.data_i(i)}
  for (i <- 0 until L1_CSA_N){
    layer1CSA(i).input.pp0_in := l1_csa_in(3 * i)
    layer1CSA(i).input.pp1_in := l1_csa_in(3 * i + 1)
    layer1CSA(i).input.pp2_in := l1_csa_in(3 * i + 2)
    l1_csa_os(i)              := layer1CSA(i).output.S
    l1_csa_oc(i)              := layer1CSA(i).output.C
    l1_csa_out(2 * i)           := l1_csa_os(i)
    l1_csa_out(2 * i + 1)         := l1_csa_oc(i)
  }
  if(L1_RES_N != 0) {
    for(i <- 0 until L1_RES_N){
      l1_csa_out(L1_OUT_N-1-i) := io.data_i(IN_CHANNEL-1-i)
    }
  }
  if(L1_OUT_N == 2) {
    rca_in := l1_csa_out
  } else {

    // layer2
    val l2_csa_in = WireInit(VecInit(Seq.fill(L2_INP_N)(0.S(C_WIDTH.W))))
    val l2_csa_os = WireInit(VecInit(Seq.fill(L2_CSA_N)(0.S(C_WIDTH.W))))
    val l2_csa_oc = WireInit(VecInit(Seq.fill(L2_CSA_N)(0.S(C_WIDTH.W))))
    val l2_csa_out = WireInit(VecInit(Seq.fill(L2_OUT_N)(0.S(C_WIDTH.W))))
    val layer2CSA = Seq.fill(L2_CSA_N)(Module(new pp_compressor3_2(C_WIDTH)))

    for (i <- 0 until L2_INP_N){l2_csa_in(i) := l1_csa_out(i)}
    for (i <- 0 until L2_CSA_N){
      layer2CSA(i).input.pp0_in := l2_csa_in(3 * i)
      layer2CSA(i).input.pp1_in := l2_csa_in(3 * i + 1)
      layer2CSA(i).input.pp2_in := l2_csa_in(3 * i + 2)
      l2_csa_os(i)              := layer2CSA(i).output.S
      l2_csa_oc(i)              := layer2CSA(i).output.C
      l2_csa_out(2 * i)           := l2_csa_os(i)
      l2_csa_out(2 * i + 1)         := l2_csa_oc(i)
    }
    if (L2_RES_N != 0) {
      for (i <- 0 until L2_RES_N){
        l2_csa_out(L2_OUT_N-1-i) := l1_csa_out(L1_OUT_N-1-i)
      }
    }
    if (L2_OUT_N == 2) {
      rca_in := l2_csa_out
    } else {

      // layer3
      val l3_csa_in = WireInit(VecInit(Seq.fill(L3_INP_N)(0.S(C_WIDTH.W))))
      val l3_csa_os = WireInit(VecInit(Seq.fill(L3_CSA_N)(0.S(C_WIDTH.W))))
      val l3_csa_oc = WireInit(VecInit(Seq.fill(L3_CSA_N)(0.S(C_WIDTH.W))))
      val l3_csa_out = WireInit(VecInit(Seq.fill(L3_OUT_N)(0.S(C_WIDTH.W))))
      val layer3CSA = Seq.fill(L3_CSA_N)(Module(new pp_compressor3_2(C_WIDTH)))

      for (i <- 0 until L3_INP_N){l3_csa_in(i) := l2_csa_out(i)}
      for (i <- 0 until L3_CSA_N) {
        layer3CSA(i).input.pp0_in := l3_csa_in(3 * i)
        layer3CSA(i).input.pp1_in := l3_csa_in(3 * i + 1)
        layer3CSA(i).input.pp2_in := l3_csa_in(3 * i + 2)
        l3_csa_os(i) := layer3CSA(i).output.S
        l3_csa_oc(i) := layer3CSA(i).output.C
        l3_csa_out(2 * i) := l3_csa_os(i)
        l3_csa_out(2 * i + 1) := l3_csa_oc(i)
      }
      if (L3_RES_N != 0) {
        for (i <- 0 until L3_RES_N) {
          l3_csa_out(L3_OUT_N - 1 - i) := l2_csa_out(L2_OUT_N - 1 - i)
        }
      }
      if (L3_OUT_N == 2) {
        rca_in := l3_csa_out
      } else {

        // layer4

        val l4_csa_in = WireInit(VecInit(Seq.fill(L4_INP_N)(0.S(C_WIDTH.W))))
        val l4_csa_os = WireInit(VecInit(Seq.fill(L4_CSA_N)(0.S(C_WIDTH.W))))
        val l4_csa_oc = WireInit(VecInit(Seq.fill(L4_CSA_N)(0.S(C_WIDTH.W))))
        val l4_csa_out = WireInit(VecInit(Seq.fill(L4_OUT_N)(0.S(C_WIDTH.W))))
        val layer4CSA = Seq.fill(L4_CSA_N)(Module(new pp_compressor3_2(C_WIDTH)))

        for (i <- 0 until L4_INP_N) {l4_csa_in(i) := l3_csa_out(i)}
        for (i <- 0 until L4_CSA_N) {
          layer4CSA(i).input.pp0_in := l4_csa_in(3 * i)
          layer4CSA(i).input.pp1_in := l4_csa_in(3 * i + 1)
          layer4CSA(i).input.pp2_in := l4_csa_in(3 * i + 2)
          l4_csa_os(i)              := layer4CSA(i).output.S
          l4_csa_oc(i)              := layer4CSA(i).output.C
          l4_csa_out(2 * i)         := l4_csa_os(i)
          l4_csa_out(2 * i + 1)     := l4_csa_oc(i)
        }
        if (L4_RES_N != 0) {
          for (i <- 0 until L4_RES_N) {
            l4_csa_out(L4_OUT_N - 1 - i) := l3_csa_out(L3_OUT_N - 1 - i)
          }
        }
        if (L4_OUT_N == 2) {
          rca_in := l4_csa_out
        } else {

          // layer5

          val l5_csa_in = WireInit(VecInit(Seq.fill(L5_INP_N)(0.S(C_WIDTH.W))))
          val l5_csa_os = WireInit(VecInit(Seq.fill(L5_CSA_N)(0.S(C_WIDTH.W))))
          val l5_csa_oc = WireInit(VecInit(Seq.fill(L5_CSA_N)(0.S(C_WIDTH.W))))
          val l5_csa_out = WireInit(VecInit(Seq.fill(L5_OUT_N)(0.S(C_WIDTH.W))))
          val layer5CSA = Seq.fill(L5_CSA_N)(Module(new pp_compressor3_2(C_WIDTH)))

          for (i <- 0 until L5_INP_N) {l5_csa_in(i) := l4_csa_out(i)}
          for (i <- 0 until L5_CSA_N) {
            layer5CSA(i).input.pp0_in := l5_csa_in(3 * i)
            layer5CSA(i).input.pp1_in := l5_csa_in(3 * i + 1)
            layer5CSA(i).input.pp2_in := l5_csa_in(3 * i + 2)
            l5_csa_os(i) := layer5CSA(i).output.S
            l5_csa_oc(i) := layer5CSA(i).output.C
            l5_csa_out(2 * i) := l5_csa_os(i)
            l5_csa_out(2 * i + 1) := l5_csa_oc(i)
          }
          if (L5_RES_N != 0) {
            for (i <- 0 until L5_RES_N) {
              l5_csa_out(L5_OUT_N - 1 - i) := l4_csa_out(L4_OUT_N - 1 - i)
            }
          }
          if (L5_OUT_N == 2) {
            rca_in := l5_csa_out
          } else {

            // layer6

            val l6_csa_in = WireInit(VecInit(Seq.fill(L6_INP_N)(0.S(C_WIDTH.W))))
            val l6_csa_os = WireInit(VecInit(Seq.fill(L6_CSA_N)(0.S(C_WIDTH.W))))
            val l6_csa_oc = WireInit(VecInit(Seq.fill(L6_CSA_N)(0.S(C_WIDTH.W))))
            val l6_csa_out = WireInit(VecInit(Seq.fill(L6_OUT_N)(0.S(C_WIDTH.W))))
            val layer6CSA = Seq.fill(L6_CSA_N)(Module(new pp_compressor3_2(C_WIDTH)))

            for (i <- 0 until L6_INP_N) {l6_csa_in(i) := l5_csa_out(i)}
            for (i <- 0 until L6_CSA_N) {
              layer6CSA(i).input.pp0_in := l6_csa_in(3 * i)
              layer6CSA(i).input.pp1_in := l6_csa_in(3 * i + 1)
              layer6CSA(i).input.pp2_in := l6_csa_in(3 * i + 2)
              l6_csa_os(i) := layer6CSA(i).output.S
              l6_csa_oc(i) := layer6CSA(i).output.C
              l6_csa_out(2 * i) := l6_csa_os(i)
              l6_csa_out(2 * i + 1) := l6_csa_oc(i)
            }
            if (L6_RES_N != 0) {
              for (i <- 0 until L6_RES_N) {
                l6_csa_out(L6_OUT_N - 1 - i) := l5_csa_out(L5_OUT_N - 1 - i)
              }
            }
            if (L6_OUT_N == 2) {
              rca_in := l6_csa_out
            } else {

              // layer7

              val l7_csa_in = WireInit(VecInit(Seq.fill(L7_INP_N)(0.S(C_WIDTH.W))))
              val l7_csa_os = WireInit(VecInit(Seq.fill(L7_CSA_N)(0.S(C_WIDTH.W))))
              val l7_csa_oc = WireInit(VecInit(Seq.fill(L7_CSA_N)(0.S(C_WIDTH.W))))
              val l7_csa_out = WireInit(VecInit(Seq.fill(L7_OUT_N)(0.S(C_WIDTH.W))))
              val layer7CSA = Seq.fill(L7_CSA_N)(Module(new pp_compressor3_2(C_WIDTH)))

              for (i <- 0 until L7_INP_N) {
                l7_csa_in(i) := l6_csa_out(i)
              }
              for (i <- 0 until L7_CSA_N) {
                layer7CSA(i).input.pp0_in := l7_csa_in(3 * i)
                layer7CSA(i).input.pp1_in := l7_csa_in(3 * i + 1)
                layer7CSA(i).input.pp2_in := l7_csa_in(3 * i + 2)
                l7_csa_os(i) := layer7CSA(i).output.S
                l7_csa_oc(i) := layer7CSA(i).output.C
                l7_csa_out(2 * i) := l7_csa_os(i)
                l7_csa_out(2 * i + 1) := l7_csa_oc(i)
              }
              if (L7_RES_N != 0) {
                for (i <- 0 until L7_RES_N) {
                  l7_csa_out(L7_OUT_N - 1 - i) := l6_csa_out(L6_OUT_N - 1 - i)
                }
              }
              if (L7_OUT_N == 2) {
                rca_in := l7_csa_out
              } else {
                rca_in := Vec(2, 0.U)
              }
            }
          }
        }
      }
    }
  }

  rca.input.a_in := rca_in(0)
  rca.input.b_in := rca_in(1)
  rca.input.c_in := 0.S
  io.data_o      := rca.output.S
}