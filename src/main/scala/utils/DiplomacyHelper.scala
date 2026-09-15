package utils

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.HashSet

object DiplomacyGlobalView {

  def dump(enableNodes: Boolean = true, enableVerbose: Boolean = false, enableRoutes: Boolean = true): Unit = {

    // print diplomacy global nodes
    if (enableNodes) {

      println("[Diplomacy] ========================")
      println(s"[Diplomacy] ${BaseNodeGlobalView.nodes.size} nodes spawned in total.")
      BaseNodeGlobalView.nodes.foreach(n =>
        println(s"  ${n.name} ${n.valName.name} [${n.lazyModule.instanceName}:${n.lazyModule.moduleName}:${n.lazyModule.line}] ${n.getClass().getName()} ${n.hashCode}"))
      println("[Diplomacy] ------------------------")
      println(s"[Diplomacy] All top terminal (input, source) nodes:")
      BaseNodeGlobalView.nodes.filter(_.inputs.isEmpty).foreach(n =>
        println(s"  ${n.name} ${n.valName.name} [${n.lazyModule.instanceName}:${n.lazyModule.moduleName}:${n.lazyModule.line}] ${n.getClass().getName()} ${n.hashCode}"))
      println("[Diplomacy] ------------------------")
      println(s"[Diplomacy] All bottom terminal (output, sink) nodes:")
      BaseNodeGlobalView.nodes.filter(_.outputs.isEmpty).foreach(n =>
        println(s"  ${n.name} ${n.valName.name} [${n.lazyModule.instanceName}:${n.lazyModule.moduleName}:${n.lazyModule.line}] ${n.getClass().getName()} ${n.hashCode}"))
      println("[Diplomacy] ========================")
    }

    // print diplomacy verbose tree with all connected nodes
    if (enableVerbose) {

      def space(depth: Int) = (0 until depth).foldLeft("")((s: String, i: Int) => { s ++ " " })

      def dfsOutputTree(node: BaseNode, depth: Int = 0): Unit = {
        println(s"[Diplomacy] ${space(depth)}-> (${depth}) ${node.name} ${node.valName.name} ${node.hashCode}")
        node.outputs.map(_._1).foreach(n => dfsOutputTree(n, depth + 1))
      }

      println("[Diplomacy] ========================")
      BaseNodeGlobalView.nodes.filter(_.inputs.isEmpty).foreach { n =>
        println(s"[Diplomacy] Generated verbose tree from root (input, source) -> ${n.name} ${n.valName.name} ${n.hashCode}:")
        dfsOutputTree(n)
        println("[Diplomacy] ------------------------")
      }
      println("[Diplomacy] ========================")
    }

    // print diplomacy global routes
    if (enableRoutes) {

      val pa48 = AddressSet(0x0, 0xFFFF_FFFFFFFFL)
      val pa48Ranges = ArrayBuffer[(BaseNode, AddressRange)]()

      def space(depth: Int) = (0 until depth).foldLeft("")((s: String, i: Int) => { s ++ " " })

      def intersect2D(enable: Boolean, base: Seq[AddressSet], factor: Seq[AddressSet]): Seq[AddressSet] = {
        if (!enable || factor.isEmpty)
          base
        else
          AddressSet.unify(base.foldLeft(Seq[AddressSet]())((bseq, ba) => {
            bseq ++ factor.foldLeft(Seq[AddressSet]())((fseq, fa) => {
              val a = ba intersect fa
              if (a.isDefined) fseq :+ a.get else fseq
          })}))
      }

      def dfsRouteFromInput(node: BaseNode, fold: Boolean = true, mapped: Seq[AddressSet] = Seq(), depthIn: Int = 0, tunnel: Option[BaseNode] = None): Unit = {

        var depth = depthIn
        if (depth == 0) {
          println(s"[Diplomacy]   -> (0) ${node.name} ${node.valName.name} ${node.hashCode}")
          depth = depth + 1
        }

        // pick pathes with this input node
        XbarGlobalView.xbars.filter(_.routes.foldLeft(false)((b, p) => b | p.foldLeft(false)((b, r) => b | (r._1 == node))))
          // collect addresses for seperate nodes
          //                                       self                                     fanout    addresses
          .foldLeft(scala.collection.immutable.Map[BaseNode, scala.collection.immutable.Map[BaseNode, Seq[AddressSet]]]())(
            (mapSelf, view) => mapSelf + ( (view.self, view.routes.zip(view.outputSegs)
              .foldLeft(mapSelf.getOrElse(view.self, scala.collection.immutable.Map[BaseNode, Seq[AddressSet]]()))((mapFanout, ro) => {
                mapFanout + ( (ro._2, mapFanout.getOrElse(ro._2, Seq[AddressSet]()) ++ ro._1.filter(r => r._1 == node).map(_._2)) )
              }))
            ))
          // search seperate path (selected by tunnel vision if specified) with collected addresses and outputs
          .filter(p => tunnel.map(_ == p._1).getOrElse(true)).foreach { case (self, mapFanout) =>

            // print router (crossbar)
            print(s"[Diplomacy]   ${space(depth)}-> (${depth}) ${self.name} ${self.valName.name} ${self.hashCode}")
            println()

            // search router outputs
            mapFanout.foreach { r =>
              if (r._1.name.equals("xbar.node")) {
                // dfs next tunneled router
                dfsRouteFromInput(self, fold, intersect2D(fold, r._2, mapped), depth + 1, Some(r._1))
              } else if (r._1.isInstanceOf[TLNexusNode]) {
                if (false) {
                  // distinguish custom nexus nodes
                } else {
                  // print unrecognized nexus nodes
                  print(s"[Diplomacy]   ${space(depth + 1)}-> (${depth + 1}) (unknown nexus node) ${r._1.name} ${r._1.valName.name} ${r._1.hashCode}")
                  println()
                }
              } else {
                // print next non-rounter terminal
                print(s"[Diplomacy]   ${space(depth + 1)}-> (${depth + 1}) ${r._1.name} ${r._1.valName.name} ${r._1.hashCode}")
                intersect2D(fold, r._2, mapped).foreach { a =>
                  print(s"\n[Diplomacy]   ${space(depth + 1)} +  |  ${a}")
                  pa48Ranges ++= a.intersect(pa48).map(_.toRanges.map((r._1, _))).getOrElse(Seq())
                }
                println()
              }
            }
          }
      }

      println("[Diplomacy] ========================")
      println("[Diplomacy] All routes under scope of input terminals:")
      XbarGlobalView.xbars.foldLeft(HashSet[BaseNode]())((set, view) => set ++ view.inputTerminals)
        .foreach(t => {
          println("[Diplomacy] - - - - - - - - - - - -")
          dfsRouteFromInput(t)
          println("[Diplomacy]")
          println("[Diplomacy]   As flatten PA48 ranges:")
          println(s"[Diplomacy]   -> ${t.name} ${t.valName.name} ${t.hashCode}")
          pa48Ranges.sortWith((a, b) => a._2.base < b._2.base).foldLeft(Seq[(Option[BaseNode], (BigInt, BigInt))]())((seq, range) => {
            if (!seq.isEmpty) {
              seq.last._1 match {
                case Some(last) => {
                  if (seq.last._2._2 == range._2.base)
                    if (last == range._1)
                      seq.init :+ ((Some(last), (seq.last._2._1, range._2.end)))
                    else
                      seq :+ ((Some(range._1), (range._2.base, range._2.end)))
                  else
                    seq :+ ((None, (seq.last._2._2, range._2.base))) :+ ((Some(range._1), (range._2.base, range._2.end)))
                }
                case None => {
                  if (seq.last._2._2 == range._2.base)
                    seq :+ ((Some(range._1), (range._2.base, range._2.end)))
                  else
                    seq.init :+ ((None, (seq.last._2._1, range._2.base))) :+ ((Some(range._1), (range._2.base, range._2.end)))
                }
            }} else
              seq :+ ((Some(range._1), (range._2.base, range._2.end)))
          }).foreach(r => r._1 match {
            case Some(n) =>
              println(s"[Diplomacy]     ${"[0x%016X - 0x%016X]".format(r._2._1, r._2._2)} ${n.name} ${n.valName.name} ${n.hashCode}")
            case None =>
              println(s"[Diplomacy]     ${"[0x%016X - 0x%016X]".format(r._2._1, r._2._2)} <! check address blackhole or overlap !>")
          })
          pa48Ranges.clear()
        })
      println("[Diplomacy] ========================")
    }
  }
}
