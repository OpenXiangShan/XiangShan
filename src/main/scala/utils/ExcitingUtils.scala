package chisel3

import chisel3.internal.NamedComponent
import chisel3.util.experimental.BoringUtils
import scala.collection.mutable

object ExcitingUtils {

  object ConnectionType extends Enumeration {
    val Perf = Value("perf")
    val Func = Value("func")
    val Debug = Value("debug")
  }

  type ConnectionType = ConnectionType.Value
  val Perf = ConnectionType.Perf
  val Func = ConnectionType.Func
  val Debug = ConnectionType.Debug

  private def strToErrorMsg(str: String) = "\u001b[1m\u001b[;31m"+ str +"\u001b[0m"

  private class Connection
  (
    var connType: ConnectionType,
    var sourceModule: Option[String] = None,
    var sinkModule: Option[String] = None
  ){

    override def toString: String =
      s"type:[$connType] source location:[${sourceModule.getOrElse(strToErrorMsg("Not Found"))}]" +
        s" sink location:[${sinkModule.getOrElse(strToErrorMsg("Not Found"))}]"

    def isLeagleConnection: Boolean = sourceModule.nonEmpty && sinkModule.nonEmpty
  }

  private val map = mutable.LinkedHashMap[String, Connection]()

  def addSource
  (
    component: NamedComponent,
    name: String,
    connType: ConnectionType = Func,
    disableDedup: Boolean = false,
    uniqueName: Boolean = false
  ): String = {
    val conn = map.getOrElseUpdate(name, new Connection(connType))
    require(conn.connType == connType)
    conn.sourceModule = Some(component.parentModName)
    BoringUtils.addSource(component, name, disableDedup, uniqueName)
  }

  def addSink
  (
    component: InstanceId,
    name: String,
    connType: ConnectionType = Func,
    disableDedup: Boolean = false,
    forceExists: Boolean = false
  ): Unit = {
    val conn = map.getOrElseUpdate(name, new Connection(connType))
    require(conn.connType == connType)
    conn.sinkModule = Some(component.parentModName)
    BoringUtils.addSink(component, name, disableDedup, forceExists)
  }

  def fixConnections(): Unit ={
    val dontCare = WireInit(0.U)
    for((name, conn) <- map){
      if(conn.sinkModule.isEmpty){
        addSink(dontCare, name, conn.connType)
      }
      if(conn.sourceModule.isEmpty){
        addSource(dontCare, name, conn.connType)
      }
    }
  }


  def checkAndDisplay(): Unit = {
    var leagle = true
    val buf = new mutable.StringBuilder()
    for((id, conn) <- map){
      buf ++= s"Connection:[$id] $conn\n"
      if(!conn.isLeagleConnection) leagle = false
    }
    print(buf)
    require(leagle, strToErrorMsg("Error: Illeagle connection found!"))
  }

}
