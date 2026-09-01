package oceanus.compactchi

/* 
* Enumeration of CCHI Channels.
*/
sealed class CCHIChannelType(ordinal              : Int,
                             name                 : String,
                             val maxOpcodeWidth   : Int,
                             val canonicalName    : String)
  extends Enum[CCHIChannelType](name, ordinal)

object CCHIChannelType {

  val EVT: CCHIChannelType        = new CCHIChannelType(0, "EVT"  , 1, "EVT")
  val SNP: CCHIChannelType        = new CCHIChannelType(1, "SNP"  , 2, "SNP")
  val REQ: CCHIChannelType        = new CCHIChannelType(2, "REQ"  , 6, "REQ")
  val DnDAT: CCHIChannelType      = new CCHIChannelType(3, "DnDAT", 2, "DAT from Downstream")
  val DnRSP: CCHIChannelType      = new CCHIChannelType(4, "DnRSP", 3, "RSP from Downstream")
  val UpDAT: CCHIChannelType      = new CCHIChannelType(5, "UpDAT", 2, "DAT from Upstream")
  val UpRSP: CCHIChannelType      = new CCHIChannelType(6, "UpRSP", 1, "RSP from Upstream")
}