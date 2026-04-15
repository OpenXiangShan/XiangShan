package utility

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChiselDBTest extends AnyFlatSpec with Matchers {
  behavior of "ChiselDB"

  class DummyEntry extends Bundle {
    val value = UInt(8.W)
  }

  private def hasRealInit(cpp: String, tableName: String): Boolean = {
    cpp.contains(s"void init_db_${tableName}() {\n  // create table")
  }

  private def hasDummyInit(cpp: String, tableName: String): Boolean = {
    cpp.contains(s"void init_db_${tableName}() {}")
  }

  it should "only emit basic tables when ChiselDB is enabled" in {
    val disabledBasicTable = "UnitTestBasicDisabled"
    val disabledNonBasicTable = "UnitTestNonBasicDisabled"
    val enabledBasicTable = "UnitTestBasicEnabled"
    val enabledNonBasicTable = "UnitTestNonBasicEnabled"

    ChiselDB.init(false)
    ChiselDB.createTable(disabledBasicTable, new DummyEntry, basicDB = true)
    ChiselDB.createTable(disabledNonBasicTable, new DummyEntry, basicDB = false)
    val cppWhenDisabled = ChiselDB.getCpp

    hasDummyInit(cppWhenDisabled, disabledBasicTable) shouldBe true
    hasDummyInit(cppWhenDisabled, disabledNonBasicTable) shouldBe true

    ChiselDB.init(true)
    ChiselDB.createTable(enabledBasicTable, new DummyEntry, basicDB = true)
    ChiselDB.createTable(enabledNonBasicTable, new DummyEntry, basicDB = false)
    val cppWhenEnabled = ChiselDB.getCpp

    hasRealInit(cppWhenEnabled, enabledBasicTable) shouldBe true
    hasDummyInit(cppWhenEnabled, enabledBasicTable) shouldBe false
    hasDummyInit(cppWhenEnabled, enabledNonBasicTable) shouldBe true
  }
}
