import mill._
import mill.modules.Util
import scalalib._
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`
import $ivy.`com.lihaoyi::mill-contrib-bsp:$MILL_VERSION`
import mill.contrib.buildinfo.BuildInfo
import $file.chisel3.build
import $file.firrtl.build
import $file.treadle.build
import $file.chiseltest.build
import $file.`berkeley-hardfloat`.build
import $file.`rocket-chip`.common
import $file.`api-config-chipsalliance`.`build-rules`.mill.build

val sv = "2.12.12"

object myfirrtl extends firrtl.build.firrtlCrossModule(sv) {
  override def millSourcePath = os.pwd / "firrtl"
}

object mychisel3 extends chisel3.build.chisel3CrossModule(sv) {
  override def millSourcePath = os.pwd / "chisel3"

  def firrtlModule: Option[PublishModule] = Some(myfirrtl)

  def treadleModule: Option[PublishModule] = Some(mytreadle)
}

object mytreadle extends treadle.build.treadleCrossModule(sv) {
  override def millSourcePath = os.pwd / "treadle"

  def firrtlModule: Option[PublishModule] = Some(myfirrtl)
}

object mychiseltest extends chiseltest.build.chiseltestCrossModule(sv) {
  override def scalaVersion = sv
  override def millSourcePath = os.pwd / "chiseltest"
  def chisel3Module: Option[PublishModule] = Some(mychisel3)
  def treadleModule: Option[PublishModule] = Some(mytreadle)
}

object myhardfloat extends `berkeley-hardfloat`.build.hardfloat {
  override def scalaVersion = sv

  def chisel3Module: Option[PublishModule] = Some(mychisel3)
}

object myconfig extends `api-config-chipsalliance`.`build-rules`.mill.build.config with PublishModule {
  override def scalaVersion = sv

  override def millSourcePath = os.pwd / "api-config-chipsalliance" / "design" / "craft"

  override def pomSettings = T {
    myrocketchip.pomSettings()
  }

  override def publishVersion = T {
    myrocketchip.publishVersion()
  }
}

object myrocketchip extends `rocket-chip`.common.CommonRocketChip {
  override def scalaVersion = sv

  override def millSourcePath = os.pwd / "rocket-chip"

  def chisel3Module: Option[PublishModule] = Some(mychisel3)

  def hardfloatModule: PublishModule = myhardfloat

  def configModule: PublishModule = myconfig
}


trait CommonModule extends ScalaModule {
  override def scalaVersion = sv

  override def scalacOptions = Seq("-Xsource:2.11")

  override def moduleDeps: Seq[ScalaModule] = Seq(mychisel3)

  private val macroParadise = ivy"org.scalamacros:::paradise:2.1.1"

  override def compileIvyDeps = Agg(macroParadise)

  override def scalacPluginIvyDeps = Agg(macroParadise)
}

object myinclusivecache extends CommonModule {
  override def millSourcePath = os.pwd / "block-inclusivecache-sifive" / "design" / "craft" / "inclusivecache"

  override def moduleDeps = super.moduleDeps ++ Seq(myrocketchip)
}

object myblocks extends CommonModule with SbtModule {
  override def moduleDeps = super.moduleDeps ++ Seq(myrocketchip)
}

object XiangShan extends CommonModule with SbtModule {
  override def millSourcePath = millOuterCtx.millSourcePath
  override def forkArgs = Seq("-Xmx10G")
  override def moduleDeps = super.moduleDeps ++ Seq(
    myrocketchip,
    myinclusivecache,
  )

  object test extends Tests {
    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.0",
    )
    override def moduleDeps = super.moduleDeps ++ Seq(
      mychiseltest
    )
    def testFrameworks = Seq(
      "org.scalatest.tools.Framework"
    )
  }
}
