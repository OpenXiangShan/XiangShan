import os.Path
import mill._
import mill.modules.Util
import scalalib._
import coursier.maven.MavenRepository
import $file.`rocket-chip`.chisel3.build
import $file.`rocket-chip`.firrtl.build

object CustomZincWorkerModule extends ZincWorkerModule {
  def repositories() = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/releases"),
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
  )
}

trait CommonModule extends ScalaModule
{
  override def scalaVersion = "2.12.10"
  override def scalacOptions = Seq("-Xsource:2.11")
  override def zincWorker = CustomZincWorkerModule
  private val macroParadise = ivy"org.scalamacros:::paradise:2.1.0"
  override def compileIvyDeps = Agg(macroParadise)
  override def scalacPluginIvyDeps = Agg(macroParadise)
}

object `rocket-chip` extends SbtModule with CommonModule {

  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"${scalaOrganization()}:scala-reflect:${scalaVersion()}",
    ivy"org.json4s::json4s-jackson:3.6.1"
  )

  object rocketFirrtl extends $file.`rocket-chip`.firrtl.build.firrtlCrossModule("2.12.11") {
    override def millSourcePath = super.millSourcePath / 'firrtl
  }

  object rocketChisel extends $file.`rocket-chip`.chisel3.build.chisel3CrossModule("2.12.11") {
    override def millSourcePath = super.millSourcePath / 'chisel3
    def firrtlModule: Option[PublishModule] = Some(rocketFirrtl)
  }


  object `api-config-chipsalliance` extends CommonModule {
    override def millSourcePath = super.millSourcePath / 'design / 'craft
  }

  object macros extends SbtModule with CommonModule

  object hardfloat extends SbtModule with CommonModule {
    override def moduleDeps = super.moduleDeps ++ Seq(rocketChisel)
  }

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketChisel, `api-config-chipsalliance`, macros, hardfloat
  )

}

object XiangShan extends CommonModule with SbtModule {
  override def millSourcePath = millOuterCtx.millSourcePath
  override def forkArgs = Seq("-Xmx10G")
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"edu.berkeley.cs::chisel3:3.3.2"
  )
  override def moduleDeps = super.moduleDeps ++ Seq(`rocket-chip`)

  object test extends Tests {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.scalatest::scalatest:3.0.4",
      ivy"edu.berkeley.cs::chisel-iotesters:1.2+",
      ivy"edu.berkeley.cs::chiseltest:0.2.1"
    )

    def testFrameworks = Seq(
      "org.scalatest.tools.Framework"
    )

    def testOnly(args: String*) = T.command {
      super.runMain("org.scalatest.tools.Runner", args: _*)
    }
  }

}

