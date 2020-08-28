import os.Path
import mill._
import mill.modules.Util
import scalalib._
import coursier.maven.MavenRepository

object CustomZincWorkerModule extends ZincWorkerModule {
  def repositories() = Seq(
    MavenRepository("https://maven.aliyun.com/repository/public"),
    MavenRepository("https://maven.aliyun.com/repository/apache-snapshots")
  )
}

trait CommonModule extends ScalaModule {
  override def scalaVersion = "2.12.10"

  override def scalacOptions = Seq("-Xsource:2.11")

  override def zincWorker = CustomZincWorkerModule

  private val macroParadise = ivy"org.scalamacros:::paradise:2.1.0"

  override def compileIvyDeps = Agg(macroParadise)

  override def scalacPluginIvyDeps = Agg(macroParadise)
}

object `rocket-chip` extends SbtModule with CommonModule {

  val rocketChisel = Agg(
    ivy"edu.berkeley.cs::chisel3:3.3.1"
  )

  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"${scalaOrganization()}:scala-reflect:${scalaVersion()}",
    ivy"org.json4s::json4s-jackson:3.6.1"
  ) ++ rocketChisel


  object `api-config-chipsalliance` extends CommonModule {
    override def millSourcePath = super.millSourcePath / 'design / 'craft
  }

  object macros extends SbtModule with CommonModule

  object hardfloat extends SbtModule with CommonModule {
    override def ivyDeps = super.ivyDeps() ++ rocketChisel
  }

  override def moduleDeps = super.moduleDeps ++ Seq(
    `api-config-chipsalliance`, macros, hardfloat
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

