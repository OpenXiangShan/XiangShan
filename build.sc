import mill._, scalalib._
import coursier.maven.MavenRepository

object CustomZincWorkerModule extends ZincWorkerModule {
  def repositories() = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/releases"),
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
  )  
}

/**
 * Scala 2.12 module that is source-compatible with 2.11.
 * This is due to Chisel's use of structural types. See
 * https://github.com/freechipsproject/chisel3/issues/606
 */
trait HasXsource211 extends ScalaModule {
  override def scalacOptions = T {
    super.scalacOptions() ++ Seq(
      "-deprecation",
      "-unchecked",
      "-Xsource:2.11"
    )
  }
}

trait HasChisel3 extends ScalaModule {
  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.2.0-RC1"
 )
}

trait HasChiselTests extends CrossSbtModule  {
  object test extends Tests {
    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.0.4",
      ivy"edu.berkeley.cs::chisel-iotesters:1.2+",
      ivy"edu.berkeley.cs::chiseltest:0.2-SNAPSHOT"
    )
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}

trait HasMacroParadise extends ScalaModule {
  // Enable macro paradise for @chiselName et al
  val macroPlugins = Agg(ivy"org.scalamacros:::paradise:2.1.0")
  def scalacPluginIvyDeps = macroPlugins
  def compileIvyDeps = macroPlugins
}

object chiselModule extends CrossSbtModule with HasChisel3 with HasChiselTests with HasXsource211 with HasMacroParadise {
  def zincWorker = CustomZincWorkerModule
  def crossScalaVersion = "2.11.12"
}

